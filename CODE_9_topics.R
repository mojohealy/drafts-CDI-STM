# LOAD PACKAGES
# For data processing and analysis
library(bibliometrix)
library(stm)
library(tidystm)
library(quanteda)
library(cld3)
library(spacyr)
library(tidytext)
library(tidyverse)
library(stminsights)
library(writexl)
library(flextable)

# For tables and figures
library(RColorBrewer)
library(cowplot)
library(flextable)
library(officer)
library(igraph)
library(ggraph)

# Initialise spacy
spacy_initialize()

# LOAD PACKAGES
# For data processing and analysis
library(bibliometrix)
library(stm)
library(tidystm)
library(quanteda)
library(cld3)
library(spacyr)
library(tidytext)
library(tidyverse)
library(stminsights)
library(writexl)
library(flextable)

# For tables and figures
library(RColorBrewer)
library(cowplot)
library(flextable)
library(officer)
library(igraph)
library(ggraph)


# Initialise spacy
spacy_initialize()


# READ DATA
# Index data files in data folders
scopus_data <-
  list.files(
    path = ("data"),
    recursive = TRUE,
    pattern = "\\.csv$",
    full.names = TRUE
  )


# Read and convert to bibliographic dataframe
full_data <- convert2df(scopus_data, "scopus", "csv")



# Normalise van der Heijden and van der Heijde
full_data$AU <- str_replace_all(full_data$AU, 
                                "VAN DER HEIJDEN BIJM|HEIJDEN BIJMVD|MARIA VAN DER HEIJDEN BIJ|IJM VAN DER HEIJDEN B", 
                                "VAN DER HEIJDEN B")

# PRE-PROCESSING DATA FOR TOPIC MODELLING
# Clean abstracts 
# Remove rows with no abstract 
full_data_clean <- full_data %>%
  filter(!is.na(AB) & AB != "[NO ABSTRACT AVAILABLE]")

# Segment abstracts by sentence
full_data_sentences <- full_data_clean %>%
  select(AB, SR, TI, PY, SO, AU, DI, TC, Affiliations) %>%
  separate_longer_delim(AB, ".") 

# Remove copyright information
full_data_sentences <- full_data_sentences %>%
  filter(
    !grepl("©", AB) &
      !grepl("ALL RIGHTS RESERVED", AB) &
      !grepl("ALL RIGHT RESERVED", AB) &
      !grepl("COPYRIGHT", AB) &
      !grepl("LTD", AB) &
      !grepl(" LLC", AB) &
      !grepl(" PTY ", AB) &
      !grepl("WILEY", AB) &
      !grepl("TAYLOR & FRANCIS", AB) &
      !grepl("ELSEVIER", AB)
  ) %>%
  filter(!startsWith(AB, prefix = " PUBLISHED BY "))

# Select only sentences in English
full_data_sentences <-
  subset(full_data_sentences,
         detect_language(full_data_sentences$AB) == "en")

# Rejoin sentences into full abstracts
full_data_clean <-
  full_data_sentences %>% group_by(SR, TI, PY, SO, AU, DI, TC, Affiliations) %>%
  summarise(AB = paste0(AB, collapse = ".")) %>%
  ungroup()

# Seperate authors in AU field
full_data_clean <- full_data_clean %>%
  separate_wider_delim(
    cols = AU,
    delim = ";",
    names_sep = "",
    too_few = "align_start"
  )

# Create ref column

full_data_clean <- full_data_clean %>%
  mutate(
    Title_Short = case_when(
      # Handle missing titles
      is.na(TI) ~ NA_character_,
      
      # Split on colon or question mark
      str_detect(TI, "[:?]") ~ str_trim(str_extract(TI, "^[^:?]+")),
      
      # Split on dash only if it's clearly separating clauses
      # (dash with spaces around it, or dash followed by capital letter indicating new clause)
      str_detect(TI, "\\s+[—–‒-]+\\s+[A-Z]") ~ str_trim(str_extract(TI, "^[^—–‒-]+(?=\\s+[—–‒-]+\\s+[A-Z])")),
      str_detect(TI, "\\s+[—–‒-]+\\s+") ~ str_trim(str_extract(TI, "^[^—–‒-]+(?=\\s+[—–‒-]+\\s+)")),
      
      # Use full title if no clear separators
      TRUE ~ str_trim(TI)
    ),
    
    # Create ref column with error handling
    ref = case_when(
      is.na(AU1) | is.na(PY) | is.na(Title_Short) ~ NA_character_,
      TRUE ~ paste(str_trim(AU1), str_trim(as.character(PY)), Title_Short, sep = ", ")
    )
  ) %>%
  select(-Title_Short)


# Assign custom stopwords from abstract meta language
custom_stopwords <-
  c(
    "abstract",
    "aim",
    "aims",
    "analyse",
    "analysis",
    "approach",
    "argue",
    "argues",
    "article",
    "author",
    "authors",
    "available",
    "background",
    "conclude",
    "conclusion",
    "conclusions",
    "contribution",
    "contributions",
    "data",
    "describe",
    "describes",
    "design",
    "discuss",
    "employability",
    "examine",
    "examines",
    "explore",
    "explores",
    "finding",
    "findings",
    "high",
    "implication",
    "implications",
    "investigate",
    "investigates",
    "limitations",
    "method",
    "methodology",
    "model",
    "originality",
    "outlines",
    "paper",
    "provide",
    "purpose",
    "report",
    "research",
    "result",
    "results",
    "sample",
    "study",
    "suggest",
    "survey"
  )

# Tokenise and lemmatise abstracts; remove stopwords and custom stopwords, remove words less than three letters

abstract_tokens <-
  spacy_parse(full_data_clean$AB,
              lemma = TRUE,
              entity = FALSE,
              pos = FALSE)

abstract_tokens <- abstract_tokens %>% as.tokens(use_lemma = T)

abstract_tokens <- abstract_tokens %>%
  tokens(
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    split_hyphens = TRUE
  ) %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_remove(custom_stopwords) %>%
  tokens_remove() %>%
  tokens_tolower() %>%
  tokens_keep(min_nchar = 3) %>%
  tokens_remove(pattern = "[[:digit:]]", valuetype = "regex")

# Create document-feature matrix
dfm <- dfm(abstract_tokens)


# Convert DFM to STM format with metadata
stm <- convert(dfm, to = "stm", docvars = full_data_clean)



# MODELLING
# Model nine topics
set.seed(0909)
nine_topic_model <-
  stm(
    documents = stm$documents,
    vocab = stm$vocab,
    K = 9,
    verbose = TRUE,
    data = stm$meta
  )

# EVALUATE MEANING OF MODEL
# Label model keywords
nine_topics_keywords <- labelTopics(nine_topic_model, n = 20)

# Find representative titles
nine_topics_texts <- findThoughts(nine_topic_model, stm$meta$TI, n = 25)

# ILLUSTRATE 9 TOPIC MODEL
# Assign topics to vector
nine_topics <- c("1. Job insecurity, engagement, and proactive behaviours", 
                   "2. Work identity and temporary or precarious employment", 
                   "3. Recruitment and talent management", 
                   "4. Women’s careers and sustainable development", 
                   "5. Mentoring, relationships, and career development", 
                   "6. Leadership and career change", 
                   "7. Graduate employability, job search, and career choice",
                   "8. Work-family dynamics and well-being", 
                   "9. Expatriate careers and international adjustment")

# Convert model to tidy data format and rename topics
nine_topics_data <- make.dt(nine_topic_model, meta = stm$meta) %>% 
rename("1. Job Insecurity, Engagement, and Proactive Behaviours"        = Topic1,
       "2. Work Identity and Temporary or Precarious Employment"                  = Topic2,
         "3. Recruitment and Talent Management" = Topic3,
         "4. Women’s Careers and Sustainable Development"     = Topic4,
         "5. Mentoring, Relationships, and Career Development"   = Topic5,
         "6. Leadership and Career Change"     = Topic6,
         "7. Graduate Employability, Job Search, and Career Choice"           = Topic7,
         "8. Work-Family Dynamics and Well-being"                       = Topic8,
         "9. Expatriate Careers and International Adjustment"            = Topic9)

# Export data as excel
write_xlsx(nine_topics_data, "output/nine_topic_model.xlsx")

# Mean topic proportions
topic_summary <- nine_topics_data %>%
  select(all_of(nine_topics)) %>%
  pivot_longer(nine_topics, 
               names_to = "Topic", 
               values_to = "TopicMean") %>%
  arrange(desc(Topic)) %>% 
  summarise(TopicMean = mean(TopicMean),
            .by = Topic)


# Assign vertices colours
topic_colours <- c("#A6CEE3", 
                   "#1F78B4", 
                   "#B2DF8A", 
                   "#33A02C", 
                   "#FB9A99", 
                   "#E31A1C", 
                   "#FDBF6F", 
                   "#FF7F00", 
                   "#CAB2D6")



# Topic prevalence by top ten authors facet plot
# Topic prevalence by Ref

topics_by_AU1 <- nine_topics_data %>%
  select(AU1, c(nine_topics))

topics_by_AU1 <-
  filter(
    topics_by_AU1,
    AU1 %in% c("AKKERMANS J",
               "ARTHUR MB",
               "BARUCH Y",
               "BAKKER AB",
               "BRISCOE JP",
               "DEMEROUTI E", 
               "DONALD WE",
               "FERRIS GR",
               "HALL DT",
               "HARRISON JA",
               "JAWAHAR IM",
               "KHAPOVA SN",
               "KUNDI YM",
               "SCHAUFELI WB",
               "STUMPF SA",
               "SULLIVAN SE",
               "VAN DER HEIJDEN B")
  )

topics_by_AU1 <- topics_by_AU1 %>%
  pivot_longer(!AU1, names_to = "Topic", values_to = "TopicMean")

# Assign factor order to topics
topics_by_AU1$Topic <-
  fct_relevel(topics_by_AU1$Topic, nine_topics)

plot_topics_by_AU1 <-
  ggplot(topics_by_AU1, aes(x = Topic, y = TopicMean,
                            fill = Topic)) + geom_col(position = "dodge") + 
  facet_wrap( ~AU1,
              labeller = labeller(AU1 = label_wrap_gen(20)),
              nrow = 3) + theme(axis.text.y = element_text(size = 20)) + theme(
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.y = element_blank(),
                plot.margin = margin(1, 1, 1, 1, "cm"),
                legend.title = element_text(size = 36),
                legend.text = element_text(size = 32),
                legend.key.height = unit(3, "lines"),
                strip.text.x = element_text(size = 20)
              ) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) + scale_fill_brewer(palette = "Paired")

ggsave(
  "plot_9_topics_AU1.png",
  plot_topics_by_AU1,
  path = "output/",
  width = 45,
  height = 25,
  dpi = 300
)


# Topic prevalence by Ref

topics_by_ref <- nine_topics_data %>%
  select(ref, c(nine_topics))

topics_by_ref <-
  filter(
    topics_by_ref,
    ref %in% c("BAKKER AB, 2008, TOWARDS A MODEL OF WORK ENGAGEMENT",
               "SCHAUFELI WB, 2009, BURNOUT",
               "BACKHAUS K, 2004, CONCEPTUALIZING AND RESEARCHING EMPLOYER BRANDING",
               "VAN GELDEREN M, 2008, EXPLAINING ENTREPRENEURIAL INTENTIONS BY MEANS OF THE THEORY OF PLANNED BEHAVIOUR",
               "BARUCH Y, 2004, TRANSFORMING CAREERS",
               "DEMEROUTI E, 2009, PRESENT BUT SICK",
               "SCHAUFELI WB, 2015, ENGAGING LEADERSHIP IN THE JOB DEMANDS",
               "AGARWAL UA, 2012, LINKING LMX, INNOVATIVE WORK BEHAVIOUR AND TURNOVER INTENTIONS",
               "MILLER AN, 2011, PUBLISH OR PERISH",
               "INKSON K, 2003, “THE BIG OE”",
               "AL ARISS A, 2010, MODES OF ENGAGEMENT",
               "O'NEIL DA, 2005, WOMEN'S CAREER DEVELOPMENT PHASES",
               "DONALD WE, 2018, STUDENTS’ PERCEPTIONS OF EDUCATION AND EMPLOYABILITY")
  )

topics_by_ref <- topics_by_ref %>%
  pivot_longer(!ref, names_to = "Topic", values_to = "TopicMean")

# Assign factor order to topics
topics_by_ref$Topic <-
  fct_relevel(topics_by_ref$Topic, nine_topics)

plot_topics_by_ref <-
  ggplot(topics_by_ref, aes(x = Topic, y = TopicMean,
                           fill = Topic)) + geom_col(position = "dodge") + 
  facet_wrap( ~ref,
              labeller = labeller(ref = label_wrap_gen(20)),
              nrow = 3) + theme(axis.text.y = element_text(size = 20)) + theme(
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.y = element_blank(),
                plot.margin = margin(1, 1, 1, 1, "cm"),
                legend.title = element_text(size = 36),
                legend.text = element_text(size = 32),
                legend.key.height = unit(3, "lines"),
                strip.text.x = element_text(size = 20)
              ) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) + scale_fill_brewer(palette = "Paired")

ggsave(
  "plot_9_topics_ref.png",
  plot_topics_by_ref,
  path = "output/",
  width = 45,
  height = 25,
  dpi = 300
)


# TOPIC by YEAR

topic_by_year <- nine_topics_data %>%
  group_by(PY) %>%
  summarise(across(all_of(nine_topics), mean, na.rm = TRUE), .groups = 'drop')

# Reshape the data for plotting (convert from wide to long format)
topic_by_year_long <- topic_by_year %>%
  pivot_longer(cols = all_of(nine_topics), 
               names_to = "Topic", 
               values_to = "Proportion")

topic_by_year_long$Topic <-
  fct_relevel(topic_by_year_long$Topic, nine_topics)

# Create the line plot
plot_topics_by_year <-
  ggplot(topic_by_year_long, aes(x = PY, y = Proportion, color = Topic)) +
  geom_smooth(se = TRUE, alpha = 0.1) +  # Removed the colour aesthetic mapping
  scale_color_manual(values = topic_colours) +
  theme(
    text = element_text(family = "Times New Roman", size = 16),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.title = element_text(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(vjust = -2),
    axis.title.y = element_text(vjust = 2)
  ) +
  labs(y = "Topic proportions") +
  scale_x_continuous(
    name = "Publication Year",
    breaks = seq(1995, 2025, 5),
    limits = c(1995, 2025),
    expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0))

ggsave(
  "plot_9_topics_PY.png",
  plot_topics_by_year,
  path = "output/",
  width = 15,
  height = 8,
  dpi = 300
)

# Topics citations
library(dplyr)
library(ggplot2)
library(tidyr)

# Create long format data with citation counts
topic_summary <- nine_topics_data %>%
  select(TC, all_of(nine_topics)) %>%
  pivot_longer(cols = all_of(nine_topics), 
               names_to = "Topic", 
               values_to = "Proportion") %>%
  group_by(Topic) %>%
  summarise(
    Avg_Proportion = mean(Proportion),
    Avg_Citations = weighted.mean(TC, Proportion),
    Total_Articles = sum(Proportion > 0.1),  # Articles where topic is prominent
    .groups = 'drop'
  )

# Plot by TC
plot_topics_by_TC <- 
  ggplot(topic_summary, aes(x = Avg_Proportion, y = Avg_Citations, 
                            size = Total_Articles, color = Topic)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = topic_colours) +
  scale_size_continuous(range = c(3, 15)) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman", size = 16),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.title = element_text(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(vjust = -2),
    axis.title.y = element_text(vjust = 2)
  ) +
  labs(
    x = "Average Topic Proportion",
    y = "Average Citations",
    size = "Articles with Topic > 10%",
    color = "Topic"
  ) +
  scale_x_continuous(expand = expansion(mult = 0.05)) +
  scale_y_continuous(expand = expansion(mult = 0.05))

ggsave(
  "plot_9_topics_TC.png",
  plot_topics_by_TC,
  path = "output/",
  width = 15,
  height = 8,
  dpi = 300
)
