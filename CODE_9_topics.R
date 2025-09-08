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

stm_converted <- quanteda::convert(dfm, to = "stm")

# Add metadata manually
stm <- list(
  documents = stm_converted$documents,
  vocab = stm_converted$vocab,
  meta = full_data_clean
)


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


# CREATE TABLE HERE - before any reordering
topic_table <- data.frame(
  Topic_Number = paste0("Topic ", 1:9),
  Highest_Prob_Words = sapply(1:9, function(i) paste(nine_topics_keywords$prob[i, 1:10], collapse = ", ")),
  FREX_Words = sapply(1:9, function(i) paste(nine_topics_keywords$frex[i, 1:10], collapse = ", ")),
  Representative_Titles = sapply(1:9, function(i) paste(nine_topics_texts$docs[[i]][1:3], collapse = " | "))
)

# Export to Excel
write_xlsx(topic_table, "output/nine_topic_keywords_table.xlsx")

# Create formatted Word table
topic_flextable <- flextable(topic_table) %>%
  theme_apa() %>%
  autofit() %>%
  width(j = "Topic_Number", width = 1.5) %>%
  width(j = "Highest_Prob_Words", width = 2.5) %>%
  width(j = "FREX_Words", width = 2.5) %>%
  width(j = "Representative_Titles", width = 4) %>%
  align(align = "left", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "body") %>%
  fontsize(size = 12, part = "header") %>%
  bold(part = "header")

save_as_docx(topic_flextable, path = "output/nine_topic_keywords_table.docx")

# Then continue with your existing reordering code...
# Convert model to tidy data format
nine_topics_data <- make.dt(nine_topic_model, meta = stm$meta)


# ILLUSTRATE 9 TOPIC MODEL


# Convert model to tidy data format
nine_topics_data <- make.dt(nine_topic_model, meta = stm$meta) 

# Get the current order by average proportion
current_topic_order <- nine_topics_data %>%
  select(Topic1:Topic9) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "Topic", values_to = "Avg_Proportion") %>%
  arrange(desc(Avg_Proportion)) %>%
  pull(Topic)

# Define your final topic names 
nine_topics <- c("1. Sustainable, protean, boundaryless, and women's careers",
                 "2. Job insecurity, engagement, and proactive behaviours",  
                 "3. Work-family dynamics and well-being", 
                 "4. Recruitment and talent management", 
                 "5. Leadership and career change", 
                 "6. Graduate employability, job search, and career choice", 
                 "7. Mentoring, relationships, and career development",
                 "8. Expatriate careers and international adjustment",
                 "9. Work identity and temporary or precarious employment")

# Rename columns directly
nine_topics_data <- nine_topics_data %>%
  rename(
    !!nine_topics[1] := !!current_topic_order[1],
    !!nine_topics[2] := !!current_topic_order[2],
    !!nine_topics[3] := !!current_topic_order[3],
    !!nine_topics[4] := !!current_topic_order[4],
    !!nine_topics[5] := !!current_topic_order[5],
    !!nine_topics[6] := !!current_topic_order[6],
    !!nine_topics[7] := !!current_topic_order[7],
    !!nine_topics[8] := !!current_topic_order[8],
    !!nine_topics[9] := !!current_topic_order[9]
  )


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


topics_by_AU1 <- nine_topics_data %>%
  select(AU1, all_of(nine_topics)) %>%
  filter(
    AU1 %in% c("AKKERMANS J", "ARTHUR MB", "BARUCH Y", "BAKKER AB",
               "BRISCOE JP", "DEMEROUTI E", "DONALD WE", "FERRIS GR", "HOFER A",
               "HALL DT", "HARRISON JA", "JAWAHAR IM", "KHAPOVA SN",
               "KUNDI YM", "LO PRESTI A", "SCHAUFELI WB", "STUMPF SA", "SULLIVAN SE",
               "VAN DER HEIJDEN B")
  ) %>%

  group_by(AU1) %>%
  summarise(across(all_of(nine_topics), mean, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(!AU1, names_to = "Topic", values_to = "TopicMean")

plot_topics_by_AU1 <-
  ggplot(topics_by_AU1, aes(x = Topic, y = TopicMean, fill = Topic)) + 
  geom_col(position = "dodge", color = "black", size = 0.2) +
  facet_wrap(~AU1, nrow = 3, 
             labeller = labeller(AU1 = label_wrap_gen(25))) +
  
  theme_minimal() +
  theme(
    # Base font settings
    text = element_text(family = "Times New Roman", size = 12),
    
    # Bold only for main title and axis/legend titles
    plot.title = element_text(family = "Times New Roman", size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.y = element_text(family = "Times New Roman", size = 18, face = "bold", margin = margin(r = 10)),
    legend.title = element_text(family = "Times New Roman", size = 18, face = "bold"),
    
    # Normal weight for content text
    strip.text = element_text(family = "Times New Roman", size = 12, face = "plain", margin = margin(4, 4, 4, 4)),
    strip.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.text = element_text(family = "Times New Roman", size = 16, face = "plain"),
    axis.text.y = element_text(family = "Times New Roman", size = 12),
    plot.caption = element_text(family = "Times New Roman", size = 12, hjust = 0, margin = margin(t = 10)),
    
    # Axis formatting
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Legend positioning
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.margin = margin(t = 10),
    
    # Panel formatting
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.margin = margin(2, 2, 2, 2, "cm")
  ) +
  
  # Labels
  labs(
    y = "Topic proportion",
    fill = "Research topics"
  ) +
  
  # Legend layout
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = TRUE,
      title.position = "top"
    )
  ) +
  
  # Y-axis formatting
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_fill_manual(values = topic_colours)

# Save plot
ggsave(
  "plot_9_topics_AU1.png",
  plot_topics_by_AU1,
  path = "output/",
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

# Topic prevalence by Ref

topics_by_ref <- nine_topics_data %>%
  select(ref, all_of(nine_topics)) %>%
  filter(
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
  ) %>%
  # Add aggregation step to prevent stacking lines (in case of duplicates)
  group_by(ref) %>%
  summarise(across(all_of(nine_topics), mean, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(!ref, names_to = "Topic", values_to = "TopicMean")

plot_topics_by_ref <-
  ggplot(topics_by_ref, aes(x = Topic, y = TopicMean, fill = Topic)) + 
  geom_col(position = "dodge", color = "black", size = 0.2) +
  facet_wrap(~ref, nrow = 4, ncol = 3,
             labeller = labeller(ref = label_wrap_gen(25))) +
  
  theme_minimal() +
  theme(
    # Base font settings
    text = element_text(family = "Times New Roman", size = 12),
    
    # Bold only for main title and axis/legend titles
    plot.title = element_text(family = "Times New Roman", size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.y = element_text(family = "Times New Roman", size = 16, face = "bold", margin = margin(r = 10)),
    legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    
    # Normal weight for content text
    strip.text = element_text(family = "Times New Roman", size = 12, face = "plain", margin = margin(4, 4, 4, 4)),
    strip.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.text = element_text(family = "Times New Roman", size = 16, face = "plain"),
    axis.text.y = element_text(family = "Times New Roman", size = 12),
    plot.caption = element_text(family = "Times New Roman", size = 12, hjust = 0, margin = margin(t = 10)),
    
    # Axis formatting
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Legend positioning
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.margin = margin(t = 10),
    
    # Panel formatting
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5, "cm")
  ) +
  
  # Labels
  labs(
    y = "Topic proportion",
    fill = "Research topics"
  ) +
  
  # Legend layout
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = TRUE,
      title.position = "top"
    )
  ) +
  
  # Y-axis formatting
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_fill_manual(values = topic_colours)

# Save plot
ggsave(
  "plot_9_topics_ref.png",
  plot_topics_by_ref,
  path = "output/",
  width = 14,
  height = 14,
  dpi = 300,
  bg = "white"
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

# Create the line plot
plot_topics_by_year <-
  ggplot(topic_by_year_long, aes(x = PY, y = Proportion, color = Topic)) +
  geom_smooth(se = TRUE, alpha = 0.1) +
  scale_color_manual(values = topic_colours) +
  
  theme_minimal() +
  theme(
    # Base font settings
    text = element_text(family = "Times New Roman", size = 12),
    
    # Bold only for main title and axis/legend titles
    plot.title = element_text(family = "Times New Roman", size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(family = "Times New Roman", size = 16, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(family = "Times New Roman", size = 16, face = "bold", margin = margin(r = 10)),
    legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    
    # Normal weight for content text
    legend.text = element_text(family = "Times New Roman", size = 16, face = "plain"),
    axis.text.x = element_text(family = "Times New Roman", size = 12),
    axis.text.y = element_text(family = "Times New Roman", size = 12),
    plot.caption = element_text(family = "Times New Roman", size = 12, hjust = 0, margin = margin(t = 10)),
    
    # Legend positioning
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.margin = margin(t = 40),
    
    # Panel formatting
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5, "cm")
  ) +
  
  # Labels
  labs(
    x = "Publication year",
    y = "Topic proportion",
    color = "Research topics"
  ) +
  
  # Legend layout
  guides(
    color = guide_legend(
      ncol = 2,
      byrow = TRUE,
      title.position = "top"
    )
  ) +
  
  # Axis formatting
  scale_x_continuous(
    breaks = seq(1995, 2025, 5),
    limits = c(1995, 2025),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  )

ggsave(
  "plot_9_topics_PY.png",
  plot_topics_by_year,
  path = "output/",
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

# Find topic citation impact
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
  
  theme_minimal() +
  theme(
    # Base font settings
    text = element_text(family = "Times New Roman", size = 12),
    
    # Bold only for main title and axis/legend titles
    plot.title = element_text(family = "Times New Roman", size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(family = "Times New Roman", size = 18, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(family = "Times New Roman", size = 18, face = "bold", margin = margin(r = 10)),
    legend.title = element_text(family = "Times New Roman", size = 18, face = "bold"),
    
    # Normal weight for content text
    legend.text = element_text(family = "Times New Roman", size = 16, face = "plain"),
    axis.text.x = element_text(family = "Times New Roman", size = 12),
    axis.text.y = element_text(family = "Times New Roman", size = 12),
    plot.caption = element_text(family = "Times New Roman", size = 12, hjust = 0, margin = margin(t = 10)),
    
    # Legend positioning
    legend.position = "bottom",
    legend.key.size = unit(0.8, "cm"),  # Increased from 0.4 to 0.8
    legend.margin = margin(t = 10),
    
    # Panel formatting
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5, "cm")
  ) +
  
  # Labels
  labs(
    x = "Average topic proportion",
    y = "Average citations",
    size = "Articles with topic > 10%",
    color = "Research topics"
  ) +
  
  # Legend layout
  guides(
    color = guide_legend(
      ncol = 2,
      byrow = TRUE,
      title.position = "top",
      override.aes = list(size = 5)  # Makes legend circles bigger
    ),
    size = guide_legend(
      title.position = "top"
    )
  ) +
  
  # Axis formatting
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(expand = expansion(mult = 0.05))

ggsave(
  "plot_9_topics_TC.png",
  plot_topics_by_TC,
  path = "output/",
  width = 16,  # Increased from 14
  height = 14, # Increased from 10
  dpi = 300,
  bg = "white"
)