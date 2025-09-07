


# MODEL SELECTION
# Run diagnostic for broad range of topics
# Set seed for reproducibility
set.seed(0909)

# Assign broad range
k3_30 <- seq(3, 30, by = 3)
# Run searchK with broad range for K
search_broad <-
  searchK(
    stm$documents,
    stm$vocab,
    K = k3_30,
    init.type = "Spectral",
    data = stm$meta
  )

# Create table of broad semantic coherence/exclusivity
search_broad_semcoh <-
  as.data.frame(search_broad$results$semcoh) %>%
  pivot_longer(cols = everything()) %>%
  rename(`Semantic Coherence` = value)
search_broad_exclus <-
  as.data.frame(search_broad$results$exclus) %>%
  pivot_longer(cols = everything()) %>%
  rename(Exclusivity = value)

semcoh_broad <- cbind(search_broad_semcoh, search_broad_exclus) %>%
  select(2, 4)

# Create plot of broad semantic coherence/exclusivity
plot_semcoh_broad <-
  ggplot(semcoh_broad,
         mapping = aes(`Semantic Coherence`,
                       `Exclusivity`)) + 
  geom_point() + 
  geom_text(label = k3_30,
            nudge_x = -0.05,
            nudge_y = 0.05) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  xlab("Semantic Coherence") + 
  ylab("Exclusivity") + 
  labs(title = "3 to 30 topics")


ggsave(
  "plot_semcoh_broad.png",
  plot_semcoh_broad,
  path = "output/",
  width = 9,
  height = 6,
  dpi = 300
)

# Run diagnostic for fine range topics
# Set seed for reproducibility
set.seed(0909)
# Assign fine range
k6_15 <- (6:15)
# Run searchK with broad range for K
search_fine <-
  searchK(
    stm$documents,
    stm$vocab,
    K = k6_15,
    init.type = "Spectral",
    data = stm$meta
  )

# Create table of fine semantic coherence/exclusivity
search_fine_semcoh <- as.data.frame(search_fine$results$semcoh) %>%
  pivot_longer(cols = everything()) %>%
  rename(`Semantic Coherence` = value)
search_fine_exclus <- as.data.frame(search_fine$results$exclus) %>%
  pivot_longer(cols = everything()) %>%
  rename(Exclusivity = value)

semcoh_fine <- cbind(search_fine_semcoh, search_fine_exclus) %>%
  select(2, 4)


# Create plot for fine semantic coherence/exclusivity
plot_semcoh_fine <-
  ggplot(semcoh_fine,
         mapping = aes(`Semantic Coherence`,
                       `Exclusivity`)) + 
  geom_point() + 
  geom_text(label = k6_15,
            nudge_x = -0.05,
            nudge_y = 0.05) +
  papaja::theme_apa() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  xlab("Semantic Coherence") +
  ylab("Exclusivity") +
  labs(title = "6 to 15 topics")

ggsave(
  "plot_semcoh_fine.png",
  plot_semcoh_fine,
  path = "output/",
  width = 9,
  height = 6,
  dpi = 300
)


# Plot both broad and fine together in one figure
plot_semcoh_combined <-
  plot_grid(plot_semcoh_broad, plot_semcoh_fine)

ggsave(
  "plot_semcoh_combined.png",
  plot_semcoh_combined,
  path = "output/",
  width = 15,
  height = 6,
  dpi = 300
)


# MODELLING

# Model seven topics
set.seed(0909)
seven_topic_model <-
  stm(
    documents = stm$documents,
    vocab = stm$vocab,
    K = 7,
    verbose = TRUE,
    data = stm$meta
  )

# Model eight topics
set.seed(0909)
eight_topic_model <-
  stm(
    documents = stm$documents,
    vocab = stm$vocab,
    K = 8,
    verbose = TRUE,
    data = stm$meta
  )

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

# Model ten topics
set.seed(0909)
ten_topic_model <-
  stm(
    documents = stm$documents,
    vocab = stm$vocab,
    K = 10,
    verbose = TRUE,
    data = stm$meta
  )







# DESCRIPTIVE STATS

# DESCRIPTIVE STATS
# Extract most cited and productive TI/AU/SO
# Extract most cited articles
most_cited_TI <-
  full_data %>%
  select(SR, TI, TC, DI) %>%
  arrange(desc(TC)) %>%
  slice_head(n = 20) %>%
  remove_rownames()

# Extract author h-index
hindex_AU <- Hindex(full_data, field = "author", sep = ";")
hindex_AU <- as.data.frame(hindex_AU$H)
hindex_AU <- hindex_AU %>% select(Element, h_index) %>%
  arrange(desc(h_index)) %>%
  slice_head(n = 20) %>%
  remove_rownames()

# Extract journal h-index
hindex_SO <- Hindex(full_data, field = "source", sep = ";")
hindex_SO <- as.data.frame(hindex_SO$H)
hindex_SO <- hindex_SO %>% select(Element, h_index) %>%
  arrange(desc(h_index)) %>%
  slice_head(n = 20) %>%
  remove_rownames()

# Create tibble of descriptive findings and rename columns
summary_most_cited_productive <-
  tibble(bind_cols(most_cited_TI, hindex_AU)) %>%
  rename(
    "Most cited articles SR"          = SR,
    "Most cited articles TI"          = TI,
    "Times article cited"             = TC,
    "Highest h-index authors"         = Element,
    "Author h-index"                  = h_index
  )

# Create table for most_cited_TI
table_most_cited_TI <-
  flextable(
    summary_most_cited_productive,
    col_keys = c("Most cited articles SR", "Most cited articles TI", "Times article cited")
  ) %>%
  autofit() %>%
  theme_apa() %>%
  colformat_double(digits = 0) %>%
  align_text_col(align = "left", header = FALSE)

save_as_docx(
  table_most_cited_TI,
  path = "output/Table_most_cited_TI.docx"
)

# Create table for descriptive statistics


table_hindex <-
  flextable(
    summary_most_cited_productive,
    col_keys = c(
      "Highest h-index authors",
      "Author h-index",
      "Highest h-index journals",
      "Journal h-index"
    )
  ) %>%
  autofit() %>%
  theme_apa() %>%
  colformat_double(digits = 0) %>%
  align_text_col(align = "left", header = FALSE)

save_as_docx(table_hindex,
             path = "output/Table_hindex.docx")



