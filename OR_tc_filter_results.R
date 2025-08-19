factors2 <- c("T_pad_c", "N_pad_positive", "grade", "ER_negative", "PR_negative", "HER2_positive", "ki67", "histology")

# input results, from Fun_uni_logistic
#output filtered_results

# Define only the terms you want to plot and their labels
term_labels <- tibble(
  term = c(
    "T_pad_c_2cat>20mm", "N_pad_positiveYes", "grade3",
    "ER_negativeYes",    "PR_negativeYes",    "HER2_positiveYes",
    "ki67High", "histologylobular"  
  ),
  order = seq_along(term),
  label = c(
    # Labels for selected Cohort 1 terms
    "Tumor size >20", "Lymph nodes positive", "Grade 3", 
    "ER negative", "PR negative", "HER2 positive", "Ki-67 high", "Lobular histology"
  )
)

# Filter results to only include terms in term_labels
filtered_results <- results %>%
  filter(term %in% term_labels$term) |> 
  left_join(term_labels, by = "term") %>%
  mutate(
    global_order = -order, # global order for facet
    label = ifelse(is.na(label), term, label),
    association = case_when(
      est > 1 & p.value < 0.05 ~ "Positive",
      est < 1 & p.value < 0.05 ~ "Negative",
      TRUE ~ "NS"
    )
  )