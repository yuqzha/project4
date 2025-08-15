### create a function for descriptive tables (group by)
## using case
# create_descriptive_table(
#   data = pop_baseline,
#   group_var = "included_2visits",
#   continuous_vars = c("Age", "HbA1c_result", "diabetes_duration"),
#   factor_vars = c("sex","Indigenous","RA3")
# )

create_descriptive_table <- function(data, group_var, continuous_vars, factor_vars) {
  # Check if grouping variable exists
  if (!group_var %in% colnames(data)) {
    stop(paste("Grouping variable", group_var, "not found in the dataset."))
  }
  
  # Get unique labels of the group_var and their counts
  group_labels <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(across(all_of(group_var))) %>%
    mutate(label = paste0(.data[[group_var]], " (n=", n, ")")) %>%
    pull(label)
  
  # Continuous variables: Calculate mean (SD) and t-test p-value
  continuous_summary <- continuous_vars %>%
    lapply(function(var) {
      summary_data <- data %>%
        group_by(across(all_of(group_var))) %>%
        summarise(
          Mean_SD = paste0(
            round(median(.data[[var]], na.rm = TRUE), 2), " (",
            round(quantile(.data[[var]], 0.25, na.rm = TRUE), 2), ", ",
            round(quantile(.data[[var]], 0.75, na.rm = TRUE), 2), ")"
          ), .groups = "drop") %>%
        pivot_wider(names_from = !!sym(group_var), values_from = Mean_SD)
      
      p_value <- t.test(data[[var]] ~ data[[group_var]])$p.value
      summary_data <- summary_data %>%
        mutate(Variable = var, Statistic = "Median (IQR)", Factor_Level = NA, p_value = round(p_value, 3))
      
      summary_data
    }) %>%
    bind_rows()
  
  # Factor variables: Calculate frequencies and chi-square p-value
  factor_summary <- factor_vars %>%
    lapply(function(var) {
      summary_data <- data %>%
        select(all_of(var), all_of(group_var)) %>%
        group_by(across(all_of(group_var)), .data[[var]]) %>%
        summarise(Count = n(), .groups = "drop") %>%
        group_by(across(all_of(group_var))) %>%
        mutate(Percentage = Count / sum(Count) * 100) %>%
        ungroup() %>%
        mutate(
          Value = paste0(Count, " (", round(Percentage, 1), "%)"),
          Factor_Level = as.character(.data[[var]])
        ) %>%
        select(Factor_Level, !!sym(group_var), Value) %>%
        pivot_wider(names_from = !!sym(group_var), values_from = Value)
      
      # Perform chi-square test
      contingency_table <- table(data[[var]], data[[group_var]])
      p_value <- chisq.test(contingency_table)$p.value
      summary_data <- summary_data %>%
        mutate(Variable = var, Statistic = "Count (Percentage)", p_value = round(p_value, 3))
      
      summary_data
    }) %>%
    bind_rows()
  
  # Combine continuous and factor summaries
  descriptive_table <- bind_rows(continuous_summary, factor_summary) %>%
    select(Variable, Statistic, Factor_Level, everything())
  
  # Add custom header with group labels
  header <- c("Variable", "Statistic", "Factor Level",
              group_labels,
              "p-value")
  
  # Choose output format based on the current output type
  if (knitr::is_html_output()) {
    # For HTML output, use kable with HTML styling
    descriptive_table %>%
      knitr::kable(col.names = header, align = "c") %>%
      kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
  } else {
    # For Word output, use simple kable without HTML styling
    # This creates a native, editable Word table
    colnames(descriptive_table) <- header
    knitr::kable(descriptive_table, format = "simple", align = "c")
  }
}