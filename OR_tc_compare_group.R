# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Assuming your data is in a dataframe called 'results_df'
# If not, replace 'results_df' with your actual dataframe name

# First, let's check the structure and create a safer approach
print("Original data structure:")
print(head(results_df))
print("Unique pd_BIRADs_3cat values:")
print(unique(results_df$pd_BIRADs_3cat))

# 1. Prepare data for comparison - more robust approach
# First, create separate datasets for each group
group_AB <- results_df %>%
  filter(pd_BIRADs_3cat == "A+B:[0,18]") %>%
  select(predictor_var, term, est, min95, max95, p.value, label) %>%
  rename(est_AB = est, min95_AB = min95, max95_AB = max95, p_value_AB = p.value)

group_D <- results_df %>%
  filter(pd_BIRADs_3cat == "D:(49,100]") %>%
  select(predictor_var, term, est, min95, max95, p.value, label) %>%
  rename(est_D = est, min95_D = min95, max95_D = max95, p_value_D = p.value)

# Join the datasets
comparison_data <- group_AB %>%
  full_join(group_D, by = c("predictor_var", "term", "label")) %>%
  # Remove rows where either group is missing
  filter(!is.na(est_AB) & !is.na(est_D))

print("Comparison data structure:")
print(head(comparison_data))
print("Column names:")
print(names(comparison_data))

# 2. Calculate OR ratio and other metrics
comparison_data <- comparison_data %>%
  mutate(
    # OR ratio (Group A+B divided by Group D)
    or_ratio = est_AB / est_D,
    
    # Log OR difference (more appropriate for statistical comparison)
    log_or_diff = log(est_AB) - log(est_D),
    
    # Check if confidence intervals overlap
    ci_overlap = !(min95_AB > max95_D | max95_AB < min95_D),
    
    # Significance difference (both p-values < 0.05 but non-overlapping CIs)
    significant_difference = (p_value_AB < 0.05 & p_value_D < 0.05 & !ci_overlap)
  )

# 3. Statistical test for OR differences
# Function to calculate z-score for OR comparison
calculate_z_score <- function(or1, ci1_lower, ci1_upper, or2, ci2_lower, ci2_upper) {
  # Calculate standard errors from confidence intervals
  se1 <- (log(ci1_upper) - log(ci1_lower)) / (2 * 1.96)
  se2 <- (log(ci2_upper) - log(ci2_lower)) / (2 * 1.96)
  
  # Calculate z-score for difference in log ORs
  log_or_diff <- log(or1) - log(or2)
  se_diff <- sqrt(se1^2 + se2^2)
  z_score <- log_or_diff / se_diff
  
  # Calculate p-value (two-tailed)
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  return(list(z_score = z_score, p_value = p_value))
}

# Apply statistical test
comparison_data <- comparison_data %>%
  rowwise() %>%
  mutate(
    test_result = list(calculate_z_score(est_AB, min95_AB, max95_AB,
                                         est_D, min95_D, max95_D)),
    z_score = test_result$z_score,
    comparison_p_value = test_result$p_value,
    significant_comparison = comparison_p_value < 0.05
  ) %>%
  select(-test_result) %>%
  ungroup()

# 4. Summary table
summary_table <- comparison_data %>%
  select(predictor_var, label, 
         est_AB, min95_AB, max95_AB, p_value_AB,
         est_D, min95_D, max95_D, p_value_D,
         or_ratio, z_score, comparison_p_value, significant_comparison) %>%
  arrange(comparison_p_value)

print("OR Comparison Results:")
print(summary_table)

# 5. Visualization
# Forest plot comparing ORs
plot_data <- results_df %>%
  filter(pd_BIRADs_3cat %in% c("A+B:[0,18]", "D:(49,100]"))

forest_plot <- ggplot(plot_data, aes(x = est, y = reorder(label, order), 
                                     color = pd_BIRADs_3cat)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = min95, xmax = max95), 
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Odds Ratios Comparison Between pd_BIRADs Groups",
       x = "Odds Ratio (log scale)",
       y = "Predictor Variables",
       color = "pd_BIRADs Group") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(forest_plot)

# 6. OR Ratio plot
ratio_plot <- ggplot(comparison_data, aes(x = or_ratio, y = reorder(label, or_ratio))) +
  geom_point(aes(color = significant_comparison), size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.7) +
  scale_x_log10() +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "red")) +
  labs(title = "OR Ratio: A+B:[0,18] / D:(49,100]",
       x = "OR Ratio (log scale)",
       y = "Predictor Variables",
       color = "Significantly Different") +
  theme_minimal()

print(ratio_plot)

# 7. Detailed results for significant differences
cat("\n=== SIGNIFICANT DIFFERENCES ===\n")
significant_results <- comparison_data %>%
  filter(significant_comparison) %>%
  select(label, est_AB, est_D, or_ratio, 
         comparison_p_value, z_score)

if(nrow(significant_results) > 0) {
  print(significant_results)
} else {
  cat("No statistically significant differences found between groups.\n")
}

# 8. Export results
write.csv(comparison_data, "or_comparison_results.csv", row.names = FALSE)
cat("\nResults exported to 'or_comparison_results.csv'\n")