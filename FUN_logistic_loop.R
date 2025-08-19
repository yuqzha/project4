library(dplyr)
library(broom)
library(ggplot2)

# Define the function with optional group_var and covariates
logis_loop <- function(data, outcome_var, predictor_vars, group_var = NULL, covariates = NULL) {
  
  # Create symbol for the outcome
  outcome_sym <- rlang::sym(outcome_var)
  
  # Create symbols for covariates if provided
  covariate_syms <- NULL
  if (!is.null(covariates)) {
    covariate_syms <- rlang::syms(covariates)
  }
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each predictor variable
  for (predictor in predictor_vars) {
    
    predictor_sym <- rlang::sym(predictor)
    
    # Build the formula
    if (is.null(covariates)) {
      # Original formula without covariates
      formula_rhs <- rlang::expr(!!predictor_sym)
    } else {
      # Formula with covariates
      covariate_terms <- paste(covariates, collapse = " + ")
      formula_rhs <- rlang::expr(!!predictor_sym + !!rlang::parse_expr(covariate_terms))
    }
    
    # If no group_var is provided
    if (is.null(group_var)) {
      # Filter for complete cases (non-missing predictor and covariates)
      if (is.null(covariates)) {
        filtered_data <- data %>% filter(!is.na(!!predictor_sym))
      } else {
        # Create filter condition for all covariates
        covariate_filters <- map(covariate_syms, ~rlang::expr(!is.na(!!.x)))
        all_filters <- c(list(rlang::expr(!is.na(!!predictor_sym))), covariate_filters)
        filter_condition <- reduce(all_filters, ~rlang::expr(!!.x & !!.y))
        
        filtered_data <- data %>% filter(!!filter_condition)
      }
      
      group_results <- filtered_data %>%
        do(tidy(glm(!!outcome_sym ~ !!formula_rhs, data = ., family = binomial), conf.int = TRUE)) %>%
        # Exponentiate estimate and confidence intervals
        mutate(
          est = exp(estimate),
          min95 = exp(conf.low),
          max95 = exp(conf.high),
          predictor_var = predictor  # Add predictor variable name
        ) %>%
        filter(term != "(Intercept)") # Remove intercept for plotting
      
    } else {
      # If group_var is provided
      group_sym <- rlang::sym(group_var)
      
      # Filter for complete cases (non-missing predictor and covariates)
      if (is.null(covariates)) {
        filtered_data <- data %>% filter(!is.na(!!predictor_sym))
      } else {
        # Create filter condition for all covariates
        covariate_filters <- map(covariate_syms, ~rlang::expr(!is.na(!!.x)))
        all_filters <- c(list(rlang::expr(!is.na(!!predictor_sym))), covariate_filters)
        filter_condition <- reduce(all_filters, ~rlang::expr(!!.x & !!.y))
        
        filtered_data <- data %>% filter(!!filter_condition)
      }
      
      group_results <- filtered_data %>%
        group_by(!!group_sym) %>%
        do(tidy(glm(!!outcome_sym ~ !!formula_rhs, data = ., family = binomial), conf.int = TRUE)) %>%
        ungroup() %>%
        # Exponentiate estimate and confidence intervals
        mutate(
          est = exp(estimate),
          min95 = exp(conf.low),
          max95 = exp(conf.high),
          predictor_var = predictor  # Add predictor variable name
        ) %>%
        filter(term != "(Intercept)") # Remove intercept for plotting
    }
    
    # If covariates are present, filter to only show the main predictor terms
    if (!is.null(covariates)) {
      # Remove covariate terms from results, keeping only terms that contain the predictor name
      group_results <- group_results %>%
        filter(!term %in% covariates) %>%
        filter(str_detect(term, predictor) | !term %in% covariates)
    }
    
    # Append to results list
    results_list[[predictor]] <- group_results
  }
  
  # Combine all results into a single data frame
  logistic_results <- bind_rows(results_list)
  
  # Select relevant columns based on whether group_var exists
  if (is.null(group_var)) {
    logistic_results <- logistic_results %>%
      select(predictor_var, term, est, min95, max95, p.value)
  } else {
    logistic_results <- logistic_results %>%
      select(predictor_var, term, est, min95, max95, p.value, !!group_sym)
  }
  
  return(logistic_results)
}

# Helper function to diagnose term matching issues
check_term_matches <- function(results, term_labels) {
  available_terms <- unique(results$term)
  expected_terms <- term_labels$term
  
  cat("Available terms in results:\n")
  print(available_terms)
  cat("\nExpected terms from labels:\n")
  print(expected_terms)
  
  matched <- expected_terms %in% available_terms
  cat("\nMatching status:\n")
  for(i in seq_along(expected_terms)) {
    cat(sprintf("'%s' : %s\n", expected_terms[i], ifelse(matched[i], "Found", "Not found")))
  }
}

# Function to plot odds ratios with optional faceting by group
# plot_odds_ratios <- function(results, 
#                              group_var = NULL, 
#                              reference_terms = NULL,
#                              term_labels = NULL,
#                              debug = FALSE) {
#   
#   # Create data with estimates and CI text
#   data <- results %>%
#     mutate(est_ci = sprintf("%.2f (%.2f, %.2f)", est, min95, max95))
#   
#   # If reference terms are provided, set them to 1 and "ref"
#   if (!is.null(reference_terms)) {
#     data <- data %>%
#       mutate(across(c(est, min95, max95), 
#                     ~replace(., term %in% reference_terms, 1)),
#              est_ci = replace(est_ci, term %in% reference_terms, "ref"))
#   }
#   
#   # If term labels are provided, join them
#   if (!is.null(term_labels)) {
#     # For debugging, check if terms match
#     if (debug) {
#       check_term_matches(data, term_labels)
#     }
#     
#     data <- data %>%
#       left_join(term_labels, by = "term") %>%
#       # If no match found, use the original term as label
#       mutate(label = ifelse(is.na(label), term, label),
#              order = ifelse(is.na(order), row_number() + max(term_labels$order, na.rm = TRUE), order))
#   } else {
#     # If no labels provided, use the terms as labels
#     data <- data %>%
#       mutate(label = term,
#              order = row_number())
#   }
#   
#   # Create the plot
#   p <- ggplot(data, aes(x = reorder(label, -order), y = est)) +
#     theme_bw(base_size = 14) +
#     scale_y_continuous(limits = c(0.00000001, 15), 
#                        breaks = c(1.0, 2.0, 3.0, 5.0)) +
#     geom_hline(yintercept = 1, linetype = 5, color = "grey") +
#     geom_point(shape = 23, size = 1) +
#     geom_point(data = filter(data, min95 > 1),
#                shape = 23, size = 1, color = "darkred") +
#     geom_errorbar(aes(ymin = min95, ymax = max95),
#                   alpha = 1, width = 0.2, size = 0.5) +
#     geom_errorbar(data = filter(data, min95 > 1),
#                   aes(ymin = min95, ymax = max95),
#                   alpha = 1, width = 0.2, size = 0.5, color = "darkred") +
#     geom_text(aes(y = 12, label = est_ci), size = 3) +
#     ggtitle("") +
#     xlab("") +
#     ylab("Odds ratio") +
#     coord_flip()
#   
#   # Add facet if group_var is provided
#   if (!is.null(group_var)) {
#     p <- p + facet_wrap(as.formula(paste("~", group_var)), scales = "free_y")
#   }
#   
#   return(p)
# }

# Example usage:

# Without grouping or covariates (original functionality)
# results <- univariable_logistic(data = df, 
#                               outcome_var = "outcome", 
#                               predictor_vars = c("age", "bmi", "sex"))

# Without grouping but with covariate adjustment
# results <- univariable_logistic(data = df, 
#                               outcome_var = "outcome", 
#                               predictor_vars = c("bmi", "sex"),
#                               covariates = c("age_at_diagnosis"))

# With multiple covariates
# results <- univariable_logistic(data = df, 
#                               outcome_var = "outcome", 
#                               predictor_vars = c("bmi", "sex"),
#                               covariates = c("age_at_diagnosis", "baseline_score"))

# With grouping and covariate adjustment
# results_grouped <- univariable_logistic(data = df, 
#                                       outcome_var = "outcome", 
#                                       predictor_vars = c("bmi", "sex"),
#                                       group_var = "study_site",
#                                       covariates = c("age_at_diagnosis"))

# Define reference terms (optional)
# ref_terms <- c("age<20", "sex1")

# Define term labels (optional)
# term_order <- tibble(
#   term = c("age20-40", "age>40", "sex2", "bmi2"),
#   order = seq(1, 4, 1),
#   label = c("Age 20-40", "Age >40", "Female", "BMI >25")
# )

# Plot results
# plot_odds_ratios(results, 
#                  reference_terms = ref_terms,
#                  term_labels = term_order)

# With grouping
# plot_odds_ratios(results_grouped, 
#                  group_var = "study_site",
#                  reference_terms = ref_terms,
#                  term_labels = term_order)