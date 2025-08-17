library(dplyr)
library(broom)
library(ggplot2)
library(survival)

# Define the function to fit Cox model, with optional group_var, different sets of covariate
# Univariable models (no adjustment)
# results1 <- coxph_loop(
#   data = cancer_data,
#   outcome_var = "death",
#   time_var = "survival_time",
#   predictor_vars = c("treatment", "stage"),
#   covariates = NULL
# )
# 
# # Adjust for age only
# results2 <- coxph_loop(
#   data = cancer_data,
#   outcome_var = "death", 
#   time_var = "survival_time",
#   predictor_vars = c("treatment", "stage"),
#   covariates = "age"
# )
# 
# # Adjust for age and sex
# results3 <- coxph_loop(
#   data = cancer_data,
#   outcome_var = "death",
#   time_var = "survival_time", 
#   predictor_vars = c("treatment", "stage"),
#   covariates = c("age", "sex")
# )

coxph_loop <- function(data, outcome_var, time_var, predictor_vars, 
                       covariates = NULL, group_var = NULL) {
  
  # Create symbol for the outcome
  outcome_sym <- rlang::sym(outcome_var)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each predictor variable
  for (predictor in predictor_vars) {
    
    predictor_sym <- rlang::sym(predictor)
    
    # Build the formula
    if (is.null(covariates)) {
      # Univariable model
      formula_str <- paste("Surv(", time_var, ", ", outcome_var, ") ~ ", predictor)
    } else {
      # Multivariable model with covariates
      covariate_str <- paste(covariates, collapse = " + ")
      formula_str <- paste("Surv(", time_var, ", ", outcome_var, ") ~ ", 
                           predictor, " + ", covariate_str)
    }
    
    # Convert to formula object
    model_formula <- as.formula(formula_str)
    
    # Create vector of all variables needed (for filtering NAs)
    all_vars <- c(predictor, covariates)
    
    # If no group_var is provided
    if (is.null(group_var)) {
      group_results <- data %>%
        # Filter out rows with missing values in any of the model variables
        filter(if_all(all_of(all_vars), ~ !is.na(.))) %>%
        do(tidy(coxph(model_formula, data = .), 
                conf.int = TRUE, exponentiate = TRUE)) %>%
        mutate(
          predictor_var = predictor,  # Add predictor variable name
          est = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
          p = round(p.value,3)
        ) %>%
        # Only keep the main predictor term (not covariates)
        filter(term != "(Intercept)" & startsWith(term, predictor))
      
    } else {
      # If group_var is provided
      group_sym <- rlang::sym(group_var)
      
      group_results <- data %>%
        # Filter out rows with missing values in any of the model variables
        filter(if_all(all_of(all_vars), ~ !is.na(.))) %>%
        group_by(!!group_sym) %>%
        do(tidy(coxph(model_formula, data = .), 
                conf.int = TRUE, exponentiate = TRUE)) %>%
        ungroup() %>%
        mutate(
          predictor_var = predictor,  # Add predictor variable name
          est = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
          p = round(p.value,3)
        ) %>%
        # Only keep the main predictor term (not covariates)
        filter(term != "(Intercept)" & startsWith(term, predictor))
    }
    
    # Append to results list
    results_list[[predictor]] <- group_results
  }
  
  # Combine all results into a single data frame
  model_results <- bind_rows(results_list)
  
  # Select relevant columns based on whether group_var exists
  if (is.null(group_var)) {
    model_results <- model_results %>%
      select(predictor_var, term, estimate, conf.low, conf.high, p)
  } else {
    model_results <- model_results %>%
      select(predictor_var, term, estimate, conf.low, conf.high,est, p, !!group_sym)
  }
  
  return(model_results)
}