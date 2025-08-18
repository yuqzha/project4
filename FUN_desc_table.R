create_descriptive_table <- function(data, group_var, variables = NULL, continuous_vars = NULL, 
                                     use_median = TRUE, digits = 2, exact_tests = FALSE,
                                     variable_labels = NULL) {
  
  # Load required packages
  required_packages <- c("dplyr", "tidyr", "knitr", "kableExtra")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not found:", paste(missing_packages, collapse = ", ")))
  }
  
  # Input validation
  if (missing(data) || missing(group_var)) {
    stop("Both 'data' and 'group_var' arguments are required.")
  }
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  if (!group_var %in% colnames(data)) {
    stop(paste("Grouping variable '", group_var, "' not found in the dataset.", sep = ""))
  }
  
  if (is.null(variables)) {
    stop("The 'variables' argument must be specified - provide the list of variables to include in the table.")
  }
  
  # Set continuous_vars to empty if NULL
  if (is.null(continuous_vars)) {
    continuous_vars <- character(0)
  }
  
  # Use the supplied variables list
  all_vars <- variables
  
  # Automatically classify: specified continuous_vars are continuous, rest from variables list are factors
  factor_vars <- setdiff(all_vars, continuous_vars)
  
  # Check if specified variables exist in the dataset
  missing_vars <- all_vars[!all_vars %in% colnames(data)]
  if (length(missing_vars) > 0) {
    warning(paste("Variables not found in dataset:", paste(missing_vars, collapse = ", ")))
    continuous_vars <- continuous_vars[continuous_vars %in% colnames(data)]
    factor_vars <- factor_vars[factor_vars %in% colnames(data)]
    all_vars <- all_vars[all_vars %in% colnames(data)]
  }
  
  # Convert group variable to factor if it isn't already
  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- as.factor(data[[group_var]])
  }
  
  # Get unique levels and their counts
  group_summary <- data %>%
    dplyr::group_by(across(dplyr::all_of(group_var))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(across(dplyr::all_of(group_var)))
  
  # Create column headers with counts
  group_labels <- paste0(group_summary[[group_var]], " (n=", group_summary$n, ")")
  
  # Initialize empty list for results
  results_list <- list()
  
  # Process variables in the order specified in the variables list
  results_list <- list()
  
  for (var in all_vars) {
    if (var %in% continuous_vars) {
      # Process as continuous variable
      
      # Skip if variable has no numeric values
      if (!is.numeric(data[[var]])) {
        warning(paste("Variable '", var, "' is not numeric. Converting or skipping.", sep = ""))
        next
      }
      
      # Enhanced handling: Check for all NA values or no data
      if (all(is.na(data[[var]])) || length(data[[var]]) == 0) {
        # Create an empty summary with NAs
        empty_summary <- data.frame(Variable = var, 
                                    Statistic = ifelse(use_median, "Median (Q1, Q3)", "Mean (SD)"), 
                                    Factor_Level = NA_character_,
                                    stringsAsFactors = FALSE)
        # Add one column for each group level with "NA" values
        for (group_level in levels(data[[group_var]])) {
          empty_summary[[as.character(group_level)]] <- "NA"
        }
        results_list[[var]] <- empty_summary
        next
      }
      
      # Calculate non-NA counts by group for continuous variables
      non_na_counts <- data %>%
        dplyr::select(dplyr::all_of(var), dplyr::all_of(group_var)) %>%
        dplyr::filter(!is.na(.data[[var]])) %>%
        dplyr::group_by(across(dplyr::all_of(group_var))) %>%
        dplyr::summarise(non_na_count = dplyr::n(), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = dplyr::all_of(group_var), values_from = non_na_count) %>%
        dplyr::mutate(dplyr::across(-dplyr::any_of(c("Variable", "Statistic", "Factor_Level")), as.character))
      
      if (use_median) {
        # Enhanced median and IQR calculation with NA handling
        summary_data <- data %>%
          dplyr::group_by(across(dplyr::all_of(group_var))) %>%
          dplyr::summarise(
            summary_stat = ifelse(all(is.na(.data[[var]])), "NA", 
                                  paste0(
                                    round(median(.data[[var]], na.rm = TRUE), digits), " (",
                                    round(quantile(.data[[var]], 0.25, na.rm = TRUE), digits), ", ",
                                    round(quantile(.data[[var]], 0.75, na.rm = TRUE), digits), ")"
                                  )
            ), 
            .groups = "drop"
          ) %>%
          tidyr::pivot_wider(names_from = dplyr::all_of(group_var), values_from = summary_stat)
        
        statistic_label <- "Median (Q1, Q3)"
        
      } else {
        # Enhanced mean and SD calculation with NA handling
        summary_data <- data %>%
          dplyr::group_by(across(dplyr::all_of(group_var))) %>%
          dplyr::summarise(
            summary_stat = ifelse(all(is.na(.data[[var]])), "NA",
                                  paste0(
                                    round(mean(.data[[var]], na.rm = TRUE), digits), " (",
                                    round(sd(.data[[var]], na.rm = TRUE), digits), ")"
                                  )
            ), 
            .groups = "drop"
          ) %>%
          tidyr::pivot_wider(names_from = dplyr::all_of(group_var), values_from = summary_stat)
        
        statistic_label <- "Mean (SD)"
      }
      
      # Add metadata to summary statistics
      summary_data <- summary_data %>%
        dplyr::mutate(
          Variable = var, 
          Statistic = statistic_label, 
          Factor_Level = NA_character_,
          .before = 1
        )
      
      # Create the non-NA count summary row for continuous variables
      non_na_summary <- non_na_counts %>%
        dplyr::mutate(
          Variable = var,
          Statistic = "Non-missing observations",
          Factor_Level = NA_character_,
          .before = 1
        )
      
      # Combine summary statistics first, then non-NA summary row
      combined_data <- dplyr::bind_rows(summary_data, non_na_summary)
      results_list[[var]] <- combined_data
      
    } else {
      # Process as factor variable
      
      # Convert to factor if not already
      if (!is.factor(data[[var]])) {
        data[[var]] <- as.factor(data[[var]])
      }
      
      # Enhanced handling: Check for all NA values
      if (all(is.na(data[[var]]))) {
        # Create an empty summary with NAs
        empty_summary <- data.frame(Variable = var, 
                                    Statistic = "Count (%)", 
                                    Factor_Level = NA_character_,
                                    stringsAsFactors = FALSE)
        # Add one column for each group level with "NA" values
        for (group_level in levels(data[[group_var]])) {
          empty_summary[[as.character(group_level)]] <- "NA"
        }
        results_list[[var]] <- empty_summary
        next
      }
      
      # Create contingency table (excluding NAs for statistical tests)
      contingency_table <- table(data[[var]], data[[group_var]], useNA = "no")
      
      # Calculate non-NA counts by group for the summary row
      non_na_counts <- data %>%
        dplyr::select(dplyr::all_of(var), dplyr::all_of(group_var)) %>%
        dplyr::filter(!is.na(.data[[var]])) %>%
        dplyr::group_by(across(dplyr::all_of(group_var))) %>%
        dplyr::summarise(non_na_count = dplyr::n(), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = dplyr::all_of(group_var), values_from = non_na_count) %>%
        dplyr::mutate(dplyr::across(-dplyr::any_of(c("Variable", "Statistic", "Factor_Level")), as.character))
      
      # Calculate percentages and format (excluding NAs)
      category_data <- data %>%
        dplyr::select(dplyr::all_of(var), dplyr::all_of(group_var)) %>%
        dplyr::filter(!is.na(.data[[var]])) %>%  # Remove NA values for percentage calculation
        dplyr::group_by(across(dplyr::all_of(group_var)), .data[[var]]) %>%
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
        dplyr::group_by(across(dplyr::all_of(group_var))) %>%
        dplyr::mutate(Percentage = Count / sum(Count) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          summary_stat = paste0(Count, " (", round(Percentage, 1), "%)"),
          Factor_Level = as.character(.data[[var]])
        ) %>%
        dplyr::select(Factor_Level, dplyr::all_of(group_var), summary_stat) %>%
        tidyr::pivot_wider(names_from = dplyr::all_of(group_var), values_from = summary_stat)
      
      # Add metadata to category data
      category_data <- category_data %>%
        dplyr::mutate(
          Variable = var, 
          Statistic = "Count (%)", 
          .before = 1
        )
      
      # Create the non-NA count summary row
      non_na_summary <- non_na_counts %>%
        dplyr::mutate(
          Variable = var,
          Statistic = "Non-missing observations",
          Factor_Level = NA_character_,
          .before = 1
        )
      
      # Combine category data first, then non-NA summary row
      combined_data <- dplyr::bind_rows(category_data, non_na_summary)
      results_list[[var]] <- combined_data
    }
  }
  
  # Combine all results in the order they were processed
  descriptive_table <- dplyr::bind_rows(results_list) %>%
    dplyr::select(Variable, Statistic, Factor_Level, dplyr::everything())
  
  # Apply variable labels if provided
  if (!is.null(variable_labels)) {
    descriptive_table <- descriptive_table %>%
      dplyr::mutate(
        Variable = ifelse(Variable %in% names(variable_labels), 
                          variable_labels[Variable], 
                          Variable)
      )
  }
  
  # Create header
  header <- c("Variable", "Statistic", "Factor Level", group_labels)
  
  # Return appropriate format
  if (knitr::is_html_output()) {
    # HTML output with styling
    descriptive_table %>%
      knitr::kable(
        col.names = header, 
        align = "c",
        format = "html",
        table.attr = "class='table table-striped table-hover'"
      ) %>%
      kableExtra::kable_styling(
        full_width = FALSE, 
        bootstrap_options = c("striped", "hover", "condensed"),
        position = "center"
      ) %>%
      kableExtra::column_spec(1, bold = TRUE)
  } else {
    # Simple output for Word/PDF
    colnames(descriptive_table) <- header
    knitr::kable(
      descriptive_table, 
      format = "pipe",
      align = "c"
    )
  }
}