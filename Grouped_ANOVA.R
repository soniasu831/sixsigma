#' @title Grouped ANOVA Significance Summary for Bus Pricing
#' @description
#' Runs one-way ANOVAs across subgroups (e.g., by state), testing a chosen factor (e.g., manufacturer)
#' against a numeric response (e.g., base price). Automatically returns a table summarizing
#' which groups have significant differences and prints assumption test results.
#'
#' @param df Data frame containing the bus dataset.
#' @param group_by Character. Column to group by (e.g. "state").
#' @param factor_col Character. Column to use as the categorical factor in ANOVA (e.g. "bus_manufacturer").
#' @param response_col Character. Response variable (default = "base_price").
#' @param alpha Numeric. Significance threshold (default = 0.05).
#' 
#' @return A data frame summarizing group, p-value, and whether the ANOVA was significant.
#' @examples
#' results_table <- run_grouped_anova(df, group_by = "state",
#'                                    factor_col = "bus_manufacturer",
#'                                    response_col = "base_price")
#' @export

run_grouped_anova <- function(df, group_by, factor_col, response_col = "base_price", alpha = 0.05) {
  
  library(dplyr)
  library(car)
  
  # --- Input checks --------------------------------------------------
  if (!all(c(group_by, factor_col, response_col) %in% names(df))) {
    stop("One or more specified columns not found in dataset.")
  }
  
  df[[group_by]] <- as.factor(df[[group_by]])
  df[[factor_col]] <- as.factor(df[[factor_col]])
  
  grouped_data <- split(df, df[[group_by]])
  
  results <- data.frame(
    Group = character(),
    DF_Factor = numeric(),
    DF_Residuals = numeric(),
    F_value = numeric(),
    P_value = numeric(),
    Shapiro_p = numeric(),
    Levene_p = numeric(),
    Significant = logical(),
    stringsAsFactors = FALSE
  )
  
  for (g in names(grouped_data)) {
    sub_df <- grouped_data[[g]]
    
    # Skip small groups or single-level factors
    if (nrow(sub_df) < 3 || nlevels(sub_df[[factor_col]]) < 2) {
      next
    }
    
    formula <- as.formula(paste("`", response_col, "` ~ `", factor_col, "`", sep = ""))
    
    model <- tryCatch(aov(formula, data = sub_df), error = function(e) NULL)
    if (is.null(model)) next
    
    summary_out <- tryCatch(summary(model)[[1]], error = function(e) NULL)
    if (is.null(summary_out) || !"Pr(>F)" %in% names(summary_out)) next
    
    # Extract safe statistics
    p_val <- as.numeric(summary_out[1, "Pr(>F)"])
    f_val <- as.numeric(summary_out[1, "F value"])
    df_factor <- as.numeric(summary_out[1, "Df"])
    df_res <- as.numeric(summary_out["Residuals", "Df"])
    
    shapiro_p <- tryCatch(shapiro.test(residuals(model))$p.value, error = function(e) NA)
    levene_p <- tryCatch(leveneTest(formula, data = sub_df)[["Pr(>F)"]][1], error = function(e) NA)
    
    results <- rbind(results, data.frame(
      Group = g,
      DF_Factor = df_factor,
      DF_Residuals = df_res,
      F_value = f_val,
      P_value = p_val,
      Shapiro_p = shapiro_p,
      Levene_p = levene_p,
      Significant = !is.na(p_val) && p_val < alpha,
      stringsAsFactors = FALSE
    ))
  }
  
  results <- results %>%
    arrange(P_value) %>%
    mutate(Significance = ifelse(Significant, "✅ Significant", "❌ Not significant"))
  
  print(results)
  invisible(results)
}


# Load your data
df <- read.csv("buses_with_price_per_seat_and_converted_dates.csv")

# Example 1: Run ANOVA by state within each manufacturer
results_table_1 <- run_grouped_anova(
  df,
  group_by = "bus_manufacturer",
  factor_col = "state",
  response_col = "base_price"
)

# Example 2: Run ANOVA by state within each type
results_table_2 <- run_grouped_anova(
  df,
  group_by = "bus_type",
  factor_col = "state",
  response_col = "base_price"
)

# Example 3: Run ANOVA by manufacturer within each type
results_table_3 <- run_grouped_anova(
  df,
  group_by = "bus_type",
  factor_col = "bus_manufacturer",
  response_col = "base_price"
)
