#' @title Automatic ANOVA for Bus Pricing Analysis
#' @description
#' Performs one-way ANOVA tests to evaluate whether the mean base price
#' of buses significantly differs based on categorical factors such as
#' source type, purchase year, manufacturer, bus type, seating capacity, and more.
#'
#' For each categorical variable, the function:
#'   1. Fits an ANOVA model (`aov`)
#'   2. Checks assumptions of normality and equal variances
#'   3. Displays significance results
#'   4. Runs Tukey’s HSD post-hoc test for significant factors
#'   5. Generates a boxplot visualization
#'
#' @param df A data frame containing the bus dataset.
#' @param response_col Character. The column name for the response variable (default: `"base_price"`).
#' @param alpha Numeric. Significance level for hypothesis tests (default = 0.05).
#'
#' @return A list containing ANOVA model results, p-values, and assumption test results for each factor.
#' @examples
#' \dontrun{
#' # Example usage:
#' results <- run_bus_anova(df, response_col = "base_price")
#' }
#' @export
#'

install.packages(c("car", "ggplot2", "dplyr"))

run_bus_anova <- function(df, response_col = "base_price", alpha = 0.05) {
  
  # --- Load required packages -----------------------------------------
  library(dplyr)     # For data manipulation
  library(ggplot2)   # For visualization
  library(car)       # For Levene's test (homogeneity of variances)
  
  # --- Ensure the response column exists ------------------------------
  if (!response_col %in% names(df)) {
    stop(paste("Response column", response_col, "not found in dataset."))
  }
  
  # --- Identify categorical variables ---------------------------------
  # We consider factors or character columns (excluding the response variable)
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  cat_vars <- setdiff(cat_vars, response_col)
  
  # --- Exclude unwanted columns ---------------------------------------
  exclude_vars <- c("source_url", "date_published_or_updated")
  cat_vars <- setdiff(cat_vars, exclude_vars)
  
  # --- Stop if no categorical variables remain ------------------------
  if (length(cat_vars) == 0) {
    stop("No valid categorical variables found in dataset.")
  }
  
  # --- Initialize list to store results -------------------------------
  results <- list()
  
  # --- Loop through each categorical variable -------------------------
  for (var in cat_vars) {
    cat("\n=====================================================\n")
    cat("Running ANOVA for:", var, "\n")
    cat("=====================================================\n")
    
    # Convert variable to factor (if not already)
    df[[var]] <- as.factor(df[[var]])
    
    # Skip if only one unique value (ANOVA invalid)
    if (nlevels(df[[var]]) < 2) {
      cat("⚠️ Skipping", var, "- only one unique level found.\n")
      next
    }
    
    # Dynamically build formula: base_price ~ var
    formula <- as.formula(paste("`", response_col, "` ~ `", var, "`", sep = ""))
    
    # --- Fit ANOVA model ----------------------------------------------
    model <- aov(formula, data = df)
    
    # Display ANOVA summary
    summary_out <- summary(model)
    print(summary_out)
    
    # Extract p-value
    p_val <- summary_out[[1]][["Pr(>F)"]][1]
    
    # --- Check assumptions --------------------------------------------
    cat("\n-- Checking assumptions --\n")
    
    # 1. Normality of residuals (Shapiro-Wilk test)
    shapiro_p <- shapiro.test(residuals(model))$p.value
    
    # 2. Homogeneity of variances (Levene's test)
    levene_p <- car::leveneTest(formula, data = df)[["Pr(>F)"]][1]
    
    cat(sprintf("Shapiro-Wilk p = %.4f (normality)\n", shapiro_p))
    cat(sprintf("Levene p = %.4f (equal variances)\n", levene_p))
    
    # --- Store results -------------------------------------------------
    results[[var]] <- list(
      model = model,
      p_val = p_val,
      shapiro_p = shapiro_p,
      levene_p = levene_p
    )
    
    # --- Interpretation & Visualization -------------------------------
    if (p_val < alpha) {
      cat("\n✅ Significant difference detected for", var, "\n")
      
      # Run Tukey HSD post-hoc test
      print(TukeyHSD(model))
      
      # Create and display a boxplot
      p <- ggplot(df, aes_string(x = var, y = response_col, fill = var)) +
        geom_boxplot(alpha = 0.7, outlier.color = "red") +
        theme_minimal(base_size = 13) +
        labs(
          title = paste("Base Price by", var),
          subtitle = paste("ANOVA p =", round(p_val, 4)),
          x = var,
          y = "Base Price"
        ) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(
            angle = -45,     # tilt upward right
            hjust = 0,       # anchor left edge of label
            vjust = 1        # lift slightly
          ),
          plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
          plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
          axis.title.x = element_text(margin = margin(t = 15)),
          axis.title.y = element_text(margin = margin(r = 15)),
          plot.margin = margin(t = 15, r = 120, b = 25, l = 15) 
        ) +
        theme(plot.title.position = "plot")
      
      print(p)
      
    } else {
      cat("\n❌ No significant difference detected for", var, "\n")
    }
  }
  
  # --- Return all results invisibly -----------------------------------
  invisible(results)
}

# =====================================================
# Example usage (with your uploaded data)
# =====================================================
df <- read.csv("buses.csv")
results <- run_bus_anova(df, response_col = "base_price")

