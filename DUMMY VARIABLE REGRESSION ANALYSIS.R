# Load required packages
install.packages(c("dplyr", "tidyr", "broom", "ggplot2", "car", "lmtest"))
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(car)
library(lmtest)

# 1. CREATE SAMPLE DATA (Replace this with your actual data loading)
# Assuming your data is in a CSV file named 'gtbank_data.csv'
getwd()
setwd("C:/Users/pc/Downloads")
sample_data <- read.csv("bank-data.csv")
print("Original Data:")
print(sample_data)

# 2. RESHAPE DATA FROM WIDE TO LONG FORMAT
long_data <- sample_data %>%
  pivot_longer(
    cols = c(West_Africa, East_Africa, Europe),
    names_to = "Region",
    values_to = "Revenue"
  ) %>%
  arrange(Year, Region)

# CREATE DUMMY VARIABLES AND TIME VARIABLE
analysis_data <- long_data %>%
  mutate(
    # Dummy variables (Reference: West_Africa)
    D1 = as.numeric(Region == "East_Africa"),
    D2 = as.numeric(Region == "Europe"),
    # Time variable (2013 = 1, 2014 = 2, ..., 2024 = 12)
    T = Year - 2012,
    # Interaction terms
    D1T = D1 * T,
    D2T = D2 * T
  )

print("Transformed Data for Analysis:")
print(head(analysis_data, 36))
print(analysis_data[21:36, ])

# 4. EXPLORATORY DATA ANALYSIS
# Summary statistics by region
cat("\n--- Summary Statistics by Region ---\n")
summary_stats <- analysis_data %>%
  group_by(Region) %>%
  summarise(
    Mean_Revenue = mean(Revenue),
    SD_Revenue = sd(Revenue),
    Min_Revenue = min(Revenue),
    Max_Revenue = max(Revenue),
    Growth_Rate = (last(Revenue) - first(Revenue)) / first(Revenue) * 100
  )
print(summary_stats)

# Time series plot
ggplot(analysis_data, aes(x = Year, y = Revenue, color = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "GTBank Revenue Trends by Region (2013-2024)",
       y = "Revenue", x = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("West_Africa" = "blue", 
                                "East_Africa" = "red", 
                                "Europe" = "green"))

# 5. RUN THE DUMMY VARIABLE REGRESSION
model <- lm(Revenue ~ D1 + D2 + T + D1T + D2T, data = analysis_data)

cat("\n--- REGRESSION RESULTS ---\n")
print(summary(model))

# Tidy output for better interpretation
tidy_results <- tidy(model)
cat("\n--- TIDY REGRESSION RESULTS ---\n")
print(tidy_results)

# 6. INTERPRETATION OF RESULTS
cat("\n--- INTERPRETATION GUIDE ---\n")
cat("Reference Region: West_Africa\n\n")

coefficients <- coef(model)

cat("Starting Revenue (2013):\n")
cat(sprintf("West_Africa: %.2f\n", coefficients["(Intercept)"]))
cat(sprintf("East_Africa: %.2f (Difference: %.2f, p-value: %.4f)\n", 
            coefficients["(Intercept)"] + coefficients["D1"],
            coefficients["D1"],
            tidy_results$p.value[tidy_results$term == "D1"]))
cat(sprintf("Europe: %.2f (Difference: %.2f, p-value: %.4f)\n\n", 
            coefficients["(Intercept)"] + coefficients["D2"],
            coefficients["D2"],
            tidy_results$p.value[tidy_results$term == "D2"]))

cat("Annual Growth Rates:\n")
cat(sprintf("West_Africa: %.2f units per year\n", coefficients["T"]))
cat(sprintf("East_Africa: %.2f units per year (Difference: %.2f, p-value: %.4f)\n", 
            coefficients["T"] + coefficients["D1T"],
            coefficients["D1T"],
            tidy_results$p.value[tidy_results$term == "D1T"]))
cat(sprintf("Europe: %.2f units per year (Difference: %.2f, p-value: %.4f)\n\n", 
            coefficients["T"] + coefficients["D2T"],
            coefficients["D2T"],
            tidy_results$p.value[tidy_results$term == "D2T"]))

# 7. ANSWERING YOUR RESEARCH AIMS
cat("--- ANSWERS TO RESEARCH AIMS ---\n")

# Generate the complete ANOVA table
cat("--- COMPLETE ANOVA TABLE ---\n")
anova_table <- anova(model)
print(anova_table)

# Calculate overall model significance
f_statistic <- summary(model)$fstatistic[1]
f_pvalue <- pf(summary(model)$fstatistic[1], 
               summary(model)$fstatistic[2], 
               summary(model)$fstatistic[3], 
               lower.tail = FALSE)

cat("\nAim 1: Effect of regions on revenue:\n")
cat(sprintf("Overall model F-statistic: %.2f, p-value: %.4f\n", f_statistic, f_pvalue))
if(f_pvalue < 0.05) {
  cat("CONCLUSION: The regression model is statistically significant.\n")
  cat("This means regions and time together have a significant effect on revenue.\n")
} else {
  cat("CONCLUSION: The regression model is not statistically significant.\n")
}

# Additional interpretation of ANOVA components
cat("\n--- ANOVA COMPONENT INTERPRETATION ---\n")
cat("The ANOVA table breaks down the contribution of each variable:\n\n")

# Extract key information from ANOVA table
ss_regression <- sum(anova_table$`Sum Sq`[1:5])  # Sum of squares for all predictors
ss_residual <- anova_table$`Sum Sq`[6]           # Sum of squares for residuals
df_regression <- sum(anova_table$Df[1:5])        # Degrees of freedom for predictors
df_residual <- anova_table$Df[6]                 # Degrees of freedom for residuals

cat(sprintf("Total variance explained by model: %.2f (Sum Sq Regression)\n", ss_regression))
cat(sprintf("Unexplained variance (error): %.2f (Sum Sq Residual)\n", ss_residual))
cat(sprintf("Proportion of variance explained: %.1f%%\n", (ss_regression/(ss_regression + ss_residual))*100))

# Check significance of individual components
cat("\nIndividual Variable Significance:\n")
for(i in 1:5) {
  variable <- rownames(anova_table)[i]
  p_value <- anova_table$`Pr(>F)`[i]
  significance <- ifelse(p_value < 0.05, "SIGNIFICANT", "Not significant")
  
  if(!is.na(p_value)) {
    cat(sprintf("- %-6s: p-value = %.4f (%s)\n", variable, p_value, significance))
  }
}

# Enhanced interpretation for your research aims
cat("\n--- ENHANCED INTERPRETATION FOR RESEARCH AIMS ---\n")

# Test if region dummies collectively matter (D1, D2, D1T, D2T)
cat("To specifically test 'Effect of regions on revenue', we should check if\n")
cat("the region-related terms (D1, D2, D1T, D2T) are jointly significant:\n\n")

# Create a reduced model without region terms for comparison
reduced_model <- lm(Revenue ~ T, data = analysis_data)
full_model <- model

# Perform F-test comparing models
comparison_test <- anova(reduced_model, full_model)
cat("F-test comparing model with vs without region terms:\n")
print(comparison_test)

if(comparison_test$`Pr(>F)`[2] < 0.05) {
  cat(sprintf("\nCONCLUSION: Regions have a statistically significant effect on revenue \n"))
  cat(sprintf("(F = %.2f, p-value = %.4f)\n", 
              comparison_test$F[2], comparison_test$`Pr(>F)`[2]))
} else {
  cat(sprintf("\nCONCLUSION: Regions do not have a statistically significant effect on revenue\n"))
  cat(sprintf("(F = %.2f, p-value = %.4f)\n", 
              comparison_test$F[2], comparison_test$`Pr(>F)`[2]))
}





# =============================================================================
# AIM 2: WHICH REGION CONTRIBUTES MOST TO GROWTH - DETAILED ANALYSIS
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("AIM 2: WHICH GEOGRAPHICAL REGION CONTRIBUTES MOST TO REVENUE GROWTH\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# Extract coefficients with explicit source identification
cat("STEP 1: Extract coefficients from regression model:\n")
cat("---------------------------------------------------\n")
coefficients_table <- tidy(model)
print(coefficients_table)

cat("\nSTEP 2: Calculate annual growth rates for each region:\n")
cat("------------------------------------------------------\n")

# CORRECTED: Extract coefficients as numeric values, not named vectors
T_coef <- as.numeric(coefficients["T"])
D1T_coef <- as.numeric(coefficients["D1T"])
D2T_coef <- as.numeric(coefficients["D2T"])

# Alternative method: Extract from tidy results
T_coef_alt <- tidy_results$estimate[tidy_results$term == "T"]
D1T_coef_alt <- tidy_results$estimate[tidy_results$term == "D1T"]
D2T_coef_alt <- tidy_results$estimate[tidy_results$term == "D2T"]

cat("Method 1 - From coefficients() function:\n")
cat(sprintf("Base growth rate (T coefficient from regression table): %.2f\n", T_coef))
cat(sprintf("East_Africa interaction effect (D1T coefficient): %.2f\n", D1T_coef))
cat(sprintf("Europe interaction effect (D2T coefficient): %.2f\n\n", D2T_coef))

cat("Method 2 - From tidy results:\n")
cat(sprintf("Base growth rate (T coefficient): %.2f\n", T_coef_alt))
cat(sprintf("East_Africa interaction effect (D1T coefficient): %.2f\n", D1T_coef_alt))
cat(sprintf("Europe interaction effect (D2T coefficient): %.2f\n\n", D2T_coef_alt))

# Calculate growth rates with explicit formulas using numeric values
growth_rates <- c(
  West_Africa = T_coef,
  East_Africa = T_coef + D1T_coef,
  Europe = T_coef + D2T_coef
)

cat("Growth Rate Calculations:\n")
cat(sprintf("West_Africa growth = T = %.2f\n", growth_rates["West_Africa"]))
cat(sprintf("East_Africa growth = T + D1T = %.2f + (%.2f) = %.2f\n", 
            T_coef, D1T_coef, growth_rates["East_Africa"]))
cat(sprintf("Europe growth = T + D2T = %.2f + (%.2f) = %.2f\n\n", 
            T_coef, D2T_coef, growth_rates["Europe"]))

# Statistical test for growth rate differences
cat("STEP 3: Statistical significance of growth rate differences:\n")
cat("------------------------------------------------------------\n")

# Get p-values from tidy results
d1t_pvalue <- tidy_results$p.value[tidy_results$term == "D1T"]
d2t_pvalue <- tidy_results$p.value[tidy_results$term == "D2T"]
d1t_tstat <- tidy_results$statistic[tidy_results$term == "D1T"]
d2t_tstat <- tidy_results$statistic[tidy_results$term == "D2T"]

# Test if East_Africa growth is significantly different from West_Africa
cat("Hypothesis Test: East_Africa vs West_Africa growth rates\n")
cat("H0: East_Africa growth rate = West_Africa growth rate\n")
cat("Ha: East_Africa growth rate ≠ West_Africa growth rate\n")
cat(sprintf("Test statistic: t = %.3f, p-value = %.6f\n", d1t_tstat, d1t_pvalue))

# Test if Europe growth is significantly different from West_Africa
cat("\nHypothesis Test: Europe vs West_Africa growth rates\n")
cat("H0: Europe growth rate = West_Africa growth rate\n")
cat("Ha: Europe growth rate ≠ West_Africa growth rate\n")
cat(sprintf("Test statistic: t = %.3f, p-value = %.6f\n", d2t_tstat, d2t_pvalue))

# Identify the region with maximum growth
max_growth_region <- names(which.max(growth_rates))
max_growth_rate <- max(growth_rates)

cat("\nSTEP 4: Final determination:\n")
cat("----------------------------\n")
cat(sprintf("Highest growth rate: %s with %.2f units/year\n", max_growth_region, max_growth_rate))
cat("\nGROWTH RATE RANKING:\n")
growth_ranking <- sort(growth_rates, decreasing = TRUE)
for(i in 1:length(growth_ranking)) {
  cat(sprintf("%d. %s: %.2f units/year\n", i, names(growth_ranking)[i], growth_ranking[i]))
}

# Calculate percentage differences
west_growth <- growth_rates["West_Africa"]
east_growth <- growth_rates["East_Africa"]
europe_growth <- growth_rates["Europe"]

east_percent_diff <- ((east_growth - west_growth) / west_growth) * 100
europe_percent_diff <- ((europe_growth - west_growth) / west_growth) * 100

cat(sprintf("\nGrowth rate compared to West_Africa:\n"))
cat(sprintf("East_Africa: %.1f%% of West_Africa's growth rate\n", (east_growth/west_growth)*100))
cat(sprintf("Europe: %.1f%% of West_Africa's growth rate\n", (europe_growth/west_growth)*100))

cat(sprintf("\nCONCLUSION FOR AIM 2: %s contributes most significantly to revenue growth\n", 
            max_growth_region))
cat(sprintf("with an annual growth rate of %.2f units.\n", max_growth_rate))

if(d1t_pvalue < 0.05 | d2t_pvalue < 0.05) {
  cat("The growth rate differences are statistically significant (p < 0.05).\n")
} else {
  cat("The growth rate differences are not statistically significant.\n")
}


# =============================================================================
# AIM 3: EFFECT OF TIME ON REVENUE - DETAILED ANALYSIS
# =============================================================================

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("AIM 3: EXAMINE THE EFFECT OF TIME ON GTBANK'S REVENUE\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

cat("STEP 1: Locate the Time coefficient in regression results:\n")
cat("---------------------------------------------------------\n")
cat("Looking for the 'T' term in the coefficients table:\n")
time_row <- tidy_results[tidy_results$term == "T", ]
print(time_row)

cat("\nSTEP 2: Statistical hypothesis test:\n")
cat("------------------------------------\n")
cat("H0: Time has no effect on revenue (β₃ = 0)\n")
cat("Ha: Time has an effect on revenue (β₃ ≠ 0)\n\n")

time_estimate <- time_row$estimate
time_std_error <- time_row$std.error
time_t_stat <- time_row$statistic
time_pvalue <- time_row$p.value

cat(sprintf("Time coefficient (β₃) = %.2f\n", time_estimate))
cat(sprintf("Standard Error = %.2f\n", time_std_error))
cat(sprintf("t-statistic = %.3f\n", time_t_stat))
cat(sprintf("p-value = %.6f\n\n", time_pvalue))

cat("STEP 3: Interpretation:\n")
cat("-----------------------\n")
if(time_pvalue < 0.05) {
  cat("✓ REJECT null hypothesis (p < 0.05)\n")
  cat(sprintf("CONCLUSION: Time has a statistically significant effect on revenue.\n"))
  cat(sprintf("For each year, revenue changes by %.2f units on average.\n", time_estimate))
  if(time_estimate > 0) {
    cat("This represents a POSITIVE time trend - revenue increases over time.\n")
  } else {
    cat("This represents a NEGATIVE time trend - revenue decreases over time.\n")
  }
} else {
  cat("✓ FAIL TO REJECT null hypothesis (p ≥ 0.05)\n")
  cat("CONCLUSION: Time does not have a statistically significant effect on revenue.\n")
}

# =============================================================================
# AIM 4: DOES INFLUENCE OF REGION DEPEND ON TIME - DETAILED ANALYSIS
# =============================================================================

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("AIM 4: DOES INFLUENCE OF REGION ON REVENUE DEPEND ON LEVEL OF TIME?\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

cat("STEP 1: Examine interaction terms in regression results:\n")
cat("-------------------------------------------------------\n")
cat("Interaction terms test whether the effect of region changes over time.\n\n")

# Extract interaction terms explicitly
interaction_terms <- tidy_results[tidy_results$term %in% c("D1T", "D2T"), ]
print(interaction_terms)

cat("\nSTEP 2: Statistical hypothesis tests for interactions:\n")
cat("------------------------------------------------------\n")

# D1T interaction test
d1t_row <- tidy_results[tidy_results$term == "D1T", ]
cat("Interaction: East_Africa × Time (D1T)\n")
cat("H0: The effect of East_Africa vs West_Africa does NOT depend on time (β₄ = 0)\n")
cat("Ha: The effect of East_Africa vs West-Africa DOES depend on time (β₄ ≠ 0)\n")
cat(sprintf("t-statistic = %.3f, p-value = %.6f\n", 
            d1t_row$statistic, d1t_row$p.value))

# D2T interaction test  
d2t_row <- tidy_results[tidy_results$term == "D2T", ]
cat("\nInteraction: Europe × Time (D2T)\n")
cat("H0: The effect of Europe vs West_Africa does NOT depend on time (β₅ = 0)\n")
cat("Ha: The effect of Europe vs West_Africa DOES depend on time (β₅ ≠ 0)\n")
cat(sprintf("t-statistic = %.3f, p-value = %.6f\n", 
            d2t_row$statistic, d2t_row$p.value))

cat("\nSTEP 3: Overall conclusion about interaction effects:\n")
cat("-----------------------------------------------------\n")

interaction_pvalues <- c(
  D1T = d1t_row$p.value,
  D2T = d2t_row$p.value
)

significant_interactions <- sum(interaction_pvalues < 0.05)
total_interactions <- length(interaction_pvalues)

cat(sprintf("Number of significant interactions: %d out of %d\n", 
            significant_interactions, total_interactions))

if(any(interaction_pvalues < 0.05)) {
  cat("✓ REJECT the null hypothesis for at least one interaction\n")
  cat("CONCLUSION: YES, the influence of region on revenue DEPENDS on time.\n")
  cat("This means the growth trajectories are significantly different across regions.\n\n")
  
  # Specify which interactions are significant
  cat("Significant regional differences in growth patterns:\n")
  if(interaction_pvalues["D1T"] < 0.05) {
    cat("- East_Africa has a significantly different growth pattern from West_Africa\n")
  }
  if(interaction_pvalues["D2T"] < 0.05) {
    cat("- Europe has a significantly different growth pattern from West_Africa\n")
  }
} else {
  cat("✓ FAIL TO REJECT the null hypothesis for all interactions\n")
  cat("CONCLUSION: NO, the influence of region on revenue does NOT depend on time.\n")
  cat("This means regions have parallel growth trends over time.\n")
}

# =============================================================================
# SUMMARY TABLE OF KEY FINDINGS
# =============================================================================

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("SUMMARY TABLE: KEY STATISTICAL FINDINGS\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

summary_df <- data.frame(
  Research_Aim = c(
    "Aim 1: Overall regional effect",
    "Aim 2: Highest growth region", 
    "Aim 3: Time effect significance",
    "Aim 4: Time-region interaction"
  ),
  Statistical_Test = c(
    "Overall F-test",
    "Growth rate comparison",
    "T-coefficient t-test", 
    "Interaction term t-tests"
  ),
  Test_Statistic = c(
    sprintf("F = %.2f", f_statistic),
    sprintf("Max growth = %.2f", max_growth_rate),
    sprintf("t = %.3f", time_t_stat),
    sprintf("D1T: t = %.3f, D2T: t = %.3f", d1t_row$statistic, d2t_row$statistic)
  ),
  P_Value = c(
    sprintf("%.6f", f_pvalue),
    "N/A",
    sprintf("%.6f", time_pvalue),
    sprintf("D1T: %.6f, D2T: %.6f", d1t_row$p.value, d2t_row$p.value)
  ),
  Conclusion = c(
    ifelse(f_pvalue < 0.05, "Significant regional effects", "No significant effects"),
    max_growth_region,
    ifelse(time_pvalue < 0.05, "Significant time effect", "No time effect"),
    ifelse(any(interaction_pvalues < 0.05), "Significant interaction", "No interaction")
  )
)

print(summary_df)
cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")









# 8. REGRESSION ASSUMPTION CHECKING
cat("\n--- REGRESSION ASSUMPTION DIAGNOSTICS ---\n")

# 8.1 Normality of residuals
shapiro_test <- shapiro.test(residuals(model))
cat("1. Normality of Residuals (Shapiro-Wilk test):\n")
cat(sprintf("   W = %.4f, p-value = %.4f\n", shapiro_test$statistic, shapiro_test$p.value))

if(shapiro_test$p.value > 0.05) {
  cat("   ✓ Residuals are normally distributed (assumption satisfied)\n")
} else {
  cat("   ✗ Residuals are not normally distributed (assumption violated)\n")
}

# QQ plot for normality
qqnorm(residuals(model))
qqline(residuals(model), col = "red")
title("Q-Q Plot: Normality of Residuals")

# 8.2 Homoscedasticity (Constant variance)
bp_test <- bptest(model)
cat("\n2. Homoscedasticity (Breusch-Pagan test):\n")
cat(sprintf("   BP = %.4f, p-value = %.4f\n", bp_test$statistic, bp_test$p.value))

if(bp_test$p.value > 0.05) {
  cat("   ✓ Constant variance (homoscedasticity) assumption satisfied\n")
} else {
  cat("   ✗ Non-constant variance (heteroscedasticity) detected\n")
}

# Plot residuals vs fitted values
plot(fitted(model), residuals(model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# 8.3 Independence of residuals (Durbin-Watson test)
dw_test <- dwtest(model)
cat("\n3. Independence of Residuals (Durbin-Watson test):\n")
cat(sprintf("   DW = %.4f, p-value = %.4f\n", dw_test$statistic, dw_test$p.value))

if(dw_test$p.value > 0.05) {
  cat("   ✓ Residuals are independent (no autocorrelation)\n")
} else {
  cat("   ✗ Residuals show autocorrelation\n")
}

# 8.4 Multicollinearity check
vif_values <- vif(model)
cat("\n4. Multicollinearity Check (Variance Inflation Factors):\n")
print(vif_values)

if(all(vif_values < 5)) {
  cat("   ✓ No concerning multicollinearity (all VIF < 5)\n")
} else if(all(vif_values < 10)) {
  cat("   ! Moderate multicollinearity (some VIF between 5-10)\n")
} else {
  cat("   ✗ High multicollinearity detected (some VIF > 10)\n")
}

# 8.5 Linearity check
cat("\n5. Linearity Check (Component + Residual plots):\n")
crPlots(model, 
        ylab = "Comp + Resid",
        cex.lab = 1.2,
        main = "Component+Residual Plots: Linearity Check")


# =============================================================================
# CONTINUING FROM YOUR ASSUMPTION CHECKING RESULTS
# =============================================================================

# First, let's create the pnorm and qnorm graphs to understand the normal distribution
cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("UNDERSTANDING NORMAL DISTRIBUTION WITH pnorm() AND qnorm()\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# =============================================================================
# 1. pnorm() AND qnorm() GRAPHS
# =============================================================================


# Graph 1: pnorm() - Cumulative Distribution Function
x <- seq(-4, 4, length.out = 1000)
y_cdf <- pnorm(x)

plot(x, y_cdf, type = "l", lwd = 2, col = "blue",
     main = "pnorm() - Cumulative Distribution Function\nP(Z ≤ z)",
     xlab = "Z-value", ylab = "Cumulative Probability P(Z ≤ z)")
grid()

# Add key points
key_points <- c(-1.96, -1, 0, 1, 1.96)
for(point in key_points) {
  points(point, pnorm(point), col = "red", pch = 19, cex = 1.2)
}
abline(h = c(0, 0.025, 0.5, 0.975, 1), lty = 2, col = "gray50")

# Graph 2: qnorm() - Quantile Function (Inverse CDF)
p <- seq(0.001, 0.999, length.out = 1000)
q_values <- qnorm(p)

plot(p, q_values, type = "l", lwd = 2, col = "red",
     main = "qnorm() - Quantile Function\nFinds z such that P(Z ≤ z) = p",
     xlab = "Probability p", ylab = "Quantile z")
grid()

# Add key probabilities
key_probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
for(prob in key_probs) {
  points(prob, qnorm(prob), col = "blue", pch = 19, cex = 1.2)
}

# Graph 3: Demonstration of inverse relationship
z_val <- 1.5
p_val <- pnorm(z_val)
q_val <- qnorm(p_val)

plot(x, pnorm(x), type = "l", lwd = 2, col = "purple",
     main = "Inverse Relationship: pnorm(qnorm(p)) = p",
     xlab = "Z-value", ylab = "P(Z ≤ z)")
abline(v = z_val, lty = 2, col = "red")
abline(h = p_val, lty = 2, col = "red")
points(z_val, p_val, col = "red", pch = 19, cex = 1.5)
text(0, 0.8, sprintf("pnorm(%.1f) = %.3f", z_val, p_val), col = "red")
text(0, 0.7, sprintf("qnorm(%.3f) = %.1f", p_val, q_val), col = "red")

# Graph 4: Your actual residuals vs normal distribution
qqnorm(residuals(model), main = "Your Data: Q-Q Plot of Residuals")
qqline(residuals(model), col = "red")

# Reset plotting area
par(mfrow = c(1, 1))

# =============================================================================
# 2. LOG-LINEAR TRANSFORMATION TO ADDRESS ASSUMPTION VIOLATIONS
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("LOG-LINEAR TRANSFORMATION TO ADDRESS ASSUMPTION VIOLATIONS\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# Check if we have any zero or negative revenues (can't take log of these)
cat("Checking revenue data for log transformation:\n")
cat(sprintf("Minimum revenue: %.2f\n", min(analysis_data$Revenue)))
cat(sprintf("Number of zero/negative revenues: %d\n", sum(analysis_data$Revenue <= 0)))

# Apply log transformation
if(min(analysis_data$Revenue) <= 0) {
  # Add a constant to make all values positive
  constant <- abs(min(analysis_data$Revenue)) + 1
  analysis_data$Log_Revenue <- log(analysis_data$Revenue + constant)
  cat(sprintf("Added constant %.2f before log transformation\n", constant))
} else {
  analysis_data$Log_Revenue <- log(analysis_data$Revenue)
  cat("Applied direct log transformation (all revenues > 0)\n")
}

cat(sprintf("Range of log-transformed revenue: [%.4f, %.4f]\n", 
            min(analysis_data$Log_Revenue), max(analysis_data$Log_Revenue)))

# Fit the log-linear model
cat("\nFitting log-linear model...\n")
log_model <- lm(Log_Revenue ~ D1 + D2 + T + D1T + D2T, data = analysis_data)

# Display log model summary
cat("\n--- LOG-LINEAR MODEL SUMMARY ---\n")
print(summary(log_model))

# Compare R-squared values
cat(sprintf("\nR-squared comparison:\n"))
cat(sprintf("Original model:  %.4f\n", summary(model)$r.squared))
cat(sprintf("Log-linear model: %.4f\n", summary(log_model)$r.squared))


# For viewing the complete transformed dataset
cat("\nCOMPLETE LOG-TRANSFORMED DATASET:\n")
cat("=================================\n")

# Option 1: View all data in console (might be large)
print(analysis_data)

# Option 2: Save to CSV for external viewing
write.csv(analysis_data, "log_transformed_gtbank_data.csv", row.names = FALSE)
cat("Complete dataset saved to 'log_transformed_gtbank_data.csv'\n")

# Option 3: Create a nicely formatted table for reporting
library(knitr)
cat("\nFORMATTED LOG-TRANSFORMED DATA TABLE:\n")
cat("====================================\n")
kable(analysis_data, format = "simple", digits = 4)

# Option 4: Regional summary of transformation effects
cat("\nREGIONAL SUMMARY OF TRANSFORMATION EFFECTS:\n")
cat("==========================================\n")
regional_summary <- analysis_data %>%
  group_by(Region) %>%
  summarise(
    Avg_Revenue = mean(Revenue),
    Avg_Log_Revenue = mean(Log_Revenue),
    SD_Revenue = sd(Revenue),
    SD_Log_Revenue = sd(Log_Revenue),
    CV_Revenue = sd(Revenue)/mean(Revenue),  # Coefficient of variation
    CV_Log_Revenue = sd(Log_Revenue)/mean(Log_Revenue)
  )

print(regional_summary)

cat("\nNote: Coefficient of Variation (CV) shows relative variability.\n")
cat("Lower CV after transformation indicates reduced relative variability.\n")


# =============================================================================
# 3. INTERPRETING LOG-LINEAR MODEL COEFFICIENTS
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("INTERPRETING LOG-LINEAR MODEL COEFFICIENTS\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# Extract coefficients
log_coef <- coef(log_model)
log_se <- summary(log_model)$coefficients[, 2]

interpretation_df <- data.frame(
  Coefficient = names(log_coef),
  Estimate = log_coef,
  Std_Error = log_se,
  Percentage_Effect = (exp(log_coef) - 1) * 100,
  Interpretation = ""
)

# Add interpretations
interpretation_df$Interpretation[1] <- "Baseline revenue (West Africa, 2013)"
for(i in 2:nrow(interpretation_df)) {
  pct_effect <- interpretation_df$Percentage_Effect[i]
  if(grepl("T$", interpretation_df$Coefficient[i]) && !grepl("D", interpretation_df$Coefficient[i])) {
    interpretation_df$Interpretation[i] <- sprintf("Annual growth rate: %.1f%% per year", pct_effect)
  } else if(grepl("D1$|D2$", interpretation_df$Coefficient[i])) {
    interpretation_df$Interpretation[i] <- sprintf("Initial difference: %.1f%%", pct_effect)
  } else if(grepl("D1T|D2T", interpretation_df$Coefficient[i])) {
    interpretation_df$Interpretation[i] <- sprintf("Difference in growth rate: %.1f%% per year", pct_effect)
  }
}

print(interpretation_df)

# =============================================================================
# 4. RE-CHECKING ASSUMPTIONS FOR LOG-LINEAR MODEL
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("RE-CHECKING REGRESSION ASSUMPTIONS FOR LOG-LINEAR MODEL\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# 4.1 Normality of residuals for log model
cat("1. NORMALITY OF RESIDUALS (Log-Linear Model):\n")
cat("---------------------------------------------\n")
shapiro_log <- shapiro.test(residuals(log_model))
cat(sprintf("   Shapiro-Wilk test: W = %.4f, p-value = %.4f\n", 
            shapiro_log$statistic, shapiro_log$p.value))

if(shapiro_log$p.value > 0.05) {
  cat("   ✓ Residuals are normally distributed (assumption satisfied)\n")
} else {
  cat("   ✗ Residuals are not normally distributed (assumption violated)\n")
}

# QQ plot for log model
qqnorm(residuals(log_model), main = "Q-Q Plot: Log-Linear Model Residuals")
qqline(residuals(log_model), col = "red")

# 4.2 Homoscedasticity for log model
cat("\n2. HOMOSCEDASTICITY (Log-Linear Model):\n")
cat("----------------------------------------\n")
bp_log <- bptest(log_model)
cat(sprintf("   Breusch-Pagan test: BP = %.4f, p-value = %.4f\n", 
            bp_log$statistic, bp_log$p.value))

if(bp_log$p.value > 0.05) {
  cat("   ✓ Constant variance (homoscedasticity) assumption satisfied\n")
} else {
  cat("   ✗ Non-constant variance (heteroscedasticity) detected\n")
}

# Residuals vs fitted for log model
plot(fitted(log_model), residuals(log_model),
     xlab = "Fitted Values (Log Scale)", ylab = "Residuals",
     main = "Residuals vs Fitted: Log-Linear Model")
abline(h = 0, col = "red")

# 4.3 Independence of residuals for log model
cat("\n3. INDEPENDENCE OF RESIDUALS (Log-Linear Model):\n")
cat("-----------------------------------------------\n")
dw_log <- dwtest(log_model)
cat(sprintf("   Durbin-Watson test: DW = %.4f, p-value = %.4f\n", 
            dw_log$statistic, dw_log$p.value))

if(dw_log$p.value > 0.05) {
  cat("   ✓ Residuals are independent (no autocorrelation)\n")
} else {
  cat("   ✗ Residuals show autocorrelation\n")
}

# 4.4 Multicollinearity check for log model
cat("\n4. MULTICOLLINEARITY CHECK (Log-Linear Model):\n")
cat("----------------------------------------------\n")
vif_log <- vif(log_model)
print(vif_log)

if(all(vif_log < 5)) {
  cat("   ✓ No concerning multicollinearity (all VIF < 5)\n")
} else if(all(vif_log < 10)) {
  cat("   ! Moderate multicollinearity (some VIF between 5-10)\n")
} else {
  cat("   ✗ High multicollinearity detected (some VIF > 10)\n")
}

# 4.5 Linearity check for log model
cat("\n5. LINEARITY CHECK (Log-Linear Model):\n")
cat("--------------------------------------\n")
crPlots(log_model, 
        main = "Component+Residual Plots: Log-Linear Model",
        ylab = "Comp+Resid",
        cex.lab = 1.2)  # Reduce label size to 80%

# =============================================================================
# 5. COMPARISON OF ORIGINAL VS LOG-LINEAR MODEL ASSUMPTIONS
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("COMPARISON: ORIGINAL VS LOG-LINEAR MODEL ASSUMPTIONS\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

comparison_df <- data.frame(
  Assumption = c("Normality", "Homoscedasticity", "Independence", "Multicollinearity"),
  Original_Model = c(
    ifelse(shapiro_test$p.value > 0.05, "✓ Satisfied", "✗ Violated"),
    ifelse(bp_test$p.value > 0.05, "✓ Satisfied", "✗ Violated"),
    ifelse(dw_test$p.value > 0.05, "✓ Satisfied", "✗ Violated"),
    ifelse(all(vif_values < 5), "✓ No issue", ifelse(all(vif_values < 10), "! Moderate", "✗ High"))
  ),
  Log_Linear_Model = c(
    ifelse(shapiro_log$p.value > 0.05, "✓ Satisfied", "✗ Violated"),
    ifelse(bp_log$p.value > 0.05, "✓ Satisfied", "✗ Violated"),
    ifelse(dw_log$p.value > 0.05, "✓ Satisfied", "✗ Violated"),
    ifelse(all(vif_log < 5), "✓ No issue", ifelse(all(vif_log < 10), "! Moderate", "✗ High"))
  ),
  Improvement = c(
    ifelse(shapiro_log$p.value > shapiro_test$p.value, "✓ Improved", "No change"),
    ifelse(bp_log$p.value > bp_test$p.value, "✓ Improved", "No change"),
    ifelse(dw_log$p.value > dw_test$p.value, "✓ Improved", "No change"),
    "Same structure"
  )
)

print(comparison_df)

# =============================================================================
# 6. FINAL RECOMMENDATIONS
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("FINAL RECOMMENDATIONS\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# Calculate improvement in p-values
normality_improvement <- shapiro_log$p.value - shapiro_test$p.value
homoscedasticity_improvement <- bp_log$p.value - bp_test$p.value

cat("ASSESSMENT OF LOG TRANSFORMATION:\n")
cat("---------------------------------\n")

if(normality_improvement > 0 | homoscedasticity_improvement > 0) {
  cat("✓ Log transformation provided improvement in:\n")
  if(normality_improvement > 0) {
    cat(sprintf("  - Normality: p-value increased by %.4f\n", normality_improvement))
  }
  if(homoscedasticity_improvement > 0) {
    cat(sprintf("  - Homoscedasticity: p-value increased by %.4f\n", homoscedasticity_improvement))
  }
  
  cat("\nRECOMMENDATION: Use the log-linear model for your final analysis.\n")
  cat("Interpret coefficients as percentage effects on revenue.\n")
} else {
  cat("✗ Log transformation did not significantly improve assumptions.\n")
  cat("Consider alternative approaches:\n")
  cat("  - Robust regression\n")
  cat("  - Generalized Linear Models (Gamma family)\n")
  cat("  - Non-parametric methods\n")
}

# Show how to interpret key coefficients
cat("\nKEY INTERPRETATIONS FOR LOG-LINEAR MODEL:\n")
cat("-----------------------------------------\n")
cat("For West Africa (reference region):\n")
west_growth_pct <- (exp(log_coef["T"]) - 1) * 100
cat(sprintf("  Annual growth rate: %.1f%% per year\n", west_growth_pct))

if("D1T" %in% names(log_coef)) {
  east_growth_diff_pct <- (exp(log_coef["D1T"]) - 1) * 100
  cat(sprintf("East Africa vs West Africa growth difference: %.1f%% per year\n", east_growth_diff_pct))
}

if("D2T" %in% names(log_coef)) {
  europe_growth_diff_pct <- (exp(log_coef["D2T"]) - 1) * 100
  cat(sprintf("Europe vs West Africa growth difference: %.1f%% per year\n", europe_growth_diff_pct))
}

cat("\nNote: For statistical inference, consider using robust standard errors\n")
cat("to account for any remaining heteroscedasticity.\n")

















# 9. PREDICTION AND VISUALIZATION
# Create prediction data
pred_data <- expand.grid(
  Year = 2013:2024,
  Region = c("West_Africa", "East_Africa", "Europe")
) %>%
  mutate(
    T = Year - 2012,
    D1 = as.numeric(Region == "East_Africa"),
    D2 = as.numeric(Region == "Europe"),
    D1T = D1 * T,
    D2T = D2 * T
  )

# Add predictions
pred_data$Predicted_Revenue <- predict(model, newdata = pred_data)

# Plot actual vs predicted
ggplot() +
  geom_line(data = pred_data, aes(x = Year, y = Predicted_Revenue, color = Region), 
            linewidth = 1.2, linetype = "dashed") +
  geom_point(data = analysis_data, aes(x = Year, y = Revenue, color = Region), size = 2) +
  geom_line(data = analysis_data, aes(x = Year, y = Revenue, color = Region), linewidth = 0.7) +
  labs(title = "GTBank Revenue: Actual vs Predicted Trends",
       subtitle = "Points = Actual, Dashed lines = Predicted",
       y = "Revenue", x = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("West_Africa" = "blue", 
                                "East_Africa" = "red", 
                                "Europe" = "green"))

cat("\n--- ANALYSIS COMPLETE ---\n")












# =============================================================================
# FINAL ANALYSIS: TESTING RESEARCH OBJECTIVES WITH LOG-LINEAR MODEL
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("FINAL ANALYSIS: TESTING RESEARCH OBJECTIVES WITH LOG-LINEAR MODEL\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# =============================================================================
# OBJECTIVE 1: EFFECT OF REGIONS ON REVENUE - WITH ANOVA TABLE
# =============================================================================

cat("OBJECTIVE 1: TO FIND THE EFFECT OF REGIONS ON THE REVENUE OF GTBANK\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n\n")

# Method 1: Complete ANOVA Table for the log-linear model
cat("METHOD 1: COMPLETE ANOVA TABLE FOR LOG-LINEAR MODEL\n")
cat("---------------------------------------------------\n")
anova_log <- anova(log_model)
print(anova_log)

# Extract key information from ANOVA
total_ss <- sum(anova_log$`Sum Sq`)
regression_ss <- sum(anova_log$`Sum Sq`[1:5])
residual_ss <- anova_log$`Sum Sq`[6]
r_squared <- regression_ss / total_ss

cat(sprintf("\nANOVA SUMMARY:\n"))
cat(sprintf("Total Sum of Squares: %.4f\n", total_ss))
cat(sprintf("Regression Sum of Squares: %.4f\n", regression_ss))
cat(sprintf("Residual Sum of Squares: %.4f\n", residual_ss))
cat(sprintf("R-squared: %.4f (%.1f%% of variance explained)\n", r_squared, r_squared * 100))

# Method 2: Overall F-test for the model
cat("\nMETHOD 2: OVERALL MODEL SIGNIFICANCE (F-TEST)\n")
cat("---------------------------------------------\n")
f_statistic <- summary(log_model)$fstatistic[1]
f_pvalue <- pf(summary(log_model)$fstatistic[1], 
               summary(log_model)$fstatistic[2], 
               summary(log_model)$fstatistic[3], 
               lower.tail = FALSE)

cat(sprintf("F-statistic: %.4f, p-value: %.6f\n", f_statistic, f_pvalue))
if(f_pvalue < 0.05) {
  cat("✓ REGIONS AND TIME TOGETHER HAVE A STATISTICALLY SIGNIFICANT EFFECT ON REVENUE\n")
} else {
  cat("✗ NO SIGNIFICANT EFFECT OF REGIONS AND TIME ON REVENUE DETECTED\n")
}

# Method 3: Test region terms collectively (D1, D2, D1T, D2T) using ANOVA comparison
cat("\nMETHOD 3: SPECIFIC TEST FOR REGIONAL EFFECTS (ANOVA MODEL COMPARISON)\n")
cat("---------------------------------------------------------------------\n")
reduced_model <- lm(Log_Revenue ~ T, data = analysis_data)  # Model without region terms
full_model <- log_model  # Model with region terms

region_comparison <- anova(reduced_model, full_model)
print(region_comparison)

if(region_comparison$`Pr(>F)`[2] < 0.05) {
  cat(sprintf("\n✓ REGIONS SIGNIFICANTLY AFFECT REVENUE BEYOND TIME EFFECTS\n"))
  cat(sprintf("  F = %.2f, p = %.6f\n", region_comparison$F[2], region_comparison$`Pr(>F)`[2]))
  cat(sprintf("  Additional variance explained by regions: %.1f%%\n", 
              (1 - (deviance(full_model)/deviance(reduced_model))) * 100))
} else {
  cat("\n✗ NO SIGNIFICANT ADDITIONAL REGIONAL EFFECT BEYOND TIME TRENDS\n")
}

# Method 4: Individual region coefficient significance from ANOVA
cat("\nMETHOD 4: INDIVIDUAL VARIABLE CONTRIBUTION FROM ANOVA\n")
cat("-----------------------------------------------------\n")

# Create a detailed breakdown of each variable's contribution
variable_contributions <- data.frame(
  Variable = rownames(anova_log)[1:5],
  Sum_Squares = anova_log$`Sum Sq`[1:5],
  Mean_Square = anova_log$`Mean Sq`[1:5],
  F_Value = anova_log$`F value`[1:5],
  P_Value = anova_log$`Pr(>F)`[1:5],
  Percentage_Contribution = (anova_log$`Sum Sq`[1:5] / total_ss) * 100
)

print(variable_contributions)

# Identify which region-related terms are significant
region_terms <- variable_contributions[grepl("D1|D2", variable_contributions$Variable), ]
significant_region_terms <- sum(region_terms$P_Value < 0.05, na.rm = TRUE)

cat(sprintf("\nSignificant region-related terms: %d out of %d\n",
            significant_region_terms, nrow(region_terms)))

if(significant_region_terms > 0) {
  cat("Significant region terms:\n")
  for(i in 1:nrow(region_terms)) {
    if(region_terms$P_Value[i] < 0.05) {
      cat(sprintf("  - %s (p = %.4f, contributes %.1f%% to variance)\n",
                  region_terms$Variable[i], region_terms$P_Value[i],
                  region_terms$Percentage_Contribution[i]))
    }
  }
}

# Final conclusion for Objective 1
cat("\nCONCLUSION FOR OBJECTIVE 1:\n")
cat("---------------------------\n")
if(f_pvalue < 0.05 && region_comparison$`Pr(>F)`[2] < 0.05) {
  cat("✓ STRONG EVIDENCE: Regions have a statistically significant effect on GTBank revenue\n")
  cat("  Both the overall model and regional terms specifically are significant\n")
} else if(f_pvalue < 0.05 && region_comparison$`Pr(>F)`[2] >= 0.05) {
  cat("○ MIXED EVIDENCE: Time effects are significant, but regional effects are not\n")
  cat("  Revenue patterns are driven more by time than by regional differences\n")
} else {
  cat("✗ NO EVIDENCE: No significant effect of regions or time on revenue detected\n")
}


# =============================================================================
# OBJECTIVE 2: IDENTIFY REGION WITH HIGHEST REVENUE GROWTH
# =============================================================================
log_summary <- summary(log_model)

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("OBJECTIVE 2: IDENTIFY WHICH REGION CONTRIBUTES MOST TO REVENUE GROWTH\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n\n")

# Calculate annual growth rates for each region (in percentage terms)
cat("CALCULATING ANNUAL GROWTH RATES BY REGION:\n")
cat("------------------------------------------\n")

# Extract coefficients as numeric values
T_coef <- as.numeric(coef(log_model)["T"])
D1T_coef <- as.numeric(coef(log_model)["D1T"])
D2T_coef <- as.numeric(coef(log_model)["D2T"])

# Calculate growth rates in percentage terms
growth_rates_pct <- c(
  West_Africa = (exp(T_coef) - 1) * 100,
  East_Africa = (exp(T_coef + D1T_coef) - 1) * 100,
  Europe = (exp(T_coef + D2T_coef) - 1) * 100
)

cat("ANNUAL REVENUE GROWTH RATES (%):\n")
for(i in 1:length(growth_rates_pct)) {
  cat(sprintf("  %-12s: %7.2f%% per year\n", 
              names(growth_rates_pct)[i], growth_rates_pct[i]))
}

# Identify the region with maximum growth
max_growth_region <- names(which.max(growth_rates_pct))
max_growth_rate <- max(growth_rates_pct)

cat(sprintf("\nHIGHEST GROWTH REGION: %s (%.2f%% per year)\n", 
            max_growth_region, max_growth_rate))

# Statistical significance of growth differences
cat("\nSTATISTICAL SIGNIFICANCE OF GROWTH DIFFERENCES:\n")
cat("-----------------------------------------------\n")
d1t_pvalue <- log_summary$coefficients["D1T", 4]
d2t_pvalue <- log_summary$coefficients["D2T", 4]

cat(sprintf("East Africa vs West Africa growth difference: p-value = %.6f\n", d1t_pvalue))
cat(sprintf("Europe vs West Africa growth difference: p-value = %.6f\n", d2t_pvalue))

if(d1t_pvalue < 0.05 | d2t_pvalue < 0.05) {
  cat("✓ GROWTH RATES ARE STATISTICALLY SIGNIFICANTLY DIFFERENT ACROSS REGIONS\n")
} else {
  cat("✗ NO STATISTICALLY SIGNIFICANT DIFFERENCES IN GROWTH RATES\n")
}

# Growth ranking
cat("\nGROWTH RATE RANKING:\n")
cat("--------------------\n")
growth_ranking <- sort(growth_rates_pct, decreasing = TRUE)
for(i in 1:length(growth_ranking)) {
  cat(sprintf("%d. %-12s: %6.2f%% per year\n", 
              i, names(growth_ranking)[i], growth_ranking[i]))
}

  
# Calculate relative performance
cat("\nRELATIVE PERFORMANCE ANALYSIS:\n")
cat("------------------------------\n")
west_growth <- growth_rates_pct["West_Africa"]
east_growth <- growth_rates_pct["East_Africa"]
europe_growth <- growth_rates_pct["Europe"]

cat(sprintf("East Africa growth relative to West Africa: %.1f%%\n", (east_growth/west_growth)*100))
cat(sprintf("Europe growth relative to West Africa: %.1f%%\n", (europe_growth/west_growth)*100))

# Strategic implications
cat("\nSTRATEGIC IMPLICATIONS:\n")
cat("-----------------------\n")
if(max_growth_rate > 10) {
  cat("✓ Very strong growth environment - consider expansion\n")
} else if(max_growth_rate > 5) {
  cat("○ Moderate growth - focus on efficiency and market share\n")
} else {
  cat("⚠ Low growth environment -可能需要 strategic review\n")
}

if(abs(growth_rates_pct["East_Africa"] - growth_rates_pct["Europe"]) < 2) {
  cat("○ East Africa and Europe have similar growth patterns\n")
} else {
  growth_diff <- growth_rates_pct["East_Africa"] - growth_rates_pct["Europe"]
  if(growth_diff > 0) {
    cat(sprintf("✓ East Africa outperforming Europe by %.1f percentage points\n", growth_diff))
  } else {
    cat(sprintf("✓ Europe outperforming East Africa by %.1f percentage points\n", abs(growth_diff)))
  }
}


  # =============================================================================
  # OBJECTIVE 3: EFFECT OF TIME ON REVENUE
  # =============================================================================
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n")
  cat("OBJECTIVE 3: EXAMINE THE EFFECT OF TIME ON GTBANK'S REVENUE\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n\n")
  
  # Test the time coefficient
  time_coef <- log_summary$coefficients["T", ]
  time_estimate <- time_coef[1]
  time_std_error <- time_coef[2]
  time_t_stat <- time_coef[3]
  time_pvalue <- time_coef[4]
  
  cat("TIME COEFFICIENT ANALYSIS:\n")
  cat("--------------------------\n")
  cat(sprintf("Coefficient estimate: %.6f\n", time_estimate))
  cat(sprintf("Standard error: %.6f\n", time_std_error))
  cat(sprintf("t-statistic: %.4f\n", time_t_stat))
  cat(sprintf("p-value: %.6f\n", time_pvalue))
  
  # Convert to percentage interpretation
  time_effect_pct <- (exp(time_estimate) - 1) * 100
  
  cat(sprintf("\nINTERPRETATION: Each year, revenue changes by approximately %.2f%%\n", 
              time_effect_pct))
  
  if(time_pvalue < 0.05) {
    cat("✓ TIME HAS A STATISTICALLY SIGNIFICANT EFFECT ON REVENUE\n")
    if(time_effect_pct > 0) {
      cat("  This represents SIGNIFICANT POSITIVE GROWTH over time\n")
    } else {
      cat("  This represents SIGNIFICANT NEGATIVE TREND over time\n")
    }
  } else {
    cat("✗ NO STATISTICALLY SIGNIFICANT TIME EFFECT DETECTED\n")
  }
  
  # =============================================================================
  # OBJECTIVE 4: DOES REGIONAL INFLUENCE DEPEND ON TIME?
  # =============================================================================
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n")
  cat("OBJECTIVE 4: DOES INFLUENCE OF REGION ON REVENUE DEPEND ON LEVEL OF TIME?\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n\n")
  
  # Test interaction terms (D1T and D2T)
  cat("TESTING INTERACTION EFFECTS (Region × Time):\n")
  cat("--------------------------------------------\n")
  
  interaction_terms <- log_summary$coefficients[grepl("T$", rownames(log_summary$coefficients)) & 
                                                  grepl("D", rownames(log_summary$coefficients)), ]
  
  print(interaction_terms)
  
  significant_interactions <- sum(interaction_terms[,4] < 0.05)
  total_interactions <- nrow(interaction_terms)
  
  cat(sprintf("\nSignificant interactions: %d out of %d\n", 
              significant_interactions, total_interactions))
  
  if(any(interaction_terms[,4] < 0.05)) {
    cat("✓ REGIONAL INFLUENCE ON REVENUE DEPENDS ON TIME (Significant interaction)\n")
    cat("  This means different regions have different growth patterns over time\n\n")
    
    # Interpret significant interactions
    if("D1T" %in% rownames(interaction_terms) && interaction_terms["D1T", 4] < 0.05) {
      d1t_effect_pct <- (exp(interaction_terms["D1T", 1]) - 1) * 100
      cat(sprintf("  - East Africa growth pattern is significantly different from West Africa\n"))
      cat(sprintf("    The growth rate difference is approximately %.2f%% per year\n", d1t_effect_pct))
    }
    
    if("D2T" %in% rownames(interaction_terms) && interaction_terms["D2T", 4] < 0.05) {
      d2t_effect_pct <- (exp(interaction_terms["D2T", 1]) - 1) * 100
      cat(sprintf("  - Europe growth pattern is significantly different from West Africa\n"))
      cat(sprintf("    The growth rate difference is approximately %.2f%% per year\n", d2t_effect_pct))
    }
  } else {
    cat("✗ REGIONAL INFLUENCE DOES NOT DEPEND ON TIME (No significant interaction)\n")
    cat("  This means all regions have parallel growth trends over time\n")
  }
  
  # =============================================================================
  # COMPREHENSIVE SUMMARY OF FINDINGS
  # =============================================================================
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n")
  cat("COMPREHENSIVE SUMMARY OF RESEARCH FINDINGS\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n\n")
  
  # Create summary table
  findings_summary <- data.frame(
    Objective = c(
      "1. Effect of regions on revenue",
      "2. Highest growth region", 
      "3. Effect of time on revenue",
      "4. Region-time interaction"
    ),
    Statistical_Test = c(
      "Overall F-test & Region terms test",
      "Growth rate comparison & Interaction tests",
      "Time coefficient t-test",
      "Interaction terms t-tests"
    ),
    Result = c(
      ifelse(f_pvalue < 0.05, "Significant regional effect", "No significant effect"),
      max_growth_region,
      ifelse(time_pvalue < 0.05, "Significant time effect", "No time effect"),
      ifelse(any(interaction_terms[,4] < 0.05), "Significant interaction", "No interaction")
    ),
    Key_Statistic = c(
      sprintf("F = %.2f, p = %.4f", f_statistic, f_pvalue),
      sprintf("%.2f%% growth", max_growth_rate),
      sprintf("t = %.3f, p = %.4f", time_t_stat, time_pvalue),
      ifelse(any(interaction_terms[,4] < 0.05), 
             sprintf("%d sig. interactions", significant_interactions),
             "No sig. interactions")
    ),
    Business_Implication = c(
      ifelse(f_pvalue < 0.05, "Regional strategy matters", "Regional focus may not be needed"),
      "Focus resources on highest growth region",
      ifelse(time_pvalue < 0.05, "Market is growing/declining", "Stable market conditions"),
      ifelse(any(interaction_terms[,4] < 0.05), "Customize regional strategies over time", "Uniform strategy across regions")
    )
  )
  
  print(findings_summary)
  
  # =============================================================================
  # FINAL INTERPRETATION AND RECOMMENDATIONS
  # =============================================================================
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n")
  cat("FINAL INTERPRETATION AND STRATEGIC RECOMMENDATIONS\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n\n")
  
  cat("BASED ON THE LOG-LINEAR MODEL ANALYSIS:\n")
  cat("--------------------------------------\n\n")
  
  # Objective 1 interpretation
  cat("1. REGIONAL EFFECTS:\n")
  if(f_pvalue < 0.05) {
    cat("   ✓ GTBank's revenue is significantly influenced by geographical regions.\n")
    cat("   ✓ Regional strategies should be customized rather than uniform.\n")
  } else {
    cat("   ✗ No significant regional differences in revenue patterns detected.\n")
    cat("   ✓ Consider standardized banking products across regions.\n")
  }
  
  # Objective 2 interpretation
  cat("\n2. GROWTH LEADERSHIP:\n")
  cat(sprintf("   ✓ %s is the primary growth driver with %.2f%% annual growth.\n", 
              max_growth_region, max_growth_rate))
  cat("   ✓ Allocate more resources and investment to this region.\n")
  
  # Growth comparison
  growth_spread <- max(growth_rates_pct) - min(growth_rates_pct)
  if(growth_spread > 10) {
    cat(sprintf("   ! Large growth disparity (%.1f%%) suggests regional rebalancing needed.\n", growth_spread))
  }
  
  # Objective 3 interpretation
  cat("\n3. TIME TREND:\n")
  if(time_pvalue < 0.05) {
    if(time_effect_pct > 0) {
      cat(sprintf("   ✓ Strong positive growth trend (%.2f%% annually) across the bank.\n", time_effect_pct))
      cat("   ✓ Favorable market conditions for expansion.\n")
    } else {
      cat(sprintf("   ✗ Concerning negative trend (%.2f%% annually) requires intervention.\n", time_effect_pct))
    }
  } else {
    cat("   ✓ Stable revenue pattern over time - predictable business environment.\n")
  }
  
  # Objective 4 interpretation
  cat("\n4. STRATEGIC FLEXIBILITY:\n")
  if(any(interaction_terms[,4] < 0.05)) {
    cat("   ✓ Regional strategies must evolve over time (significant interactions).\n")
    cat("   ✓ Regular review and adjustment of regional approaches needed.\n")
  } else {
    cat("   ✓ Consistent regional relationships over time allow stable strategies.\n")
  }
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n")
  cat("ANALYSIS COMPLETE - USE THESE INSIGHTS FOR STRATEGIC DECISION MAKING\n")
  cat(paste(rep("=", 70), collapse = ""))
  cat("\n")
  
  
  
  
  
  
  # =============================================================================
  # ORIGINAL MODEL ESTIMATES (BEFORE LOG TRANSFORMATION)
  # =============================================================================
  
  cat("ORIGINAL MODEL PARAMETER ESTIMATES\n")
  cat("===================================\n\n")
  
  # Display the original model summary
  original_summary <- summary(model)
  print(original_summary)
  
  # Extract and display just the coefficients table
  cat("\nCOEFFICIENTS TABLE - ORIGINAL MODEL:\n")
  cat("------------------------------------\n")
  coefficients_table <- coef(original_summary)
  print(coefficients_table)
  
  # Simple interpretation of key parameters
  cat("\nKEY PARAMETER INTERPRETATIONS:\n")
  cat("------------------------------\n")
  
  coef_values <- coef(model)
  cat(sprintf("Intercept (West_Africa baseline): %.2f\n", coef_values["(Intercept)"]))
  cat(sprintf("Time trend (Annual growth): %.2f units per year\n", coef_values["T"]))
  cat(sprintf("East Africa vs West Africa initial difference: %.2f\n", coef_values["D1"]))
  cat(sprintf("Europe vs West Africa initial difference: %.2f\n", coef_values["D2"]))
  cat(sprintf("East Africa growth difference: %.2f per year\n", coef_values["D1T"]))
  cat(sprintf("Europe growth difference: %.2f per year\n", coef_values["D2T"]))
  
  # Model fit statistics
  cat(sprintf("\nMODEL FIT STATISTICS:\n"))
  cat(sprintf("R-squared: %.4f\n", original_summary$r.squared))
  cat(sprintf("Adjusted R-squared: %.4f\n", original_summary$adj.r.squared))
  cat(sprintf("F-statistic: %.2f (p-value: %.6f)\n", 
              original_summary$fstatistic[1], 
              pf(original_summary$fstatistic[1], 
                 original_summary$fstatistic[2], 
                 original_summary$fstatistic[3], 
                 lower.tail = FALSE)))
  
  
  
  # =============================================================================
  # pnorm AND qnorm PLOTS FOR LOG-TRANSFORMED MODEL
  # =============================================================================
  
  cat("NORMALITY CHECK: pnorm AND qnorm PLOTS FOR LOG-TRANSFORMED MODEL\n")
  cat("===============================================================\n\n")
  
  # Set up plotting area
  par(mfrow = c(3, 1))
  
  # Get residuals from log-transformed model
  log_residuals <- residuals(log_model)
  
    # Plot 2: pnorm - Cumulative Distribution Function
  x <- seq(-3, 3, length.out = 1000)
  y_cdf <- pnorm(x)
  
  plot(x, y_cdf, type = "l", lwd = 2, col = "blue",
       main = "Pnorm Normal CDF Plot",
       xlab = "Theoretical Quantiles", ylab = "Cumulative Probability")
  grid()
  
  # Add points showing where our residuals fall
  std_residuals <- scale(log_residuals)
  points(sort(std_residuals), pnorm(sort(std_residuals)), 
         col = "red", pch = 19, cex = 0.6)
  
  # Plot 3: qnorm - Quantile Function
  p <- seq(0.01, 0.99, length.out = 1000)
  q_values <- qnorm(p)
  
  plot(p, q_values, type = "l", lwd = 2, col = "red",
       main = "Qnorm Normal Quantiles Plot",
       xlab = "Probability", ylab = "Quantile")
  grid()
  
  # Plot 4: Histogram with normal curve overlay
  hist(log_residuals, freq = FALSE, breaks = 20, 
       main = "Residual Distribution vs Normal",
       xlab = "Log Model Residuals", col = "lightblue")
  curve(dnorm(x, mean = mean(log_residuals), sd = sd(log_residuals)), 
        add = TRUE, col = "red", lwd = 2)
  
  # Reset plotting
  par(mfrow = c(1, 1))
  
  # Shapiro-Wilk test for log model residuals
  shapiro_log <- shapiro.test(log_residuals)
  cat(sprintf("Shapiro-Wilk test for log model residuals: W = %.4f, p-value = %.4f\n", 
              shapiro_log$statistic, shapiro_log$p.value))
  
  if(shapiro_log$p.value > 0.05) {
    cat("✓ Log model residuals are normally distributed\n")
  } else {
    cat("✗ Log model residuals are not normally distributed\n")
  }
  