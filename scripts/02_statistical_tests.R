# =============================================================================
# Wine Quality Data - Statistical Hypothesis Testing
# Master's Level Statistical Analysis
# =============================================================================

# Load required packages
packages <- c("tidyverse", "nortest", "car", "rstatix", "ggpubr")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
sapply(packages, install_if_missing)

# Load data
wine <- read.csv("data/wine_quality.csv", sep = ";")

# -----------------------------------------------------------------------------
# 1. NORMALITY TESTING
# -----------------------------------------------------------------------------

cat("=============================================================\n")
cat("           NORMALITY TESTS FOR ALL VARIABLES                 \n")
cat("=============================================================\n\n")

# Function to perform multiple normality tests
normality_tests <- function(x, var_name) {
  # Shapiro-Wilk (for n < 5000)
  if (length(x) <= 5000) {
    sw_test <- shapiro.test(sample(x, min(5000, length(x))))
  } else {
    sw_test <- list(statistic = NA, p.value = NA)
  }

  # Anderson-Darling test
  ad_test <- ad.test(x)

  # Kolmogorov-Smirnov test
  ks_test <- ks.test(x, "pnorm", mean(x), sd(x))

  data.frame(
    Variable = var_name,
    SW_statistic = round(sw_test$statistic, 4),
    SW_pvalue = round(sw_test$p.value, 6),
    AD_statistic = round(ad_test$statistic, 4),
    AD_pvalue = round(ad_test$p.value, 6),
    KS_statistic = round(ks_test$statistic, 4),
    KS_pvalue = round(ks_test$p.value, 6)
  )
}

# Apply to all numeric variables
normality_results <- do.call(rbind, lapply(names(wine), function(v) {
  normality_tests(wine[[v]], v)
}))

cat("Normality Test Results (H0: Data is normally distributed)\n")
cat("p-value < 0.05 suggests non-normality\n\n")
print(normality_results)

# Q-Q plots for visual normality assessment
png("output/qq_plots.png", width = 1200, height = 900)
par(mfrow = c(3, 4))
for (var in names(wine)) {
  qqnorm(wine[[var]], main = paste("Q-Q Plot:", var), col = "steelblue")
  qqline(wine[[var]], col = "red", lwd = 2)
}
dev.off()

# -----------------------------------------------------------------------------
# 2. HOMOGENEITY OF VARIANCE TESTS
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           HOMOGENEITY OF VARIANCE TESTS                     \n")
cat("=============================================================\n\n")

# Levene's test for each variable across quality groups
cat("Levene's Test for Homogeneity of Variance (by Quality Group)\n")
cat("H0: Variances are equal across groups\n\n")

levene_results <- data.frame(
  Variable = character(),
  F_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(wine)[names(wine) != "quality"]) {
  test_result <- leveneTest(wine[[var]] ~ factor(wine$quality))
  levene_results <- rbind(levene_results, data.frame(
    Variable = var,
    F_statistic = round(test_result$`F value`[1], 4),
    p_value = round(test_result$`Pr(>F)`[1], 6)
  ))
}

print(levene_results)

# -----------------------------------------------------------------------------
# 3. KRUSKAL-WALLIS TEST (Non-parametric ANOVA)
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           KRUSKAL-WALLIS TEST                               \n")
cat("=============================================================\n\n")

cat("Testing if chemical properties differ significantly across quality levels\n")
cat("H0: No difference in medians across quality groups\n\n")

kw_results <- data.frame(
  Variable = character(),
  H_statistic = numeric(),
  df = numeric(),
  p_value = numeric(),
  Effect_size = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(wine)[names(wine) != "quality"]) {
  kw_test <- kruskal.test(wine[[var]] ~ factor(wine$quality))

  # Calculate effect size (epsilon squared)
  n <- nrow(wine)
  k <- length(unique(wine$quality))
  epsilon_sq <- (kw_test$statistic - k + 1) / (n - k)

  kw_results <- rbind(kw_results, data.frame(
    Variable = var,
    H_statistic = round(kw_test$statistic, 4),
    df = kw_test$parameter,
    p_value = format(kw_test$p.value, scientific = TRUE, digits = 4),
    Effect_size = round(epsilon_sq, 4)
  ))
}

print(kw_results)

# -----------------------------------------------------------------------------
# 4. POST-HOC ANALYSIS: DUNN'S TEST
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           POST-HOC ANALYSIS (DUNN'S TEST)                   \n")
cat("=============================================================\n\n")

# Perform Dunn's test for top 3 significant variables
top_vars <- c("alcohol", "volatile.acidity", "sulphates")

for (var in top_vars) {
  cat(paste("\n--- Dunn's Test for", var, "---\n"))
  dunn_result <- dunn_test(
    data = wine,
    formula = as.formula(paste(var, "~ factor(quality)")),
    p.adjust.method = "bonferroni"
  )
  print(dunn_result)
}

# -----------------------------------------------------------------------------
# 5. CORRELATION SIGNIFICANCE TESTS
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           CORRELATION SIGNIFICANCE TESTS                    \n")
cat("=============================================================\n\n")

# Pearson correlation with quality
cat("Correlation with Quality (Pearson's r)\n\n")

cor_results <- data.frame(
  Variable = character(),
  Correlation = numeric(),
  t_statistic = numeric(),
  p_value = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(wine)[names(wine) != "quality"]) {
  cor_test <- cor.test(wine[[var]], wine$quality, method = "pearson")
  cor_results <- rbind(cor_results, data.frame(
    Variable = var,
    Correlation = round(cor_test$estimate, 4),
    t_statistic = round(cor_test$statistic, 4),
    p_value = format(cor_test$p.value, scientific = TRUE, digits = 4),
    CI_lower = round(cor_test$conf.int[1], 4),
    CI_upper = round(cor_test$conf.int[2], 4)
  ))
}

# Sort by absolute correlation
cor_results <- cor_results[order(abs(cor_results$Correlation), decreasing = TRUE), ]
print(cor_results)

# Visualize significant correlations
png("output/correlation_significance.png", width = 1000, height = 600)
cor_results %>%
  mutate(Significant = as.numeric(p_value) < 0.05) %>%
  ggplot(aes(x = reorder(Variable, Correlation), y = Correlation, fill = Significant)) +
  geom_col() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("grey60", "steelblue")) +
  theme_minimal() +
  labs(title = "Correlation with Wine Quality",
       subtitle = "With 95% Confidence Intervals",
       x = "Variable", y = "Pearson Correlation Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
dev.off()

# -----------------------------------------------------------------------------
# 6. CHI-SQUARE TEST FOR INDEPENDENCE
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           CHI-SQUARE TEST FOR INDEPENDENCE                  \n")
cat("=============================================================\n\n")

# Categorize alcohol into groups
wine$alcohol_category <- cut(wine$alcohol,
                              breaks = quantile(wine$alcohol, probs = c(0, 0.33, 0.66, 1)),
                              labels = c("Low", "Medium", "High"),
                              include.lowest = TRUE)

# Create contingency table
contingency_table <- table(wine$alcohol_category, wine$quality)
cat("Contingency Table: Alcohol Category vs Quality\n")
print(contingency_table)

# Chi-square test
chi_test <- chisq.test(contingency_table)
cat("\nChi-Square Test Results:\n")
print(chi_test)

# Effect size (Cramer's V)
cramers_v <- sqrt(chi_test$statistic / (sum(contingency_table) * (min(dim(contingency_table)) - 1)))
cat("\nCramer's V (Effect Size):", round(cramers_v, 4), "\n")

# Visualize
png("output/chi_square_mosaic.png", width = 800, height = 600)
mosaicplot(contingency_table,
           color = TRUE,
           main = "Mosaic Plot: Alcohol Category vs Wine Quality",
           xlab = "Alcohol Category",
           ylab = "Quality Score")
dev.off()

cat("\n=== Statistical Testing Complete! ===\n")

# Save results
write.csv(normality_results, "output/normality_tests.csv", row.names = FALSE)
write.csv(kw_results, "output/kruskal_wallis_tests.csv", row.names = FALSE)
write.csv(cor_results, "output/correlation_tests.csv", row.names = FALSE)
