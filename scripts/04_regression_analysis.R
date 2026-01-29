# =============================================================================
# Wine Quality Data - Regression Analysis
# Master's Level Predictive Modeling
# =============================================================================

# Load required packages
packages <- c("tidyverse", "car", "lmtest", "MASS", "caret", "broom", "performance")
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
# 1. DATA SPLITTING
# -----------------------------------------------------------------------------

cat("=============================================================\n")
cat("           MULTIPLE LINEAR REGRESSION ANALYSIS               \n")
cat("=============================================================\n\n")

set.seed(42)
train_index <- createDataPartition(wine$quality, p = 0.8, list = FALSE)
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# -----------------------------------------------------------------------------
# 2. FULL MODEL - ALL PREDICTORS
# -----------------------------------------------------------------------------

cat("\n=== FULL MODEL (All Predictors) ===\n\n")

full_model <- lm(quality ~ ., data = train_data)
print(summary(full_model))

# Model performance metrics
cat("\n=== Model Performance Metrics ===\n")
cat("R-squared:", round(summary(full_model)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(full_model)$adj.r.squared, 4), "\n")
cat("RSE:", round(summary(full_model)$sigma, 4), "\n")
cat("F-statistic:", round(summary(full_model)$fstatistic[1], 4), "\n")

# -----------------------------------------------------------------------------
# 3. MULTICOLLINEARITY ASSESSMENT
# -----------------------------------------------------------------------------

cat("\n=== Variance Inflation Factors (VIF) ===\n")
cat("VIF > 5 indicates problematic multicollinearity\n\n")

vif_values <- vif(full_model)
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = round(vif_values, 4)
)
vif_df <- vif_df[order(vif_df$VIF, decreasing = TRUE), ]
print(vif_df)

# Visualize VIF
png("output/vif_plot.png", width = 800, height = 500)
ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF, fill = VIF > 5)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Variance Inflation Factors",
       subtitle = "Red line indicates VIF = 5 threshold",
       x = "Variable", y = "VIF") +
  theme(legend.position = "none")
dev.off()

# -----------------------------------------------------------------------------
# 4. MODEL SELECTION - STEPWISE REGRESSION
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           STEPWISE MODEL SELECTION                          \n")
cat("=============================================================\n\n")

# Backward elimination using AIC
cat("=== Backward Elimination (AIC) ===\n")
backward_model <- step(full_model, direction = "backward", trace = 0)
cat("Selected variables:\n")
print(names(coef(backward_model))[-1])
cat("\nAIC:", round(AIC(backward_model), 2), "\n")

# Forward selection
null_model <- lm(quality ~ 1, data = train_data)
cat("\n=== Forward Selection (AIC) ===\n")
forward_model <- step(null_model,
                       scope = list(lower = null_model, upper = full_model),
                       direction = "forward", trace = 0)
cat("Selected variables:\n")
print(names(coef(forward_model))[-1])
cat("\nAIC:", round(AIC(forward_model), 2), "\n")

# Both directions
cat("\n=== Stepwise Both Directions (AIC) ===\n")
both_model <- step(full_model, direction = "both", trace = 0)
cat("Selected variables:\n")
print(names(coef(both_model))[-1])
cat("\nAIC:", round(AIC(both_model), 2), "\n")

# Compare models
cat("\n=== Model Comparison ===\n")
model_comparison <- data.frame(
  Model = c("Full", "Backward", "Forward", "Both"),
  Variables = c(length(coef(full_model)) - 1,
                length(coef(backward_model)) - 1,
                length(coef(forward_model)) - 1,
                length(coef(both_model)) - 1),
  AIC = c(AIC(full_model), AIC(backward_model),
          AIC(forward_model), AIC(both_model)),
  BIC = c(BIC(full_model), BIC(backward_model),
          BIC(forward_model), BIC(both_model)),
  Adj_R2 = c(summary(full_model)$adj.r.squared,
             summary(backward_model)$adj.r.squared,
             summary(forward_model)$adj.r.squared,
             summary(both_model)$adj.r.squared)
)
print(model_comparison)

# Use the backward model as final
final_model <- backward_model
cat("\n=== Final Model Summary ===\n")
print(summary(final_model))

# -----------------------------------------------------------------------------
# 5. REGRESSION DIAGNOSTICS
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           REGRESSION DIAGNOSTICS                            \n")
cat("=============================================================\n\n")

# Diagnostic plots
png("output/regression_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(final_model)
dev.off()

# Test for normality of residuals
cat("=== Shapiro-Wilk Test for Residual Normality ===\n")
residuals_sample <- sample(residuals(final_model), min(5000, length(residuals(final_model))))
shapiro_test <- shapiro.test(residuals_sample)
print(shapiro_test)

# Test for homoscedasticity (Breusch-Pagan test)
cat("\n=== Breusch-Pagan Test for Homoscedasticity ===\n")
bp_test <- bptest(final_model)
print(bp_test)
if (bp_test$p.value < 0.05) {
  cat("Result: Evidence of heteroscedasticity (consider robust SE or transformation)\n")
} else {
  cat("Result: No significant heteroscedasticity detected\n")
}

# Test for autocorrelation (Durbin-Watson test)
cat("\n=== Durbin-Watson Test for Autocorrelation ===\n")
dw_test <- dwtest(final_model)
print(dw_test)

# Influential observations
cat("\n=== Influential Observations (Cook's Distance) ===\n")
cooks_d <- cooks.distance(final_model)
influential <- which(cooks_d > 4 / nrow(train_data))
cat("Number of influential observations:", length(influential), "\n")
cat("Threshold used: 4/n =", round(4 / nrow(train_data), 5), "\n")

# Plot Cook's distance
png("output/cooks_distance.png", width = 900, height = 500)
plot(cooks_d, type = "h", main = "Cook's Distance",
     xlab = "Observation", ylab = "Cook's Distance", col = "steelblue")
abline(h = 4 / nrow(train_data), col = "red", lty = 2)
dev.off()

# -----------------------------------------------------------------------------
# 6. MODEL VALIDATION - PREDICTIONS
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           MODEL VALIDATION                                  \n")
cat("=============================================================\n\n")

# Predictions on test set
predictions <- predict(final_model, newdata = test_data)

# Calculate metrics
rmse <- sqrt(mean((test_data$quality - predictions)^2))
mae <- mean(abs(test_data$quality - predictions))
mape <- mean(abs((test_data$quality - predictions) / test_data$quality)) * 100
r2_test <- cor(test_data$quality, predictions)^2

cat("=== Test Set Performance ===\n")
cat("RMSE:", round(rmse, 4), "\n")
cat("MAE:", round(mae, 4), "\n")
cat("MAPE:", round(mape, 2), "%\n")
cat("R-squared (test):", round(r2_test, 4), "\n")

# Actual vs Predicted plot
png("output/actual_vs_predicted.png", width = 800, height = 600)
ggplot(data.frame(Actual = test_data$quality, Predicted = predictions),
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Wine Quality",
       subtitle = paste("Test Set R² =", round(r2_test, 3)),
       x = "Actual Quality", y = "Predicted Quality") +
  coord_fixed()
dev.off()

# Residual distribution
png("output/residual_distribution.png", width = 800, height = 500)
residuals_test <- test_data$quality - predictions
ggplot(data.frame(residuals = residuals_test), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Distribution of Prediction Residuals",
       x = "Residual (Actual - Predicted)", y = "Density")
dev.off()

# -----------------------------------------------------------------------------
# 7. COEFFICIENT INTERPRETATION
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           COEFFICIENT INTERPRETATION                        \n")
cat("=============================================================\n\n")

# Tidy coefficient summary
coef_summary <- tidy(final_model, conf.int = TRUE)
coef_summary <- coef_summary %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  )
print(coef_summary)

# Standardized coefficients (beta weights)
cat("\n=== Standardized Coefficients (Beta Weights) ===\n")
train_scaled <- as.data.frame(scale(train_data))
std_model <- lm(quality ~ ., data = train_scaled[, names(coef(final_model))[-1]])
std_coefs <- coef(std_model)[-1]
std_coefs_df <- data.frame(
  Variable = names(std_coefs),
  Beta = round(std_coefs, 4)
)
std_coefs_df <- std_coefs_df[order(abs(std_coefs_df$Beta), decreasing = TRUE), ]
print(std_coefs_df)

# Coefficient plot
png("output/coefficient_plot.png", width = 900, height = 600)
coef_summary %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Regression Coefficients with 95% CI",
       x = "Variable", y = "Coefficient Estimate")
dev.off()

# -----------------------------------------------------------------------------
# 8. CROSS-VALIDATION
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           K-FOLD CROSS-VALIDATION                           \n")
cat("=============================================================\n\n")

# 10-fold cross-validation
set.seed(42)
ctrl <- trainControl(method = "cv", number = 10)
cv_model <- train(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide +
                    total.sulfur.dioxide + pH + sulphates + alcohol,
                  data = wine,
                  method = "lm",
                  trControl = ctrl)

cat("10-Fold Cross-Validation Results:\n")
print(cv_model)
cat("\nCV RMSE:", round(cv_model$results$RMSE, 4), "\n")
cat("CV R-squared:", round(cv_model$results$Rsquared, 4), "\n")

# -----------------------------------------------------------------------------
# 9. SAVE RESULTS
# -----------------------------------------------------------------------------

# Export model summary
sink("output/regression_summary.txt")
cat("=============================================================\n")
cat("       FINAL REGRESSION MODEL SUMMARY                        \n")
cat("=============================================================\n\n")
print(summary(final_model))
cat("\n\n=== Cross-Validation Results ===\n")
print(cv_model)
sink()

write.csv(coef_summary, "output/regression_coefficients.csv", row.names = FALSE)
saveRDS(final_model, "output/final_model.rds")

cat("\n=== Regression Analysis Complete! ===\n")
