# =============================================================================
# Wine Quality Data - Machine Learning Classification
# Master's Level Predictive Modeling
# =============================================================================

# Load required packages
packages <- c("tidyverse", "caret", "randomForest", "e1071", "pROC",
              "ROCR", "class", "nnet", "gbm")
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
# 1. DATA PREPARATION
# -----------------------------------------------------------------------------

cat("=============================================================\n")
cat("           MACHINE LEARNING CLASSIFICATION                   \n")
cat("=============================================================\n\n")

# Create binary classification target (Good: quality >= 6)
wine$quality_class <- factor(ifelse(wine$quality >= 6, "Good", "Poor"))

cat("=== Class Distribution ===\n")
print(table(wine$quality_class))
print(prop.table(table(wine$quality_class)) * 100)

# Split data
set.seed(42)
train_index <- createDataPartition(wine$quality_class, p = 0.8, list = FALSE)
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]

# Prepare features (exclude original quality and new class from X)
X_train <- train_data %>% select(-quality, -quality_class)
y_train <- train_data$quality_class
X_test <- test_data %>% select(-quality, -quality_class)
y_test <- test_data$quality_class

cat("\nTraining set:", nrow(train_data), "samples\n")
cat("Test set:", nrow(test_data), "samples\n")

# Preprocessing: scale features
preproc <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preproc, X_train)
X_test_scaled <- predict(preproc, X_test)

# -----------------------------------------------------------------------------
# 2. MODEL TRAINING WITH CROSS-VALIDATION
# -----------------------------------------------------------------------------

# Cross-validation setup
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

# Store results
model_results <- data.frame()

# --- LOGISTIC REGRESSION ---
cat("\n=== Training Logistic Regression ===\n")
set.seed(42)
model_lr <- train(
  x = X_train_scaled,
  y = y_train,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)
cat("CV AUC:", round(max(model_lr$results$ROC), 4), "\n")

# --- RANDOM FOREST ---
cat("\n=== Training Random Forest ===\n")
set.seed(42)
model_rf <- train(
  x = X_train_scaled,
  y = y_train,
  method = "rf",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = expand.grid(mtry = c(2, 4, 6, 8))
)
cat("Best mtry:", model_rf$bestTune$mtry, "\n")
cat("CV AUC:", round(max(model_rf$results$ROC), 4), "\n")

# --- SUPPORT VECTOR MACHINE ---
cat("\n=== Training SVM (Radial Kernel) ===\n")
set.seed(42)
model_svm <- train(
  x = X_train_scaled,
  y = y_train,
  method = "svmRadial",
  trControl = ctrl,
  metric = "ROC",
  tuneLength = 5
)
cat("Best parameters: C =", model_svm$bestTune$C, ", sigma =",
    round(model_svm$bestTune$sigma, 4), "\n")
cat("CV AUC:", round(max(model_svm$results$ROC), 4), "\n")

# --- K-NEAREST NEIGHBORS ---
cat("\n=== Training K-Nearest Neighbors ===\n")
set.seed(42)
model_knn <- train(
  x = X_train_scaled,
  y = y_train,
  method = "knn",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = expand.grid(k = c(3, 5, 7, 9, 11, 15))
)
cat("Best k:", model_knn$bestTune$k, "\n")
cat("CV AUC:", round(max(model_knn$results$ROC), 4), "\n")

# --- GRADIENT BOOSTING ---
cat("\n=== Training Gradient Boosting ===\n")
set.seed(42)
model_gbm <- train(
  x = X_train_scaled,
  y = y_train,
  method = "gbm",
  trControl = ctrl,
  metric = "ROC",
  verbose = FALSE,
  tuneGrid = expand.grid(
    n.trees = c(100, 200),
    interaction.depth = c(3, 5),
    shrinkage = 0.1,
    n.minobsinnode = 10
  )
)
cat("Best parameters: trees =", model_gbm$bestTune$n.trees,
    ", depth =", model_gbm$bestTune$interaction.depth, "\n")
cat("CV AUC:", round(max(model_gbm$results$ROC), 4), "\n")

# -----------------------------------------------------------------------------
# 3. MODEL COMPARISON
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           MODEL COMPARISON                                  \n")
cat("=============================================================\n\n")

# Collect all models
models_list <- list(
  "Logistic Regression" = model_lr,
  "Random Forest" = model_rf,
  "SVM (Radial)" = model_svm,
  "KNN" = model_knn,
  "Gradient Boosting" = model_gbm
)

# Compare using resamples
resamps <- resamples(models_list)
cat("=== Cross-Validation Performance Summary ===\n")
print(summary(resamps))

# Visualization of model comparison
png("output/ml_model_comparison.png", width = 1000, height = 600)
bwplot(resamps, metric = "ROC",
       main = "Model Comparison - ROC AUC (10-Fold CV)")
dev.off()

# -----------------------------------------------------------------------------
# 4. TEST SET EVALUATION
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           TEST SET EVALUATION                               \n")
cat("=============================================================\n\n")

evaluate_model <- function(model, X_test, y_test, model_name) {
  # Predictions
  pred_class <- predict(model, X_test)
  pred_prob <- predict(model, X_test, type = "prob")[, "Good"]

  # Confusion matrix
  cm <- confusionMatrix(pred_class, y_test, positive = "Good")

  # ROC and AUC
  roc_obj <- roc(y_test, pred_prob, levels = c("Poor", "Good"))

  list(
    name = model_name,
    accuracy = cm$overall["Accuracy"],
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    precision = cm$byClass["Precision"],
    f1 = cm$byClass["F1"],
    auc = auc(roc_obj),
    confusion_matrix = cm$table,
    roc = roc_obj
  )
}

# Evaluate all models
results <- lapply(names(models_list), function(name) {
  evaluate_model(models_list[[name]], X_test_scaled, y_test, name)
})
names(results) <- names(models_list)

# Summary table
test_summary <- data.frame(
  Model = sapply(results, function(x) x$name),
  Accuracy = sapply(results, function(x) round(x$accuracy, 4)),
  Sensitivity = sapply(results, function(x) round(x$sensitivity, 4)),
  Specificity = sapply(results, function(x) round(x$specificity, 4)),
  Precision = sapply(results, function(x) round(x$precision, 4)),
  F1_Score = sapply(results, function(x) round(x$f1, 4)),
  AUC = sapply(results, function(x) round(x$auc, 4))
)
rownames(test_summary) <- NULL

cat("=== Test Set Performance ===\n")
print(test_summary)

# Best model
best_model_name <- test_summary$Model[which.max(test_summary$AUC)]
cat("\nBest Model (by AUC):", best_model_name, "\n")

# -----------------------------------------------------------------------------
# 5. ROC CURVES
# -----------------------------------------------------------------------------

png("output/roc_curves_all_models.png", width = 900, height = 700)
colors <- c("steelblue", "forestgreen", "tomato", "purple", "orange")

plot(results[[1]]$roc, col = colors[1], lwd = 2,
     main = "ROC Curves - All Models",
     xlab = "1 - Specificity (False Positive Rate)",
     ylab = "Sensitivity (True Positive Rate)")

for (i in 2:length(results)) {
  lines(results[[i]]$roc, col = colors[i], lwd = 2)
}

legend("bottomright",
       legend = paste0(names(results), " (AUC: ",
                       round(sapply(results, function(x) x$auc), 3), ")"),
       col = colors, lwd = 2, cex = 0.9)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()

# -----------------------------------------------------------------------------
# 6. FEATURE IMPORTANCE (Random Forest)
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           FEATURE IMPORTANCE                                \n")
cat("=============================================================\n\n")

# Random Forest variable importance
rf_importance <- varImp(model_rf)
cat("=== Random Forest Variable Importance ===\n")
print(rf_importance)

png("output/feature_importance_rf.png", width = 800, height = 500)
plot(rf_importance, main = "Random Forest - Variable Importance")
dev.off()

# GBM variable importance
gbm_importance <- varImp(model_gbm)
cat("\n=== Gradient Boosting Variable Importance ===\n")
print(gbm_importance)

# -----------------------------------------------------------------------------
# 7. CONFUSION MATRICES
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           CONFUSION MATRICES                                \n")
cat("=============================================================\n\n")

for (name in names(results)) {
  cat("\n---", name, "---\n")
  print(results[[name]]$confusion_matrix)
}

# Best model confusion matrix visualization
best_cm <- confusionMatrix(predict(models_list[[best_model_name]], X_test_scaled),
                            y_test, positive = "Good")

png("output/best_model_confusion_matrix.png", width = 600, height = 500)
fourfoldplot(best_cm$table,
             color = c("#CC6666", "#99CC99"),
             main = paste("Confusion Matrix -", best_model_name))
dev.off()

# -----------------------------------------------------------------------------
# 8. SAVE RESULTS
# -----------------------------------------------------------------------------

write.csv(test_summary, "output/ml_model_comparison.csv", row.names = FALSE)
saveRDS(models_list[[best_model_name]], "output/best_ml_model.rds")

cat("\n=============================================================\n")
cat("           MACHINE LEARNING ANALYSIS COMPLETE!               \n")
cat("=============================================================\n")
