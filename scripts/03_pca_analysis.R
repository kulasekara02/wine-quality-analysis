# =============================================================================
# Wine Quality Data - Principal Component Analysis (PCA)
# Master's Level Multivariate Analysis
# =============================================================================

# Load required packages
packages <- c("tidyverse", "factoextra", "FactoMineR", "corrplot", "gridExtra")
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
# 1. DATA PREPARATION FOR PCA
# -----------------------------------------------------------------------------

cat("=============================================================\n")
cat("           PRINCIPAL COMPONENT ANALYSIS (PCA)                \n")
cat("=============================================================\n\n")

# Separate predictors and target
X <- wine %>% select(-quality)
y <- wine$quality

# Check for multicollinearity (condition for PCA)
cat("=== Correlation Check Before PCA ===\n")
cor_matrix <- cor(X)
high_cor <- which(abs(cor_matrix) > 0.7 & cor_matrix != 1, arr.ind = TRUE)
cat("Variable pairs with correlation > 0.7:\n")
for (i in 1:nrow(high_cor)) {
  if (high_cor[i, 1] < high_cor[i, 2]) {
    cat(sprintf("%s - %s: %.3f\n",
                rownames(cor_matrix)[high_cor[i, 1]],
                colnames(cor_matrix)[high_cor[i, 2]],
                cor_matrix[high_cor[i, 1], high_cor[i, 2]]))
  }
}

# -----------------------------------------------------------------------------
# 2. PCA EXECUTION
# -----------------------------------------------------------------------------

# Perform PCA with scaling (standardization)
pca_result <- prcomp(X, center = TRUE, scale. = TRUE)

# Also using FactoMineR for additional insights
pca_facto <- PCA(X, scale.unit = TRUE, graph = FALSE)

cat("\n=== PCA Summary ===\n")
print(summary(pca_result))

# -----------------------------------------------------------------------------
# 3. EIGENVALUES AND VARIANCE EXPLAINED
# -----------------------------------------------------------------------------

cat("\n=== Eigenvalues ===\n")
eigenvalues <- pca_result$sdev^2
variance_explained <- eigenvalues / sum(eigenvalues) * 100
cumulative_variance <- cumsum(variance_explained)

eigen_df <- data.frame(
  PC = paste0("PC", 1:length(eigenvalues)),
  Eigenvalue = round(eigenvalues, 4),
  Variance_Percent = round(variance_explained, 2),
  Cumulative_Percent = round(cumulative_variance, 2)
)
print(eigen_df)

# Scree plot
png("output/pca_scree_plot.png", width = 1000, height = 500)
p1 <- fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 35),
               main = "Scree Plot - Variance Explained by Each PC")

p2 <- ggplot(eigen_df, aes(x = 1:nrow(eigen_df), y = Cumulative_Percent)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:nrow(eigen_df)) +
  theme_minimal() +
  labs(title = "Cumulative Variance Explained",
       x = "Principal Component", y = "Cumulative Variance (%)")

grid.arrange(p1, p2, ncol = 2)
dev.off()

# Kaiser criterion (eigenvalue > 1)
cat("\n=== Kaiser Criterion (Eigenvalue > 1) ===\n")
cat("Number of PCs to retain:", sum(eigenvalues > 1), "\n")

# -----------------------------------------------------------------------------
# 4. LOADINGS ANALYSIS
# -----------------------------------------------------------------------------

cat("\n=== Principal Component Loadings ===\n")
loadings <- pca_result$rotation
print(round(loadings[, 1:5], 4))

# Contribution of variables to PCs
png("output/pca_variable_contributions.png", width = 1200, height = 500)
p1 <- fviz_contrib(pca_result, choice = "var", axes = 1,
                    title = "Contribution to PC1")
p2 <- fviz_contrib(pca_result, choice = "var", axes = 2,
                    title = "Contribution to PC2")
p3 <- fviz_contrib(pca_result, choice = "var", axes = 3,
                    title = "Contribution to PC3")
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()

# -----------------------------------------------------------------------------
# 5. BIPLOT AND VISUALIZATION
# -----------------------------------------------------------------------------

# Variable correlation plot (loading plot)
png("output/pca_variable_plot.png", width = 800, height = 700)
fviz_pca_var(pca_result,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "PCA - Variable Loading Plot")
dev.off()

# Individual scores plot colored by quality
png("output/pca_individual_plot.png", width = 900, height = 700)
fviz_pca_ind(pca_result,
             col.ind = factor(y),
             palette = "RdYlGn",
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "Quality",
             title = "PCA - Individuals by Wine Quality",
             mean.point = FALSE)
dev.off()

# Combined biplot
png("output/pca_biplot.png", width = 1000, height = 800)
fviz_pca_biplot(pca_result,
                col.ind = factor(y),
                palette = "RdYlGn",
                addEllipses = TRUE,
                label = "var",
                col.var = "black",
                repel = TRUE,
                legend.title = "Quality",
                title = "PCA Biplot - Variables and Observations")
dev.off()

# -----------------------------------------------------------------------------
# 6. PC SCORES ANALYSIS
# -----------------------------------------------------------------------------

cat("\n=== PC Scores Summary ===\n")
scores <- as.data.frame(pca_result$x)
scores$quality <- y

# Correlation between PCs and quality
pc_quality_cor <- sapply(scores[, 1:5], function(pc) cor(pc, scores$quality))
cat("\nCorrelation between Principal Components and Quality:\n")
print(round(pc_quality_cor, 4))

# ANOVA: Do PC scores differ by quality?
cat("\n=== ANOVA: PC1 by Quality Group ===\n")
anova_pc1 <- aov(PC1 ~ factor(quality), data = scores)
print(summary(anova_pc1))

cat("\n=== ANOVA: PC2 by Quality Group ===\n")
anova_pc2 <- aov(PC2 ~ factor(quality), data = scores)
print(summary(anova_pc2))

# Visualize PC scores by quality
png("output/pca_scores_by_quality.png", width = 1000, height = 400)
p1 <- ggplot(scores, aes(x = factor(quality), y = PC1, fill = factor(quality))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_minimal() +
  labs(title = "PC1 Scores by Quality", x = "Quality", y = "PC1 Score") +
  theme(legend.position = "none")

p2 <- ggplot(scores, aes(x = factor(quality), y = PC2, fill = factor(quality))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_minimal() +
  labs(title = "PC2 Scores by Quality", x = "Quality", y = "PC2 Score") +
  theme(legend.position = "none")

grid.arrange(p1, p2, ncol = 2)
dev.off()

# -----------------------------------------------------------------------------
# 7. INTERPRETATION SUMMARY
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("           PCA INTERPRETATION SUMMARY                        \n")
cat("=============================================================\n\n")

cat("PC1 (", round(variance_explained[1], 1), "% variance):\n", sep = "")
top_loadings_pc1 <- sort(abs(loadings[, 1]), decreasing = TRUE)[1:3]
cat("  Top contributors:", paste(names(top_loadings_pc1), collapse = ", "), "\n")

cat("\nPC2 (", round(variance_explained[2], 1), "% variance):\n", sep = "")
top_loadings_pc2 <- sort(abs(loadings[, 2]), decreasing = TRUE)[1:3]
cat("  Top contributors:", paste(names(top_loadings_pc2), collapse = ", "), "\n")

cat("\nPC3 (", round(variance_explained[3], 1), "% variance):\n", sep = "")
top_loadings_pc3 <- sort(abs(loadings[, 3]), decreasing = TRUE)[1:3]
cat("  Top contributors:", paste(names(top_loadings_pc3), collapse = ", "), "\n")

# Export results
write.csv(eigen_df, "output/pca_eigenvalues.csv", row.names = FALSE)
write.csv(loadings, "output/pca_loadings.csv")
write.csv(scores, "output/pca_scores.csv", row.names = FALSE)

cat("\n=== PCA Analysis Complete! ===\n")
