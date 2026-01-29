# =============================================================================
# Wine Quality Data - Exploratory Data Analysis (EDA)
# Master's Level Statistical Analysis
# =============================================================================

# Load required packages
packages <- c("tidyverse", "corrplot", "skimr", "DataExplorer", "GGally")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
sapply(packages, install_if_missing)

# -----------------------------------------------------------------------------
# 1. DATA LOADING AND INITIAL INSPECTION
# -----------------------------------------------------------------------------

# Load the wine quality dataset (semicolon-separated)
wine <- read.csv("data/wine_quality.csv", sep = ";")

# Basic structure
cat("=== Dataset Structure ===\n")
str(wine)

cat("\n=== Dataset Dimensions ===\n")
cat("Rows:", nrow(wine), "| Columns:", ncol(wine), "\n")

cat("\n=== First 10 Observations ===\n")
head(wine, 10)

# Comprehensive summary using skimr
cat("\n=== Comprehensive Summary Statistics ===\n")
skim(wine)

# -----------------------------------------------------------------------------
# 2. MISSING VALUE ANALYSIS
# -----------------------------------------------------------------------------

cat("\n=== Missing Value Analysis ===\n")
missing_summary <- colSums(is.na(wine))
print(missing_summary)

# Visual missing data pattern
png("output/missing_data_pattern.png", width = 800, height = 600)
plot_missing(wine)
dev.off()

# -----------------------------------------------------------------------------
# 3. UNIVARIATE ANALYSIS
# -----------------------------------------------------------------------------

# Distribution of target variable (quality)
cat("\n=== Quality Distribution ===\n")
print(table(wine$quality))
print(prop.table(table(wine$quality)) * 100)

# Histogram of all variables
png("output/histograms_all_variables.png", width = 1200, height = 900)
wine %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of All Variables",
       subtitle = "Wine Quality Dataset") +
  theme(strip.text = element_text(face = "bold"))
dev.off()

# Density plots for continuous variables
png("output/density_plots.png", width = 1200, height = 900)
wine %>%
  select(-quality) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Density Distributions of Chemical Properties")
dev.off()

# Boxplots to detect outliers
png("output/boxplots_outliers.png", width = 1200, height = 800)
wine %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 1) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  labs(title = "Boxplots for Outlier Detection",
       subtitle = "Red circles indicate potential outliers")
dev.off()

# -----------------------------------------------------------------------------
# 4. BIVARIATE ANALYSIS
# -----------------------------------------------------------------------------

# Quality as factor for grouped analysis
wine$quality_factor <- factor(wine$quality,
                               labels = c("Low", "Low", "Medium", "Medium", "High", "High"))

# Relationship between quality and each predictor
png("output/quality_vs_predictors.png", width = 1400, height = 1000)
wine %>%
  select(-quality_factor) %>%
  pivot_longer(-quality, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(quality), y = value, fill = factor(quality))) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_minimal() +
  labs(title = "Relationship Between Wine Quality and Chemical Properties",
       x = "Quality Score", y = "Value", fill = "Quality") +
  theme(legend.position = "bottom")
dev.off()

# -----------------------------------------------------------------------------
# 5. CORRELATION ANALYSIS
# -----------------------------------------------------------------------------

# Correlation matrix
cor_matrix <- cor(wine %>% select(-quality_factor))

cat("\n=== Correlation Matrix ===\n")
print(round(cor_matrix, 3))

# Correlation heatmap
png("output/correlation_heatmap.png", width = 1000, height = 800)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         title = "Correlation Heatmap - Wine Quality Dataset",
         mar = c(0, 0, 2, 0))
dev.off()

# Pairs plot for key variables
png("output/pairs_plot.png", width = 1200, height = 1000)
wine %>%
  select(alcohol, volatile.acidity, sulphates, citric.acid, quality) %>%
  ggpairs(aes(color = factor(quality), alpha = 0.5),
          upper = list(continuous = wrap("cor", size = 3)),
          lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5))) +
  theme_minimal() +
  labs(title = "Pairs Plot - Key Variables")
dev.off()

# -----------------------------------------------------------------------------
# 6. STATISTICAL SUMMARIES BY QUALITY GROUP
# -----------------------------------------------------------------------------

cat("\n=== Summary Statistics by Quality Level ===\n")
quality_summary <- wine %>%
  group_by(quality) %>%
  summarise(across(where(is.numeric),
                   list(mean = mean, sd = sd, median = median),
                   .names = "{.col}_{.fn}"))
print(quality_summary)

# Export summary
write.csv(quality_summary, "output/summary_by_quality.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 7. OUTLIER DETECTION USING IQR METHOD
# -----------------------------------------------------------------------------

detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(sum(x < lower | x > upper))
}

cat("\n=== Outlier Count by Variable (IQR Method) ===\n")
outlier_counts <- sapply(wine %>% select(-quality_factor), detect_outliers)
print(sort(outlier_counts, decreasing = TRUE))

cat("\n=== EDA Complete! Check the 'output' folder for visualizations ===\n")
