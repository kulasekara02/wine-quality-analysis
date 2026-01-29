# =============================================================================
# MASTER SCRIPT - Run All Analyses
# Wine Quality Data Analysis Project
# =============================================================================

cat("\n")
cat("=============================================================\n")
cat("    WINE QUALITY DATA ANALYSIS - MASTER'S LEVEL PROJECT      \n")
cat("=============================================================\n\n")

# Set working directory to project root
# setwd("path/to/my_r_project")

# Create output directory if it doesn't exist
if (!dir.exists("output")) dir.create("output")

# Run all analysis scripts in order
scripts <- c(
  "scripts/01_data_exploration.R",
  "scripts/02_statistical_tests.R",
  "scripts/03_pca_analysis.R",
  "scripts/04_regression_analysis.R",
  "scripts/05_machine_learning.R"
)

for (script in scripts) {
  cat("\n")
  cat("=============================================================\n")
  cat(" Running:", script, "\n")
  cat("=============================================================\n")
  tryCatch({
    source(script)
    cat("\n[SUCCESS]", script, "completed!\n")
  }, error = function(e) {
    cat("\n[ERROR] Failed to run", script, "\n")
    cat("Error message:", conditionMessage(e), "\n")
  })
}

cat("\n")
cat("=============================================================\n")
cat("           ALL ANALYSES COMPLETE!                            \n")
cat("=============================================================\n")
cat("\nOutput files saved to the 'output/' directory.\n")
cat("Review the generated plots and CSV files for results.\n")
