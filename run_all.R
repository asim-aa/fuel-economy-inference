# run_all.R
# Master script to reproduce all analyses

# Load libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(coin)
  library(stats)
})

cat("=== Starting project pipeline ===\n")

# Problem 1
cat("[1/4] Running Problem 1 script...\n")
source("R/01_problem1.R")

# Problem 2
cat("[2/4] Running Problem 2 script...\n")
source("R/02_problem2.R")

# Problem 3
cat("[3/4] Running Problem 3 script...\n")
source("R/03_problem3.R")

# Problem 4
cat("[4/4] Running Problem 4 script...\n")
source("R/04_problem4.R")

cat("=== Pipeline complete. Outputs saved to /outputs/ ===\n")

