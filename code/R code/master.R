# ===========================================
# Master R Script for AI Paper Replication
# File location (relative to project root):
#   code/R code/master.R
# ===========================================

# ---- Declare project root ----
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
here::i_am("code/R code/master.R")

# ---- Load required packages ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pkgs <- c(
  "haven", "rmarkdown", "readxl",
  "dplyr", "stringr", "tidyr", "forcats", "janitor", "lubridate",
  "fixest", "purrr", "broom", "tibble", "car", "margins",
  "sandwich", "lmtest", "multcomp", "kableExtra",
  "ggplot2", "patchwork", "modelsummary",
  "ggsurvfit", "survRM2", "xtable"
)
pacman::p_load(char = pkgs, install = TRUE, character.only = TRUE)

# ---- Start logging ----
log_file <- here::here("output", "master_log_R.log")
sink(log_file, split = TRUE)
cat("=== MASTER LOG START ===\n")

# ---- 1. Cleaning raw data ----
cat("\n--- Cleaning raw data ---\n")
source(here::here("code", "R code", "cleaning.R"))
rm(list = ls())

# ---- 2. Main Tables (regression outputs) ----
cat("\n--- Main Table (OLS) ---\n")
source(here::here("code", "R code", "main.R"))
rm(list = ls())

cat("\n--- Main Table (Logit and Poisson) ---\n")
source(here::here("code", "R code", "logit poisson.R"))
rm(list = ls())

cat("\n--- Main Table (Full Controls) ---\n")
source(here::here("code", "R code", "full controls.R"))
rm(list = ls())

cat("\n--- Main Table (Softwares) ---\n")
source(here::here("code", "R code", "softwares.R"))
rm(list = ls())

cat("\n--- Main Table (Error Shares) ---\n")
source(here::here("code", "R code", "error shares.R"))
rm(list = ls())

cat("\n--- Main Table (Study II Interaction) ---\n")
source(here::here("code", "R code", "study 2.R"))
rm(list = ls())

cat("\n--- Main Table (Power) ---\n")
source(here::here("code", "R code", "power.R"))
rm(list = ls())

# ---- 3. Group Differences Table ----
cat("\n--- Group Differences Table ---\n")
source(here::here("code", "R code", "branches.R"))
rm(list = ls())

# ---- 4. Balance Tables ----
cat("\n--- Balance Tables ---\n")
source(here::here("code", "R code", "balance.R"))
rm(list = ls())

# ---- 5. ChatGPT Skill Tables ----
cat("\n--- ChatGPT Skill Tables ---\n")
source(here::here("code", "R code", "gpt skill.R"))
rm(list = ls())

# ---- 6. Prompts Usage Tables ----
cat("\n--- Prompts Usage Tables ---\n")
source(here::here("code", "R code", "prompts.R"))
rm(list = ls())

# ---- 7. RMST Tables ----
cat("\n--- RMST Tables ---\n")
source(here::here("code", "R code", "rmst.R"))
rm(list = ls())

# ---- 8. Figures ----
cat("\n--- Figure: Time to First Cumulative Density ---\n")
source(here::here("code", "R code", "time to first.R"))
rm(list = ls())

cat("\n--- Figure: Reproduction Rates Over Time ---\n")
source(here::here("code", "R code", "reproduction rates.R"))
rm(list = ls())

cat("\n--- Figure: Prompt Distribution ---\n")
source(here::here("code", "R code", "prompt distribution.R"))
rm(list = ls())

# ---- Finish logging ----
cat("\n=== MASTER LOG END ===\n")
sink()