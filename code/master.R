# ===========================================
# Master R Script (S1-only outputs)
# Location: code/R code S1/master.R
# Writes to: output/S1/{tables,figures}
# - Filters out Study II (game == "Virtual 2025") in all pooled outputs
# - For panelled tables, emits only the Study I content without panel labels
# - Omits Study II–only outputs (e.g., study 2.tex)
# ===========================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
here::i_am("code/R code S1/master.R")

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pkgs <- c(
  "haven", "rmarkdown", "readxl",
  "dplyr", "stringr", "tidyr", "forcats", "janitor", "lubridate",
  "fixest", "purrr", "broom", "tibble", "car", "margins",
  "sandwich", "lmtest", "multcomp", "kableExtra",
  "ggplot2", "patchwork", "modelsummary",
  "ggsurvfit", "survRM2", "xtable", "glue"
)
pacman::p_load(char = pkgs, install = TRUE, character.only = TRUE)

dir.create(here::here("output", "S1", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output", "S1", "figures"), recursive = TRUE, showWarnings = FALSE)

log_file <- here::here("output", "S1", "master_log_R.log")
sink(log_file, split = TRUE)
cat("=== MASTER S1 LOG START ===\n")

# 1) Cleaning (produces data/AI games.rds)
cat("\n--- Cleaning raw data ---\n")
source(here::here("code", "R code", "cleaning.R"))
rm(list = ls())

# 2) Tables (S1-only)
cat("\n--- Main Table (OLS, S1) ---\n")
source(here::here("code", "R code S1", "main.R"))
rm(list = ls())

cat("\n--- Main Table (Logit/Poisson, S1) ---\n")
source(here::here("code", "R code S1", "logit poisson.R"))
rm(list = ls())

cat("\n--- Full Controls (S1 only) ---\n")
source(here::here("code", "R code S1", "full controls.R"))
rm(list = ls())

cat("\n--- Softwares (S1 only) ---\n")
source(here::here("code", "R code S1", "softwares.R"))
rm(list = ls())

cat("\n--- Error Shares (S1 only) ---\n")
source(here::here("code", "R code S1", "error shares.R"))
rm(list = ls())

cat("\n--- Power (S1 only) ---\n")
source(here::here("code", "R code S1", "power.R"))
rm(list = ls())

cat("\n--- Group Differences (S1 filtered) ---\n")
source(here::here("code", "R code S1", "branches.R"))
rm(list = ls())

cat("\n--- Balance (S1 filtered) ---\n")
source(here::here("code", "R code S1", "balance.R"))
rm(list = ls())

cat("\n--- GPT Skill (S1 filtered) ---\n")
source(here::here("code", "R code S1", "gpt skill.R"))
rm(list = ls())

cat("\n--- Prompts (S1 filtered) ---\n")
source(here::here("code", "R code S1", "prompts.R"))
rm(list = ls())

cat("\n--- RMST (S1 filtered) ---\n")
source(here::here("code", "R code S1", "rmst.R"))
rm(list = ls())

# 3) Figures (S1-only)
cat("\n--- Figures: Time to First (S1 only) ---\n")
source(here::here("code", "R code S1", "time to first.R"))
rm(list = ls())

cat("\n--- Figures: Reproduction Rates (S1 only) ---\n")
source(here::here("code", "R code S1", "reproduction rates.R"))
rm(list = ls())

cat("\n--- Figure: Prompt Distribution (S1 filtered) ---\n")
source(here::here("code", "R code S1", "prompt distribution.R"))
rm(list = ls())

# (Focus group visuals omitted in S1 to avoid heavy downloads; can be added on request.)

cat("\n=== MASTER S1 LOG END ===\n")
sink()
