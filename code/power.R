# S1-only: power table without panel labels

library(dplyr)
library(purrr)
library(fixest)
library(sandwich)

set.seed(123)

main <- readRDS("data/AI games.rds")
panelA_data <- main %>% filter(game != "Virtual 2025") %>% mutate(
  game_software = interaction(game, software, drop = TRUE),
  max_skill  = factor(max_skill),
  min_skill  = factor(min_skill),
  attendance = factor(attendance),
  branch     = factor(branch, levels = c("Human-Only","AI-Assisted","AI-Led")),
  AI_Assisted = as.integer(branch == "AI-Assisted"),
  AI_Led      = as.integer(branch == "AI-Led")
)

dep_vars <- c("reproduction", "minor_errors", "major_errors",
              "one_good_robustness", "two_good_robustness",
              "ran_one_robustness", "ran_two_robustness")
panelA_data <- panelA_data %>% mutate(across(all_of(dep_vars), as.numeric))

bootstrap_power <- function(dv, data, sims = 10000, alpha = 0.05, two_sided = TRUE){
  make_fml <- \(y) as.formula(paste(y, "~ AI_Assisted + AI_Led + number_teammates |",
                                    "game_software + max_skill + min_skill + attendance"))
  mdl <- feols(make_fml(dv), data = data, se = "hc1")
  if (!all(c("AI_Assisted","AI_Led") %in% names(coef(mdl)))) return(tibble(DV=dv, AI_Assisted=NA, AI_Led=NA))
  yhat <- fitted(mdl); resid <- resid(mdl); n <- length(resid); k <- length(coef(mdl))
  crit <- qt(1 - alpha/2, n - k)
  hits <- denom <- c(AA=0L, AL=0L)
  for (i in seq_len(sims)) {
    data$ysim <- yhat + sample(resid, n, replace=TRUE)
    mdl_sim <- tryCatch(feols(make_fml("ysim"), data = data, se = "hc1"), error = function(e) NULL)
    if (is.null(mdl_sim)) next
    tvals <- summary(mdl_sim)$coeftable[,"t value"]
    if (!is.na(tvals["AI_Assisted"])) { denom["AA"] <- denom["AA"] + 1L; if (abs(tvals["AI_Assisted"])>crit) hits["AA"] <- hits["AA"] + 1L }
    if (!is.na(tvals["AI_Led"]))      { denom["AL"] <- denom["AL"] + 1L; if (abs(tvals["AI_Led"])>crit)      hits["AL"] <- hits["AL"] + 1L }
  }
  tibble(DV=dv,
         AI_Assisted=ifelse(denom["AA"]==0,NA,hits["AA"]/denom["AA"]),
         AI_Led=ifelse(denom["AL"]==0,NA,hits["AL"]/denom["AL"]))
}

panelA_power <- map_dfr(dep_vars, bootstrap_power, data = panelA_data)

dv_labels <- c("Reproduction", "Minor errors", "Major errors",
               "One good robustness", "Two good robustness",
               "Ran one robustness", "Ran two robustness")
fmt <- \(x) ifelse(is.na(x), "", sprintf("%5.3f", x))

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)
sink("output/S1/tables/power.tex")
cat("\\begin{tabular}{l*{7}{c}}\n\\hline\\hline\n")
cat("& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
cat("                    &", paste(dv_labels, collapse = "   &"), "   \\\\\n\\hline\n")
cat("AI-Assisted power    &", paste(fmt(panelA_power$AI_Assisted), collapse = " & "), " \\\n")
cat("AI-Led power         &", paste(fmt(panelA_power$AI_Led), collapse = " & "), " \\\n")
cat("\\hline\n\\hline\n\\end{tabular}\n")
sink()

