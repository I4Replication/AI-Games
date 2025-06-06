# ===============================================================
#   Ex-post power for the “AI Games” project   (7 outcome DVs)
#   -------------------------------------------------------------
#   • Panel A  = Study 1   (all games except “Virtual 2025”)
#   • Panel B  = Study 2   (full combined sample)
#   • Rows     = AI-Assisted power, AI-Led power
#   • Columns  = 7 dependent variables
#   • Output   =  output/tables/power_main.tex   (two-sided α = .05)
# ===============================================================

# ---- 0  libraries ---------------------------------------------------------
library(dplyr)
library(purrr)
library(fixest)      # feols()
library(sandwich)    # vcovHC()
library(tidyr)

set.seed(123)        # reproducible bootstrap

# ---- 1  load & prepare ----------------------------------------------------
main <- readRDS("data/AI games.rds")

panelA_data <- main %>% filter(game != "Virtual 2025")

prep_data <- function(df) {
  df %>% 
    mutate(
      game_software = interaction(game, software, drop = TRUE),
      max_skill  = factor(max_skill),
      min_skill  = factor(min_skill),
      attendance = factor(attendance),
      branch     = factor(branch,
                          levels = c("Human-Only", "AI-Assisted", "AI-Led")),
      AI_Assisted = as.integer(branch == "AI-Assisted"),
      AI_Led      = as.integer(branch == "AI-Led")
    )
}

main        <- prep_data(main)
panelA_data <- prep_data(panelA_data)

dep_vars <- c(
  "reproduction", "minor_errors", "major_errors",
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)

main        <- main        %>% mutate(across(all_of(dep_vars), as.numeric))
panelA_data <- panelA_data %>% mutate(across(all_of(dep_vars), as.numeric))

# ---- 2  bootstrap-power helper -------------------------------------------
bootstrap_power <- function(dv, data,
                            sims      = 10000,
                            alpha     = 0.05,
                            two_sided = TRUE) {
  
  make_fml <- \(y) as.formula(
    paste(
      y, "~ AI_Assisted + AI_Led + number_teammates |",
      "game_software + max_skill + min_skill + attendance"
    )
  )
  
  ## ---- baseline ----------------------------------------------------------
  mdl <- feols(make_fml(dv), data = data, se = "hc1")
  
  needed <- c("AI_Assisted", "AI_Led")
  if (!all(needed %in% names(coef(mdl)))) {
    return(tibble(DV = dv, AI_Assisted = NA_real_, AI_Led = NA_real_))
  }
  
  y_hat <- fitted(mdl)
  resid <- resid(mdl)
  n     <- length(resid)
  k     <- length(coef(mdl))
  crit  <- if (two_sided) qt(1 - alpha/2, n - k) else qt(alpha, n - k)
  
  hits  <- denom <- c(AA = 0L, AL = 0L)
  
  ## ---- bootstrap loop ----------------------------------------------------
  for (i in seq_len(sims)) {
    data$ysim <- y_hat + sample(resid, n, replace = TRUE)
    
    mdl_sim <- tryCatch(
      feols(make_fml("ysim"), data = data, se = "hc1"),
      error = function(e) NULL
    )
    if (is.null(mdl_sim)) next
    
    # keep row-names with this variant ↓
    tvals <- summary(mdl_sim)$coeftable[, "t value"]
    
    # AI-Assisted
    if (!is.na(tvals["AI_Assisted"])) {
      denom["AA"] <- denom["AA"] + 1L
      ok <- if (two_sided) abs(tvals["AI_Assisted"]) > crit
      else            tvals["AI_Assisted"] <  crit
      if (ok) hits["AA"] <- hits["AA"] + 1L
    }
    # AI-Led
    if (!is.na(tvals["AI_Led"])) {
      denom["AL"] <- denom["AL"] + 1L
      ok <- if (two_sided) abs(tvals["AI_Led"]) > crit
      else            tvals["AI_Led"] <  crit
      if (ok) hits["AL"] <- hits["AL"] + 1L
    }
  }
  
  tibble(
    DV          = dv,
    AI_Assisted = ifelse(denom["AA"] == 0, NA, hits["AA"] / denom["AA"]),
    AI_Led      = ifelse(denom["AL"] == 0, NA, hits["AL"] / denom["AL"])
  )
}

run_panel <- function(df, lbl) {
  map_dfr(dep_vars, bootstrap_power, data = df) %>% mutate(Panel = lbl)
}

panelA_power <- run_panel(panelA_data, "Panel A: Study 1")
panelB_power <- run_panel(main,        "Panel B: Study 2 (Combined)")

# ---- 3  build LaTeX table -------------------------------------------------
dv_labels <- c(
  "Reproduction", "Minor errors", "Major errors",
  "One good robustness", "Two good robustness",
  "Ran one robustness", "Ran two robustness"
)

make_power_panel <- function(panel_df, header) {
  fmt <- \(x) ifelse(is.na(x), "", sprintf("%5.3f", x))
  vals <- panel_df %>% arrange(match(DV, dep_vars))
  row1 <- fmt(vals$AI_Assisted)
  row2 <- fmt(vals$AI_Led)
  
  paste0(
    "\\multicolumn{8}{l}{\\textbf{", header, "}}\\\\\n",
    "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n",
    "                    &", paste(dv_labels, collapse = "   &"), "   \\\\\n\\hline\n",
    "AI-Assisted power    &", paste(row1, collapse = " & "), " \\\\\n",
    "AI-Led power         &", paste(row2, collapse = " & "), " \\\\\n",
    "\\hline\n"
  )
}

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

sink("output/tables/power.tex")
cat("\\begin{tabular}{l*{7}{c}}\n")
cat("\\hline\\hline\n")
cat(make_power_panel(panelA_power, "Panel A: Study I"))
cat(make_power_panel(panelB_power, "Panel B: Studies I and II combined"))
cat("\\hline\\hline\n")
cat("\\multicolumn{8}{l}{\\it{Note:} Monte Carlo ex-post power calculations given realized sample size and observed effects as estimated in Table 2. 10.000 residual bootstrap draws \\(\\alpha = 0.05\\).}\\\\\n")
cat("\\end{tabular}\n")
sink()
