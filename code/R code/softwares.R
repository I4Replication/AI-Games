# ---- 2. Load and prep data ----
main <- readRDS("data/AI games.rds") %>%
  mutate(
    game_software = interaction(game, software, drop = TRUE),
    max_skill = as.factor(max_skill),
    min_skill = as.factor(min_skill),
    max_gpt   = as.factor(max_gpt),
    min_gpt   = as.factor(min_gpt),
    attendance = as.factor(attendance),
    branch = as.factor(branch),
    software = as.factor(software),
    game = as.factor(game)
  )

# ---- 3. Variables ----
dep_vars_soft <- c(
  "reproduction", "minor_errors", "major_errors", 
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)

panelA_data <- main %>% filter(game != "Virtual 2025")

# ---- 4. Model estimation ----
panelA_models_soft <- setNames(vector("list", length(dep_vars_soft)), dep_vars_soft)
for (dv in dep_vars_soft) {
  f <- as.formula(paste0(
    dv, " ~ branch * software + number_teammates | game + max_skill + min_skill + attendance"
  ))
  mod <- feols(f, data = panelA_data, vcov = "hetero")
  panelA_models_soft[[dv]] <- mod
}

panelB_models_soft <- setNames(vector("list", length(dep_vars_soft)), dep_vars_soft)
for (dv in dep_vars_soft) {
  f <- as.formula(paste0(
    dv, " ~ branch * software + number_teammates | game + max_skill + min_skill + attendance"
  ))
  mod <- feols(f, data = main, vcov = "hetero")
  panelB_models_soft[[dv]] <- mod
}

# ---- 5. Extraction helper for all coefficients ----
extract_soft_summary <- function(model, dv, df) {
  coef_terms <- c(
    "branchAI-Assisted",
    "branchAI-Led",
    "softwareR",
    "branchAI-Assisted:softwareR",
    "branchAI-Led:softwareR"
  )
  ctab <- broom::tidy(model, conf.int = TRUE)
  get_row <- function(term) {
    x <- ctab[ctab$term == term, ]
    list(
      estimate = if (nrow(x)) x$estimate else NA,
      se = if (nrow(x)) x$std.error else NA,
      ci_low = if (nrow(x)) x$conf.low else NA,
      ci_high = if (nrow(x)) x$conf.high else NA,
      pval = if (nrow(x)) x$p.value else NA
    )
  }
  rows <- lapply(coef_terms, get_row)
  names(rows) <- coef_terms
  ymean <- mean(df[[dv]][!is.na(df[[dv]])], na.rm = TRUE)
  nobs <- nobs(model)
  # Wald test: AA+AA:R = AL+AL:R (AI-Assisted vs AI-Led in R)
  L <- rep(0, length(coef(model)))
  names(L) <- names(coef(model))
  L["branchAI-Assisted"] <- 1
  L["branchAI-Assisted:softwareR"] <- 1
  L["branchAI-Led"] <- -1
  L["branchAI-Led:softwareR"] <- -1
  pval_r <- tryCatch({
    glht_out <- summary(multcomp::glht(model, linfct = matrix(L, nrow=1)), vcov=sandwich::vcovHC(model, type="HC1"))
    as.numeric(glht_out$test$pvalues)
  }, error = function(e) NA)
  list(
    rows = rows, ymean = ymean, N = nobs, pval_r = pval_r, controls = "\\checkmark"
  )
}

# ---- 6. Summarize for each panel ----
panelA_summary_soft <- lapply(seq_along(dep_vars_soft), function(i) {
  extract_soft_summary(panelA_models_soft[[i]], dep_vars_soft[i], panelA_data)
})
panelB_summary_soft <- lapply(seq_along(dep_vars_soft), function(i) {
  extract_soft_summary(panelB_models_soft[[i]], dep_vars_soft[i], main)
})

# ---- 7. Utility: stars for each p-value ----
get_stars <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < 0.01) return("***")
  if (pval < 0.05) return("**")
  if (pval < 0.1)  return("*")
  return("")
}

# ---- 8. Generate LaTeX panel ----
make_panel_latex_soft <- function(panel_res, panel_label) {
  vars <- c(
    "Reproduction", "\\shortstack[c]{Minor\\\\errors}", "\\shortstack[c]{Major\\\\errors}",
    "\\shortstack[c]{One good\\\\robustness}", "\\shortstack[c]{Two good\\\\robustness}",
    "\\shortstack[c]{Ran one\\\\robustness}", "\\shortstack[c]{Ran two\\\\robustness}"
  )
  indices <- 1:7
  coef_names <- c("AI-Assisted", "AI-Led", "R", "AI-Assisted $\\times$ R", "AI-Led $\\times$ R")
  coef_terms <- c("branchAI-Assisted", "branchAI-Led", "softwareR", 
                  "branchAI-Assisted:softwareR", "branchAI-Led:softwareR")
  out <- ""
  out <- paste0(out, "\\multicolumn{8}{l}{\\textbf{", panel_label, "}}\\\\\n")
  out <- paste0(out, "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
  out <- paste0(out, "                    &", paste(vars, collapse = "   &"), "   \\\\\n\\hline\n")
  for (i in 1:5) {
    coefs <- sapply(panel_res[indices], function(x) sprintf("%9.3f%s", x$rows[[i]]$estimate, get_stars(x$rows[[i]]$pval)))
    ses   <- sapply(panel_res[indices], function(x) sprintf("%9.3f", x$rows[[i]]$se))
    cis   <- sapply(panel_res[indices], function(x) sprintf("[%9.3f; %9.3f]", x$rows[[i]]$ci_low, x$rows[[i]]$ci_high))
    out <- paste0(out, coef_names[i], "         &", paste(coefs, collapse="   &"), "   \\\\\n")
    out <- paste0(out, "                    &", paste(sprintf("(%s)", ses), collapse="   &"), "   \\\\\n")
    out <- paste0(out, "                    &", paste(cis, collapse="   &"), "   \\\\\n")
  }
  ymean    <- sapply(panel_res[indices], function(x) sprintf("%9.3f", x$ymean))
  pvals    <- sapply(panel_res[indices], function(x) ifelse(is.na(x$pval_r), "", sprintf("%9.3f", x$pval_r)))
  nobs     <- sapply(panel_res[indices], function(x) as.character(x$N))
  controls <- sapply(panel_res[indices], function(x) x$controls)
  out <- paste0(out, "\\hline\n")
  out <- paste0(out, "Controls            &", paste(controls, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "Mean of dep. var    &", paste(ymean, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "p-val (AI-Assisted vs. AI-Led)&", paste(pvals, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "Obs.                &", paste(nobs, collapse="   &"), "   \\\\\n\\hline\n")
  return(out)
}

# ---- 9. Print full LaTeX table ----
print_full_latex_table_soft <- function(panelA_res, panelB_res) {
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat("\\begin{tabular}{l*{7}{c}}\n")
  cat("\\hline\\hline\n")
  cat(make_panel_latex_soft(panelA_res, "Panel A: Study 1"))
  cat("\\\\\n")
  cat(make_panel_latex_soft(panelB_res, "Panel B: Study 2 combined"))
  cat("\\hline\\hline\n")
  cat("\\multicolumn{8}{l}{\\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only branch omitted; Stata papers omitted.}\\\\\n")
  cat("\\multicolumn{8}{l}{Controls include number of teammates; game, skill, and attendance fixed effects.}\\\\\n")
  cat("\\multicolumn{8}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
  cat("\\end{tabular}\n")
}

# ---- 10. To output your LaTeX table: ----
sink("output/tables/softwares.tex")
print_full_latex_table_soft(panelA_summary_soft, panelB_summary_soft)
sink()