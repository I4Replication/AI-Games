# ---- 1. Read data -----------------------------------------------------------
main <- readRDS("data/AI games.rds")

# ---- 2. Preserve row order & split Panel A ----------------------------------
rownames(main) <- 1:nrow(main)
panelA_data    <- main %>% filter(game != "Virtual 2025")
rownames(panelA_data) <- 1:nrow(panelA_data)

# ---- 3. Factorise and build FE variables ------------------------------------
factorise_vars <- function(df) {
  df %>% mutate(
    game_software = interaction(game, software, drop = TRUE),
    max_skill     = as.factor(max_skill),
    min_skill     = as.factor(min_skill),
    max_gpt       = as.factor(max_gpt),
    min_gpt       = as.factor(min_gpt),
    attendance    = as.factor(attendance),
    branch        = as.factor(branch)
  )
}
main        <- factorise_vars(main)
panelA_data <- factorise_vars(panelA_data)

# ---- 4. Dependent variables --------------------------------------------------
dep_vars <- c(
  "reproduction", "minor_errors", "major_errors",
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)
main        <- main       %>% mutate(across(all_of(dep_vars), as.numeric))
panelA_data <- panelA_data %>% mutate(across(all_of(dep_vars), as.numeric))

# ---- 5. Fit models -----------------------------------------------------------
fit_models <- function(df) {
  map(dep_vars, \(dv)
      feols(
        as.formula(
          paste(dv,
                "~ branch + number_teammates |",
                "game_software + max_skill + min_skill + attendance")
        ),
        data = df,
        vcov = "hetero"
      )
  )
}
panelA_models <- fit_models(panelA_data)
panelB_models <- fit_models(main)

# ---- 6. Helper: joint p-value (AA = AL) --------------------------------------
get_joint_pval <- function(model) {
  out <- tryCatch(
    linearHypothesis(
      model, "branchAI-Assisted = branchAI-Led",
      vcov. = vcov(model, type = "HC1")
    ),
    error = \(e) NULL
  )
  if (is.null(out) || !"Pr(>Chisq)" %in% names(out)) return(NA_real_)
  as.numeric(out[2, "Pr(>Chisq)"])
}

# ---- 7. Extract summary ------------------------------------------------------
extract_summary <- function(model, dv, df) {
  tidy_df <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term %in% c("branchAI-Assisted", "branchAI-Led"))
  
  ai_ass <- tidy_df %>% filter(term == "branchAI-Assisted")
  ai_led <- tidy_df %>% filter(term == "branchAI-Led")
  
  used_rows <- which(!is.na(model$fitted.values))
  ymean     <- mean(df[[dv]][used_rows], na.rm = TRUE)
  
  tibble(
    coef_ai_assisted   = ai_ass$estimate,
    se_ai_assisted     = ai_ass$std.error,
    ci_low_ai_assisted = ai_ass$conf.low,
    ci_high_ai_assisted= ai_ass$conf.high,
    pval_ai_assisted   = ai_ass$p.value,
    
    coef_ai_led        = ai_led$estimate,
    se_ai_led          = ai_led$std.error,
    ci_low_ai_led      = ai_led$conf.low,
    ci_high_ai_led     = ai_led$conf.high,
    pval_ai_led        = ai_led$p.value,
    
    ymean       = ymean,
    N           = nobs(model),
    pval_joint  = get_joint_pval(model)
  )
}
panelA_res <- map2(panelA_models, dep_vars, extract_summary, df = panelA_data)
panelB_res <- map2(panelB_models, dep_vars, extract_summary, df = main)

# ---- 8. Significance stars ---------------------------------------------------
star <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .01, "***",
                ifelse(p < .05, "**",
                       ifelse(p < .10, "*", "")
                )
         )
  )
}

# ---- 9. Build LaTeX panel ----------------------------------------------------
make_panel_latex <- function(res_list, label) {
  
  fmt_num <- \(x) sprintf("%8.3f", x)
  fmt_ci  <- \(lo, hi) sprintf("[%8.3f; %8.3f]", lo, hi)
  join    <- \(v) paste(v, collapse = " & ")
  
  vars <- c("Reproduction","Minor errors","Major errors",
            "One good robustness","Two good robustness",
            "Ran one robustness","Ran two robustness")
  
  ## --- AI-Assisted lines
  coefAA <- sapply(res_list, \(x)
                   paste0(fmt_num(x$coef_ai_assisted), star(x$pval_ai_assisted)))
  seAA   <- sapply(res_list, \(x) sprintf("(%s)", fmt_num(x$se_ai_assisted)))
  ciAA   <- mapply(fmt_ci,
                   sapply(res_list, \(x) x$ci_low_ai_assisted),
                   sapply(res_list, \(x) x$ci_high_ai_assisted),
                   SIMPLIFY = TRUE)
  
  ## --- AI-Led lines
  coefAL <- sapply(res_list, \(x)
                   paste0(fmt_num(x$coef_ai_led), star(x$pval_ai_led)))
  seAL   <- sapply(res_list, \(x) sprintf("(%s)", fmt_num(x$se_ai_led)))
  ciAL   <- mapply(fmt_ci,
                   sapply(res_list, \(x) x$ci_low_ai_led),
                   sapply(res_list, \(x) x$ci_high_ai_led),
                   SIMPLIFY = TRUE)
  
  ## --- Other rows
  ymean  <- sapply(res_list, \(x) fmt_num(x$ymean))
  nobs   <- sapply(res_list, \(x) as.character(x$N))
  pjoint <- sapply(res_list, \(x) fmt_num(x$pval_joint))
  
  out <- ""
  out <- paste0(out, "\\multicolumn{8}{l}{\\textbf{", label, "}}\\\\\n")
  out <- paste0(out, "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
  out <- paste0(out, "                    & ", join(vars), " \\\\\n\\hline\n")
  out <- paste0(out, "AI-Assisted         & ", join(coefAA), " \\\\\n")
  out <- paste0(out, "                    & ", join(seAA),   " \\\\\n")
  out <- paste0(out, "                    & ", join(ciAA),   " \\\\\n")
  out <- paste0(out, "AI-Led              & ", join(coefAL), " \\\\\n")
  out <- paste0(out, "                    & ", join(seAL),   " \\\\\n")
  out <- paste0(out, "                    & ", join(ciAL),   " \\\\\n")
  out <- paste0(out, "\\hline\nControls            & ",
                paste(rep("\\checkmark", 7), collapse = " & "), " \\\\\n")
  out <- paste0(out, "Mean of dep. var    & ", join(ymean),  " \\\\\n")
  out <- paste0(out, "p-val (AI-Assisted = AI-Led) & ",
                join(pjoint), " \\\\\n")
  out <- paste0(out, "Obs.                & ", join(nobs),   " \\\\\n\\hline\n")
  out
}

# ---- 10. Print full LaTeX table ---------------------------------------------
print_full_table <- function(A, B) {
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat("\\begin{tabular}{l*{7}{c}}\n")
  cat("\\hline\\hline\n")
  cat(make_panel_latex(A, "Panel A: Study I"))
  cat("\\\\\n")
  cat(make_panel_latex(B, "Panel B: Studies I and II combined"))
  cat("\\hline\\hline\n")
  cat("\\multicolumn{8}{l}{\\it{Note:} Standard errors in parentheses; ",
      "confidence intervals in brackets. Human-only branch omitted.}\\\\\n")
  cat("\\multicolumn{8}{l}{Controls: number of teammates; ",
      "game–software, skill, and attendance fixed effects.}\\\\\n")
  cat("\\multicolumn{8}{l}{\\sym{*} $p<0.10$, \\sym{**} $p<0.05$, ",
      "\\sym{***} $p<0.01$}\\\\\n")
  cat("\\end{tabular}\n")
}

# ---- 11. Write table to file -----------------------------------------------
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
sink("output/tables/main.tex")
print_full_table(panelA_res, panelB_res)
sink()