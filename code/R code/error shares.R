########################################################################
#  MAIN SCRIPT – normalised minor / major errors (share within paper)
########################################################################
# ---------- 1. Read data ----------------------------------------------
main <- readRDS("data/AI games.rds")

# ---------- 2. Helper: normalise within paper -------------------------
normalise_errors <- function(df) {
  df %>% 
    group_by(paper) %>%                      # <— make sure `paper` exists
    mutate(
      minor_errors = minor_errors / max(minor_errors, na.rm = TRUE),
      major_errors = major_errors / max(major_errors, na.rm = TRUE)
    ) %>% 
    ungroup()
}

# ---------- 3. Prepare Panel-specific data ----------------------------
rownames(main) <- 1:nrow(main)              # keep row IDs
panelA_data <- main %>% 
  filter(game != "Virtual 2025") %>% 
  {rownames(.) <- 1:nrow(.); .}

# normalise
main        <- normalise_errors(main)
panelA_data <- normalise_errors(panelA_data)

# ---------- 4. Create FE variables & cast factors ---------------------
prep_factors <- function(df){
  df %>% 
    mutate(
      game_software = interaction(game, software, drop = TRUE),
      across(c(max_skill, min_skill, max_gpt, min_gpt,
               attendance, branch), as.factor)
    )
}
main        <- prep_factors(main)
panelA_data <- prep_factors(panelA_data)

# ---------- 5. Model lists --------------------------------------------
dep_vars <- c("minor_errors", "major_errors")      # ONLY cols 2 & 3 now

panelA_models <- map(dep_vars, ~
                       feols(
                         as.formula(paste(.x,
                                          "~ branch + number_teammates | game_software +",
                                          "max_skill + min_skill + attendance")),
                         data = panelA_data,
                         vcov = "hetero"
                       )
)

panelB_models <- map(dep_vars, ~
                       feols(
                         as.formula(paste(.x,
                                          "~ branch + number_teammates | game_software +",
                                          "max_skill + min_skill + attendance")),
                         data = main,
                         vcov = "hetero"
                       )
)

# ---------- 6. Utilities ----------------------------------------------
get_pval <- function(model){
  lh <- tryCatch(
    linearHypothesis(model,
                     "branchAI-Assisted = branchAI-Led",
                     vcov. = vcov(model, type = "HC1")),
    error = function(e) NA
  )
  if (is.data.frame(lh) && nrow(lh) >= 2 && 
      "Pr(>Chisq)" %in% names(lh)) as.numeric(lh[2,"Pr(>Chisq)"]) else NA
}

extract_summary <- function(model, dv, df){
  ctab   <- broom::tidy(model, conf.int = TRUE)
  aa_row <- ctab[ctab$term == "branchAI-Assisted", ]
  al_row <- ctab[ctab$term == "branchAI-Led", ]
  used   <- which(!is.na(model$fitted.values))
  tibble(
    coef_ai_assisted = aa_row$estimate %||% NA,
    se_ai_assisted   = aa_row$std.error %||% NA,
    ci_low_ai_assisted  = aa_row$conf.low %||% NA,
    ci_high_ai_assisted = aa_row$conf.high %||% NA,
    coef_ai_led      = al_row$estimate %||% NA,
    se_ai_led        = al_row$std.error %||% NA,
    ci_low_ai_led    = al_row$conf.low %||% NA,
    ci_high_ai_led   = al_row$conf.high %||% NA,
    ymean  = mean(df[[dv]][used], na.rm = TRUE),
    N      = nobs(model),
    pval   = get_pval(model)
  )
}

# ---------- 7. Collect results ----------------------------------------
`%||%` <- function(a,b) if(length(a)) a else b  # null-coalesce helper

panelA_res <- map2(panelA_models, dep_vars,
                   ~extract_summary(.x, .y, panelA_data))
panelB_res <- map2(panelB_models, dep_vars,
                   ~extract_summary(.x, .y, main))

# ---------- 8. LaTeX table generator (2 columns) ----------------------
get_stars <- function(p) ifelse(is.na(p),"",
                                ifelse(p<0.01,"***", ifelse(p<0.05,"**", ifelse(p<0.1,"*",""))))

make_panel_latex <- function(res, label){
  co  <- lapply(res, function(x) sprintf("%8.3f%s",
                                         x$coef_ai_assisted, get_stars(x$pval)))
  se  <- lapply(res, function(x) sprintf("(%8.3f)", x$se_ai_assisted))
  ci  <- lapply(res, function(x) sprintf("[%8.3f; %8.3f]",
                                         x$ci_low_ai_assisted, x$ci_high_ai_assisted))
  co2 <- lapply(res, function(x) sprintf("%8.3f%s",
                                         x$coef_ai_led, get_stars(x$pval)))
  se2 <- lapply(res, function(x) sprintf("(%8.3f)", x$se_ai_led))
  ci2 <- lapply(res, function(x) sprintf("[%8.3f; %8.3f]",
                                         x$ci_low_ai_led, x$ci_high_ai_led))
  y   <- lapply(res, function(x) sprintf("%8.3f", x$ymean))
  p   <- lapply(res, function(x) ifelse(is.na(x$pval),"",
                                        sprintf("%8.3f", x$pval)))
  n   <- lapply(res, function(x) as.character(x$N))
  
  out <- ""
  out <- paste0(out,
                "\\multicolumn{3}{l}{\\textbf{", label, "}}\\\\\n",
                "& (1) & (2) \\\\\n",
                "                    & Minor errors & Major errors \\\\\n\\hline\n",
                "AI-Assisted         & ", paste(co , collapse=" & "), " \\\\\n",
                "                    & ", paste(se , collapse=" & "), " \\\\\n",
                "                    & ", paste(ci , collapse=" & "), " \\\\\n",
                "AI-Led              & ", paste(co2, collapse=" & "), " \\\\\n",
                "                    & ", paste(se2, collapse=" & "), " \\\\\n",
                "                    & ", paste(ci2, collapse=" & "), " \\\\\n",
                "\\hline\nControls   & \\checkmark & \\checkmark \\\\\n",
                "Mean dep. var       & ", paste(y , collapse=" & "), " \\\\\n",
                "p-val (AI-Assisted vs. AI-Led)    & ", paste(p , collapse=" & "), " \\\\\n",
                "Obs.                & ", paste(n , collapse=" & "), " \\\\\n\\hline\n")
  out
}

print_full_latex_table <- function(resA,resB){
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n",
      "\\begin{tabular}{lcc}\n",
      "\\hline\\hline\n", sep="")
  cat(make_panel_latex(resA, "Panel A: Study I"))
  cat("\\\\\n")
  cat(make_panel_latex(resB, "Panel B: Studies I and II combined"))
  cat("\\hline\\hline\n",
      "\\multicolumn{3}{l}{\\it{Note:} Standard errors in",
      " parentheses, confidence intervals in brackets; human-only group omitted.}\\\\\n",
      "\\multicolumn{3}{l}{Controls include number of teammates; game-by-software fixed effects; maximum and minimum }\\\\\n",
      "\\multicolumn{3}{l}{position skill fixed effects; attendance fixed effects.}\\\\\n",
      "\\multicolumn{3}{l}{\\sym{*} $p<0.1$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$}\\\\\n",
      "\\end{tabular}\n", sep=" ")
}

# ---------- 9. Save LaTeX ---------------------------------------------
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
sink("output/tables/error shares.tex")
print_full_latex_table(panelA_res, panelB_res)
sink()