main <- readRDS("data/AI games.rds")
main <- main %>%
  mutate(
    game_software = interaction(game, software, drop = TRUE),
    max_skill = as.factor(max_skill),
    min_skill = as.factor(min_skill),
    max_gpt   = as.factor(max_gpt),
    min_gpt   = as.factor(min_gpt),
    attendance = as.factor(attendance),
    branch = as.factor(branch)
  )

panelA_data <- main %>% filter(game != "Virtual 2025")

dep_vars_poisson <- c("minor_errors", "major_errors")
dep_vars_logit   <- c("reproduction", "two_good_robustness", "ran_one_robustness", "ran_two_robustness")
all_vars <- c("reproduction", "minor_errors", "major_errors", "two_good_robustness", "ran_one_robustness", "ran_two_robustness")

drop_separation_groups <- function(df, dv, group_vars) {
  df$group_id <- interaction(df[, group_vars], drop = TRUE)
  group_summary <- df %>%
    group_by(group_id) %>%
    summarise(
      min_y = min(.data[[dv]], na.rm = TRUE),
      max_y = max(.data[[dv]], na.rm = TRUE)
    )
  problem_groups <- group_summary %>% filter(min_y == max_y) %>% pull(group_id)
  df <- df %>% filter(!(group_id %in% problem_groups))
  df$group_id <- NULL
  return(df)
}

# NEW: Helper function to drop single-level FE variables
drop_single_level <- function(df, vars) {
  for (v in vars) {
    if (v %in% names(df) && length(unique(na.omit(df[[v]]))) <= 1) {
      df[[v]] <- NULL
    }
  }
  return(df)
}

panelA_models <- setNames(vector("list", length(all_vars)), all_vars)
panelA_mfx    <- setNames(vector("list", length(all_vars)), all_vars)
panelA_types  <- setNames(rep(NA, length(all_vars)), all_vars)

for (dv in all_vars) {
  if (dv %in% dep_vars_poisson) {
    # POISSON with absorbed FE (as before)
    mod <- fepois(
      as.formula(paste0(dv, " ~ branch + number_teammates | game_software + max_skill + min_skill + attendance")),
      data = panelA_data,
      vcov = "hetero"
    )
    panelA_models[[dv]] <- mod
    panelA_types[dv]    <- "Poisson"
    panelA_mfx[[dv]]    <- NA
  } else {
    mod <- glm(
      as.formula(paste0(
        dv, " ~ branch + number_teammates + max_skill + min_skill + attendance + game_software"
      )),
      data = panelA_data,
      family = binomial("logit")
    )
    panelA_models[[dv]] <- mod
    panelA_types[dv]    <- "Logit"
    mfx <- tryCatch(
      margins(mod, variables = c("branchAI-Assisted", "branchAI-Led")),
      error = function(e) NA
    )
    panelA_mfx[[dv]] <- mfx
  }
}


panelB_models <- setNames(vector("list", length(all_vars)), all_vars)
panelB_mfx    <- setNames(vector("list", length(all_vars)), all_vars)
panelB_types  <- setNames(rep(NA, length(all_vars)), all_vars)

for (dv in all_vars) {
  if (dv %in% dep_vars_poisson) {
    # POISSON with absorbed FE
    mod <- fepois(
      as.formula(paste0(dv, " ~ branch + number_teammates | game_software + max_skill + min_skill + attendance")),
      data = main,
      vcov = "hetero"
    )
    panelB_models[[dv]] <- mod
    panelB_types[dv]    <- "Poisson"
    panelB_mfx[[dv]]    <- NA
  } else {
    # LOGIT with FEs as dummies
    mod <- glm(
      as.formula(paste0(
        dv, " ~ branch + number_teammates + max_skill + min_skill + attendance + game_software"
      )),
      data = main,
      family = binomial("logit")
    )
    panelB_models[[dv]] <- mod
    panelB_types[dv]    <- "Logit"
    mfx <- tryCatch(
      margins(mod, variables = c("branchAI-Assisted", "branchAI-Led")),
      error = function(e) NA
    )
    panelB_mfx[[dv]] <- mfx
  }
}


get_stars <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < 0.01) return("***")
  if (pval < 0.05) return("**")
  if (pval < 0.1)  return("*")
  return("")
}


extract_poilog_summary <- function(model, mfx, dv, type, df, panel = "A") {
  if (type == "Poisson") {
    coefs <- broom::tidy(model, conf.int = TRUE)
    ai_assisted <- coefs[coefs$term == "branchAI-Assisted", ]
    ai_led      <- coefs[coefs$term == "branchAI-Led", ]
    pval_aa <- if (nrow(ai_assisted)) ai_assisted$p.value else NA
    pval_al <- if (nrow(ai_led)) ai_led$p.value else NA
    pval <- tryCatch({
      lh <- car::linearHypothesis(model, "branchAI-Assisted = branchAI-Led", vcov. = vcov(model, type = "HC1"))
      if ("Pr(>Chisq)" %in% colnames(lh) && nrow(lh) >= 2) as.numeric(lh[2, "Pr(>Chisq)"]) else NA
    }, error = function(e) NA)
    used_rows <- which(!is.na(model$fitted.values))
    ymean <- mean(as.numeric(df[[dv]][used_rows]), na.rm = TRUE)
    nobs <- nobs(model)
    list(
      model = "Poisson",
      coef_aa = if (nrow(ai_assisted)) ai_assisted$estimate else NA,
      se_aa = if (nrow(ai_assisted)) ai_assisted$std.error else NA,
      ci_aa = if (nrow(ai_assisted)) c(ai_assisted$conf.low, ai_assisted$conf.high) else c(NA, NA),
      pval_aa = pval_aa,
      coef_al = if (nrow(ai_led)) ai_led$estimate else NA,
      se_al = if (nrow(ai_led)) ai_led$std.error else NA,
      ci_al = if (nrow(ai_led)) c(ai_led$conf.low, ai_led$conf.high) else c(NA, NA),
      pval_al = pval_al,
      ymean = ymean,
      N = nobs,
      pval = pval,  # difference between branches
      controls = "\\checkmark"
    )
  } else {
    # Logit (glm with robust SEs)
    robust_vcov <- sandwich::vcovHC(model, type = "HC1")
    robust_coefs <- lmtest::coeftest(model, vcov = robust_vcov)
    coefnames <- rownames(robust_coefs)
    ai_assisted_idx <- which(coefnames == "branchAI-Assisted")
    ai_led_idx      <- which(coefnames == "branchAI-Led")
    crit <- qnorm(0.975)
    coef_aa <- if (length(ai_assisted_idx) == 1) robust_coefs[ai_assisted_idx, 1] else NA
    se_aa   <- if (length(ai_assisted_idx) == 1) robust_coefs[ai_assisted_idx, 2] else NA
    ci_aa   <- if (length(ai_assisted_idx) == 1) coef_aa + c(-1, 1) * crit * se_aa else c(NA, NA)
    pval_aa <- if (length(ai_assisted_idx) == 1) robust_coefs[ai_assisted_idx, 4] else NA
    coef_al <- if (length(ai_led_idx) == 1) robust_coefs[ai_led_idx, 1] else NA
    se_al   <- if (length(ai_led_idx) == 1) robust_coefs[ai_led_idx, 2] else NA
    ci_al   <- if (length(ai_led_idx) == 1) coef_al + c(-1, 1) * crit * se_al else c(NA, NA)
    pval_al <- if (length(ai_led_idx) == 1) robust_coefs[ai_led_idx, 4] else NA
    pval <- tryCatch({
      lh <- car::linearHypothesis(model, "branchAI-Assisted = branchAI-Led", vcov. = robust_vcov)
      if ("Pr(>Chisq)" %in% colnames(lh) && nrow(lh) >= 2) as.numeric(lh[2, "Pr(>Chisq)"]) else NA
    }, error = function(e) NA)
    used_rows <- which(!is.na(model$fitted.values))
    ymean <- mean(as.numeric(df[[dv]][used_rows]), na.rm = TRUE)
    nobs <- nobs(model)
    list(
      model = "Logit",
      coef_aa = coef_aa,
      se_aa = se_aa,
      ci_aa = ci_aa,
      pval_aa = pval_aa,
      coef_al = coef_al,
      se_al = se_al,
      ci_al = ci_al,
      pval_al = pval_al,
      ymean = ymean,
      N = nobs,
      pval = pval,
      controls = "\\checkmark"
    )
  }
}


panelA_summary <- mapply(
  extract_poilog_summary,
  panelA_models[all_vars], panelA_mfx[all_vars], all_vars,
  panelA_types[all_vars],             # <--- type goes here, one for each model
  MoreArgs = list(df = panelA_data),
  SIMPLIFY = FALSE
)


panelB_summary <- mapply(
  extract_poilog_summary,
  panelB_models[all_vars], panelB_mfx[all_vars], all_vars,
  panelB_types[all_vars],             # <--- type goes here, one for each model
  MoreArgs = list(df = main),
  SIMPLIFY = FALSE
)


make_panel_latex_poilog <- function(panel_res, panel_label) {
  vars <- c(
    "Reproduction", 
    "\\shortstack[c]{Minor\\\\errors}", 
    "\\shortstack[c]{Major\\\\errors}", 
    "\\shortstack[c]{Two good\\\\robustness}", 
    "\\shortstack[c]{Ran one\\\\robustness}", 
    "\\shortstack[c]{Ran two\\\\robustness}"
  )
  indices <- 1:6 # For only the first 6 variables
  
  coefs_aa <- sapply(panel_res[indices], function(x) sprintf("%15.3f%s", x$coef_aa, get_stars(x$pval_aa)))
  ses_aa   <- sapply(panel_res[indices], function(x) sprintf("%15.3f", x$se_aa))
  ci_aa    <- sapply(panel_res[indices], function(x) sprintf("[%15.3f; %15.3f]", x$ci_aa[1], x$ci_aa[2]))
  coefs_al <- sapply(panel_res[indices], function(x) sprintf("%15.3f%s", x$coef_al, get_stars(x$pval_al)))
  ses_al   <- sapply(panel_res[indices], function(x) sprintf("%15.3f", x$se_al))
  ci_al    <- sapply(panel_res[indices], function(x) sprintf("[%15.3f; %15.3f]", x$ci_al[1], x$ci_al[2]))
  ymean    <- sapply(panel_res[indices], function(x) sprintf("%9.3f", x$ymean))
  pvals    <- sapply(panel_res[indices], function(x) ifelse(is.na(x$pval), "", sprintf("%9.3f", x$pval)))
  nobs     <- sapply(panel_res[indices], function(x) as.character(x$N))
  models   <- sapply(panel_res[indices], function(x) x$model)
  controls <- sapply(panel_res[indices], function(x) x$controls)
  
  out <- ""
  out <- paste0(out, "\\multicolumn{7}{l}{\\textbf{", panel_label, "}}\\\\\n")
  out <- paste0(out, "& (1) & (2) & (3) & (4) & (5) & (6)\\\\\n")
  out <- paste0(out, "                    &", paste(vars, collapse = "   &"), "   \\\\\n\\hline\n")
  out <- paste0(out, "AI-Assisted         &", paste(coefs_aa, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(sprintf("(%s)", ses_aa), collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(ci_aa, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "AI-Led              &", paste(coefs_al, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(sprintf("(%s)", ses_al), collapse="   &"), "   \\\\\n")
  out <- paste0(out, "                    &", paste(ci_al, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "\\hline\n")
  out <- paste0(out, "Model               &", paste(models, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "Controls            &", paste(controls, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "Mean of dep. var    &", paste(ymean, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "p-val (AI-Assisted vs. AI-Led)&", paste(pvals, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "Obs.                &", paste(nobs, collapse="   &"), "   \\\\\n\\hline\n")
  return(out)
}


print_full_latex_table_poilog <- function(panelA_res, panelB_res) {
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat("\\begin{tabular}{l*{6}{c}}\n")
  cat("\\hline\\hline\n")
  cat(make_panel_latex_poilog(panelA_res, "Panel A: Study I"))
  cat("\\\\\n")
  cat(make_panel_latex_poilog(panelB_res, "Panel B: Studies I and II combined"))
  cat("\\hline\\hline\n")
  cat("\\multicolumn{7}{p{0.8\\textwidth}}{\\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only group omitted. The model for One good robustness is not included due to insufficient observations, preventing it from converging. Marginal effects reported for Logit models.}\\\\\n")
  cat("\\multicolumn{7}{l}{Controls include number of teammates; game--software, skill, and attendance fixed effects.}\\\\\n")
  cat("\\multicolumn{7}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
  cat("\\end{tabular}\n")
}

# panelA_summary and panelB_summary as above
print_full_latex_table_poilog(panelA_summary, panelB_summary)

sink("output/tables/logit poisson.tex")
print_full_latex_table_poilog(panelA_summary, panelB_summary)
sink()