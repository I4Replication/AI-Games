# S1-only main table (OLS), outputs to output/S1/tables/main.tex

library(dplyr)
library(fixest)
library(purrr)
library(broom)
library(tibble)

main <- readRDS("data/AI games.rds")

# Keep Study I only
panelA_data <- main %>% filter(game != "Virtual 2025")

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
panelA_data <- factorise_vars(panelA_data)

dep_vars <- c(
  "reproduction", "minor_errors", "major_errors",
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)
panelA_data <- panelA_data %>% mutate(across(all_of(dep_vars), as.numeric))

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

get_joint_pval <- function(model) {
  # Wald test for H0: beta_AA = beta_AL using robust vcov
  b <- coef(model)
  V <- stats::vcov(model)  # already robust per feols(vcov="hetero")
  if (!all(c("branchAI-Assisted","branchAI-Led") %in% names(b))) return(NA_real_)
  C <- rep(0, length(b)); names(C) <- names(b)
  C["branchAI-Assisted"] <- 1
  C["branchAI-Led"] <- -1
  diff <- sum(C * b)
  var_diff <- as.numeric(t(C) %*% V %*% C)
  if (is.na(var_diff) || var_diff <= 0) return(NA_real_)
  W <- (diff^2) / var_diff
  pchisq(W, df = 1, lower.tail = FALSE)
}

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

star <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .01, "***",
                ifelse(p < .05, "**",
                       ifelse(p < .10, "*", "")
                )
         )
  )
}

fmt_num <- \(x) sprintf("%8.3f", x)
fmt_ci  <- \(lo, hi) sprintf("[%8.3f; %8.3f]", lo, hi)
join    <- \(v) paste(v, collapse = " & ")
vars <- c("Reproduction","Minor errors","Major errors",
          "One good robustness","Two good robustness",
          "Ran one robustness","Ran two robustness")

coefAA <- sapply(panelA_res, \(x)
                 paste0(fmt_num(x$coef_ai_assisted), star(x$pval_ai_assisted)))
seAA   <- sapply(panelA_res, \(x) sprintf("(%s)", fmt_num(x$se_ai_assisted)))
ciAA   <- mapply(fmt_ci,
                 sapply(panelA_res, \(x) x$ci_low_ai_assisted),
                 sapply(panelA_res, \(x) x$ci_high_ai_assisted),
                 SIMPLIFY = TRUE)
coefAL <- sapply(panelA_res, \(x)
                 paste0(fmt_num(x$coef_ai_led), star(x$pval_ai_led)))
seAL   <- sapply(panelA_res, \(x) sprintf("(%s)", fmt_num(x$se_ai_led)))
ciAL   <- mapply(fmt_ci,
                 sapply(panelA_res, \(x) x$ci_low_ai_led),
                 sapply(panelA_res, \(x) x$ci_high_ai_led),
                 SIMPLIFY = TRUE)
ymean  <- sapply(panelA_res, \(x) fmt_num(x$ymean))
nobs   <- sapply(panelA_res, \(x) as.character(x$N))
pjoint <- sapply(panelA_res, \(x) fmt_num(x$pval_joint))

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)

sink("output/S1/tables/main.tex")
cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
cat("\\begin{tabular}{l*{7}{c}}\n")
cat("\\hline\\hline\n")
cat("& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
cat("                    & ", join(vars), " \\\\\n", sep = "")
cat("\\hline\n")
cat("AI-Assisted         & ", join(coefAA), " \\\\\n", sep = "")
cat("                    & ", join(seAA),   " \\\\\n", sep = "")
cat("                    & ", join(ciAA),   " \\\\\n", sep = "")
cat("AI-Led              & ", join(coefAL), " \\\\\n", sep = "")
cat("                    & ", join(seAL),   " \\\\\n", sep = "")
cat("                    & ", join(ciAL),   " \\\\\n", sep = "")
cat("\\hline\n")
cat("Controls            & ", paste(rep("\\checkmark", 7), collapse = " & "), " \\\\\n", sep = "")
cat("Mean of dep. var    & ", join(ymean),  " \\\\\n", sep = "")
cat("p-val (AI-Assisted = AI-Led) & ", join(pjoint), " \\\\\n", sep = "")
cat("Obs.                & ", join(nobs),   " \\\\\n", sep = "")
cat("\\hline\n\\hline\n")
cat("\\multicolumn{8}{l}{\\it{Note:} Standard errors in parentheses; confidence intervals in brackets. Human-only group omitted.}\\\\\n")
cat("\\multicolumn{8}{l}{Controls: number of teammates; game-by-software fixed effects; maximum and minimum position skill fixed effects; attendance fixed effects.}\\\\\n")
cat("\\multicolumn{8}{l}{\\sym{*} $p<0.10$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$}\\\\\n")
cat("\\end{tabular}\n")
sink()
