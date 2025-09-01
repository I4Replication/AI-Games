# S1-only Logit/Poisson main table, outputs to output/S1/tables/logit poisson.tex

library(dplyr)
library(fixest)
library(broom)
library(car)
library(margins)

main <- readRDS("data/AI games.rds")
panelA_data <- main %>% filter(game != "Virtual 2025") %>%
  mutate(
    game_software = interaction(game, software, drop = TRUE),
    max_skill = as.factor(max_skill),
    min_skill = as.factor(min_skill),
    max_gpt   = as.factor(max_gpt),
    min_gpt   = as.factor(min_gpt),
    attendance = as.factor(attendance),
    branch = as.factor(branch)
  )

dep_vars_poisson <- c("minor_errors", "major_errors")
dep_vars_logit   <- c("reproduction", "two_good_robustness", "ran_one_robustness", "ran_two_robustness")
all_vars <- c("reproduction", "minor_errors", "major_errors", "two_good_robustness", "ran_one_robustness", "ran_two_robustness")

panelA_models <- setNames(vector("list", length(all_vars)), all_vars)
panelA_mfx    <- setNames(vector("list", length(all_vars)), all_vars)
panelA_types  <- setNames(rep(NA, length(all_vars)), all_vars)

for (dv in all_vars) {
  if (dv %in% dep_vars_poisson) {
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

get_stars <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < 0.01) return("***")
  if (pval < 0.05) return("**")
  if (pval < 0.1)  return("*")
  return("")
}

extract_poilog_summary <- function(model, mfx, dv, type, df) {
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
      nobs = nobs,
      pval_joint = pval
    )
  } else {
    # Logit: try marginal effects; if unavailable, fall back to coefficients with robust SEs
    coefs <- broom::tidy(model, conf.int = FALSE)
    rob_vcov <- sandwich::vcovHC(model, type = "HC1")
    se_robust <- sqrt(diag(rob_vcov))
    get_me <- function(v) {
      if (is.list(mfx) && !is.null(mfx) && !all(is.na(mfx))) {
        tryCatch({
          est <- subset(mfx, factor == v)$AME
          se  <- subset(mfx, factor == v)$SE
          c(est[1], se[1])
        }, error = function(e) c(NA, NA))
      } else c(NA, NA)
    }
    me_aa <- get_me("branchAI-Assisted")
    me_al <- get_me("branchAI-Led")
    # fallback to coefficients if margins failed
    if (is.na(me_aa[0+1])) {
      idx <- which(coefs$term == "branchAI-Assisted")
      me_aa <- c(if (length(idx)) coefs$estimate[idx] else NA_real_, if (length(idx)) se_robust[idx] else NA_real_)
    }
    if (is.na(me_al[0+1])) {
      idx <- which(coefs$term == "branchAI-Led")
      me_al <- c(if (length(idx)) coefs$estimate[idx] else NA_real_, if (length(idx)) se_robust[idx] else NA_real_)
    }
    # compute 95% CIs for logit outputs (AME or fallback coef) when SE available
    ci_aa <- c(NA_real_, NA_real_)
    ci_al <- c(NA_real_, NA_real_)
    if (!is.na(me_aa[2])) ci_aa <- c(me_aa[1] - 1.96*me_aa[2], me_aa[1] + 1.96*me_aa[2])
    if (!is.na(me_al[2])) ci_al <- c(me_al[1] - 1.96*me_al[2], me_al[1] + 1.96*me_al[2])
    # Manual robust Wald p-value for H0: beta_AA = beta_AL
    b <- coef(model)
    if (all(c("branchAI-Assisted","branchAI-Led") %in% names(b))) {
      C <- rep(0, length(b)); names(C) <- names(b)
      C["branchAI-Assisted"] <- 1; C["branchAI-Led"] <- -1
      diff <- sum(C * b)
      var_diff <- as.numeric(t(C) %*% rob_vcov %*% C)
      pval <- if (!is.na(var_diff) && var_diff > 0) pchisq((diff^2)/var_diff, df = 1, lower.tail = FALSE) else NA_real_
    } else {
      pval <- NA_real_
    }
    ymean <- mean(as.numeric(panelA_data[[dv]]), na.rm = TRUE)
    nobs  <- nobs(model)
    list(
      model = "Logit",
      coef_aa = me_aa[1], se_aa = me_aa[2], ci_aa = ci_aa, pval_aa = NA,
      coef_al = me_al[1], se_al = me_al[2], ci_al = ci_al, pval_al = NA,
      ymean = ymean, nobs = nobs, pval_joint = pval
    )
  }
}

summaries <- lapply(names(panelA_models), function(nm) {
  extract_poilog_summary(panelA_models[[nm]], panelA_mfx[[nm]], nm, panelA_types[[nm]], panelA_data)
})

fmt <- function(x) ifelse(is.na(x), "", sprintf("%8.3f", x))
fmt_ci <- function(ci) ifelse(any(is.na(ci)), "", sprintf("[%8.3f; %8.3f]", ci[1], ci[2]))

rows <- function(field) sapply(summaries, function(s) fmt(s[[field]]))
rows_ci <- function(field) sapply(summaries, function(s) fmt_ci(s[[field]]))

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)
sink("output/S1/tables/logit poisson.tex")
br <- " \\\\\n+"
cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
cat("\\begin{tabular}{l*{6}{c}}\n\\hline\\hline\n")
cat("& (1) & (2) & (3) & (4) & (5) & (6)\\\\\n")
cat("                    &Reproduction   &\\shortstack[c]{Minor\\\\errors}   &\\shortstack[c]{Major\\\\errors}   &\\shortstack[c]{Two good\\\\robustness}   &\\shortstack[c]{Ran one\\\\robustness}   &\\shortstack[c]{Ran two\\\\robustness}   ", br, sep = "")
cat("\\hline\n")
cat("AI-Assisted         & ", paste0(rows("coef_aa"), collapse = " & "), br, sep = "")
cat("                    & ", paste0(ifelse(rows("se_aa")=="","", paste0("(", rows("se_aa"), ")")), collapse = " & "), br, sep = "")
cat("                    & ", paste0(rows_ci("ci_aa"), collapse = " & "), br, sep = "")
cat("AI-Led              & ", paste0(rows("coef_al"), collapse = " & "), br, sep = "")
cat("                    & ", paste0(ifelse(rows("se_al")=="","", paste0("(", rows("se_al"), ")")), collapse = " & "), br, sep = "")
cat("                    & ", paste0(rows_ci("ci_al"), collapse = " & "), br, sep = "")
cat("\\hline\n")
model_row <- ifelse(names(panelA_models) %in% dep_vars_poisson, "Poisson", "Logit")
cat("Model               & ", paste0(model_row, collapse = "   &"), "   ", br, sep = "")
cat("Controls            & ", paste(rep("\\checkmark", 6), collapse = " & "), br, sep = "")
cat("Mean of dep. var    & ", paste0(sapply(summaries, function(s) fmt(s$ymean)), collapse = " & "), br, sep = "")
cat("p-val (AI-Assisted vs. AI-Led)& ", paste0(sapply(summaries, function(s) fmt(s$pval_joint)), collapse = " & "), br, sep = "")
cat("Obs.                & ", paste0(sapply(summaries, function(s) s$nobs), collapse = "   &"), "   ", br, sep = "")
cat("\\hline\n\\hline\n")
cat("\\multicolumn{7}{p{0.8\\textwidth}}{\\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only group omitted. Marginal effects reported for Logit models.}\\\\\n")
cat("\\multicolumn{7}{l}{Controls include number of teammates; game-by-software fixed effects; maximum and minimum position skill fixed effects; attendance fixed effects.}\\\\\n")
cat("\\multicolumn{7}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
cat("\\end{tabular}\n")
sink()

# Sanitize: remove any accidental leading '+' characters per line
out_path <- "output/S1/tables/logit poisson.tex"
txt <- readLines(out_path, warn = FALSE)
txt <- sub("^\\+", "", txt)
writeLines(txt, out_path)
