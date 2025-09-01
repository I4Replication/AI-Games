# S1-only: softwares interaction table, Study I only (no panel label)

library(dplyr)
library(fixest)
library(broom)

df <- readRDS("data/AI games.rds") %>%
  filter(game != "Virtual 2025") %>%
  mutate(
    game        = as.factor(game),
    software    = as.factor(software),
    max_skill   = as.factor(max_skill),
    min_skill   = as.factor(min_skill),
    attendance  = as.factor(attendance),
    branch      = as.factor(branch)
  )

dep_vars <- c(
  "reproduction", "minor_errors", "major_errors",
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)
df <- df %>% mutate(across(all_of(dep_vars), as.numeric))

models <- lapply(dep_vars, function(dv) feols(
  as.formula(paste(dv, "~ branch*software + number_teammates |",
                   "game + max_skill + min_skill + attendance")),
  data = df, vcov = "hetero"))

summarise_model <- function(m, dv) {
  td <- broom::tidy(m, conf.int = TRUE)
  get_row <- function(term) {
    x <- td[td$term == term, ]
    list(
      estimate = if (nrow(x)) x$estimate else NA,
      se       = if (nrow(x)) x$std.error else NA,
      lo       = if (nrow(x)) x$conf.low else NA,
      hi       = if (nrow(x)) x$conf.high else NA
    )
  }
  ymean <- mean(df[[dv]], na.rm = TRUE)
  tibble(
    b_aa     = get_row("branchAI-Assisted")$estimate,
    se_aa    = get_row("branchAI-Assisted")$se,
    lo_aa    = get_row("branchAI-Assisted")$lo,
    hi_aa    = get_row("branchAI-Assisted")$hi,
    b_al     = get_row("branchAI-Led")$estimate,
    se_al    = get_row("branchAI-Led")$se,
    lo_al    = get_row("branchAI-Led")$lo,
    hi_al    = get_row("branchAI-Led")$hi,
    b_R      = get_row("softwareR")$estimate,
    se_R     = get_row("softwareR")$se,
    b_int_aa = get_row("branchAI-Assisted:softwareR")$estimate,
    se_int_aa= get_row("branchAI-Assisted:softwareR")$se,
    b_int_al = get_row("branchAI-Led:softwareR")$estimate,
    se_int_al= get_row("branchAI-Led:softwareR")$se,
    ymean = ymean, N = nobs(m)
  )
}

res <- Map(summarise_model, models, dep_vars)

fmt <- \(x) sprintf("% .3f", x)

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)
sink("output/S1/tables/softwares.tex")
cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
cat("\\begin{tabular}{l*{7}{c}}\n\\hline\\hline\n")
cat("& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
cat("                    &Reproduction   &\\shortstack[c]{Minor\\\\errors}   &\\shortstack[c]{Major\\\\errors}   &\\shortstack[c]{One good\\\\robustness}   &\\shortstack[c]{Two good\\\\robustness}   &\\shortstack[c]{Ran one\\\\robustness}   &\\shortstack[c]{Ran two\\\\robustness}   \\\\\n+\\hline\n")
cat("AI-Assisted         & ", paste(sapply(res, \(x) fmt(x$b_aa)), collapse = " & "), " \\\n+")
cat("                    & ", paste(sapply(res, \(x) sprintf("(%s)", fmt(x$se_aa))), collapse = " & "), " \\\n+")
cat("AI-Led              & ", paste(sapply(res, \(x) fmt(x$b_al)), collapse = " & "), " \\\n+")
cat("                    & ", paste(sapply(res, \(x) sprintf("(%s)", fmt(x$se_al))), collapse = " & "), " \\\n+")
cat("R                   & ", paste(sapply(res, \(x) fmt(x$b_R)), collapse = " & "), " \\\n+")
cat("AI-Assisted $\\times$ R & ", paste(sapply(res, \(x) fmt(x$b_int_aa)), collapse = " & "), " \\\n+")
cat("                    & ", paste(sapply(res, \(x) sprintf("(%s)", fmt(x$se_int_aa))), collapse = " & "), " \\\n+")
cat("AI-Led $\\times$ R  & ", paste(sapply(res, \(x) fmt(x$b_int_al)), collapse = " & "), " \\\n+")
cat("                    & ", paste(sapply(res, \(x) sprintf("(%s)", fmt(x$se_int_al))), collapse = " & "), " \\\n+")
cat("\\hline\n")
cat("Controls            & ", paste(rep("\\checkmark", 7), collapse = " & "), " \\\n+")
cat("Mean of dep. var    & ", paste(sapply(res, \(x) fmt(x$ymean)), collapse = " & "), " \\\n+")
cat("Obs.                & ", paste(sapply(res, \(x) x$N), collapse = " & "), " \\\n+")
cat("\\hline\n\\hline\n")
cat("\\multicolumn{8}{l}{\\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only group omitted; Stata papers omitted.}\\\\\n")
cat("\\multicolumn{8}{l}{Controls include number of teammates; game and software fixed effects; maximum and minimum position skill fixed effects; attendance fixed effects.}\\\\\n")
cat("\\multicolumn{8}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
cat("\\end{tabular}\n")
sink()

# Sanitize: remove any accidental leading '+' characters and ensure double \\\\\n+out_path <- "output/S1/tables/softwares.tex"
txt <- readLines(out_path, warn = FALSE)
txt <- sub("^\\+", "", txt)
txt <- sub("(?<!\\\\)\\\\$", "\\\\\\\\", txt, perl = TRUE)
writeLines(txt, out_path)
