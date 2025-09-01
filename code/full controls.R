# S1-only: full controls table listing all coefficients per DV (OLS), Study I only

library(dplyr)
library(broom)
library(sandwich)

df <- readRDS("data/AI games.rds") %>% filter(game != "Virtual 2025")

dep_vars <- c(
  "reproduction", "minor_errors", "major_errors",
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)

clean_term <- function(term) {
  term <- gsub("`", "", term)
  term <- gsub(":", " $\\\\times$ ", term)
  term <- gsub("branch",        "Branch: ", term)
  term <- gsub("number_teammates", "Number of teammates", term)
  term <- gsub("game",          "Game: ", term)
  term <- gsub("software",      "Software: ", term)
  term <- gsub("max_skill",     "Maximum academic level: ", term)
  term <- gsub("min_skill",     "Minimum academic level: ", term)
  term <- gsub("attendance",    "Attendance: ", term)
  term <- gsub("\\.\\.\\.", "", term)
  term <- trimws(term)
  term
}

fit_and_extract <- function(dv) {
  f <- as.formula(paste0(
    dv, " ~ branch + number_teammates + game*software + max_skill + min_skill + attendance"
  ))
  mod <- lm(f, data = df)
  tt <- broom::tidy(mod)
  # robust SE and CIs
  V <- sandwich::vcovHC(mod, type = "HC1")
  se <- sqrt(diag(V))
  tt$std.error <- se[match(tt$term, names(se))]
  tt$conf.low  <- tt$estimate - 1.96*tt$std.error
  tt$conf.high <- tt$estimate + 1.96*tt$std.error
  tt$latex_term <- sapply(tt$term, clean_term)
  list(table = tt, ymean = mean(df[[dv]], na.rm = TRUE), N = nobs(mod))
}

tables <- lapply(dep_vars, fit_and_extract)

all_terms <- Reduce(union, lapply(tables, function(x) x$table$latex_term))
all_terms <- setdiff(all_terms, "(Intercept)")

symstar <- function(est, p){
  if (is.na(p)) return(est)
  if (p < .01) return(paste0(est, "***"))
  if (p < .05) return(paste0(est, "**"))
  if (p < .10) return(paste0(est, "*"))
  est
}

fmt <- function(x) ifelse(is.na(x), "", sprintf("% .3f", x))
fmt_ci <- function(lo, hi) ifelse(is.na(lo) | is.na(hi), "", sprintf("[% .3f; % .3f]", lo, hi))

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)
sink("output/S1/tables/full controls (s1).tex")
br <- " \\\\\n+"
cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
cat("\\begin{tabular}{l*{7}{c}}\n\\hline\\hline\n")
cat("& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
cat("                    &Reproduction   &\\shortstack[c]{Minor\\\\errors}   &\\shortstack[c]{Major\\\\errors}   &\\shortstack[c]{One good\\\\robustness}   &\\shortstack[c]{Two good\\\\robustness}   &\\shortstack[c]{Ran one\\\\robustness}   &\\shortstack[c]{Ran two\\\\robustness}    ", br, sep = "")
cat("\\hline\n")

for (term in all_terms) {
  # coefficient row with stars
  coefs <- sapply(tables, function(tb){
    row <- tb$table[tb$table$latex_term == term,]
    if (nrow(row)==0) "" else symstar(fmt(row$estimate), row$p.value)
  })
  cat(term, " & ", paste(coefs, collapse = " & "), br, sep = "")
  # robust SE row
  ses <- sapply(tables, function(tb){
    row <- tb$table[tb$table$latex_term == term,]
    if (nrow(row)==0) "" else paste0("(", fmt(row$std.error), ")")
  })
  cat("                    & ", paste(ses, collapse = " & "), br, sep = "")
  # CI row
  cis <- sapply(tables, function(tb){
    row <- tb$table[tb$table$latex_term == term,]
    if (nrow(row)==0) "" else fmt_ci(row$conf.low, row$conf.high)
  })
  cat("                    & ", paste(cis, collapse = " & "), br, sep = "")
}

cat("\\hline\n")
cat("Controls            & ", paste(rep("\\checkmark", length(dep_vars)), collapse = " & "), br, sep = "")
cat("Mean of dep. var    & ", paste(sapply(tables, function(tb) fmt(tb$ymean)), collapse = " & "), br, sep = "")
cat("Obs.                & ", paste(sapply(tables, function(tb) tb$N), collapse = " & "), br, sep = "")
cat("\\hline\n\\hline\n")
cat("\\multicolumn{8}{l}{\\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only group omitted.}\\\\\n")
cat("\\multicolumn{8}{l}{Controls include number of teammates; game-by-software fixed effects; maximum and minimum position skill fixed effects; attendance fixed effects.}\\\\\n")
cat("\\multicolumn{8}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
cat("\\end{tabular}\n")
sink()

# Sanitize: remove any accidental leading '+' characters and ensure proper row endings
out_path <- "output/S1/tables/full controls (s1).tex"
txt <- readLines(out_path, warn = FALSE)
txt <- sub("^\\+", "", txt)
txt <- sub("[[:space:]]+$", "", txt)
txt <- gsub("(?<!\\\\)\\\\$", "\\\\\\\\", txt, perl = TRUE)
writeLines(txt, out_path)
