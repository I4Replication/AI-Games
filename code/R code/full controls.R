# --- 1. Load and prep data
main <- readRDS("data/AI games.rds") %>%
  mutate(
    branch = as.factor(branch),
    game = as.factor(game),
    software = as.factor(software),
    max_skill = as.factor(max_skill),
    min_skill = as.factor(min_skill),
    max_gpt = as.factor(max_gpt),
    min_gpt = as.factor(min_gpt),
    attendance = as.factor(attendance)
  )

dep_vars <- c(
  "reproduction", "minor_errors", "major_errors", 
  "one_good_robustness", "two_good_robustness",
  "ran_one_robustness", "ran_two_robustness"
)

# --- 2. Helper: clean term names for LaTeX
clean_term <- function(term) {
  term <- gsub("`", "", term)
  term <- gsub(":", " $\\\\times$ ", term)
  term <- gsub("branch",        "Branch: ", term)
  term <- gsub("number_teammates", "Number of teammates", term)
  term <- gsub("game",          "Game: ", term)
  term <- gsub("software",      "Software: ", term)
  term <- gsub("max_skill",     "Maximum academic level: ", term)
  term <- gsub("min_skill",     "Minimum academic level: ", term)
  term <- gsub("max_gpt",       "Maximum GPT level: ", term)
  term <- gsub("min_gpt",       "Minimum GPT level: ", term)
  term <- gsub("attendance",    "Attendance: ", term)
  term <- gsub("\\.\\.\\.", "", term)
  
  ## --- only swap isolated digits, not digits that are part of a larger number
  term <- gsub("\\b0\\b", "Never",        term)
  term <- gsub("\\b1\\b", "Beginner",     term)
  term <- gsub("\\b2\\b", "Intermediate", term)
  term <- gsub("\\b3\\b", "Advanced",     term)
  term <- gsub("\\b4\\b", "Researcher",   term)
  term <- gsub("\\b5\\b", "Postdoc",      term)
  term <- gsub("\\b6\\b", "Professor",    term)
  ## -------------------------------------------------------------
  
  trimws(term)
}

get_stars <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < 0.01) return("***")
  if (pval < 0.05) return("**")
  if (pval < 0.1)  return("*")
  return("")
}

# --- 3. Extraction for each model
extract_full_reg <- function(model, dv, df) {
  ctab <- broom::tidy(model, conf.int = TRUE)
  rob_vcov <- vcovHC(model, type="HC1")
  se_robust <- sqrt(diag(rob_vcov))
  ctab$std.error <- se_robust
  ctab$conf.low <- ctab$estimate - 1.96 * ctab$std.error
  ctab$conf.high <- ctab$estimate + 1.96 * ctab$std.error
  
  ctab$latex_term <- sapply(ctab$term, clean_term)
  
  pval <- tryCatch({
    lh <- car::linearHypothesis(model, "branchAI-Assisted - branchAI-Led = 0", vcov. = rob_vcov)
    if ("Pr(>F)" %in% colnames(lh) && nrow(lh) >= 2) as.numeric(lh[2, "Pr(>F)"])
    else if ("Pr(>Chisq)" %in% colnames(lh) && nrow(lh) >= 2) as.numeric(lh[2, "Pr(>Chisq)"])
    else NA
  }, error = function(e) NA)
  list(
    table = ctab, 
    ymean = mean(df[[dv]], na.rm=TRUE), 
    N = nobs(model), 
    pval = pval
  )
}

# --- 4. Estimate all models and extract info
make_panel_tables <- function(dat) {
  models <- setNames(vector("list", length(dep_vars)), dep_vars)
  tables <- list()
  for (dv in dep_vars) {
    f <- as.formula(paste0(
      dv, "~ branch + number_teammates + game*software + max_skill + min_skill + attendance"
    ))
    mod <- lm(f, data=dat)
    models[[dv]] <- mod
    tables[[dv]] <- extract_full_reg(mod, dv, dat)
  }
  tables
}

panelA_data <- main %>% filter(game != "Virtual 2025")
panelA_tables <- make_panel_tables(panelA_data)
panelB_tables <- make_panel_tables(main)

# --- 5. Function to print one full table in LaTeX
make_full_latex_table <- function(tables, panel_name="") {
  all_terms <- Reduce(union, lapply(tables, function(x) x$table$latex_term))
  all_terms <- setdiff(all_terms, "(Intercept)")
  out <- ""
  out <- paste0(out, "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  out <- paste0(out, "\\begin{tabular}{l*{7}{c}}\n")
  out <- paste0(out, "\\hline\\hline\n")
  if(panel_name != "") {
    out <- paste0(out, "\\multicolumn{8}{l}{\\textbf{", panel_name, "}}\\\\\n")
  }
  out <- paste0(out, "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\\n")
  out <- paste0(out, "                    &Reproduction   &\\shortstack[c]{Minor\\\\errors}   &\\shortstack[c]{Major\\\\errors}   &\\shortstack[c]{One good\\\\robustness}   &\\shortstack[c]{Two good\\\\robustness}   &\\shortstack[c]{Ran one\\\\robustness}   &\\shortstack[c]{Ran two\\\\robustness}   \\\\\n")
  out <- paste0(out, "\\hline\n")
  for (term in all_terms) {
    coefs <- sapply(tables, function(x) {
      row <- x$table[x$table$latex_term == term,]
      if (nrow(row)) sprintf("%9.3f%s", row$estimate, get_stars(row$p.value)) else ""
    })
    ses <- sapply(tables, function(x) {
      row <- x$table[x$table$latex_term == term,]
      if (nrow(row)) sprintf("%9.3f", row$std.error) else ""
    })
    cis <- sapply(tables, function(x) {
      row <- x$table[x$table$latex_term == term,]
      if (nrow(row)) sprintf("[%9.3f; %9.3f]", row$conf.low, row$conf.high) else ""
    })
    if (any(coefs != "")) {
      out <- paste0(out, term, " &", paste(coefs, collapse="   &"), "   \\\\\n")
      out <- paste0(out, "                    &", paste(sprintf("(%s)", ses), collapse="   &"), "   \\\\\n")
      out <- paste0(out, "                    &", paste(cis, collapse="   &"), "   \\\\\n")
    }
  }
  ymeans <- sapply(tables, function(x) sprintf("%9.3f", x$ymean))
  pvals  <- sapply(tables, function(x) ifelse(is.na(x$pval), "", sprintf("%9.3f", x$pval)))
  nobs   <- sapply(tables, function(x) as.character(x$N))
  out <- paste0(out, "\\hline\n")
  out <- paste0(out, "Mean of dep. var    &", paste(ymeans, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "p-val (AI-Assisted vs. AI-Led)&", paste(pvals, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "Observations        &", paste(nobs, collapse="   &"), "   \\\\\n")
  out <- paste0(out, "\\hline\\hline\n")
  out <- paste0(out, "\\multicolumn{8}{l}{\\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only branch omitted.}\\\\\n")
  out <- paste0(out, "\\multicolumn{8}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
  out <- paste0(out, "\\end{tabular}\n")
  out
}

# --- 6. Print/export Panel A and Panel B as two tables ---
panelA_latex <- make_full_latex_table(panelA_tables, "Panel A: Study I")
panelB_latex <- make_full_latex_table(panelB_tables, "Panel B: Studies I and II combined")

cat(panelA_latex, file = "output/tables/full controls (s1).tex")
cat(panelB_latex, file = "output/tables/full controls (s2).tex")
