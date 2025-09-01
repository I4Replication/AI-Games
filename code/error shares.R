# S1-only: error shares table (keep Study I only, no panel label)

library(dplyr)
library(fixest)
library(broom)

df <- readRDS("data/AI games.rds") %>% filter(game != "Virtual 2025")

df <- df %>% mutate(
  game_software = interaction(game, software, drop = TRUE),
  max_skill     = as.factor(max_skill),
  min_skill     = as.factor(min_skill),
  attendance    = as.factor(attendance),
  branch        = as.factor(branch)
)

dep_vars <- c("minor_errors", "major_errors")
df <- df %>% mutate(across(all_of(dep_vars), as.numeric))

models <- lapply(dep_vars, function(dv) feols(
  as.formula(paste(dv, "~ branch |",
                   "game_software + max_skill + min_skill + attendance")),
  data = df, vcov = "hetero"))

summ <- function(m, dv){
  td <- broom::tidy(m, conf.int = TRUE)
  pick <- function(pat, col) { v <- td[grepl(pat, td$term), col]; if (length(v)==0) NA_real_ else v[1] }
  tibble(
    b_aa = pick("branchAI-Assisted$", "estimate"), se_aa = pick("branchAI-Assisted$", "std.error"),
    lo_aa = pick("branchAI-Assisted$", "conf.low"), hi_aa = pick("branchAI-Assisted$", "conf.high"),
    b_al = pick("branchAI-Led$", "estimate"), se_al = pick("branchAI-Led$", "std.error"),
    lo_al = pick("branchAI-Led$", "conf.low"), hi_al = pick("branchAI-Led$", "conf.high"),
    mean = mean(df[[dv]], na.rm=TRUE), N = nobs(m),
    p_joint = tryCatch({
      w <- car::linearHypothesis(m, "branchAI-Assisted = branchAI-Led", vcov. = vcov(m, type = "HC1"));
      as.numeric(w[2, "Pr(>Chisq)"])
    }, error = function(e) NA_real_)
  )
}
res <- Map(summ, models, dep_vars)

fmt <- \(x) sprintf("% .3f", x)
fmt_ci <- \(lo,hi) sprintf("[% .3f; % .3f]", lo, hi)

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)
sink("output/S1/tables/error shares.tex")
cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
cat("\\begin{tabular}{l*{2}{c}}\n\\hline\\hline\n")
cat("& (1) & (2) \\\\ \n")
cat("                    & Minor errors & Major errors \\\\ \n")
cat("\\hline\n")
cat("AI-Assisted         & ", paste(fmt(sapply(res, `[[`, "b_aa")), collapse = " & "), " \\\n")
cat("                    & ", paste(paste0("(", fmt(sapply(res, `[[`, "se_aa")), ")"), collapse = " & "), " \\\n")
cat("                    & ", paste(mapply(fmt_ci, sapply(res, `[[`, "lo_aa"), sapply(res, `[[`, "hi_aa")), collapse = " & "), " \\\n")
cat("AI-Led              & ", paste(fmt(sapply(res, `[[`, "b_al")), collapse = " & "), " \\\n")
cat("                    & ", paste(paste0("(", fmt(sapply(res, `[[`, "se_al")), ")"), collapse = " & "), " \\\n")
cat("                    & ", paste(mapply(fmt_ci, sapply(res, `[[`, "lo_al"), sapply(res, `[[`, "hi_al")), collapse = " & "), " \\\n")
cat("\\hline\n")
cat("Controls   & \\checkmark & \\checkmark \\\\ \n")
cat("Mean dep. var       & ", paste(fmt(sapply(res, `[[`, "mean")), collapse = " & "), " \\\\ \n")
cat("p-val (AI-Assisted vs. AI-Led)    & ", paste(fmt(sapply(res, `[[`, "p_joint")), collapse = " & "), " \\\\ \n")
cat("Obs.                & ", paste(sapply(res, `[[`, "N"), collapse = " & "), " \\\\ \n")
cat("\\hline\n\\hline\n")
cat(" \\multicolumn{3}{l}{\\it{Note:} Standard errors in  parentheses, confidence intervals in brackets; human-only group omitted.}\\\\\n")
cat(" \\multicolumn{3}{l}{Controls include number of teammates; game-by-software fixed effects; maximum and minimum position skill fixed effects; attendance fixed effects.}\\\\\n")
cat(" \\multicolumn{3}{l}{\\sym{*} $p<0.1$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$}\\\\\n")
cat(" \\end{tabular}\n")
sink()

# Sanitize: ensure double \\\\\nout_path <- "output/S1/tables/error shares.tex"
txt <- readLines(out_path, warn = FALSE)
# trim trailing whitespace
txt <- sub("[[:space:]]+$", "", txt)
# ensure lines ending with exactly one backslash get two
txt <- gsub("(?<!\\\\)\\\\$", "\\\\\\\\", txt, perl = TRUE)
writeLines(txt, out_path)
