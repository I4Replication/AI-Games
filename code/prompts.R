# S1-filtered prompts table to output/S1/tables/prompts.tex

library(dplyr)
library(tidyr)
library(kableExtra)

df <- readRDS("data/AI games.rds") |> as.data.frame() |> filter(game != "Virtual 2025")

df$branch <- as.factor(df$branch)

varlist <- c("reproduction", "time2_reproduction", "minor_errors",
             "time2_first_minor", "major_errors", "time2_first_major",
             "one_good_robustness", "two_good_robustness",
             "ran_one_robustness", "ran_two_robustness")

var_labels <- c(
  reproduction          = "Reproduction",
  time2_reproduction    = "Minutes to reproduction",
  minor_errors          = "Number of minor errors",
  time2_first_minor     = "Minutes to first minor error",
  major_errors          = "Number of major errors",
  time2_first_major     = "Minutes to first major error",
  one_good_robustness   = "At least one appropriate robustness check",
  two_good_robustness   = "At least two appropriate robustness checks",
  ran_one_robustness    = "Ran at least one appropriate robustness check",
  ran_two_robustness    = "Ran at least two appropriate robustness check"
)

df[varlist] <- lapply(df[varlist], as.numeric)

df_ai  <- df |> filter(branch == "AI-Assisted")

prompts_median <- median(df_ai$prompts, na.rm = TRUE)
df_ai <- df_ai |> mutate(
  prompts_group = ifelse(prompts <= prompts_median, "Below/equal to median", "Above median")
)

stats_prompts_df <- df_ai |>
  group_by(prompts_group) |>
  summarise(across(all_of(varlist), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), .names = "{col}_{fn}"), .groups = "drop")

long_prompts_df <- stats_prompts_df |>
  pivot_longer(-prompts_group, names_to = c("variable","stat"), names_pattern = "^(.*)_(mean|sd)$") |>
  mutate(variable = dplyr::recode(as.character(variable), !!!var_labels))

wide_prompts_stats <- long_prompts_df |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(mean_sd = ifelse(variable %in% c("Minutes to reproduction","Minutes to first minor error","Minutes to first major error"),
                          paste0(sprintf("%.1f", mean), "<br>(", sprintf("%.1f", sd), ")"),
                          paste0(sprintf("%.3f", mean), "<br>(", sprintf("%.3f", sd), ")")))

table_df <- wide_prompts_stats %>% dplyr::select(variable, prompts_group, mean_sd) %>% dplyr::distinct(variable, prompts_group, .keep_all = TRUE) %>% tidyr::pivot_wider(names_from  = prompts_group, values_from = mean_sd, values_fn   = list(mean_sd = dplyr::first))

diff_p_df <- lapply(varlist, function(v){
  below  <- df_ai |> filter(prompts_group == "Below/equal to median") |> pull(!!sym(v))
  above  <- df_ai |> filter(prompts_group == "Above median") |> pull(!!sym(v))
  diff   <- mean(above, na.rm = TRUE) - mean(below, na.rm = TRUE)
  pval   <- tryCatch(t.test(above, below, var.equal = FALSE)$p.value, error = \(e) NA_real_)
  tibble(variable_raw = v, diff = diff, pval = pval)
}) |> bind_rows()

diff_p_df <- diff_p_df %>% dplyr::mutate(variable = dplyr::recode(variable_raw, !!!var_labels)) %>% dplyr::select(-variable_raw)

diff_p_df <- diff_p_df %>% dplyr::mutate(
  diff_fmt = ifelse(variable %in% c("Minutes to reproduction","Minutes to first minor error","Minutes to first major error"), sprintf("%.1f", diff), sprintf("%.3f", diff)),
  p_fmt    = ifelse(is.na(pval), "NA", ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval))),
  diff_p   = paste0(diff_fmt, "<br>\\relax[", p_fmt, "]")
) %>% dplyr::select(variable, diff_p)

final_df <- table_df |> left_join(diff_p_df, by = "variable") |> rename(Variable = variable)
final_df[is.na(final_df)] <- "-"

group_counts <- df_ai |> count(prompts_group, name = "n")
name_with_n <- function(group_label){ nval <- group_counts |> filter(prompts_group == group_label) |> pull(n); paste0(group_label, "\\\\(n=", nval, ")") }

orig_cols   <- colnames(final_df)
group_cols  <- orig_cols[orig_cols %in% group_counts$prompts_group]
new_headers <- sapply(group_cols, name_with_n, USE.NAMES = FALSE)
colnames(final_df) <- c("Variable", new_headers, "Difference")

latex_df <- final_df |> mutate(across(everything(), ~ gsub("<br>", "\\\\\\\\", .x))) |> mutate(across(everything(), ~ paste0("\\shortstack{", .x, "}")))
# Apply shortstack to headers as well to improve alignment
colnames(latex_df) <- paste0("\\shortstack{", colnames(final_df), "}")

prompts_table_body <- kable(latex_df, format   = "latex", booktabs = TRUE, align = c("l","c","c","c"), escape   = FALSE, hline_after = c(0), linesep  = "") |>
  row_spec(1:(nrow(latex_df)-1), extra_latex_after = "[1em]")

prompts_table_body <- gsub("NA", "-", prompts_table_body)

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)
cat(
  "\\begin{table}[ht]\n",
  "\\centering\n",
  "\\caption{Comparison of Key Metrics by Prompt Levels within AI-Assisted Group}\n",
  "\\label{tab:comparison_metrics_prompts}\n",
  "{\\scriptsize\n",
  prompts_table_body, "\n",
  "}\n",
  "\\\\\n",
  "\\multicolumn{4}{p{0.8\\textwidth}}{\\textit{Note:} Columns 2--3 present means and standard errors in parentheses for individual groups (Human-only, AI-Assisted, and AI-Led); column 4 shows mean differences and $p$-values in brackets for the indicated group comparison. Groups are defined by the median number of prompts (",
  prompts_median,
  ") in the AI-Assisted sample.}\n",
  "\\end{table}",
  file = "output/S1/tables/prompts.tex"
)
