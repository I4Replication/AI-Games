############################################################
#### FULL SCRIPT — adds a “Difference (p-value)” column ####
############################################################
# ---------------------------------------------------------
# Read and prepare the data
# ---------------------------------------------------------
df <- readRDS("data/AI games.rds") |>
         as.data.frame()

# Ensure 'branch' is a factor and map codes → labels
df$branch <- as.factor(df$branch)
branch_labels <- c("1" = "Human-Only",
                   "2" = "AI-Assisted",
                   "3" = "AI-Led")
df$branch <- dplyr::recode_factor(df$branch, !!!branch_labels)

# Variables of interest + nice display names
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
  ran_two_robustness    = "Ran at least two appropriate robustness checks"
)

# Ensure numeric
df[varlist] <- lapply(df[varlist], as.numeric)

# ---------------------------------------------------------
#  Analysis restricted to AI-Assisted teams (branch == 2)
# ---------------------------------------------------------
df_ai  <- df |> filter(branch == "AI-Assisted")

# Median split on number of prompts
prompts_median <- median(df_ai$prompts, na.rm = TRUE)
df_ai <- df_ai |> mutate(
  prompts_group = ifelse(prompts <= prompts_median,
                         "Below/equal to median", "Above median")
)

# ---------------------------------------------------------
# 1) Means & SDs for each group  ---------------------------
# ---------------------------------------------------------
stats_prompts_df <- df_ai |>
  group_by(prompts_group) |>
  summarise(across(all_of(varlist),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x,   na.rm = TRUE)),
                   .names = "{col}_{fn}"),
            .groups = "drop")

# Long → recode labels → wide (so each row = variable × group)
long_prompts_df <- stats_prompts_df |>
  pivot_longer(-prompts_group,
               names_to = c("variable","stat"),
               names_pattern = "^(.*)_(mean|sd)$") |>
  mutate(variable = dplyr::recode(as.character(variable), !!!var_labels))

wide_prompts_stats <- long_prompts_df |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(mean_sd = ifelse(variable %in%
                            c("Minutes to reproduction",
                              "Minutes to first minor error",
                              "Minutes to first major error"),
                          paste0(sprintf("%.1f", mean), "<br>(",
                                 sprintf("%.1f", sd), ")"),
                          paste0(sprintf("%.3f", mean), "<br>(",
                                 sprintf("%.3f", sd), ")")))

table_df <- wide_prompts_stats %>%                       # pipe with %>%
  dplyr::select(variable, prompts_group, mean_sd) %>%    # ← be explicit
  dplyr::distinct(variable, prompts_group, .keep_all = TRUE) %>% 
  tidyr::pivot_wider(names_from  = prompts_group,
                     values_from = mean_sd,
                     values_fn   = list(mean_sd = dplyr::first))

# ---------------------------------------------------------
# 2) Difference & Welch-t p-value for each variable --------
# ---------------------------------------------------------
diff_p_df <- lapply(varlist, function(v){
  below  <- df_ai |>
    filter(prompts_group == "Below/equal to median") |>
    pull(!!sym(v))
  above  <- df_ai |>
    filter(prompts_group == "Above median") |>
    pull(!!sym(v))
  diff   <- mean(above, na.rm = TRUE) - mean(below, na.rm = TRUE)   # ↑ minus ↓
  pval   <- tryCatch(t.test(above, below, var.equal = FALSE)$p.value,
                     error = \(e) NA_real_)
  tibble(variable_raw = v,
         diff        = diff,
         pval        = pval)
}) |> bind_rows()

# Re-label variables
diff_p_df <- diff_p_df %>% 
  dplyr::mutate(variable = dplyr::recode(variable_raw, !!!var_labels)) %>% 
  dplyr::select(-variable_raw)

# Format the difference + p-value string
diff_p_df <- diff_p_df %>% 
  dplyr::mutate(
    diff_fmt = ifelse(variable %in%
                        c("Minutes to reproduction",
                          "Minutes to first minor error",
                          "Minutes to first major error"),
                      sprintf("%.1f", diff),
                      sprintf("%.3f", diff)),
    p_fmt    = ifelse(is.na(pval), "NA",
                      ifelse(pval < 0.001, "<0.001",
                             sprintf("%.3f", pval))),
    # Add \relax so [ is not parsed as an optional argument
    diff_p   = paste0(diff_fmt, "<br>\\relax[", p_fmt, "]")
  ) %>% 
  dplyr::select(variable, diff_p)    # ← note the namespace

# ---------------------------------------------------------
# 3) Merge + counts in headers -----------------------------
# ---------------------------------------------------------
final_df <- table_df |>
  left_join(diff_p_df, by = "variable") |>
  rename(Variable = variable)

# Replace NAs with dash
final_df[is.na(final_df)] <- "-"

# Observation counts for header
group_counts <- df_ai |>
  count(prompts_group, name = "n")

name_with_n <- function(group_label){
  nval <- group_counts |>
    filter(prompts_group == group_label) |>
    pull(n)
  paste0(group_label, "\\\\(n=", nval, ")")
}

orig_cols   <- colnames(final_df)
group_cols  <- orig_cols[orig_cols %in% group_counts$prompts_group]
new_headers <- sapply(group_cols, name_with_n, USE.NAMES = FALSE)

colnames(final_df) <- c("Variable", new_headers, "Difference")

# ---------------------------------------------------------
# 4) Convert <br> → LaTeX and wrap in \shortstack ----------
# ---------------------------------------------------------
latex_df <- final_df |>
  mutate(across(everything(), ~ gsub("<br>", "\\\\\\\\", .x))) |>
  mutate(across(everything(), ~ paste0("\\shortstack{", .x, "}")))

colnames(latex_df) <- paste0("\\shortstack{", colnames(latex_df), "}")

# ---------------------------------------------------------
# 5) Build & write the LaTeX table -------------------------
# ---------------------------------------------------------
prompts_table_body <- kable(latex_df,
                            format   = "latex",
                            booktabs = TRUE,
                            align    = c("l","c","c","c"),
                            escape   = FALSE,
                            hline_after = c(0),
                            linesep  = "") |>
  row_spec(1:(nrow(latex_df)-1),
           extra_latex_after = "[1em]")

prompts_table_body <- gsub("NA", "-", prompts_table_body)

# make sure output directory exists
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

cat(
  "\\begin{table}[ht]\n",
  "\\centering\n",
  "\\caption{Comparison of Key Metrics by Prompt Levels within AI-Assisted Group}\n",
  "\\label{tab:comparison_metrics_prompts}\n",
  "{\\scriptsize\n",
  prompts_table_body, "\n",
  "}\n",
  "\\\\\n",  # (optional) blank line after the body
  "\\multicolumn{4}{p{0.8\\textwidth}}{\\textit{Note:} Columns 2--3 present means and standard errors in parentheses for individual groups (Human-only, AI-Assisted, and AI-Led); column 4 shows mean differences and $p$-values in brackets for the indicated group comparison. Groups are defined by the median number of prompts (", 
  prompts_median, 
  ") in the AI-Assisted sample.}\n",
  "\\end{table}",
  file = "output/tables/prompts.tex"
)