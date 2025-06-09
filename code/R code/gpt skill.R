# ---------------------------------------------------------------------------
# Load data and labels
# ---------------------------------------------------------------------------
df <- readRDS("data/AI games.rds") |> as.data.frame()

df$branch <- factor(df$branch)
branch_labels <- c("1" = "Human-Only", "2" = "AI-Assisted", "3" = "AI-Led")
df$branch <- dplyr::recode_factor(df$branch, !!!branch_labels)

varlist <- c("reproduction","time2_reproduction","minor_errors",
             "time2_first_minor","major_errors","time2_first_major",
             "one_good_robustness","two_good_robustness",
             "ran_one_robustness","ran_two_robustness")

var_labels <- c(
  reproduction = "Reproduction",
  time2_reproduction = "Minutes to reproduction",
  minor_errors = "Number of minor errors",
  time2_first_minor = "Minutes to first minor error",
  major_errors = "Number of major errors",
  time2_first_major = "Minutes to first major error",
  one_good_robustness = "At least one appropriate robustness check",
  two_good_robustness = "At least two appropriate robustness checks",
  ran_one_robustness = "Ran at least one appropriate robustness check",
  ran_two_robustness = "Ran at least two appropriate robustness checks"
)

df[varlist] <- lapply(df[varlist], as.numeric)

# ---------------------------------------------------------------------------
# Create experience variable
# ---------------------------------------------------------------------------
df <- df %>%
  mutate(
    experience = case_when(
      max_gpt %in% c("Never", "Beginner", "Intermediate") ~ "low_intermediate",
      max_gpt == "Advanced" ~ "high",
      TRUE ~ NA_character_
    )
  )

# ---------------------------------------------------------------------------
# Filter only AI-Assisted and AI-Led, and create group
# ---------------------------------------------------------------------------
df_filtered <- df %>%
  filter(branch %in% c("AI-Assisted", "AI-Led"), !is.na(experience)) %>%
  mutate(group = paste(branch, experience))


# ---------------------------------------------------------------------------
# Compute means & SDs per group
# ---------------------------------------------------------------------------
stats_exp_df <- df_filtered %>% 
  group_by(group) %>% 
  summarise(across(all_of(varlist),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x,   na.rm = TRUE)),
                   .names = "{col}_{fn}"),
            .groups = "drop")

long_exp_df <- stats_exp_df %>%
  pivot_longer(-group,
               names_to = c("variable","stat"),
               names_pattern = "^(.*)_(mean|sd)$") %>%
  mutate(variable = var_labels[variable])

wide_exp_stats <- long_exp_df %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(mean_sd = ifelse(variable %in%
                            c("Minutes to reproduction",
                              "Minutes to first minor error",
                              "Minutes to first major error"),
                          paste0(sprintf("%.1f", mean), "<br>(",
                                 sprintf("%.1f", sd), ")"),
                          paste0(sprintf("%.3f", mean), "<br>(",
                                 sprintf("%.3f", sd), ")")))

table_df <- wide_exp_stats %>% 
  dplyr::select(variable, group, mean_sd) %>% 
  tidyr::pivot_wider(names_from = group, values_from = mean_sd)

table_df[is.na(table_df)] <- "-"

# ---------------------------------------------------------------------------
# Compute 1–2 and 3–4 differences + Welch p-values
# (1 = AI-Assisted high, 2 = AI-Assisted low/int,
#  3 = AI-Led high,      4 = AI-Led low/int)
# ---------------------------------------------------------------------------
grp1 <- "AI-Assisted high"
grp2 <- "AI-Assisted low_intermediate"
grp3 <- "AI-Led high"
grp4 <- "AI-Led low_intermediate"

diff_list <- lapply(varlist, function(v){
  a1 <- df_filtered %>% filter(group == grp1) %>% pull(!!sym(v))
  a2 <- df_filtered %>% filter(group == grp2) %>% pull(!!sym(v))
  a3 <- df_filtered %>% filter(group == grp3) %>% pull(!!sym(v))
  a4 <- df_filtered %>% filter(group == grp4) %>% pull(!!sym(v))
  
  t12 <- tryCatch(t.test(a1, a2, var.equal = FALSE)$p.value, error = \(e) NA_real_)
  t34 <- tryCatch(t.test(a3, a4, var.equal = FALSE)$p.value, error = \(e) NA_real_)
  
  tibble(variable_raw = v,
         diff12 = mean(a1, na.rm = TRUE) - mean(a2, na.rm = TRUE),
         p12    = t12,
         diff34 = mean(a3, na.rm = TRUE) - mean(a4, na.rm = TRUE),
         p34    = t34)
}) %>% bind_rows()

diff_list <- diff_list %>% 
  mutate(variable = var_labels[variable_raw]) %>% 
  dplyr::select(-variable_raw)

fmt_diff_p <- function(diff, p, minutes = FALSE){
  diff_out <- ifelse(minutes, sprintf("%.1f", diff), sprintf("%.3f", diff))
  p_out    <- ifelse(is.na(p), "NA",
                     ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  # Add \relax so [ is not parsed as an optional argument
  paste0(diff_out, "<br>\\relax[", p_out, "]")
}

diff_df <- diff_list %>% 
  mutate(
    diff12_fmt = mapply(fmt_diff_p, diff12, p12,
                        minutes = variable %in%
                          c("Minutes to reproduction",
                            "Minutes to first minor error",
                            "Minutes to first major error")),
    diff34_fmt = mapply(fmt_diff_p, diff34, p34,
                        minutes = variable %in%
                          c("Minutes to reproduction",
                            "Minutes to first minor error",
                            "Minutes to first major error"))
  ) %>% 
  dplyr::select(variable, diff12_fmt, diff34_fmt) %>% 
  rename(`AI-Assisted \\\\ High vs Low` = diff12_fmt,
         `AI-Led \\\\ High vs Low`     = diff34_fmt)

# ---------------------------------------------------------------------------
# Merge means ± SD with difference columns
# ---------------------------------------------------------------------------
final_df <- table_df %>% 
  left_join(diff_df, by = "variable") %>% 
  rename(Variable = variable)

final_df[is.na(final_df)] <- "-"

# ---------------------------------------------------------------------------
# Add (n=) counts to the four original group headers
# ---------------------------------------------------------------------------
group_counts <- df_filtered %>% count(group, name = "n")

add_n <- function(colname_raw){
  n_val <- group_counts %>% filter(group == colname_raw) %>% pull(n)
  new   <- gsub("low_intermediate","low/medium", colname_raw)
  new   <- gsub("AI-Assisted high","\\\\shortstack{AI-Assisted \\\\\\\\ high experience", new)
  new   <- gsub("AI-Assisted low/medium","\\\\shortstack{AI-Assisted \\\\\\\\ low/medium experience", new)
  new   <- gsub("AI-Led high","\\\\shortstack{AI-Led \\\\\\\\ high experience", new)
  new   <- gsub("AI-Led low/medium","\\\\shortstack{AI-Led \\\\\\\\ low/medium experience", new)
  paste0(new,"\\\\(n=", n_val, ")}")
}

orig_cols  <- names(final_df)
grp_cols   <- orig_cols[orig_cols %in% group_counts$group]
final_cols <- c("Variable",
                sapply(grp_cols, add_n, USE.NAMES = FALSE),
                "\\shortstack{AI-Assisted \\\\ High vs Low}",
                "\\shortstack{AI-Led \\\\ High vs Low}")

names(final_df) <- final_cols

# ---------------------------------------------------------------------------
# Convert <br> to LaTeX line breaks and wrap in \shortstack
# ---------------------------------------------------------------------------
latex_df <- final_df %>% 
  mutate(across(everything(), ~ gsub("<br>", "\\\\\\\\", .x))) %>% 
  mutate(across(everything(), ~ paste0("\\shortstack{", .x, "}")))


# ---------------------------------------------------------------------------
# Build and write LaTeX table
# ---------------------------------------------------------------------------
table_body <- kable(latex_df,
                    format   = "latex",
                    booktabs = TRUE,
                    escape   = FALSE,
                    align    = c("l","c","c","c","c","c","c"),
                    hline_after = c(0),
                    linesep  = "") %>% 
  row_spec(1:(nrow(latex_df)-1), extra_latex_after = "[1em]") %>% 
  gsub("NA","-", .)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

cat(
  "\\begin{table}[ht]
\\centering
\\caption{AI-Assisted and AI-Led Metrics by Experience Level}
\\label{tab:comparison_experience}
\\tiny", table_body,"
\multicolumn{7}{p{0.9\textwidth}}{\it{Note:} Columns 2--5 present means and standard errors in parentheses for individual groups (Human-only, AI-Assisted, and AI-Led); the difference columns show mean differences and $p$-values in brackets for the indicated group comparisons.}
\\end{table}",
)