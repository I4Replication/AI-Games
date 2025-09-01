# S1 filtered versions of group-differences tables

library(dplyr)
library(tidyr)
library(kableExtra)

df <- data.frame(readRDS("data/AI games.rds")) %>%
  filter(game != "Virtual 2025")

df$branch <- as.factor(df$branch)

varlist <- c(
  "reproduction","time2_reproduction","minor_errors","time2_first_minor",
  "major_errors","time2_first_major","one_good_robustness","two_good_robustness",
  "ran_one_robustness","ran_two_robustness"
)
df[varlist] <- lapply(df[varlist], as.numeric)

var_labels <- c(
  reproduction = "Reproduction",
  time2_reproduction = "Minutes to reproduction",
  minor_errors = "Number of minor errors",
  time2_first_minor = "Minutes to first minor error",
  major_errors = "Number of major errors",
  time2_first_major = "Minutes to first major error",
  one_good_robustness = "At least one good robustness check",
  two_good_robustness = "At least two good robustness checks",
  ran_one_robustness = "Ran at least one good robustness check",
  ran_two_robustness = "Ran at least two good robustness checks"
)

make_latex_table <- function(df, outpath){
  stats_df <- df %>% group_by(branch) %>%
    summarise(across(all_of(varlist), list(mean = ~mean(.x, na.rm=TRUE), sd = ~sd(.x, na.rm=TRUE)), .names="{col}_{fn}"), .groups="drop")
  long_df <- stats_df %>% pivot_longer(cols = -branch, names_to = c("variable","stat"), names_pattern = "^(.*)_(mean|sd)$") %>%
    mutate(variable = dplyr::recode(variable, !!!var_labels))
  wide_stats <- long_df %>% pivot_wider(names_from = stat, values_from = value) %>%
    mutate(mean_sd = ifelse(variable %in% c("Minutes to reproduction","Minutes to first minor error","Minutes to first major error"),
                            paste0(sprintf("%.1f", mean), "<br>(", sprintf("%.1f", sd), ")"),
                            paste0(sprintf("%.3f", mean), "<br>(", sprintf("%.3f", sd), ")"))) %>%
    dplyr::select(variable, branch, mean_sd) %>% pivot_wider(names_from = branch, values_from = mean_sd)
  branches <- levels(df$branch); pairs <- combn(branches, 2, simplify=FALSE)
  get_diff <- function(var,b1,b2){ d1 <- df %>% filter(branch==b1) %>% pull(!!sym(var)); d2 <- df %>% filter(branch==b2) %>% pull(!!sym(var));
    diff <- mean(d1,na.rm=TRUE)-mean(d2,na.rm=TRUE); p <- tryCatch(t.test(d1,d2,var.equal=TRUE)$p.value, error=function(e) NA_real_);
    data.frame(variable = var_labels[[var]], comparison=paste(b1,"vs",b2), diff_mean = diff, p_value = p) }
  diff_df <- do.call(rbind, lapply(varlist, function(v) do.call(rbind, lapply(pairs, \(pr) get_diff(v, pr[1], pr[2]))))) %>%
    mutate(p_fmt = ifelse(is.na(p_value), "NA", ifelse(p_value<0.001, "\\textless0.001", sprintf("%.3f", p_value))),
           diff_p = ifelse(variable %in% c("Minutes to reproduction","Minutes to first minor error","Minutes to first major error"),
                           paste0(sprintf("%.1f", diff_mean), "<br>\\relax[", p_fmt, "]"),
                           paste0(sprintf("%.3f", diff_mean), "<br>\\relax[", p_fmt, "]")),
           comp_col = gsub(" vs ", "_", comparison)) %>%
    dplyr::select(variable, comp_col, diff_p) %>% pivot_wider(names_from = comp_col, values_from = diff_p)
  final_table <- wide_stats %>% left_join(diff_df, by="variable") %>% mutate(across(everything(), ~ ifelse(is.na(.) | .=="NA<br>(NA)", "-<br>(-)", .)))
  colnames(final_table) <- c("\\textbf{Variable}", "\\textbf{Human-Only}", "\\textbf{AI-Assisted}", "\\textbf{AI-Led}",
                             "\\textbf{\\shortstack{Human-Only\\\\vs\\\\AI-Assisted}}",
                             "\\textbf{\\shortstack{Human-Only\\\\vs\\\\AI-Led}}",
                             "\\textbf{\\shortstack{AI-Assisted\\\\vs\\\\AI-Led}}")
  final_table_clean <- final_table %>% mutate(across(-names(final_table[1,1]), ~gsub("<br>", "\\\\\\\\", .))) %>%
    mutate(across(-names(final_table[1,1]), ~paste0("\\shortstack{", . ,"}")))
  table_body <- kable(final_table_clean, format = "latex", align = c("l","c","c","c","c","c","c"), escape = FALSE, booktabs=TRUE, linesep="") %>%
    row_spec(1:(nrow(final_table_clean)-1), extra_latex_after = "[1em]")
  dir.create(dirname(outpath), recursive = TRUE, showWarnings = FALSE)
  cat(
    "\\begin{table}[ht]\n      \\centering\n      \\caption{Comparison of Human, AI-Assisted, and AI-Led Metrics  }\n",
    "\\label{tab:comparison_metrics_third}\n",
    "{\\scriptsize\n",
    table_body, "\n",
    "\\multicolumn{7}{p{0.9\\textwidth}}{\\textit{Note:} Columns 2--4 present means and standard errors in parentheses for individual groups (Human-only, AI-Assisted, and AI-Led); columns 5--7 present differences in means and p-values in brackets for group comparisons (Human-Only vs AI-Assisted, Human-Only vs AI-Led, and AI-Assisted vs AI-Led).}}\n",
    "\\end{table}",
    file = outpath)
}

# Emit S1-only version into pooled filename and also the s1-labeled file
make_latex_table(df, outpath = "output/S1/tables/branches.tex")
make_latex_table(df, outpath = "output/S1/tables/branches_s1.tex")

