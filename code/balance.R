# S1-only balance table to output/S1/tables/balance.tex

library(dplyr)
library(tidyr)
library(kableExtra)

df <- data.frame(readRDS("data/AI games.rds")) %>% filter(game != "Virtual 2025")

df$branch <- factor(df$branch, levels = c("Human-Only","AI-Assisted","AI-Led"))

skill_levels <- c("Student" = 1, "Researcher" = 2, "Postdoc" = 3, "Professor" = 4)
gpt_levels   <- c("Never" = 1, "Beginner" = 2, "Intermediate" = 3, "Advanced" = 4)

df <- df %>% mutate(
  min_skill_num = skill_levels[as.character(min_skill)],
  max_skill_num = skill_levels[as.character(max_skill)],
  min_gpt_num   = gpt_levels[as.character(min_gpt)],
  max_gpt_num   = gpt_levels[as.character(max_gpt)]
)

df <- df %>% mutate(
  min_professor    = ifelse(!is.na(min_skill_num) & min_skill_num == 4, 1, 0),
  min_postdoc      = ifelse(!is.na(min_skill_num) & min_skill_num == 3, 1, 0),
  min_researcher   = ifelse(!is.na(min_skill_num) & min_skill_num == 2, 1, 0),
  min_student      = ifelse(!is.na(min_skill_num) & min_skill_num == 1, 1, 0),
  max_professor    = ifelse(!is.na(max_skill_num) & max_skill_num == 4, 1, 0),
  max_postdoc      = ifelse(!is.na(max_skill_num) & max_skill_num == 3, 1, 0),
  max_researcher   = ifelse(!is.na(max_skill_num) & max_skill_num == 2, 1, 0),
  max_student      = ifelse(!is.na(max_skill_num) & max_skill_num == 1, 1, 0),
  min_gpt_never        = ifelse(!is.na(min_gpt_num) & min_gpt_num == 1, 1, 0),
  min_gpt_beginner     = ifelse(!is.na(min_gpt_num) & min_gpt_num == 2, 1, 0),
  min_gpt_intermediate = ifelse(!is.na(min_gpt_num) & min_gpt_num == 3, 1, 0),
  min_gpt_advanced     = ifelse(!is.na(min_gpt_num) & min_gpt_num == 4, 1, 0),
  max_gpt_never        = ifelse(!is.na(max_gpt_num) & max_gpt_num == 1, 1, 0),
  max_gpt_beginner     = ifelse(!is.na(max_gpt_num) & max_gpt_num == 2, 1, 0),
  max_gpt_intermediate = ifelse(!is.na(max_gpt_num) & max_gpt_num == 3, 1, 0),
  max_gpt_advanced     = ifelse(!is.na(max_gpt_num) & max_gpt_num == 4, 1, 0)
)

varlist <- c(
  "number_teammates",
  "min_professor","min_postdoc","min_researcher","min_student",
  "max_professor","max_postdoc","max_researcher","max_student",
  "combined_follow",
  "min_gpt_never","min_gpt_beginner","min_gpt_intermediate","min_gpt_advanced",
  "max_gpt_never","max_gpt_beginner","max_gpt_intermediate","max_gpt_advanced"
)

var_labels <- c(
  number_teammates         = "Number of teammates",
  min_professor            = "Minimum academic level: Professor",
  min_postdoc              = "Minimum academic level: Postdoc",
  min_researcher           = "Minimum academic level: Researcher",
  min_student              = "Minimum academic level: Student",
  max_professor            = "Maximum academic level: Professor",
  max_postdoc              = "Maximum academic level: Postdoc",
  max_researcher           = "Maximum academic level: Researcher",
  max_student              = "Maximum academic level: Student",
  combined_follow          = "Average years of coding experience",
  min_gpt_never            = "Min ChatGPT level: Never",
  min_gpt_beginner         = "Min ChatGPT level: Beginner",
  min_gpt_intermediate     = "Min ChatGPT level: Intermediate",
  min_gpt_advanced         = "Min ChatGPT level: Advanced",
  max_gpt_never            = "Max ChatGPT level: Never",
  max_gpt_beginner         = "Max ChatGPT level: Beginner",
  max_gpt_intermediate     = "Max ChatGPT level: Intermediate",
  max_gpt_advanced         = "Max ChatGPT level: Advanced"
)

stats_df <- df %>% group_by(branch) %>%
  summarise(across(all_of(varlist), list(mean = ~ mean(.x, na.rm = TRUE), sd   = ~ sd(.x, na.rm = TRUE)), .names = "{col}_{fn}"), .groups = "drop")

long_df <- stats_df %>% pivot_longer(cols = -branch, names_to = c("variable","stat"), names_pattern = "^(.*)_(mean|sd)$") %>%
  mutate(variable = dplyr::recode(variable, !!!var_labels))

wide_stats <- long_df %>% pivot_wider(names_from = stat, values_from = value) %>%
  mutate(mean_sd = ifelse(variable %in% c("Average years of coding experience"),
                          paste0(sprintf("%.3f", mean), "<br>(", sprintf("%.3f", sd), ")"),
                          paste0(sprintf("%.3f", mean), "<br>(", sprintf("%.3f", sd), ")"))) %>%
  dplyr::select(variable, branch, mean_sd) %>% pivot_wider(names_from = branch, values_from = mean_sd)

branches <- levels(df$branch); pairs <- combn(branches, 2, simplify=FALSE)
get_diff <- function(var, b1, b2){
  d1 <- df %>% filter(branch==b1) %>% pull(!!sym(var)); d2 <- df %>% filter(branch==b2) %>% pull(!!sym(var));
  diff <- mean(d1,na.rm=TRUE)-mean(d2,na.rm=TRUE)
  pval <- tryCatch(t.test(d1,d2,var.equal=TRUE)$p.value, error=function(e) NA_real_)
  data.frame(variable = var_labels[[var]], comparison = paste(b1,"vs",b2), diff_mean = diff, p_value = pval)
}
diff_df <- do.call(rbind, lapply(varlist, function(v) do.call(rbind, lapply(pairs, \(pr) get_diff(v, pr[1], pr[2]))))) %>%
  mutate(p_fmt = ifelse(is.na(p_value),"NA", ifelse(p_value<0.001, "\\textless0.001", sprintf("%.3f", p_value))),
         diff_p = paste0(sprintf("%.3f", diff_mean), "<br>\\relax[", p_fmt, "]"), comp_col = gsub(" vs ", "_", comparison)) %>%
  dplyr::select(variable, comp_col, diff_p) %>% pivot_wider(names_from = comp_col, values_from = diff_p)

final_table <- wide_stats %>% left_join(diff_df, by = "variable") %>% mutate(across(everything(), ~ ifelse(is.na(.) | .=="NA<br>(NA)", "-<br>(-)", .)))

colnames(final_table) <- c("\\textbf{Variable}", "\\textbf{Human-Only}", "\\textbf{AI-Assisted}", "\\textbf{AI-Led}",
                           "\\textbf{\\shortstack{Human-Only\\\\vs\\\\AI-Assisted}}",
                           "\\textbf{\\shortstack{Human-Only\\\\vs\\\\AI-Led}}",
                           "\\textbf{\\shortstack{AI-Assisted\\\\vs\\\\AI-Led}}")

final_table_clean <- final_table %>% mutate(across(-names(final_table[1,1]), ~gsub("<br>", "\\\\\\\\", .))) %>%
  mutate(across(-names(final_table[1,1]), ~paste0("\\shortstack{", . ,"}")))

table_body <- kable(final_table_clean, format = "latex", align  = c("l","c","c","c","c","c","c"), escape = FALSE, booktabs = TRUE, linesep = "") %>%
  row_spec(1:(nrow(final_table_clean)-1), extra_latex_after = "[1em]")

dir.create("output/S1/tables", recursive = TRUE, showWarnings = FALSE)
cat(
  "\\begin{table}[ht]\n  \\centering\n  \\caption{Balance of Team-Level Characteristics by Group}\n  \\label{tab:balance_table}\n  {\\scriptsize\n \n",
  table_body, "\n",
  "  \\multicolumn{7}{p{0.9\\textwidth}}{\\textit{Note:} Columns 2--4 present means and standard errors in parentheses for individual groups (Human-only, AI-Assisted, and AI-Led); the difference columns show mean differences and $p$-values in brackets for the indicated group comparisons.}\n",
  "  \\end{table}",
  file = "output/S1/tables/balance.tex"
)

