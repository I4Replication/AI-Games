############################################################
## FULL SCRIPT – outputs a simple LaTeX table like sample ##
############################################################
## 2. Read data -------------------------------------------------
df <- data.frame(readRDS("data/AI games.rds"))

## 3. Basic prep ------------------------------------------------
df$branch <- as.factor(df$branch)
branch_labels <- c("1" = "Human-Only",
                   "2" = "AI-Assisted",
                   "3" = "AI-Led")

df$branch <- forcats::fct_recode(df$branch, !!!branch_labels)

varlist <- c("reproduction",        "time2_reproduction",
             "minor_errors",        "time2_first_minor",
             "major_errors",        "time2_first_major",
             "one_good_robustness", "two_good_robustness",
             "ran_one_robustness",  "ran_two_robustness")

df[varlist] <- lapply(df[varlist], as.numeric)

var_labels <- c(
  reproduction          = "Reproduction",
  time2_reproduction    = "Minutes to reproduction",
  minor_errors          = "Number of minor errors",
  time2_first_minor     = "Minutes to first minor error",
  major_errors          = "Number of major errors",
  time2_first_major     = "Minutes to first major error",
  one_good_robustness   = "At least one good robustness check",
  two_good_robustness   = "At least two good robustness checks",
  ran_one_robustness    = "Ran at least one good robustness check",
  ran_two_robustness    = "Ran at least two good robustness checks"
)

# ========== Function to produce table ==============
make_latex_table <- function(df, suffix = "", caption_add = "", label_add = "") {
  # Means & SDs
  stats_df <- df %>%
    group_by(branch) %>%
    summarise(across(all_of(varlist),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x,   na.rm = TRUE)),
                     .names = "{col}_{fn}"),
              .groups = "drop")
  
  long_df <- stats_df %>%
    pivot_longer(
      cols         = -branch,
      names_to     = c("variable", "stat"),
      names_pattern = "^(.*)_(mean|sd)$"
    ) %>%
    mutate(
      variable = dplyr::recode(variable, !!!var_labels)   
    )
  
  wide_stats <- long_df %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(mean_sd = ifelse(
      variable %in% c("Minutes to reproduction",
                      "Minutes to first minor error",
                      "Minutes to first major error"),
      paste0(sprintf("%.1f", mean), "<br>(", sprintf("%.1f", sd), ")"),
      paste0(sprintf("%.3f", mean), "<br>(", sprintf("%.3f", sd), ")")
    )) %>%
    dplyr::select(variable, branch, mean_sd) %>%
    pivot_wider(names_from = branch, values_from = mean_sd)
  
  # Pair-wise differences & p-values
  branches     <- levels(df$branch)
  branch_pairs <- combn(branches, 2, simplify = FALSE)
  
  get_diff <- function(var, b1, b2){
    d1 <- df %>% filter(branch == b1) %>% pull(!!sym(var))
    d2 <- df %>% filter(branch == b2) %>% pull(!!sym(var))
    diff <- pval <- NA_real_
    if(length(na.omit(d1))>1 && length(na.omit(d2))>1){
      tmp  <- tryCatch(t.test(d1,d2,var.equal=TRUE), error=function(e) NULL)
      if(!is.null(tmp)){
        diff <- mean(d1,na.rm=TRUE)-mean(d2,na.rm=TRUE)
        pval <- tmp$p.value
      }
    }
    data.frame(variable   = var_labels[[var]],
               comparison = paste(b1,"vs",b2),
               diff_mean  = diff,
               p_value    = pval,
               stringsAsFactors=FALSE)
  }
  
  diff_df <- do.call(rbind,
                     lapply(varlist, function(v)
                       do.call(rbind, lapply(branch_pairs,
                                             \(pair) get_diff(v, pair[1], pair[2])))))
  
  diff_df <- diff_df %>%
    mutate(
      p_fmt = ifelse(
        is.na(p_value), "NA",
        ifelse(p_value < 0.001, "\\textless0.001",
               sprintf("%.3f", p_value))
      ),
      diff_p = ifelse(
        variable %in% c("Minutes to reproduction",
                        "Minutes to first minor error",
                        "Minutes to first major error"),
        paste0(sprintf("%.1f", diff_mean), "<br>(", p_fmt, ")"),
        paste0(sprintf("%.3f", diff_mean), "<br>(", p_fmt, ")")
      ),
      comp_col = gsub(" vs ", "_", comparison)
    ) %>%
    dplyr::select(variable, comp_col, diff_p) %>%
    pivot_wider(names_from = comp_col, values_from = diff_p)
  
  # Merge means/SDs and differences
  final_table <- wide_stats %>%
    left_join(diff_df, by="variable") %>%
    mutate(across(everything(),
                  ~ ifelse(is.na(.) | . == "NA<br>(NA)", "-<br>(-)", .)))
  
  # Simple column names
  colnames(final_table) <- c("\\textbf{Variable}", 
                             "\\textbf{Human-Only}", 
                             "\\textbf{AI-Assisted}", 
                             "\\textbf{AI-Led}", 
                             "\\textbf{\\shortstack{Human-Only\\\\vs\\\\AI-Assisted}}", 
                             "\\textbf{\\shortstack{Human-Only\\\\vs\\\\AI-Led}}", 
                             "\\textbf{\\shortstack{AI-Assisted\\\\vs\\\\AI-Led}}")
  
  # Convert <br> to \\, wrap in \shortstack
  final_table_clean <- final_table %>%
    mutate(across(-names(final_table[1,1]), ~gsub("<br>", "\\\\\\\\", .))) %>%
    mutate(across(-names(final_table[1,1]), ~paste0("\\shortstack{", . ,"}")))
  
  # Build LaTeX body with kableExtra
  table_body <- kable(final_table_clean,
                      format = "latex",
                      align  = c("l", "c", "c", "c", "c", "c", "c"),
                      escape = FALSE,
                      booktabs = TRUE,
                      linesep = "") %>%
    row_spec(1:(nrow(final_table_clean)-1), extra_latex_after = "[1em]")
  
  # Write the LaTeX table file
  cat(
    "\\begin{table}[ht]
      \\centering
      \\caption{Comparison of Human, AI-Assisted, and AI-Led Metrics", caption_add, "}\n",
    paste0("\\label{tab:comparison_metrics", label_add, "}\n"),
    "{\\scriptsize\n",
    table_body, "\n",
    "\\multicolumn{7}{p{0.9\\textwidth}}{\\textit{Note:} Standard errors in parentheses for individual branches (Human-only, AI-Assisted, and AI-Led); p-values in parentheses for branch comparisons (Human-Only vs AI-Assisted, Human-Only vs AI-Led, and AI-Assisted vs AI-Led).}}\n",
    "\\end{table}",
    file = paste0("output/tables/branches", suffix, ".tex"))
}

# =========== TABLE 1: All Games ===================
make_latex_table(df, suffix = "", caption_add = "", label_add = "_third")

# =========== TABLE 2: Excluding game 9 ============
df_no9 <- df %>% filter(game != "Virtual 2025")
make_latex_table(df_no9, suffix = "_s1", caption_add = " (Study 1)", label_add = "_third_s1")