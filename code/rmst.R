# S1-only RMST table to output/S1/tables/rmst.tex

library(dplyr)
library(xtable)
library(survRM2)

main <- readRDS("data/AI games.rds") %>%
  filter(game != "Virtual 2025") %>%
  group_by(branch) %>% mutate(branch_team_n = row_number()) %>% ungroup()

vars <- c("time2_reproduction", "time2_first_minor", "time2_first_major")
tau  <- 420

lab <- c(
  time2_reproduction = "Minutes to reproduction",
  time2_first_minor  = "Minutes to first minor error",
  time2_first_major  = "Minutes to first major error"
)

ext <- function(x) {
  data.frame(m_A = x$RMST.arm1$rmst[1], se_A = x$RMST.arm1$rmst[2],
             m_B = x$RMST.arm0$rmst[1], se_B = x$RMST.arm0$rmst[2],
             diff = x$unadjusted.result[1,1], p = x$unadjusted.result[1,4])
}
fs   <- function(x) formatC(signif(x, 3), digits = 3, format = "fg")
fp   <- function(p) ifelse(p < .001, "<0.001", formatC(round(p, 3), digits = 3, format = "f"))
cell <- function(m, s) sprintf("\\shortstack{%s\\\\(%s)}", fs(m), fs(s))
cDif <- function(d, p) sprintf("\\shortstack{%s\\\\(%s)}", fs(d), fp(p))

out <- purrr::map_dfr(vars, function(v) {
  df <- main %>%
    mutate(event = ifelse(!is.na(.data[[v]]), 1, 0), time  = ifelse(is.na(.data[[v]]),  tau, .data[[v]])) %>%
    dplyr::select(branch, time, event)
  mk <- function(a, b) { tmp <- df %>% filter(branch %in% c(a, b)) %>% mutate(arm = branch == a); ext(rmst2(tmp$time, tmp$event, tmp$arm, tau = tau)) }
  h_ai  <- mk("Human-Only",  "AI-Assisted")
  h_li  <- mk("Human-Only",  "AI-Led")
  ai_li <- mk("AI-Assisted", "AI-Led")
  tibble::tibble(
    Variable                = lab[[v]],
    `Human only`            = cell(h_ai$m_A,  h_ai$se_A),
    `AI-Assisted`           = cell(h_ai$m_B,  h_ai$se_B),
    `AI-Led`                = cell(h_li$m_B,  h_li$se_B),
    `Human vs AI-Assisted`  = cDif(h_ai$diff,     h_ai$p),
    `Human vs AI-Led`       = cDif(h_li$diff,     h_li$p),
    `AI-Assisted vs AI-Led` = cDif(ai_li$diff,    ai_li$p)
  )
})

names(out) <- c(
  "Variable",
  "Human only",
  "AI-Assisted",
  "AI-Led",
  "\\shortstack{Human\\\\vs\\\\AI-Assisted}",
  "\\shortstack{Human\\\\vs\\\\AI-Led}",
  "\\shortstack{AI-Assisted\\\\vs\\\\AI-Led}"
)

xt <- xtable(out, caption = "Restricted-mean time without success (minutes) and contrasts", label = "tab:rmst")
align(xt) <- c("l", "l", rep("c", 6))

add.lines <- list(
  pos = list(-1, 0, nrow(out)),
  command = c("\\hline\\hline\n","\\hline\\hline\n",
              paste0("\\hline\\hline\n","\\multicolumn{", ncol(out),
                     "}{p{0.6\\textwidth}}{\\it Note:~Each cell shows the mean RMST in minutes with the standard error below in parentheses. Contrast columns present the mean difference with its two-sided ",
                     "\\emph{p}-value below. Times are right-censored at 420 minutes (7 hours).}\n"))
)

dir.create("output/S1/tables", showWarnings = FALSE, recursive = TRUE)
tex_lines <- capture.output(print.xtable(xt, include.rownames = FALSE, sanitize.text.function = identity, hline.after = NULL, add.to.row = add.lines, caption.placement = "top"))
tbl_line <- grep("^\\\\begin\\{table\\}", tex_lines)[1]
tex_lines <- append(tex_lines, "\\tiny", after = tbl_line)
writeLines(tex_lines, "output/S1/tables/rmst.tex")

