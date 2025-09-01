# S1-only Kaplan–Meier figures to output/S1/figures

library(dplyr)
library(ggsurvfit)
library(ggplot2)
library(survival)

main <- readRDS("data/AI games.rds") |>
  filter(game != "Virtual 2025") |>
  group_by(branch) |>
  mutate(branch_team_n = row_number()) |>
  ungroup()

varlist <- c("time2_reproduction", "time2_first_minor", "time2_first_major")
xtitles <- c(
  time2_reproduction = "Minutes to reproduction",
  time2_first_minor  = "Minutes to first minor error",
  time2_first_major  = "Minutes to first major error"
)

fill_colors <- c("Human-Only" = "#0072B2", "AI-Assisted" = "#D55E00", "AI-Led" = "#009E73")

dir.create("output/S1/figures", showWarnings = FALSE, recursive = TRUE)

for (var in varlist) {
  df <- main |>
    mutate(event = ifelse(!is.na(.data[[var]]), 1, 0), time  = ifelse(is.na(.data[[var]]), 420, .data[[var]])) |>
    dplyr::select(branch_team_n, branch, time, event)
  survmod <- survfit2(Surv(time, event) ~ branch, data = df)
  p <- ggsurvfit(survmod, type = "risk") +
    add_confidence_interval() +
    labs(x = xtitles[[var]], y = "Cumulative proportion") +
    scale_color_manual(values = fill_colors) +
    scale_fill_manual(values = fill_colors) +
    ylim(0, 1) + theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), plot.background  = element_rect(fill = "white", colour = NA), legend.position  = "top", legend.title     = element_blank())
  ggsave(sprintf("output/S1/figures/%s.pdf", var), plot = p, width = 8, height = 4)
  ggsave(sprintf("output/S1/figures/%s (s1).pdf", var), plot = p, width = 8, height = 4)
}
