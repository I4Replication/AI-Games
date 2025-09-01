# S1-only reproduction and error difference figures, to output/S1/figures

library(dplyr)
library(tidyr)
library(ggplot2)

main <- readRDS("data/AI games.rds")

df_summary <- main %>%
  filter(game != "Virtual 2025") %>%
  group_by(game2, branch) %>%
  summarise(
    reproduction = mean(reproduction, na.rm = TRUE),
    minor_errors = mean(minor_errors, na.rm = TRUE),
    major_errors = mean(major_errors, na.rm = TRUE),
    .groups = "drop"
  )

df_wide <- df_summary %>%
  tidyr::pivot_wider(
    names_from = branch,
    values_from = c(reproduction, minor_errors, major_errors),
    names_sep = ""
  ) %>%
  mutate(
    rep_machine   = `reproductionHuman-Only` - `reproductionAI-Led`,
    rep_cyorg     = `reproductionHuman-Only` - `reproductionAI-Assisted`, 
    minor_machine = `minor_errorsHuman-Only` - `minor_errorsAI-Led`,
    minor_cyorg   = `minor_errorsHuman-Only` - `minor_errorsAI-Assisted`,
    major_machine = `major_errorsHuman-Only` - `major_errorsAI-Led`,
    major_cyorg   = `major_errorsHuman-Only` - `major_errorsAI-Assisted`
  )

game2_labels <- c(
  "Toronto" = "Toronto\n(Feb)",
  "Ottawa" = "Ottawa\n(May)",
  "Sheffield" = "Sheffield\n(Jun)",
  "Cornell" = "Cornell\n(Aug)",
  "Bogota" = "Bogota\n(Oct)",
  "Tilburg" = "Tilburg\n(Oct)",
  "Virtual" = "Virtual\n(Nov)"
)
df_wide$game2 <- factor(as.character(df_wide$game2), levels = names(game2_labels), labels = unname(game2_labels))

dir.create("output/S1/figures", recursive = TRUE, showWarnings = FALSE)

p1 <- ggplot(df_wide, aes(x = game2)) +
  geom_line(aes(y = rep_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = rep_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2) +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(-0.5, 0, 0.5, 1), limits = c(-0.5, 1)) +
  labs(y = "Difference in reproduction rate", x = "AI game", color = "", title = "", subtitle = "", caption = "") +
  theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white", color = NA), legend.position = "top", legend.title = element_blank())
ggsave("output/S1/figures/reproduction rates.pdf", p1, width = 8, height = 4)

p2 <- ggplot(df_wide, aes(x = game2)) +
  geom_line(aes(y = minor_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = minor_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  labs(y = "Differences in the number of\nminor errors detected", x = "AI game", color = "", title = "", subtitle = "", caption = "") +
  theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white", color = NA), legend.position = "top", legend.title = element_blank())
ggsave("output/S1/figures/minor errors.pdf", p2, width = 8, height = 4)

p3 <- ggplot(df_wide, aes(x = game2)) +
  geom_line(aes(y = major_machine, group = 1, color = "Human-Only Vs AI-Led"), size = 1.2) +
  geom_line(aes(y = major_cyorg, group = 1, color = "Human-Only Vs AI-Assisted"), size = 1.2) +
  scale_color_manual(values = c("Human-Only Vs AI-Led" = "#0072B2", "Human-Only Vs AI-Assisted" = "#D55E00")) +
  labs(y = "Differences in the number of\nmajor errors detected", x = "AI game", color = "", title = "", subtitle = "", caption = "") +
  theme_minimal(base_size = 13) + theme(panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white", color = NA), legend.position = "top", legend.title = element_blank())
ggsave("output/S1/figures/major errors.pdf", p3, width = 8, height = 4)

# Raw series (same naming as originals, under S1)
branch_cols <- c("Human-Only" = "#009E73", "AI-Assisted" = "#D55E00", "AI-Led" = "#0072B2")
df_summary <- main %>% group_by(game2, branch) %>%
  summarise(reproduction = mean(reproduction, na.rm=TRUE),
            minor_errors = mean(minor_errors, na.rm=TRUE),
            major_errors = mean(major_errors, na.rm=TRUE), .groups = "drop")
df_summary$game2 <- factor(as.character(df_summary$game2), levels = names(game2_labels), labels = unname(game2_labels))
# drop any rows that could not be mapped to a known label (avoids NA tick)
df_summary <- df_summary %>% filter(!is.na(game2))
df_summary$branch <- factor(df_summary$branch, levels = c("Human-Only","AI-Assisted","AI-Led"))

make_raw_plot <- function(data, yvar, ylab, percent = FALSE) {
  # drop unused factor levels to avoid NA tick marks
  data <- droplevels(data)
  ggplot(data, aes(x = game2, y = .data[[yvar]], group = branch, colour = branch)) +
    geom_line(size = 1.2) + geom_point(size = 2) +
    scale_colour_manual(values = branch_cols, name = "") +
    scale_x_discrete(drop = TRUE) +
    {if (percent) scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) else NULL} +
    labs(y = ylab, x = "AI game", title = "", subtitle = "", caption = "") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white", colour = NA), legend.position = "top", legend.title = element_blank())
}

p1_raw  <- make_raw_plot(df_summary, "reproduction", "Reproduction rate", TRUE)
ggsave("output/S1/figures/reproduction rates (raw).pdf", p1_raw, width = 8, height = 4)
p1_raw_s1 <- make_raw_plot(df_summary, "reproduction", "Reproduction rate", TRUE)
ggsave("output/S1/figures/reproduction rates (raw, s1).pdf", p1_raw_s1, width = 8, height = 4)

p2_raw <- make_raw_plot(df_summary, "minor_errors", "Number of minor errors")
ggsave("output/S1/figures/minor errors (raw).pdf", p2_raw, width = 8, height = 4)
p2_raw_s1 <- make_raw_plot(df_summary, "minor_errors", "Number of minor errors")
ggsave("output/S1/figures/minor errors (raw, s1).pdf", p2_raw_s1, width = 8, height = 4)

p3_raw <- make_raw_plot(df_summary, "major_errors", "Number of major errors")
ggsave("output/S1/figures/major errors (raw).pdf", p3_raw, width = 8, height = 4)
p3_raw_s1 <- make_raw_plot(df_summary, "major_errors", "Number of major errors")
ggsave("output/S1/figures/major errors (raw, s1).pdf", p3_raw_s1, width = 8, height = 4)
