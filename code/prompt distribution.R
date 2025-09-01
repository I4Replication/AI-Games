# S1-filtered prompt distribution for AI-Assisted, output to output/S1/figures

library(dplyr)
library(ggplot2)
library(patchwork)

main <- readRDS("data/AI games.rds") %>% filter(game != "Virtual 2025", branch == "AI-Assisted")

vars <- c("prompts", "files", "images", "words")

plots <- lapply(vars, function(v) {
  ggplot(main, aes(x = .data[[v]])) +
    geom_density(na.rm = TRUE, adjust = 1) +
    labs(x = v, y = "Density") +
    theme_minimal(base_size = 13)
})

combined <- (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]])

dir.create("output/S1/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/S1/figures/prompt distribution.pdf", combined, width = 8, height = 4)

