############################################################
# Density plots – Stata-style kernel estimates in R      
############################################################
# ---- 1. Read data ----
main <- readRDS("data/AI games.rds")
main <- main %>% filter(branch == "AI-Assisted")

# ---- 2. Variables for which we want densities ----
vars <- c("prompts", "files", "images", "words")

# ---- 3. Make one ggplot2 density plot per variable ----
density_plots <- lapply(
  vars,
  function(v) {
    ggplot(main, aes(x = .data[[v]])) +
      geom_density(na.rm = TRUE, adjust = 1) +     # ‘adjust’ is like Stata’s bandwidth scalar
      labs(
        x = v,
        y = "Density"
      ) +
      theme_minimal(base_size = 13)
  }
)

# ---- 4. Display the four plots in a 2x2 grid ----
(density_plots[[1]] | density_plots[[2]]) /
  (density_plots[[3]] | density_plots[[4]])

ggsave("output/figures/prompt distribution.pdf", (density_plots[[1]] | density_plots[[2]]) /
         (density_plots[[3]] | density_plots[[4]]), width = 8, height = 4)