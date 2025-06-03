################################################################################
## Kaplan–Meier (risk-curve) figures for the AI-Games study                   ##
################################################################################
# ---- 1. Read & prepare data ---------------------------------------------------
main <- readRDS("data/AI games.rds") |>
  group_by(branch) |>
  mutate(branch_team_n = row_number()) |>
  ungroup()

# ---- 2. Build a named list with the two datasets ------------------------------
#   • all          : full sample (your original plots)
#   • all_noV2025  : sample excluding game == "Virtual 2025"
datasets <- list(
  all         = main,
  all_noV2025 = main |> filter(game != "Virtual 2025")
)

# ---- 3. Variables & axis labels ----------------------------------------------
varlist <- c("time2_reproduction", "time2_first_minor", "time2_first_major")
xtitles <- c(
  time2_reproduction = "Minutes to reproduction",
  time2_first_minor  = "Minutes to first minor error",
  time2_first_major  = "Minutes to first major error"
)

# ---- 4. Aesthetics ------------------------------------------------------------
fill_colors <- c("Human-Only" = "#0072B2",
                 "AI-Assisted" = "#D55E00",
                 "AI-Led"      = "#009E73")

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# ---- 5. Loop over datasets *and* variables ------------------------------------
for (dname in names(datasets)) {
  
  dat <- datasets[[dname]]
  suffix <- ifelse(dname == "all", "", " (s1)")  # file-name tag
  
  for (var in varlist) {
    
    # a. Prep survival frame (censor at 420 min)
    df <- dat |>
      mutate(event = ifelse(!is.na(.data[[var]]), 1, 0),
             time  = ifelse(is.na(.data[[var]]), 420, .data[[var]])) |>
      dplyr::select(branch_team_n, branch, time, event)
    
    # b. Fit Kaplan-Meier
    survmod <- survfit2(Surv(time, event) ~ branch, data = df)
    
    # c. Draw
    p <- ggsurvfit(survmod, type = "risk") +
      add_confidence_interval() +
      labs(
        x = xtitles[[var]],
        y = "Cumulative proportion"
      ) +
      scale_color_manual(values = fill_colors) +
      scale_fill_manual(values = fill_colors) +
      ylim(0, 1) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", colour = NA),
        legend.position  = "top",
        legend.title     = element_blank()
      )
    
    # d. Save
    ggsave(
      sprintf("output/figures/%s%s.pdf", var, suffix),
      plot   = p,
      width  = 8,
      height = 4
    )
  }
}