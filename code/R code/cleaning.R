# ---- 1. Load and clean main data ----
main <- read_excel("data/AI games.xlsx", sheet = "Sheet1") %>% 
  clean_names()

# Drop team that didn't do anything
main <- main %>%
  filter(!(tolower(game) == "cornell" & tolower(team) == "4r"))

# Proper-case selected variables
vars_to_proper <- c("game", "branch", "paper", "software", "attendance", "max_skill", "min_skill", "min_gpt", "max_gpt", "min_coding", "max_coding")
main <- main %>%
  mutate(across(all_of(vars_to_proper), ~str_to_title(.)))

# Rename and create substrings
main <- main %>%
  rename(paper_game = paper) %>%
  mutate(
    paper = str_sub(paper_game, 9),
    paper_game = str_sub(paper_game, 1, 7)
  )

# Recode branch names
main <- main %>%
  mutate(branch = case_when(
    branch == "Human" ~ "Human-Only",
    branch == "Cyborg" ~ "AI-Assisted",
    branch == "Machine" ~ "AI-Led",
    TRUE ~ branch
  ))

# Normalize 'Virtual' game names
main <- main %>%
  mutate(
    game2 = case_when(
      game %in% c("Virtual Europe", "Virtual North America") ~ "Virtual",
      TRUE ~ game
    )
  )

# Factorize variables with labels
main <- main %>%
  mutate(
    game = factor(game, levels = c("Toronto", "Ottawa", "Sheffield", "Cornell", "Bogota", "Tilburg", "Virtual Europe", "Virtual North America", "Virtual 2025")),
    game2 = factor(game2, levels = c("Toronto", "Ottawa", "Sheffield", "Cornell", "Bogota", "Tilburg", "Virtual", "Virtual 2025")),
    min_coding = factor(min_coding, levels = c("Novice", "Advanced Beginner", "Competent", "Proficient", "Expert")),
    max_coding = factor(max_coding, levels = c("Novice", "Advanced Beginner", "Competent", "Proficient", "Expert")),
    min_skill = factor(min_skill, levels = c("Student", "Researcher", "Postdoc", "Professor")),
    max_skill = factor(max_skill, levels = c("Student", "Researcher", "Postdoc", "Professor")),
    min_gpt = factor(min_gpt, levels = c("Never", "Beginner", "Intermediate", "Advanced")),
    max_gpt = factor(max_gpt, levels = c("Never", "Beginner", "Intermediate", "Advanced")),
    branch = factor(branch, levels = c("Human-Only", "AI-Assisted", "AI-Led")),
    software = factor(software, levels = c("Stata", "R")),
    attendance = factor(attendance, levels = c("Virtual", "In-Person"))
  )

# ---- 2. Time corrections (Sheffield & Bogota started late) ----

# Subtract 15 minutes from time columns only for Sheffield & Bogota
main <- main %>%
  mutate(
    time_reproduction = if_else(game %in% c("Sheffield", "Bogota"),
                                time_reproduction - minutes(15),
                                time_reproduction),
    time_first_minor = if_else(game %in% c("Sheffield", "Bogota"),
                               time_first_minor - minutes(15),
                               time_first_minor),
    time_first_major = if_else(game %in% c("Sheffield", "Bogota"),
                               time_first_major - minutes(15),
                               time_first_major)
  )

# Calculate minutes since 9:00 AM base time
base_time <- as.POSIXct("1899-12-31 09:00:00", tz = "UTC")
main <- main %>%
  mutate(
    time2_reproduction = as.numeric(difftime(time_reproduction, base_time, units = "mins")),
    time2_first_minor  = as.numeric(difftime(time_first_minor, base_time, units = "mins")),
    time2_first_major  = as.numeric(difftime(time_first_major, base_time, units = "mins"))
  )

# ---- 3. Recode missing values in errors and robustness ----

main <- main %>%
  mutate(
    minor_errors = replace_na(as.numeric(minor_errors), 0),
    major_errors = replace_na(as.numeric(major_errors), 0),
    good_robustness = replace_na(as.numeric(good_robustness), 0),
    ran_robustness = replace_na(as.numeric(ran_robustness), 0),
    combined_coding = as.numeric(combined_coding),
    followup_responces = as.numeric(followup_responces)
  )

main <- main %>%
  mutate(
    one_good_robustness = good_robustness >= 1,
    two_good_robustness = good_robustness == 2,
    ran_one_robustness = ran_robustness >= 1 & one_good_robustness,
    ran_two_robustness = ran_robustness == 2 & two_good_robustness,
    combined_follow = combined_coding / followup_responces
  )

# ---- 4. Order columns ----

main <- main %>%
  dplyr::select(game, branch, team, software, paper_game, paper, number_teammates, attendance, max_skill, min_skill,
                reproduction, time_reproduction, time2_reproduction, minor_errors, time_first_minor, time2_first_minor,
                major_errors, time_first_major, time2_first_major, good_robustness, one_good_robustness, two_good_robustness,
                ran_robustness, ran_one_robustness, ran_two_robustness, everything())

# ---- 5. Load follow data and drop N-Y columns ----

follow <- read_excel("data/AI Games - Prompts Information.xlsx", sheet = "Sheet1") %>%
  clean_names()

# Drop columns from N to Y (inclusive), matching Stata drop N-Y
n_start <- which(names(follow) == "n")
n_end   <- which(names(follow) == "y")
if (length(n_start) == 1 && length(n_end) == 1 && n_start <= n_end) {
  follow <- follow[, -c(n_start:n_end)]
}

# Recode NAs as 0 in relevant columns
follow <- follow %>%
  mutate(across(c(number_of_prompts, number_of_files, number_of_images, number_of_words), ~replace_na(., 0)))

follow <- follow %>%
  mutate(game = str_to_title(game))

# Drop team that didn't do anything
follow <- follow %>%
  filter(!(tolower(game) == "cornell" & tolower(team) == "4r"))

# Collapse (sum) by game and team
agg <- follow %>%
  group_by(game, team) %>%
  summarise(
    prompts = sum(number_of_prompts, na.rm = TRUE),
    files = sum(number_of_files, na.rm = TRUE),
    images = sum(number_of_images, na.rm = TRUE),
    words = sum(number_of_words, na.rm = TRUE),
    .groups = "drop"
  )

# Factor game to match earlier labels
agg <- agg %>%
  mutate(game2 = factor(game, levels = c("Toronto", "Ottawa", "Sheffield", "Cornell", "Bogota", "Tilburg", "Virtual", "Virtual 2025")))

# ---- 6. Merge follow data into main ----
main <- main %>%
  left_join(agg, by = c("game2" = "game2", "team" = "team"))

main <- main %>%
  rename(game = game.x) %>%
  dplyr::select(-game.y)

# Recode NAs as 0 in prompts/files/images/words
main <- main %>%
  mutate(
    prompts = replace_na(prompts, 0),
    files = replace_na(files, 0),
    images = replace_na(images, 0),
    words = replace_na(words, 0)
  )

factor_vars <- c(
  "game", "game2", "branch", "software", "attendance",
  "max_skill", "min_skill", "max_gpt", "min_gpt", "max_coding", "min_coding",
  "reproduction"
)

main <- main %>%
  mutate(across(all_of(factor_vars), as.factor))

main <- main %>%
  mutate(
    reproduction = case_when(
      tolower(as.character(reproduction)) == "yes" ~ 1,
      tolower(as.character(reproduction)) == "no"  ~ 0,
      TRUE ~ NA_real_
    )
  )

# ---- 8. Save dataframe in RDS ----
saveRDS(main, file = "data/AI games.rds")