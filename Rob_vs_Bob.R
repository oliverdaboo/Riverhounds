# --- Rob Vincent vs. Bob Lilley Analysis ---

# Importing Necessary Libraries

library(tidyverse)
library(ggplot2)
library(itscalledsoccer)
library(stringr)
library(gt)

# --- Getting team level statistics for when Bob Lilley was manager
# Creating ASA object
asa<-AmericanSoccerAnalysis$new()

# getting all USL Championship Teams
uslc_teams <- asa$get_teams(leagues = 'uslc')

# getting all USL championship players
uslc_players <- asa$get_players(leagues = 'uslc')

# getting 2025 uslc team xg stats while Bob was manager
uslc_team_xg <- asa$get_team_xgoals(leagues='uslc', start_date='2025-03-07', end_date='2025-10-10')

# joining uslc teams to the team xg dataset
uslc_team_xg <- uslc_team_xg|>
  left_join(uslc_teams|>
              select(team_id, team_name, team_abbreviation), by='team_id')|>
  relocate(team_name, team_abbreviation, .after = 1)

# getting 2025 uslc team xpass while Bob was manager
uslc_team_xpass <- asa$get_team_xpass(leagues='uslc', start_date='2025-03-07', end_date='2025-10-10')

# joining xpass and xg
uslc_bob_analysis <- uslc_team_xg|>
  left_join(uslc_team_xpass|>
              select(-count_games, -competition), by='team_id')

# --- Getting team level statistics for when Rob Vincent was manager

# getting 2025 uslc team xg stats while Rob was manager
uslc_team_xg_rob <- asa$get_team_xgoals(leagues='uslc', start_date='2025-10-10', end_date='2025-11-23')

# joining uslc teams to the team xg dataset
uslc_team_xg_rob <- uslc_team_xg_rob|>
  left_join(uslc_teams|>
              select(team_id, team_name, team_abbreviation), by='team_id')|>
  relocate(team_name, team_abbreviation, .after = 1)

# getting 2025 uslc team xpass while Rob was manager
uslc_team_xpass_rob <- asa$get_team_xpass(leagues='uslc', start_date='2025-10-10', end_date='2025-11-23')

# joining xpass and xg
uslc_rob_analysis <- uslc_team_xg_rob|>
  left_join(uslc_team_xpass_rob|>
              select(-count_games, -competition), by='team_id')

# --- Aggregating every statistic to be "per 90" for comparison ---

# getting number of minutes each manager was managing for
playoff_minutes<-asa$get_team_goals_added(leagues='uslc', season_name=2025, stage_name='Playoffs')

# From the dataframe above, Riverhounds played 561 minutes in the playoffs

regular_season_minutes<-asa$get_team_goals_added(leagues='uslc', season_name=2025, 
                                                 stage_name='Regular Season')
# From the dataframe above, Riverhounds played 3011 minutes in the regular season

# In Fotmob, found that in the 3 matches Rob coached in the Regular season, 
# there were 23 minutes of extra time
# So Rob coached (90 x 3) + 23 = 293 minutes in the Regular season

# Rob coaches 561 + 293 = 854 minutes
# Bob coaches 3011 - 293 = 2718 minutes

# filtering to just Riverhounds and adding minutes column
uslc_bob_analysis <- uslc_bob_analysis|>
  filter(team_name == "Pittsburgh Riverhounds SC")|>
  mutate(minutes = 2718, nineties = minutes/90)

uslc_rob_analysis <- uslc_rob_analysis|>
  filter(team_name == "Pittsburgh Riverhounds SC")|>
  mutate(minutes = 854, nineties = minutes/90)

cols_to_min_level <- c("shots_for", "shots_against", "goals_for", "goals_against", 
                        "xgoals_for", "xgoals_against", "points", "xpoints", 
                        "attempted_passes_for", "passes_completed_over_expected_for", 
                        "attempted_passes_against", "passes_completed_over_expected_against")
uslc_bob_analysis <- uslc_bob_analysis|>
  mutate(across(all_of(cols_to_min_level),
                ~ .x / nineties,
                .names = "per_90_{.col}"))

uslc_rob_analysis <- uslc_rob_analysis|>
  mutate(across(all_of(cols_to_min_level),
                ~ .x / nineties,
                .names = "per_90_{.col}"))

# Pivoting to see just Riverhounds statistics in a vertical format
bob_longer <- uslc_bob_analysis |>
  filter(team_name == "Pittsburgh Riverhounds SC")|>
  pivot_longer(cols=c(starts_with("per_90"), goal_difference, xgoal_difference, 
                 goal_difference_minus_xgoal_difference, pass_completion_percentage_for, 
                 pass_completion_percentage_against, xpass_completion_percentage_for, 
                 xpass_completion_percentage_against, passes_completed_over_expected_p100_for, 
                 passes_completed_over_expected_p100_against, avg_vertical_distance_for, 
                 avg_vertical_distance_against), names_to = "stat", values_to = "val")|>
  rename("Bob" = val)|>
  select(stat, Bob)

rob_longer <- uslc_rob_analysis |>
  pivot_longer(cols=c(starts_with("per_90"), goal_difference, xgoal_difference, 
                      goal_difference_minus_xgoal_difference, pass_completion_percentage_for, 
                      pass_completion_percentage_against, xpass_completion_percentage_for, 
                      xpass_completion_percentage_against, passes_completed_over_expected_p100_for, 
                      passes_completed_over_expected_p100_against, avg_vertical_distance_for, 
                      avg_vertical_distance_against), names_to = "stat", values_to = "val")|>
  rename("Rob" = val)|>
  select(stat, Rob)

manager_analysis <- bob_longer|>
  left_join(rob_longer, by='stat')

# --- Formatting table nicely for a figure ---

# ordering the statistics
custom_order <- c("per_90_shots_for", "per_90_shots_against",
                  "per_90_goals_for", "per_90_goals_against", "goal_difference",
                  "per_90_xgoals_for", "per_90_xgoals_against", "xgoal_difference",
                  "goal_difference_minus_xgoal_difference", "per_90_points", "per_90_xpoints",
                  "per_90_attempted_passes_for", "per_90_attempted_passes_against", 
                  "pass_completion_percentage_for", "pass_completion_percentage_against",
                  "xpass_completion_percentage_for", "xpass_completion_percentage_against",
                  "per_90_passes_completed_over_expected_for", 
                  "per_90_passes_completed_over_expected_against",
                  "avg_vertical_distance_for", "avg_vertical_distance_against",
                  "passes_completed_over_expected_p100_for", 
                  "passes_completed_over_expected_p100_against"
                  )

# Identify for which metrics "lower is better"
lower_is_better <- c(
  "per_90_shots_against", "per_90_goals_against", "per_90_xgoals_against", "per_90_attempted_passes_against",
  "pass_completion_percentage_against", "xpass_completion_percentage_against",
  "per_90_attempted_passes_against", "per_90_passes_completed_over_expected_against",
  "passes_completed_over_expected_p100_against"
)

manager_table <- manager_analysis |>
  # Keep a clean version of 'stat' for logical checks before formatting
  mutate(is_lower_better = stat %in% lower_is_better) |>
  mutate(stat = factor(stat, levels = custom_order)) |>
  arrange(stat) |>
  mutate(
    stat_clean = str_replace_all(as.character(stat), "_", " "),
    stat_clean = str_to_title(stat_clean),
    stat_clean = str_replace_all(stat_clean, "\\bX([a-z,A-Z])", function(x) paste0("x", toupper(substr(x, 2, 2))))
  ) |>
  gt(rowname_col = "stat_clean") |>
  tab_stubhead(label = "Statistic") |>
  # Hide the original 'stat' and 'is_lower_better' columns
  cols_hide(columns = c(stat, is_lower_better)) |>
  cols_label(Bob = "Bob", Rob = "Rob") |>
  fmt_number(columns = c(Bob, Rob), decimals = 2) |>
  
  # HIGHLIGHTING LOGIC FOR BOB
  tab_style(
    style = cell_fill(color = "#C6EFCE"), # Light Green
    locations = cells_body(
      columns = Bob,
      rows = (Bob > Rob & !is_lower_better) | (Bob < Rob & is_lower_better)
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#FFC7CE"), # Light Red
    locations = cells_body(
      columns = Bob,
      rows = (Bob < Rob & !is_lower_better) | (Bob > Rob & is_lower_better)
    )
  ) |>
  
  # HIGHLIGHTING LOGIC FOR ROB
  tab_style(
    style = cell_fill(color = "#C6EFCE"),
    locations = cells_body(
      columns = Rob,
      rows = (Rob > Bob & !is_lower_better) | (Rob < Bob & is_lower_better)
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#FFC7CE"),
    locations = cells_body(
      columns = Rob,
      rows = (Rob < Bob & !is_lower_better) | (Rob > Bob & is_lower_better)
    )
  ) |>
  
  # Final Styling
  tab_options(
    column_labels.font.weight = "bold",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    stub.border.color = "black"
  )

# Save the table
manager_table |> gtsave("manager_comparison.png", expand = 10)
  