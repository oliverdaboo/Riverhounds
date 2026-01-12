# --- Striker Analysis ---

# Importing Necessary Libraries

library(tidyverse)
library(ggplot2)
library(itscalledsoccer)
library(geomtextpath) # For coord_curvedpolar in pizza plots

# Creating ASA object
asa<-AmericanSoccerAnalysis$new()

# getting all USL Championship Teams
uslc_teams <- asa$get_teams(leagues = 'uslc')

# getting all USL championship players
uslc_players <- asa$get_players(leagues = 'uslc')

# getting ST xgoals for the 2025 regular season and played at least 450 minutes and 20 shots
uslc_st_xg <- asa$get_player_xgoals(leagues = 'uslc', season_name = 2025, stage_name = "Regular Season", 
                                    minimum_minutes = 450, minimum_shots = 20, general_position = 'ST')
# Joining xg with players and teams
uslc_st_analysis <- uslc_st_xg|>
  left_join(uslc_players|>
              select(player_id, player_name, birth_date, nationality), by="player_id")|>
  relocate(any_of(names(uslc_players)))

uslc_st_analysis <- uslc_st_analysis|>
  unnest(team_id) |>
  left_join(uslc_teams |>
              select(-team_short_name), by='team_id')|>
  relocate(any_of(names(uslc_teams)))

uslc_st_goals_added_repl <- asa$get_player_goals_added(leagues = 'uslc', season_name = 2025, 
                                                  stage_name = "Regular Season", 
                                                  minimum_minutes = 450,  general_position = 'ST',
                                                  above_replacement = TRUE)
uslc_st_goals_added <- asa$get_player_goals_added(leagues = 'uslc', season_name = 2025, 
                                                       stage_name = "Regular Season", 
                                                       minimum_minutes = 450,  general_position = 'ST')

# getting st totals for g+
uslc_st_goals_added <- uslc_st_goals_added|>
  mutate(
    total_goals_added = map_dbl(data, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg= map_dbl(data, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_num_actions = map_dbl(data, ~sum(.x$count_actions, na.rm = TRUE))
  )

# getting st totals for action type g+
expanded_st <- uslc_st_goals_added|>
  select(player_id, data) |> 
  unnest(data)

wide_st <- expanded_st |>
  pivot_wider(
    names_from = action_type,
    values_from = c(goals_added_raw, goals_added_above_avg, 
                    count_actions),
    names_sep = "_",
    values_fn = sum
  )

uslc_st_goals_added <- uslc_st_goals_added|>
  left_join(wide_st, by='player_id')

# adding above replacement value to goals added
uslc_st_goals_added <- uslc_st_goals_added|>
  left_join(uslc_st_goals_added_repl|>
              select(player_id, goals_added_above_replacement), by='player_id')

# joining goals added to main analysis
uslc_st_analysis <- uslc_st_analysis|>
  left_join(uslc_st_goals_added|>
              select(-data, -competition, -minutes_played, -general_position, -team_id),
            by='player_id')

# --- Aggregating certain stats to Per 90 or per shot ---
cols_to_p90 <- c("shots", "shots_on_target", "goals", "xgoals", "key_passes", 
                 "primary_assists", "xassists", "total_goals_added",
                 "goals_added_raw_Dribbling", "goals_added_raw_Passing", 
                 "goals_added_raw_Receiving", "goals_added_raw_Shooting")
# per 90
uslc_st_analysis <- uslc_st_analysis |>
  mutate(nineties = minutes_played/90)|>
  mutate(across(all_of(cols_to_p90), 
                ~ .x / nineties, 
                .names = "per_90_{.col}"))

# per shot
uslc_st_analysis <- uslc_st_analysis |>
  mutate(
    xg_per_shot = xgoals / shots,
    xplace_per_shot = xplace / shots,
    goals_minus_xgoals_per_shot = goals_minus_xgoals / shots,
    shots_on_target_per_shot = shots_on_target / shots
  )
# dropping unnecessary columns for analyzing strikers
uslc_st_analysis <- uslc_st_analysis|>
  select(-ends_with(c("Interrupting", "Fouling")))

# --- Creating Comparison Pizza Plots and Weighted Scores for Strikers ---

# --- Step 1: Calculate Percentiles ---
uslc_st_analysis <- uslc_st_analysis |>
  mutate(across(
    c(per_90_goals, per_90_xgoals, xg_per_shot, xplace_per_shot,
      goals_minus_xgoals_per_shot, per_90_goals_added_raw_Shooting,
      per_90_primary_assists, per_90_key_passes, per_90_goals_added_raw_Passing,
      per_90_goals_added_raw_Receiving, per_90_goals_added_raw_Dribbling, 
      goals_added_above_replacement),
    ~ round(percent_rank(.x) * 100, 0),
    .names = "perc_{.col}"
  )) |>
  mutate(
    # Attacking Grade (6 Components)
    attacking_grade = round(
      (perc_per_90_goals * 0.25) + (perc_per_90_xgoals * 0.25) + 
        (perc_xg_per_shot * 0.15) + (perc_xplace_per_shot * 0.15) + 
        (perc_goals_minus_xgoals_per_shot * 0.10) + (perc_per_90_goals_added_raw_Shooting * 0.10), 0),
    
    # Creation/Value Grade (6 Components)
    creation_grade = round(
      (perc_per_90_primary_assists * 0.30) + (perc_per_90_key_passes * 0.20) + 
        (perc_per_90_goals_added_raw_Passing * 0.20) + (perc_per_90_goals_added_raw_Receiving * 0.10) + 
        (perc_per_90_goals_added_raw_Dribbling * 0.10) + (perc_goals_added_above_replacement * 0.10), 0),
    
    # Overall Score (70/30 Weighting)
    overall_grade = round((attacking_grade * 0.70) + (creation_grade * 0.30), 0)
  )

compare_st_pizza <- function(player1, player2) {
  
  # The 12 exact stats from your weighting logic
  stats_to_plot <- c(
    "perc_per_90_goals", "perc_per_90_xgoals", "perc_xg_per_shot", 
    "perc_xplace_per_shot", "perc_goals_minus_xgoals_per_shot", "perc_per_90_goals_added_raw_Shooting",
    "perc_per_90_primary_assists", "perc_per_90_key_passes", "perc_per_90_goals_added_raw_Passing",
    "perc_per_90_goals_added_raw_Receiving", "perc_per_90_goals_added_raw_Dribbling", 
    "perc_goals_added_above_replacement"
  )
  
  plot_df <- uslc_st_analysis |>
    filter(player_name %in% c(player1, player2)) |>
    select(player_name, attacking_grade, creation_grade, overall_grade, all_of(stats_to_plot)) |>
    pivot_longer(cols = all_of(stats_to_plot), names_to = "stat", values_to = "value") |>
    mutate(
      stat = str_remove(stat, "perc_"),
      stat = str_replace_all(stat, "_", " "),
      stat = str_to_title(stat),
      stat = str_replace_all(stat, "Goals Added", "G+"), # Fix strings being too long
      stat = str_replace_all(stat, "\\bXg", "xG"), # Fix xG casing
      stat = str_replace_all(stat, "xGoals", "xG"),
      stat = str_wrap(stat, width = 10),
      Category = case_when(
        str_detect(stat, "Assists|Passes|Passing|Receiving|Dribbling|Replacement") ~ "Creation & Value",
        TRUE ~ "Attacking"
      )
    )|>
    # This puts Attacking on the Right and Creation on the Left
    arrange(Category, stat) |>
    mutate(stat = factor(stat, levels = unique(stat)))
  
  p <- ggplot(plot_df, aes(x = stat, y = value, fill = Category)) +
    # Background "Full" pizza slices
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    # Actual performance slices
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +
    facet_wrap(~player_name) +
    
    # Displaying all 3 Weighted Grades in the center
    geom_text(data = filter(plot_df, stat == unique(plot_df$stat)[1]),
              aes(x = 0.5, y = -38, label = paste0("Overall: ", overall_grade, 
                                                   "\nAttack: ", attacking_grade, 
                                                   "\nCreation/Value: ", creation_grade)), 
              size = 3.5, fontface = "bold", color = "gray20", inherit.aes = FALSE) +
    
    geom_label(aes(label = value), size = 2.5, fontface = "bold", fill = "oldlace", show.legend = FALSE) +
    scale_fill_manual(values = c("#D90429", "#00B4D8")) +
    scale_y_continuous(limits = c(-50, 110)) + # Larger center hole for grades
    labs(
      title = "USLC Striker Comparison: Weighted Profiles",
      subtitle = "2025 Season | Percentiles vs all USLC Strikers",
      caption = "Overall Score = 70% Attacking & 30% Creation\nData: American Soccer Analysis",
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(face = "bold", size = 8.5),
      strip.text = element_text(face = "bold", size = 14),
      plot.background = element_rect(fill = "oldlace", color = "oldlace"),
      legend.position = "top",
      legend.title = element_blank()
    )
  
  # Save Logic 
  if (!dir.exists("pizza_plots")) dir.create("pizza_plots")
  last_name1 <- stringr::word(player1, -1)
  last_name2 <- stringr::word(player2, -1)
  file_path <- paste0("pizza_plots/", last_name1, "_", last_name2, "_comparison.png")
  
  ggsave(file_path, plot = p, height = 7, width = 11, dpi = 300)
}

compare_st_pizza("Albert Dikwa", "Augustine Williams")
compare_st_pizza("Albert Dikwa", "Oalex Anderson")
compare_st_pizza("Albert Dikwa", "Rémi Cabral")

uslc_st_analysis|>
  select(player_name, team_abbreviation, attacking_grade, creation_grade, overall_grade)|>
  arrange(desc(overall_grade))
