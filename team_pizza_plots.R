# Importing Necessary Libararies

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

# getting 2025 uslc team xg during the regular season
uslc_team_xg <- asa$get_team_xgoals(leagues='uslc', season_name=2025, stage_name='Regular Season')

# joining uslc teams to the team xg dataset
uslc_team_xg <- uslc_team_xg|>
  left_join(uslc_teams|>
              select(team_id, team_name, team_abbreviation), by='team_id')|>
  relocate(team_name, team_abbreviation, .after = 1)

# getting 2025 uslc team xpass during the regular season
uslc_team_xpass <- asa$get_team_xpass(leagues='uslc', season_name=2025, stage_name='Regular Season')

# joining xpass and xg
uslc_team_xg <- uslc_team_xg|>
  left_join(uslc_team_xpass|>
              select(-count_games, -competition), by='team_id')

# getting 2025 uslc team g+ during the regular season
uslc_team_goals_added <- asa$get_team_goals_added(leagues='uslc', season_name=2025, stage_name='Regular Season')

# --- extracting full team level g+ from the individual team data frames inside the goals_added datset ---

# getting team totals for the g+ values
uslc_team_goals_added <- uslc_team_goals_added|>
  mutate(
    total_goals_added_for = map_dbl(data, ~sum(.x$goals_added_for, na.rm = TRUE)),
    total_goals_added_against = map_dbl(data, ~sum(.x$goals_added_against, na.rm = TRUE)),
    total_num_actions_for = map_dbl(data, ~sum(.x$num_actions_for, na.rm = TRUE)),
    total_num_actions_against=map_dbl(data, ~sum(.x$num_actions_against, na.rm=TRUE))
  )

# getting team totals for the action type and g+ values
expanded_uslc <- uslc_team_goals_added |>
  select(team_id, data) |> 
  unnest(data)

# 2. Pivot to wide format to get columns for each action type
wide_goals_added_team <- expanded_uslc |>
  pivot_wider(
    names_from = action_type,
    values_from = c(goals_added_for, goals_added_against, 
                    num_actions_for, num_actions_against),
    names_sep = "_",
    values_fn = sum
  )

uslc_team_goals_added <- uslc_team_goals_added|>
  left_join(wide_goals_added_team, by='team_id')

# joining g+ with the other main set to culminate in a big analysis dataset
uslc_team_analysis <- uslc_team_xg|>
  left_join(uslc_team_goals_added|>
              select(-data, -competition), by='team_id')

# --- Constructing a pizza plot with the goals added and action type
  
goals_added_pizza<- function(team){
  # cleaning the data in order to make the pizza plot
  data <- uslc_team_analysis|>
    select(team_name, starts_with("goals_added_for"))|>
    # Shorten the names for the plot labels
    rename_with(~str_remove(., "goals_added_for_"), starts_with("goals_added_for_")) |>
    # Ordering to look clean in final plot
    relocate(Dribbling, .before = Fouling)|>
    relocate(Shooting, .before = Fouling) |>
    # Calculate percentiles for each metric across the league
    mutate(across(-team_name, ~round(percent_rank(.) * 100, 0), .names = "perc_{.col}"))
  # Filter data for the specific team
  team_df <- data |>
    filter(team_name == team) |>
    select(team_name, starts_with("perc_")) |>
    pivot_longer(cols = starts_with("perc_"), 
                 names_to = "Statistic", 
                 values_to = "Percentile") |>
    mutate(
      Statistic = str_remove(Statistic, "perc_"),
      type = case_when(
        Statistic %in% c("Passing", "Receiving", "Dribbling") ~ "In Possession",
        Statistic %in% c("Interrupting", "Fouling", "Claiming") ~ "Defending/GK",
        TRUE ~ "Shooting"
      ),
      index = row_number()
    )
  # Create the plot
  p <- ggplot(data = team_df, aes(x = reorder(Statistic, index), y = Percentile, fill = type)) +
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +  
    geom_label(aes(label = Percentile), color = "gray20", fill = "oldlace", 
               size = 2.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("#034694", "red", "gold")) +
    scale_y_continuous(limits = c(-15, 110)) + 
    labs(
      title = paste(team, "Goals Added Profile"),
      subtitle = "2025 USLC Regular Season | Percentile Rank vs League",
      caption = "Data: American Soccer Analysis",
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "oldlace", color = "oldlace"),
      panel.background = element_rect(fill = "oldlace", color = "oldlace"),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(face = "bold", size = 8, colour = "gray20"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 9)
    )
  
  # Create a clean filename (e.g., "Pittsburgh_Riverhounds_SC_g+_pizza.png")
  clean_name <- gsub(" ", "_", team)
  file_path <- paste0("pizza_plots/", clean_name, "_g+_pizza.png")
  
  # Save the plot
  ggsave(file_path, plot = p, height = 7, width = 7, dpi = 300)
  
  message(paste("Saved plot for:", team))
}

goals_added_pizza("Pittsburgh Riverhounds SC")

# --- Constructing a pizza plot with the passing statistics

passing_pizza <- function(team) {
  # 1. Clean data AND assign it back to 'data'
  data <- uslc_team_analysis |>
    select(team_name, 
           attempted_passes_for, 
           pass_completion_percentage_for, 
           xpass_completion_percentage_for, 
           avg_vertical_distance_for, 
           passes_completed_over_expected_p100_for, 
           goals_added_for_Passing) |>
    rename(
      `Pass Volume` = attempted_passes_for,
      `Pass %` = pass_completion_percentage_for,
      `Expected Pass %` = xpass_completion_percentage_for,
      `Pass Score (p100)` = passes_completed_over_expected_p100_for,
      `Verticality` = avg_vertical_distance_for,
      `g+ Passing` = goals_added_for_Passing
    ) |>
    mutate(across(-team_name, ~round(percent_rank(.) * 100, 0), .names = "perc_{.col}"))
  
  # 2. Filter for specific team and pivot
  team_df <- data |>
    filter(team_name == team) |>
    select(team_name, starts_with("perc_")) |>
    pivot_longer(cols = starts_with("perc_"), 
                 names_to = "Statistic", 
                 values_to = "Percentile") |>
    mutate(
      Statistic = str_remove(Statistic, "perc_"),
      type = case_when(
        Statistic %in% c("Pass Volume", "Pass %", "Expected Pass %") ~ "Volume/Accuracy",
        Statistic %in% c("Pass Score (p100)", "g+ Passing") ~ "Efficiency",
        Statistic == "Verticality" ~ "Style",
        TRUE ~ "Other"
      ),
      # ADDED: Create the index for reordering the plot slices
      index = row_number()
    )
  
  # 3. Create the plot
  p <- ggplot(data = team_df, aes(x = reorder(Statistic, index), y = Percentile, fill = type)) +
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +  
    geom_label(aes(label = Percentile), color = "gray20", fill = "oldlace", 
               size = 2.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("#034694", "red", "gold")) +
    scale_y_continuous(limits = c(-15, 110)) + 
    labs(
      title = paste(team, "Passing Profile"),
      subtitle = "2025 USLC Regular Season | Percentile Rank vs League",
      caption = "Data: American Soccer Analysis",
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "oldlace", color = "oldlace"),
      panel.background = element_rect(fill = "oldlace", color = "oldlace"),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(face = "bold", size = 8, colour = "gray20"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 9)
    )
  
  # 4. Save Logic
  if (!dir.exists("pizza_plots")) dir.create("pizza_plots")
  clean_name <- gsub(" ", "_", team)
  file_path <- paste0("pizza_plots/", clean_name, "_passing_pizza.png")
  
  ggsave(file_path, plot = p, height = 7, width = 7, dpi = 300)
  message(paste("Saved plot for:", team))
}

passing_pizza("Pittsburgh Riverhounds SC")

# --- Constructing a pizza plot with overall team statistics
overall_pizza <- function(team) {
  # 1. Clean data AND assign it back to 'data'
  data <- uslc_team_analysis |>
    mutate(goals_for_minus_xgoals_for = goals_for - xgoals_for,
           goals_against_minus_xgoals_against = goals_against - xgoals_against,
           shots_for_goal_conversion = goals_for/shots_for,
           shots_against_goal_conversion = goals_against/shots_against,
           points_minus_xpoints = points - xpoints,
           total_goals_added_for_minus_total_goals_added_against = total_goals_added_for - total_goals_added_against)|>
    select(team_name, 
           goals_for_minus_xgoals_for, 
           shots_for_goal_conversion,
           goals_against_minus_xgoals_against, 
           shots_against_goal_conversion,
           points_minus_xpoints,
           total_goals_added_for_minus_total_goals_added_against) |>
    rename(
      `Shooting Performance (G - xG)` = goals_for_minus_xgoals_for,
      `Defensive Performance (GA - xGA)` = goals_against_minus_xgoals_against,
      `Shot to Goal %` = shots_for_goal_conversion,
      `Shot Against to Goal %` = shots_against_goal_conversion,
      `Under/Over Performance (P - xP)` = points_minus_xpoints,
      `G+ Performance (G+ - GA+)` = total_goals_added_for_minus_total_goals_added_against
    ) |>
    mutate(across(-team_name, ~round(percent_rank(.) * 100, 0), .names = "perc_{.col}"))
  
  # 2. Filter for specific team and pivot
  team_df <- data |>
    filter(team_name == team) |>
    select(team_name, starts_with("perc_")) |>
    pivot_longer(cols = starts_with("perc_"), 
                 names_to = "Statistic", 
                 values_to = "Percentile") |>
    mutate(
      Statistic = str_remove(Statistic, "perc_"),
      type = case_when(
        Statistic %in% c("Shooting Performance (G - xG)", "Shot to Goal %") ~ "Offensive Efficiency",
        Statistic %in% c("Defensive Performance (GA - xGA)", "Shot Against to Goal %") ~ "Defensive Efficiency",
        TRUE ~ "Overall Performance"
      ),
      # ADDED: Create the index for reordering the plot slices
      index = row_number()
    )
  
  # 3. Create the plot
  p <- ggplot(data = team_df, aes(x = reorder(Statistic, index), y = Percentile, fill = type)) +
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +  
    geom_label(aes(label = Percentile), color = "gray20", fill = "oldlace", 
               size = 2.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("#034694", "red", "gold")) +
    scale_y_continuous(limits = c(-15, 110)) + 
    labs(
      title = paste(team, "Passing Profile"),
      subtitle = "2025 USLC Regular Season | Percentile Rank vs League",
      caption = "Data: American Soccer Analysis",
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "oldlace", color = "oldlace"),
      panel.background = element_rect(fill = "oldlace", color = "oldlace"),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(face = "bold", size = 8, colour = "gray20"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 9)
    )
  
  # 4. Save Logic
  if (!dir.exists("pizza_plots")) dir.create("pizza_plots")
  clean_name <- gsub(" ", "_", team)
  file_path <- paste0("pizza_plots/", clean_name, "_overall_pizza.png")
  
  ggsave(file_path, plot = p, height = 7, width = 7, dpi = 300)
  message(paste("Saved plot for:", team))
}

overall_pizza("Pittsburgh Riverhounds SC")