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
      type = factor(type, levels = c("In Possession", "Shooting", "Defending/GK")),
      index = row_number()
    )
  # Create the plot
  p <- ggplot(data = team_df, aes(x = reorder(Statistic, index), y = Percentile, fill = type)) +
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +  
    geom_label(aes(label = Percentile), color = "gray20", fill = "oldlace", 
               size = 2.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("gold","#034694","red")) +
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
           passes_completed_over_expected_p100_for, 
           goals_added_for_Passing,
           avg_vertical_distance_for) |>
    rename(
      `Pass Volume` = attempted_passes_for,
      `Pass Completion %` = pass_completion_percentage_for,
      `Expected Pass Completion %` = xpass_completion_percentage_for,
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
        Statistic %in% c("Pass Volume", "Pass Completion %", "Expected Pass Completion %") ~ "Volume/Accuracy",
        Statistic %in% c("Pass Score (p100)", "g+ Passing") ~ "Efficiency",
        Statistic == "Verticality" ~ "Style",
        TRUE ~ "Other"
      ),
      type = factor(type, levels = c("Volume/Accuracy", "Efficiency", "Style")),
      index = row_number()
    )
  
  # 3. Create the plot
  p <- ggplot(data = team_df, aes(x = reorder(Statistic, index), y = Percentile, fill = type)) +
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +  
    geom_label(aes(label = Percentile), color = "gray20", fill = "oldlace", 
               size = 2.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("#228B22", "orange", "purple")) +
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
    mutate(shots_for_goal_conversion = goals_for/shots_for,
           shots_against_goal_conversion = goals_against/shots_against,
           passes_per_shot_for = attempted_passes_for / shots_for,
           passes_per_shot_against = attempted_passes_against / shots_against,
           points_minus_xpoints = points - xpoints,
           total_goals_added_for_minus_total_goals_added_against = total_goals_added_for - total_goals_added_against)|>
    select(team_name, 
           xgoals_for, 
           shots_for_goal_conversion,
           passes_per_shot_for,
           xgoals_against, 
           shots_against_goal_conversion,
           passes_per_shot_against,
           points_minus_xpoints,
           total_goals_added_for_minus_total_goals_added_against) |>
    rename(
      `Chance Creation (xG)` = xgoals_for,
      `Chance Creation Against (xGA)` = xgoals_against,
      `Shot to Goal %` = shots_for_goal_conversion,
      `Shot Against to Goal %` = shots_against_goal_conversion,
      'Passes per Shot' = passes_per_shot_for,
      'Passes per Shot Against' = passes_per_shot_against,
      `Under/Over Performance (P - xP)` = points_minus_xpoints,
      `G+ Performance (G+ - GA+)` = total_goals_added_for_minus_total_goals_added_against
    ) |>
    mutate(
      # Standard "Higher is Better" (Offensive)
      across(
        c(`Chance Creation (xG)`, `Shot to Goal %`, `Passes per Shot`, `Passes per Shot Against`,
          `Under/Over Performance (P - xP)`, `G+ Performance (G+ - GA+)`), 
        ~round(percent_rank(.) * 100, 0), 
        .names = "perc_{.col}"
      ),
      # "Lower is Better" (Defensive - We use 1 minus the rank)
      across(
        c(`Chance Creation Against (xGA)`, `Shot Against to Goal %`), 
        ~round((1 - percent_rank(.)) * 100, 0), 
        .names = "perc_{.col}"
      )
    )
  # Explicitly Define the Order of Stats
  stat_order <- c(
    "Under/Over Performance (P - xP)", "G+ Performance (G+ - GA+)",  # Overall
    "Chance Creation (xG)", "Shot to Goal %", "Passes per Shot",  # Offensive
    "Chance Creation Against (xGA)", "Shot Against to Goal %", "Passes per Shot Against" # Defensive
  )
  
  # 2. Filter for specific team and pivot
  team_df <- data |>
    filter(team_name == team) |>
    select(team_name, starts_with("perc_")) |>
    pivot_longer(cols = starts_with("perc_"), 
                 names_to = "Statistic", 
                 values_to = "Percentile") |>
    mutate(
      Statistic = str_remove(Statistic, "perc_"),
      # Convert to factor using our predefined order
      Statistic = factor(Statistic, levels = stat_order),
      type = case_when(
        Statistic %in% stat_order[1:2] ~ "Overall Performance",
        Statistic %in% stat_order[3:5] ~ "Offensive Style",
        Statistic %in% stat_order[6:8] ~ "Defensive Style"
      ),
      # Set 'type' as a factor to fix color mapping order
      type = factor(type, levels = c("Overall Performance", "Offensive Style", "Defensive Style"))
    ) |>
    arrange(Statistic)
  
  # 3. Create the plot
  p <- ggplot(data = team_df, aes(x = Statistic, y = Percentile, fill = type)) +
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +  
    geom_label(aes(label = Percentile), color = "gray20", fill = "oldlace", 
               size = 2.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("gold", "#034694", "red")) + # Fixed order: Overall, Off, Def 
    scale_y_continuous(limits = c(-15, 110)) + 
    labs(
      title = paste(team, "Overall Profile"),
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