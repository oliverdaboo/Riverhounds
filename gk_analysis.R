# Importing Necessary Libraries

library(tidyverse)
library(ggplot2)
library(itscalledsoccer)

# Creating ASA object
asa<-AmericanSoccerAnalysis$new()

# getting all USL Championship Teams
uslc_teams <- asa$get_teams(leagues = 'uslc')

# getting 2025 uslc team xg stats during the regular season
uslc_team_xg <- asa$get_team_xgoals(leagues='uslc', season_name=2025, stage_name='Regular Season')

# joining uslc teams to the team xg dataset
uslc_team_stats <- uslc_team_xg|>
  left_join(uslc_teams|>
              select(team_id, team_name, team_abbreviation), by='team_id')|>
  relocate(team_name, team_abbreviation, .after = 1)

# getting all USL championship players
uslc_players <- asa$get_players(leagues = 'uslc')

# getting 2025 uslc gk xg stats during the regular season
uslc_gk_xg <- asa$get_goalkeeper_xgoals(leagues='uslc', season_name=2025, stage_name='Regular Season')

# getting 2025 uslc gk g+ stats during the regular season
uslc_gk_goals_added <- asa$get_goalkeeper_goals_added(leagues='uslc', season_name=2025, stage_name='Regular Season')

# getting gk totals for g+
uslc_gk_goals_added <- uslc_gk_goals_added|>
  mutate(
    total_goals_added = map_dbl(data, ~sum(.x$goals_added_raw, na.rm = TRUE)),
    total_goals_added_above_avg= map_dbl(data, ~sum(.x$goals_added_above_avg, na.rm = TRUE)),
    total_num_actions = map_dbl(data, ~sum(.x$count_actions, na.rm = TRUE))
  )

# getting gk totals for action type g+
expanded_gk <- uslc_gk_goals_added|>
  select(player_id, data) |> 
  unnest(data)

wide_gk <- expanded_gk |>
  pivot_wider(
    names_from = action_type,
    values_from = c(goals_added_raw, goals_added_above_avg, 
                    count_actions),
    names_sep = "_",
    values_fn = sum
  )

uslc_gk_goals_added <- uslc_gk_goals_added|>
  left_join(wide_gk, by='player_id')

# joining gk xg and g+ stats together
uslc_gk_stats<-uslc_gk_xg|>
  left_join(uslc_gk_goals_added|>
              select(-data, -competition, -minutes_played, -team_id), by='player_id')

# joining gk stats to get player and team names
uslc_gk_stats <- uslc_gk_stats |>
  left_join(uslc_players, by="player_id")|>
  relocate(any_of(names(uslc_players)))

uslc_gk_stats <- uslc_gk_stats |>
  unnest(team_id) |>
  left_join(uslc_teams |>
              select(-team_short_name), by='team_id')|>
  relocate(any_of(names(uslc_teams)))

# Filtering to gk who played at least 10 full matches and faced at least 40 shots
uslc_gk_stats <- uslc_gk_stats|>
  filter(minutes_played >= 900,
         shots_faced >= 40)

# adding above replacement stat to the dataset
uslc_gk_goals_added_rep <- asa$get_goalkeeper_goals_added(leagues='uslc', 
                                                          season_name=2025, 
                                                          stage_name='Regular Season',
                                                          above_replacement=TRUE)
uslc_gk_stats <- uslc_gk_stats|>
  left_join(uslc_gk_goals_added_rep|>
              select(player_id, goals_added_above_replacement), by='player_id')

# --- Add Weighted Performance Score to main dataframe ---
uslc_gk_stats <- uslc_gk_stats |>
  # 1. Calculate League-Wide Percentiles
  mutate(
    perc_Shotstopping = round(percent_rank(goals_added_above_avg_Shotstopping) * 100, 0),
    perc_Claiming     = round(percent_rank(goals_added_above_avg_Claiming) * 100, 0),
    perc_Handling     = round(percent_rank(goals_added_above_avg_Handling) * 100, 0),
    perc_Sweeping     = round(percent_rank(goals_added_above_avg_Sweeping) * 100, 0),
    perc_Passing      = round(percent_rank(goals_added_above_avg_Passing) * 100, 0),
    perc_Replacement  = round(percent_rank(goals_added_above_replacement) * 100, 0),
    perc_G_xG         = round((1 - percent_rank(goals_minus_xgoals_gk)) * 100, 0)
  ) |>
  # 2. Apply Weighting Logic
  mutate(
    gk_performance_grade = round(
      ((perc_G_xG + perc_Shotstopping)/2 * 0.50) + 
        ((perc_Claiming + perc_Handling)/2 * 0.20) + 
        ((perc_Passing + perc_Sweeping)/2 * 0.20) + 
        (perc_Replacement * 0.10), 0
    )
  )

compare_gk_pizza <- function(player1, player2) {
  
  # --- 1. Prepare Plotting Data ---
  # We pull from uslc_gk_stats which already has the grades and percentiles
  plot_df <- uslc_gk_stats |>
    filter(player_name %in% c(player1, player2)) |>
    select(
      player_name, 
      weighted_score = gk_performance_grade, 
      # Selecting the perc_ columns you created in your main script
      perc_G_xG, perc_Shotstopping, perc_Claiming, 
      perc_Handling, perc_Sweeping, perc_Passing, 
      `perc_Above Replacement` = perc_Replacement
    ) |>
    pivot_longer(
      cols = starts_with("perc_"), 
      names_to = "Statistic", 
      values_to = "Percentile"
    ) |>
    mutate(
      # Clean up the names for display
      Statistic = str_remove(Statistic, "perc_"),
      Statistic = str_replace(Statistic, "G_xG", "G-xG"),
      Statistic = str_replace(Statistic, "Above Replacement", "Above Replacement"),
      # Set order and category logic
      Statistic = factor(Statistic, levels = c(
        "Above Replacement", "G-xG", "Shotstopping", 
        "Claiming", "Handling", "Sweeping", "Passing"
      )),
      Category = case_when(
        Statistic == "Above Replacement" ~ "Total Value",
        Statistic %in% c("G-xG", "Shotstopping") ~ "Shotstopping",
        Statistic %in% c("Claiming", "Handling") ~ "Aerial/Command",
        TRUE ~ "Proactive/Dist"
      )
    )
  
  # --- 2. Create the Plot ---
  p <- ggplot(plot_df, aes(x = Statistic, y = Percentile, fill = Category)) +
    geom_bar(aes(y = 100), stat = "identity", width = 1, alpha = 0.1, fill = "gray20", color = "oldlace") +
    geom_bar(stat = "identity", width = 1, color = "oldlace") +
    coord_curvedpolar() +
    facet_wrap(~player_name) +
    
    # Label in the Center Hole (Using data from one statistic to prevent multi-layering)
    geom_text(data = filter(plot_df, Statistic == "Above Replacement"),
              aes(x = 0.5, y = -18, label = paste0("Score\n", weighted_score)), 
              size = 4, fontface = "bold", color = "gray20", inherit.aes = FALSE) +
    
    geom_label(aes(label = Percentile), size = 2.5, fontface = "bold", fill = "oldlace", show.legend = FALSE) +
    scale_fill_manual(values = c("#1B4D3E", "gold", "red", "#034694")) +
    scale_y_continuous(limits = c(-25, 115)) +
    labs(
      title = paste0(player1, " vs ", player2),
      subtitle = "2025 USLC Regular Season | Ranks relative to all League GKs",
      caption = "Weights: 50% Shotstopping, 20% Aerial, 20% Proactive/Dist, 10% Replacement Value",
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(face = "bold", size = 9),
      strip.text = element_text(face = "bold", size = 14),
      plot.background = element_rect(fill = "oldlace", color = "oldlace"),
      legend.position = "top",
      legend.title = element_blank()
    )
  
  # --- 3. Save Logic ---
  if (!dir.exists("pizza_plots")) dir.create("pizza_plots")
  last_name1 <- stringr::word(player1, -1)
  last_name2 <- stringr::word(player2, -1)
  file_path <- paste0("pizza_plots/", last_name1, "_", last_name2, "_comparison.png")
  
  ggsave(file_path, plot = p, height = 7, width = 11, dpi = 300)
}

compare_gk_pizza("Eric Dick", "Nico Campuzano")

