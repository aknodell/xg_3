######
## load data
####

# Output from my scraper, but similar format to evolving-hockey's pbp query
# tool. https://github.com/aknodell/nhlPbpScrapeR

pred_xg_shot_data <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202402", full.names = T) |>
  purrr::map(
    function(f) {
      f |>
        readr::read_csv(
          col_select = c(
            game_id:game_seconds, event_type, event_detail_1, event_detail_2,
            event_detail_3,
            event_team, home_team, away_team,
            coords_x, coords_y, event_team_zone,
            home_skaters_on, away_skaters_on, home_goalie, away_goalie,
            home_skater_strength_state
          ),
          col_types = readr::cols(
            event_type = readr::col_character(),
            event_detail_1 = readr::col_character(),
            event_detail_2 = readr::col_character(),
            event_detail_3 = readr::col_character(),
            event_team = readr::col_character(),
            home_team = readr::col_character(),
            away_team = readr::col_character(),
            event_team_zone = readr::col_character(),
            home_skater_strength_state = readr::col_character(),
            .default = readr::col_integer()
          )
        ) |>
        dplyr::filter(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK"), game_period != 5) |>
        dplyr::mutate(
          event_detail_1 = tidyr::replace_na(event_detail_1, ""),
          event_detail_2 = tidyr::replace_na(event_detail_2, ""),
          event_detail_3 = tidyr::replace_na(event_detail_3, ""),
          tmp = event_detail_1,
          event_detail_1 = ifelse(event_detail_2 == "Own Goal", event_detail_2, event_detail_1),
          event_detail_2 = ifelse(event_detail_1 == "Own Goal", tmp, event_detail_2),
          event_detail_1 = ifelse(event_detail_1 == "", "Awarded", event_detail_1),
          home_score_diff =
            cumsum(event_type == "GOAL" & (event_team == home_team)) -
            cumsum(event_type == "GOAL" & (event_team == away_team))
        ) |>
        dplyr::select(-tmp)
    }
  ) |>
  dplyr::bind_rows()


################################################################################
##########################  Data Cleaning  #####################################
################################################################################

# each shot type follows the same pattern for fenwicks, sog, and goals, coords
# are adjusted so that y is the vertical distance from the goal line with the
# goal line being 0 and x is the distance from the center making locations from
# the goalie's perspective

#####
## tips and deflections
###

tip_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Tip In", "Deflected")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(att = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

tip_att_smoothed <- tip_att

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(tip_att))) {
  for (col in seq(length(tip_att))) {
    tip_att_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, tip_att[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, tip_att[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, tip_att[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, tip_att[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, tip_att[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, tip_att[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, tip_att[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, tip_att[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, tip_att[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, tip_att[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, tip_att[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, tip_att[row, col + 1] / 2) +
         ##
         tip_att[row, col])
  }
}

tip_sog <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type %in% c("SHOT", "GOAL")) |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Tip In", "Deflected")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(sog = dplyr::tally()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(sog = tidyr::replace_na(sog, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = sog) |>
  tibble::column_to_rownames(var = "y")

tip_sog_smoothed <- tip_sog

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(tip_sog))) {
  for (col in seq(length(tip_sog))) {
    tip_sog_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, tip_sog[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, tip_sog[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, tip_sog[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, tip_sog[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, tip_sog[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, tip_sog[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, tip_sog[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, tip_sog[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, tip_sog[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, tip_sog[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, tip_sog[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, tip_sog[row, col + 1] / 2) +
         ##
         tip_sog[row, col])
  }
}

tip_g <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type == "GOAL") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Tip In", "Deflected")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(g = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(g = tidyr::replace_na(g, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = g) |>
  tibble::column_to_rownames(var = "y")

tip_g_smoothed <- tip_g

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(tip_g))) {
  for (col in seq(length(tip_g))) {
    tip_g_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, tip_g[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, tip_g[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, tip_g[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, tip_g[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, tip_g[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, tip_g[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, tip_g[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, tip_g[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, tip_g[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, tip_g[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, tip_g[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, tip_g[row, col + 1] / 2) +
         ##
         tip_g[row, col])
  }
}

#####
## slapshots
###

slap_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Slap") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(att = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

slap_att_smoothed <- slap_att

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(slap_att))) {
  for (col in seq(length(slap_att))) {
    slap_att_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, slap_att[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, slap_att[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, slap_att[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, slap_att[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, slap_att[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, slap_att[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, slap_att[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, slap_att[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, slap_att[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, slap_att[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, slap_att[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, slap_att[row, col + 1] / 2) +
         ##
         slap_att[row, col])
  }
}

slap_sog <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type %in% c("SHOT", "GOAL")) |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Slap") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(sog = dplyr::tally()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(sog = tidyr::replace_na(sog, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = sog) |>
  tibble::column_to_rownames(var = "y")

slap_sog_smoothed <- slap_sog

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(slap_sog))) {
  for (col in seq(length(slap_sog))) {
    slap_sog_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, slap_sog[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, slap_sog[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, slap_sog[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, slap_sog[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, slap_sog[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, slap_sog[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, slap_sog[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, slap_sog[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, slap_sog[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, slap_sog[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, slap_sog[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, slap_sog[row, col + 1] / 2) +
         ##
         slap_sog[row, col])
  }
}

slap_g <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type == "GOAL") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Slap") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(g = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(g = tidyr::replace_na(g, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = g) |>
  tibble::column_to_rownames(var = "y")

slap_g_smoothed <- slap_g

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(slap_g))) {
  for (col in seq(length(slap_g))) {
    slap_g_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, slap_g[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, slap_g[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, slap_g[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, slap_g[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, slap_g[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, slap_g[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, slap_g[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, slap_g[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, slap_g[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, slap_g[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, slap_g[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, slap_g[row, col + 1] / 2) +
         ##
         slap_g[row, col])
  }
}

#####
## snap shots
###

snap_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Snap") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(att = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

snap_att_smoothed <- snap_att

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(snap_att))) {
  for (col in seq(length(snap_att))) {
    snap_att_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, snap_att[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, snap_att[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, snap_att[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, snap_att[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, snap_att[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, snap_att[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, snap_att[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, snap_att[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, snap_att[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, snap_att[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, snap_att[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, snap_att[row, col + 1] / 2) +
         ##
         snap_att[row, col])
  }
}

snap_sog <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type %in% c("SHOT", "GOAL")) |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Snap") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(sog = dplyr::tally()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(sog = tidyr::replace_na(sog, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = sog) |>
  tibble::column_to_rownames(var = "y")

snap_sog_smoothed <- snap_sog

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(snap_sog))) {
  for (col in seq(length(snap_sog))) {
    snap_sog_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, snap_sog[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, snap_sog[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, snap_sog[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, snap_sog[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, snap_sog[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, snap_sog[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, snap_sog[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, snap_sog[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, snap_sog[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, snap_sog[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, snap_sog[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, snap_sog[row, col + 1] / 2) +
         ##
         snap_sog[row, col])
  }
}

snap_g <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type == "GOAL") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Snap") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(g = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(g = tidyr::replace_na(g, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = g) |>
  tibble::column_to_rownames(var = "y")

snap_g_smoothed <- snap_g

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(snap_g))) {
  for (col in seq(length(snap_g))) {
    snap_g_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, snap_g[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, snap_g[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, snap_g[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, snap_g[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, snap_g[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, snap_g[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, snap_g[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, snap_g[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, snap_g[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, snap_g[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, snap_g[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, snap_g[row, col + 1] / 2) +
         ##
         snap_g[row, col])
  }
}

#####
## wrist shots
###

wrist_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Wrist") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(att = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

wrist_att_smoothed <- wrist_att

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(wrist_att))) {
  for (col in seq(length(wrist_att))) {
    wrist_att_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, wrist_att[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, wrist_att[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, wrist_att[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, wrist_att[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, wrist_att[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, wrist_att[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, wrist_att[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, wrist_att[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, wrist_att[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, wrist_att[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, wrist_att[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, wrist_att[row, col + 1] / 2) +
         ##
         wrist_att[row, col])
  }
}

wrist_sog <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type %in% c("SHOT", "GOAL")) |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Wrist") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(sog = dplyr::tally()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(sog = tidyr::replace_na(sog, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = sog) |>
  tibble::column_to_rownames(var = "y")

wrist_sog_smoothed <- wrist_sog

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(wrist_sog))) {
  for (col in seq(length(wrist_sog))) {
    wrist_sog_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, wrist_sog[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, wrist_sog[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, wrist_sog[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, wrist_sog[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, wrist_sog[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, wrist_sog[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, wrist_sog[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, wrist_sog[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, wrist_sog[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, wrist_sog[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, wrist_sog[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, wrist_sog[row, col + 1] / 2) +
         ##
         wrist_sog[row, col])
  }
}

wrist_g <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type == "GOAL") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Wrist") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(g = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(g = tidyr::replace_na(g, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = g) |>
  tibble::column_to_rownames(var = "y")

wrist_g_smoothed <- wrist_g

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(wrist_g))) {
  for (col in seq(length(wrist_g))) {
    wrist_g_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, wrist_g[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, wrist_g[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, wrist_g[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, wrist_g[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, wrist_g[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, wrist_g[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, wrist_g[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, wrist_g[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, wrist_g[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, wrist_g[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, wrist_g[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, wrist_g[row, col + 1] / 2) +
         ##
         wrist_g[row, col])
  }
}

#####
## Backhands
###

back_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Backhand") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(att = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

back_att_smoothed <- back_att

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(back_att))) {
  for (col in seq(length(back_att))) {
    back_att_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, back_att[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, back_att[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, back_att[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, back_att[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, back_att[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, back_att[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, back_att[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, back_att[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, back_att[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, back_att[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, back_att[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, back_att[row, col + 1] / 2) +
         ##
         back_att[row, col])
  }
}

back_sog <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type %in% c("SHOT", "GOAL")) |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Backhand") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(sog = dplyr::tally()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(sog = tidyr::replace_na(sog, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = sog) |>
  tibble::column_to_rownames(var = "y")

back_sog_smoothed <- back_sog

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(back_sog))) {
  for (col in seq(length(back_sog))) {
    back_sog_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, back_sog[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, back_sog[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, back_sog[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, back_sog[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, back_sog[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, back_sog[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, back_sog[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, back_sog[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, back_sog[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, back_sog[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, back_sog[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, back_sog[row, col + 1] / 2) +
         ##
         back_sog[row, col])
  }
}

back_g <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type == "GOAL") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 == "Backhand") |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(g = dplyr::n()) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(g = tidyr::replace_na(g, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = g) |>
  tibble::column_to_rownames(var = "y")

back_g_smoothed <- back_g

# smoothing process a weighted sum of count for all coords within 2 feet of the
# recorded location, weighted by the reciprocal of (distance + 1)
for (row in seq(nrow(back_g))) {
  for (col in seq(length(back_g))) {
    back_g_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, back_g[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, back_g[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, back_g[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, back_g[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, back_g[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, back_g[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, back_g[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, back_g[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, back_g[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, back_g[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, back_g[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, back_g[row, col + 1] / 2) +
         ##
         back_g[row, col])
  }
}



################################################################################
##########################  Charts  ############################################
################################################################################

#####
## Rink Markings
###

# I did some goofy stuff here with the x and y coordinates to make the maps
# up/down instead of side to side

off_zone_markings <-
  function(legend_position = "none", show_behind_net = F, show_neutral_zone = F, big_net = F, direction = "down") {
    net_radius <- ifelse(big_net, 20/12, 18/12)
    net_depth <- ifelse(big_net, 44/12, 40/12)
    net_max_width <- ifelse(big_net, 96/12, 88/12)

    net_curve_center_x <- (net_max_width / 2) - net_radius
    net_curve_center_y <- net_depth - net_radius
    net_post_x_diff = 3 - net_curve_center_x

    right_goal_joint_front_x <-
      (net_curve_center_x) +
      sqrt(
        net_radius**2 -
          (
            sin(
              (pi / 2) -
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
                atan(net_post_x_diff / net_curve_center_y)
            ) *
              net_radius
          )**2
      )
    left_goal_joint_front_x <-
      (-net_curve_center_x) -
      sqrt(
        net_radius**2 -
          (
            sin(
              (pi / 2) -
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
                atan(net_post_x_diff / net_curve_center_y)
            ) *
              net_radius
          )**2
      )
    goal_joint_front_y <-
      (-net_curve_center_y) + (
        sin(
          (pi / 2) -
            acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
            atan(net_post_x_diff / net_curve_center_y)
        ) *
          net_radius
      )
    right_goal_joint_back_x <- net_curve_center_x
    left_goal_joint_back_x <- -net_curve_center_x
    goal_joint_back_y <- -net_depth

    xlims <- c(-42.5, 42.5)
    ylims <-
      c(
        ifelse(show_behind_net, -11, 0),
        ifelse(show_neutral_zone, 89.5, 64)
      )

    if (direction == "up") {
      xlims <- rev(xlims)
      ylims <- rev(ylims)
    }

    list(
      # center line
      ggplot2::geom_rect(
        data =
          tibble::tibble(
            xmin = -42.5,
            xmax = 42.5,
            ymin = 88.5,
            ymax = 89.5
          ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "#C8102E"
      ),
      # white markings on center line
      ggplot2::geom_rect(
        data = tibble::tibble(
          xmin = seq(-42.5, 42.5, by = 2),
          xmax = seq(-41.5, 42.5, by = 2),
          ymin = 88.5,
          ymax = 89.5
        ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "white"
      ),
      # blue line
      ggplot2::geom_rect(
        data =
          tibble::tibble(
            xmin = -42.5,
            xmax = 42.5,
            ymin = 63,
            ymax = 64
          ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "#0033A0"
      ),
      # faceoff dot red centers
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = c(-22, 22, -22, 22),
            y = c(20, 20, 69, 69),
            r = 1
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#C8102E",
        color = "#C8102E"
      ),
      # faceoff dot white spaces
      ggplot2::geom_rect(
        data =
          tibble::tibble(
            xmin = c(-23, -23, 21, 21, -23, -23, 21, 21),
            xmax = c(21, 21, 23, 23, 21, 21, 23, 23),
            ymin = c(20.75, 19, 20.75, 19, 69.75, 68, 69.75, 68),
            ymax = c(21, 19.25, 21, 19.25, 70, 68.25, 70, 68.25)
          ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "white"
      ),
      # faceoff dot outside lines
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = c(-22, 22, -22, 22),
            y = c(20, 20, 69, 69),
            r = 1
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#C8102E",
        color = "#C8102E",
        alpha = 0
      ),
      # center faceoff dot
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = 0,
            y = 89,
            r = 0.5
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#0033A0",
        color = "white"
      ),
      # faceoff circles
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = c(-22, 0, 22),
            y = c(20, 89, 20),
            r = 15
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#C8102E",
        color = "#C8102E",
        alpha = 0
      ),
      # goalie crease
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = 0,
            y = 0,
            r = 6
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#41B6E6",
        color = "#C8102E"
      ),
      # white areas to cover edges of goalie crease
      ggplot2::geom_rect(
        data =
          tibble::tibble(
            xmin = c(-6.5, 4, -7),
            xmax = c(-4, 6.5, 7),
            ymin = c(0, 0, -7),
            ymax = c(6, 6, 0)
          ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "white"
      ),
      # net color (curved sections)
      ggforce::geom_circle(
        tibble::tibble(
          x = c(-net_curve_center_x, net_curve_center_x),
          y = -net_curve_center_y,
          r = net_radius
        ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        linewidth = 0,
        color = "lightgrey",
        fill = "lightgrey"
      ),
      # net color (straight sections)
      ggplot2::geom_polygon(
        data =
          tibble::tibble(
            x =
              c(
                # left post
                -3,
                # right post
                3,
                right_goal_joint_front_x,
                right_goal_joint_back_x,
                left_goal_joint_back_x,
                left_goal_joint_front_x
              ),
            y = c(
              0,
              0,
              goal_joint_front_y,
              goal_joint_back_y,
              goal_joint_back_y,
              goal_joint_front_y
            )
          ),
        mapping = ggplot2::aes(x = x, y = y),
        fill = "lightgrey"
      ),
      # goal straight lines
      ggplot2::geom_segment(
        data =
          tibble::tibble(
            x = c(-3, left_goal_joint_back_x, 3),
            xend =
              c(
                left_goal_joint_front_x,
                right_goal_joint_back_x,
                right_goal_joint_front_x
              ),
            y = c(0, goal_joint_back_y, 0),
            yend =
              c(
                goal_joint_front_y,
                goal_joint_back_y,
                goal_joint_front_y
              )
          ),
        mapping = ggplot2::aes(
          x = x, xend = xend, y = y, yend = yend
        ),
        color = "black"
      ),
      # goal curves
      ggforce::geom_arc(
        data =
          tibble::tibble(
            x = c(-net_curve_center_x, net_curve_center_x),
            y = -net_curve_center_y,
            r = net_radius,
            start =
              c(
                pi,
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) +
                  atan(net_post_x_diff / net_curve_center_y)
              ),
            end =
              c(
                (2*pi) -
                  acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
                  atan(net_post_x_diff / net_curve_center_y),
                pi
              )
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
        color = "black"
      ),
      # red lines
      ggplot2::geom_segment(
        data =
          tibble::tibble(
            x =
              c(
                # center line border
                -42.5, -42.5,
                # inside hash marks (horizontal)
                -22 - (23/6), -22 - (23/6), -22 + (5/6), -22 + (5/6),
                22 - (23/6), 22 - (23/6),  22 + (5/6), 22 + (5/6),
                # -26, -26, -21, -21, 18, 18, 23, 23,
                # inside hash marks (vertical)
                -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
                # outside hashmarks
                (-22 - (sqrt(15**2 - 2.875**2))), (-22 - sqrt(15**2 - 2.875**2)),
                (-22 + (sqrt(15**2 - 2.875**2))), (-22 + sqrt(15**2 - 2.875**2)),
                (22 - (sqrt(15**2 - 2.875**2))), (22 - sqrt(15**2 - 2.875**2)),
                (22 + (sqrt(15**2 - 2.875**2))), (22 + sqrt(15**2 - 2.875**2)),
                # sides of goalie crease
                -4, 4,
                # goal line
                -sqrt(28**2 - 17**2) - 14.5,
                # trapezoid
                -11, 11,
                # crease hash marks
                -4, 4
              ),
            xend =
              c(
                # center line border
                42.5, 42.5,
                # inside hash marks (horizontal)
                -22 - (5/6), -22 - (5/6), -22 + (23/6), -22 + (23/6),
                22 - (5/6), 22 - (5/6),  22 + (23/6), 22 + (23/6),
                # -23, -23, -18, -18, 21, 21, 26, 26,
                # inside hash marks (vertical)
                -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
                # outside hashmarks
                (-22 - (sqrt(15**2 - 2.875**2)) - 2), (-22 - sqrt(15**2 - 2.875**2) - 2),
                (-22 + (sqrt(15**2 - 2.875**2)) + 2), (-22 + sqrt(15**2 - 2.875**2) + 2),
                (22 - (sqrt(15**2 - 2.875**2)) - 2), (22 - sqrt(15**2 - 2.875**2) - 2),
                (22 + (sqrt(15**2 - 2.875**2)) + 2), (22 + sqrt(15**2 - 2.875**2) + 2),
                # sides of goalie crease
                -4, 4,
                # goal line
                sqrt(28**2 - 17**2) + 14.5,
                # trapezoid
                -14, 14,
                # crease hash marks
                -43/12, 43/12
              ),
            y =
              c(
                # center line border
                89.5, 88.5,
                # inside hash marks (horizontal)
                22, 18, 22, 18, 22, 18, 22, 18,
                # inside hash marks (vertical)
                18, 26, 18, 26, 26, 18, 26, 18,
                # outside hashmarks
                22.875, 17.125, 22.875, 17.125, 22.875, 17.125, 22.875, 17.125,
                # sides of goalie crease
                sqrt(6**2 - 4**2), sqrt(6**2 - 4**2),
                # goal line
                0,
                # trapezoid
                0, 0,
                # crease hash marks
                4, 4
              ),
            yend =
              c(
                # center line border
                89.5, 88.5,
                # inside hash marks (horizontal)
                22, 18, 22, 18, 22, 18, 22, 18,
                # inside hash marks (vertical)
                14, 22, 14, 22, 22, 14, 22, 14,
                # outside hashmarks
                22.875, 17.125, 22.875, 17.125, 22.875, 17.125, 22.875, 17.125,
                # sides of goalie crease
                0, 0,
                # goal line
                0,
                # trapezoid
                -11, -11,
                # crease hash marks
                4, 4
              )
          ),
        mapping =
          ggplot2::aes(
            x = x, y = y, xend = xend, yend = yend
          ),
        color = "#C8102E"
      ),
      # rink straight borders
      ggplot2::geom_segment(
        data =
          tibble::tibble(
            y = c(89.5, 89.5, -11),
            yend = c(17, 17, -11),
            x = c(-42.5, 42.5, -14.5),
            xend = c(-42.5, 42.5, 14.5)
          ),
        mapping = ggplot2::aes(
          x = x, xend = xend, y = y, yend = yend
        ),
        color = "black",
        linewidth = 1
      ),
      # rink corners
      ggforce::geom_arc(
        data =
          tibble::tibble(
            x = c(14.5, -14.5),
            y = 17,
            r = 28,
            start = c(pi / 2, 3 * pi / 2),
            end = c(pi)
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
        color = "black",
        linewidth = 1
      ),
      ggplot2::coord_fixed(
        xlim = xlims,
        ylim = ylims,
        expand = F
      ),
      ggplot2::theme_minimal(),
      ggplot2::theme(
        legend.position = legend_position,
        panel.spacing.x = ggplot2::unit(2, "lines"),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank()
      )
    )
  }

#####
## On net percentage
###

# On net percentage, regular shooting percentage, and shooting percentage all
# follow the same pattern, the key things for the heat maps are tidyr::uncount()
# and ggplot2::geom_density_2d_filled(contour_var = "count")
#
# I also filtered out locations without at least 10 smoothed attempts or sog to
# avoid weird spikes from low frequency locations

back_att_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
  dplyr::left_join(
    back_sog_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed")
  ) |>
  dplyr::filter(att_smoothed >= 10) |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    event_detail_1 = "Backhand",
    smoothed_regressed_on_perc =
      as.integer((sog_smoothed / att_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_on_perc) |>
  dplyr::bind_rows(
    wrist_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        wrist_sog_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Wrist",
        smoothed_regressed_on_perc =
          as.integer((sog_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_on_perc)
  ) |>
  dplyr::bind_rows(
    tip_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        tip_sog_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Tip In/Deflection",
        smoothed_regressed_on_perc =
          as.integer((sog_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_on_perc)
  ) |>
  dplyr::bind_rows(
    slap_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        slap_sog_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Slap",
        smoothed_regressed_on_perc =
          as.integer((sog_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_on_perc)
  ) |>
  dplyr::bind_rows(
    snap_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        snap_sog_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Snap",
        smoothed_regressed_on_perc =
          as.integer((sog_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_on_perc)
  ) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  # aesthetics have to go in the individual elements because the way I did the
  # rink markings was dumb
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), contour_var = "count", bins = 9) +
  ## manually set the scale so that the lowest level is transparent
  ggplot2::scale_fill_manual(
    "On Net Percentage",
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 8)
      )
  ) +
  ggplot2::labs(
    title = "5-on-5 On Net Percentage by Shot Type (Unblocked Shots)",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )

#####
## Shooting percentage
###

back_sog_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed") |>
  dplyr::left_join(
    back_g_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
  ) |>
  dplyr::filter(sog_smoothed >= 10) |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    event_detail_1 = "Backhand",
    smoothed_regressed_sh_perc =
      as.integer((g_smoothed / sog_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_sh_perc) |>
  dplyr::bind_rows(
    wrist_sog_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed") |>
      dplyr::left_join(
        wrist_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(sog_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Wrist",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / sog_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  dplyr::bind_rows(
    tip_sog_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed") |>
      dplyr::left_join(
        tip_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(sog_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Tip In/Deflection",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / sog_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  dplyr::bind_rows(
    slap_sog_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed") |>
      dplyr::left_join(
        slap_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(sog_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Slap",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / sog_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  dplyr::bind_rows(
    snap_sog_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "sog_smoothed") |>
      dplyr::left_join(
        snap_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(sog_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Snap",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / sog_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), contour_var = "count", bins = 12) +
  ggplot2::scale_fill_manual(
    "Shooting Percentage",
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 11)
      )
  ) +
  ggplot2::labs(
    title = "5-on-5 Shooting Percentage by Shot Type (Shots on Goal)",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )

#####
## Fenwick shooting percentage
###

back_att_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
  dplyr::left_join(
    back_g_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
  ) |>
  dplyr::filter(att_smoothed >= 10) |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    event_detail_1 = "Backhand",
    smoothed_regressed_sh_perc =
      as.integer((g_smoothed / att_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_sh_perc) |>
  dplyr::bind_rows(
    wrist_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        wrist_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Wrist",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  dplyr::bind_rows(
    tip_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        tip_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Tip In/Deflection",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  dplyr::bind_rows(
    slap_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        slap_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Slap",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  dplyr::bind_rows(
    snap_att_smoothed |>
      tibble::rownames_to_column(var = "y") |>
      tidyr::pivot_longer(-c(y), names_to = "x", values_to = "att_smoothed") |>
      dplyr::left_join(
        snap_g_smoothed |>
          tibble::rownames_to_column(var = "y") |>
          tidyr::pivot_longer(-c(y), names_to = "x", values_to = "g_smoothed")
      ) |>
      dplyr::filter(att_smoothed >= 10) |>
      dplyr::mutate(
        x = as.integer(x),
        y = as.integer(y),
        event_detail_1 = "Snap",
        smoothed_regressed_sh_perc =
          as.integer((g_smoothed / att_smoothed) * 100)
      ) |>
      tidyr::uncount(smoothed_regressed_sh_perc)
  ) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), contour_var = "count", bins = 12) +
  ggplot2::scale_fill_manual(
    "Shooting Percentage",
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 11)
      )
  ) +
  ggplot2::labs(
    title = "5-on-5 Shooting Percentage by Shot Type (Unblocked Shots)",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )
