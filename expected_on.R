load_and_clean_shot_data <- function(game_ids) {
  game_ids |>
    purrr::map(
      function(f) {
        roster <-
          "../scraper_testing/clean_files/rosters_{f}.csv" |>
          glue::glue() |>
          readr::read_csv(
            col_select = c(api_id, position_category),
            col_types = readr::cols(
              api_id = readr::col_integer(),
              position_category = readr::col_character()
            )
          ) |>
          dplyr::rename(event_player_1 = api_id)

        pbp <-
          "../scraper_testing/clean_files/pbp_{f}.csv" |>
          glue::glue() |>
          readr::read_csv(
            col_select = c(
              game_id:game_seconds, event_type, event_player_1,
              tidyr::starts_with("event_detail_"), event_description,
              event_team, home_team, away_team,
              event_id, coords_x, coords_y, event_team_zone, home_team_def_zone,
              home_skaters_on, away_skaters_on, home_goalie, away_goalie,
              event_team_strength, home_skater_strength_state, shift_id
            ),
            col_types = readr::cols(
              event_type = readr::col_character(),
              event_detail_1 = readr::col_character(),
              event_detail_2 = readr::col_character(),
              event_detail_3 = readr::col_character(),
              event_description = readr::col_character(),
              event_team = readr::col_character(),
              home_team = readr::col_character(),
              away_team = readr::col_character(),
              event_team_zone = readr::col_character(),
              home_team_def_zone = readr::col_character(),
              home_skater_strength_state = readr::col_character(),
              event_team_strength = readr::col_character(),
              .default = readr::col_integer()
            )
          ) |>
          dplyr::mutate(
            fac_id = cumsum(event_type == "FAC"),
            ## home net left, away net right
            period_standardized_x = coords_x * ifelse(home_team_def_zone == "left", 1, -1),
            period_standardized_y = coords_y * ifelse(home_team_def_zone == "left", 1, -1),
            ## shot
            shot_x = period_standardized_y * ifelse(event_team == home_team, 1, -1),
            shot_y = (period_standardized_x * ifelse(event_team == home_team, 1, -1) - 89) * -1,
            shot_zone =
              dplyr::case_when(
                event_team_zone == "N" ~ "N",
                shot_y <= 64 ~ "O",
                shot_y >= 114 ~ "D"
              )
          )

        pbp |>
          dplyr::filter(
            stringr::str_detect(event_description, "GOALIE STOPPED") |
              event_type %in% c("SHOT", "MISS", "BLOCK", "GOAL"),
            game_period != 5
          ) |>
          dplyr::group_by(fac_id) |>
          dplyr::mutate(
            dplyr::across(
              tidyselect::starts_with("event_detail_"),
              .fns = function(x) x |> tidyr::replace_na("")
            ),
            reached_goalie =
              (event_type %in% c("SHOT", "GOAL") |
                 (event_type == "MISS" & event_detail_2 != "Short")) |>
              as.integer(),
            is_frozen =
              (reached_goalie == 1 &
                 dplyr::lead(stringr::str_detect(event_description, "GOALIE STOPPED")) &
                 event_team != dplyr::lead(event_team) &
                 (dplyr::lead(game_seconds) - game_seconds) <= 3) |>
              as.integer() |>
              tidyr::replace_na(0)
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK"), game_period != 5) |>
          dplyr::mutate(
            goalie = ifelse(event_team == home_team, away_goalie, home_goalie),
            tmp = event_detail_1,
            event_detail_1 = ifelse(event_detail_2 == "Own Goal", event_detail_2, event_detail_1),
            event_detail_2 = ifelse(event_detail_1 == "Own Goal", tmp, event_detail_2),
            event_detail_1 = ifelse(event_detail_1 == "", "Awarded", event_detail_1),
            home_score_diff =
              dplyr::lag(
                cumsum(event_type == "GOAL" & (event_team == home_team)) -
                  cumsum(event_type == "GOAL" & (event_team == away_team))
              ) |>
              tidyr::replace_na(0),
            is_rush =
              purrr::pmap(
                list(
                  fac = fac_id,
                  shift = shift_id,
                  secs = game_seconds,
                  tm = event_team,
                  x = period_standardized_x,
                  z = shot_zone,
                  e = event_id
                ),
                function(fac, shift, secs, tm, x, z, e) {
                  prior_events <-
                    pbp |>
                    dplyr::filter(
                      fac_id == fac,
                      shift_id <= shift,
                      event_id < e,
                      !event_type %in% c("PENL", "DELPEN"),
                      game_seconds < secs,
                      game_seconds >= secs - 8,
                      (
                        event_type == "CHANGE" &
                          event_team != tm &
                          home_skater_strength_state == tidyr::replace_na(dplyr::lag(home_skater_strength_state), "")
                      ) |
                        (
                          event_type != "CHANGE" &
                            # neutral zone
                            ((abs(period_standardized_x) <= 25) |
                               # other side of red line
                               (sign(period_standardized_x) != sign(x)))
                        )
                    ) |>
                    dplyr::mutate(
                      period_standardized_x =
                        ifelse(event_type == "CHANGE", 0, period_standardized_x)
                    )

                  if (nrow(prior_events) == 0 | tidyr::replace_na(z, "") != "O") {
                    tibble::tibble(is_rush = 0, rush_secs = 0, rush_velo = 0)
                  } else {
                    prior_events |>
                      dplyr::summarise(
                        is_rush = 1,
                        rush_secs = secs - dplyr::last(game_seconds),
                        rush_velo =
                          abs(x - dplyr::last(period_standardized_x)) /
                          rush_secs
                      )
                  }
                }
              ),
            is_off_faceoff =
              purrr::pmap(
                list(
                  fac = fac_id,
                  secs = game_seconds,
                  tm = event_team,
                  x = period_standardized_x,
                  z = shot_zone
                ),
                function(fac, secs, tm, x, z) {
                  if (tidyr::replace_na(z, "") != "O") {
                    tibble::tibble(is_off_faceoff = 0, is_off_faceoff_win = 0, faceoff_secs = 0)
                  } else {
                    prior_events <-
                      pbp |>
                      dplyr::filter(
                        fac_id == fac,
                        !event_type %in% c("PENL", "DELPEN"),
                        game_seconds <= secs,
                        game_seconds >= secs - 8,
                        !is.na(shot_zone)
                      ) |>
                      dplyr::mutate(
                        same_zone_as_shot =
                          abs(period_standardized_x) > 25 &
                          sign(period_standardized_x) == sign(x)
                      )

                    if ("FAC" %in% c(prior_events$event_type) & all(prior_events$same_zone_as_shot)) {
                      prior_events |>
                        dplyr::summarise(
                          is_off_faceoff = 1,
                          is_off_faceoff_win =
                            sum(event_type == "FAC" & event_team == tm, na.rm = T),
                          faceoff_secs = secs - min(game_seconds)
                        )
                    } else {
                      tibble::tibble(is_off_faceoff = 0, is_off_faceoff_win = 0, faceoff_secs = 0)
                    }
                  }
                }
              ),
            is_followup_shot =
              purrr::pmap_int(
                list(
                  fac = fac_id,
                  shift = shift_id,
                  secs = game_seconds,
                  tm = event_team,
                  x = period_standardized_x,
                  z = shot_zone,
                  e = event_id
                ),
                function(fac, shift, secs, tm, x, z, e) {
                  if (tidyr::replace_na(z, "") != "O") {
                    0
                  } else {
                    prior_events <-
                      pbp |>
                      dplyr::filter(
                        fac_id == fac,
                        shift_id <= shift,
                        event_id < e,
                        !event_type %in% c("PENL", "DELPEN"),
                        game_seconds <= secs,
                        game_seconds >= secs - 5,
                        !is.na(shot_zone)
                      )

                    if (nrow(prior_events) > 0) {
                      prior_events <-
                        prior_events |>
                        dplyr::mutate(
                          same_zone_as_shot =
                            abs(period_standardized_x) > 25 &
                            sign(period_standardized_x) == sign(x),
                          last_shot_by_team_time =
                            max(
                              game_seconds *
                                (event_team == tm &
                                   event_type %in% c("SHOT", "BLOCK", "MISS") &
                                   shot_zone == "O")
                            )
                        ) |>
                        dplyr::filter(game_seconds >= last_shot_by_team_time, last_shot_by_team_time != 0)
                    }

                    if (nrow(prior_events) > 0) {
                      ifelse(all(prior_events$same_zone_as_shot), 1, 0)
                    } else {
                      0
                    }
                  }
                }
              )
          ) |>
          dplyr::filter(
            home_skater_strength_state != "Penalty Shot" &
              !event_detail_1 %in% c("Own Goal", "Awarded")
          ) |>
          tidyr::unnest(c(is_rush, is_off_faceoff)) |>
          dplyr::select(
            -c(
              tmp, event_description, coords_x, coords_y, event_team_zone,
              home_team_def_zone, home_goalie, away_goalie
            )
          ) |>
          dplyr::left_join(
            roster,
            by = dplyr::join_by(event_player_1)
          )
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::group_by(game_id, fac_id) |>
    dplyr::mutate(
      last_home_shot_is_rush =
        ifelse(event_team == home_team, is_rush, NA_integer_),
      last_home_shot_secs =
        ifelse(event_team == home_team, game_seconds, NA_integer_),
      last_away_shot_is_rush =
        ifelse(event_team == away_team, is_rush, NA_integer_),
      last_away_shot_secs =
        ifelse(event_team == away_team, game_seconds, NA_integer_),
    ) |>
    tidyr::fill(
      c(
        last_home_shot_is_rush,
        last_home_shot_secs,
        last_away_shot_is_rush,
        last_away_shot_secs
      ),
      .direction = "down"
    ) |>
    dplyr::mutate(
      event_team_strength =
        dplyr::case_when(
          event_team_strength == "PP" & event_type == "BLOCK" ~ "SH",
          event_team_strength == "SH" & event_type == "BLOCK" ~ "PP",
          T ~ event_team_strength
        ),
      shot_type =
        dplyr::case_when(
          event_detail_1 %in% c("Wrist", "Snap") ~ "Wrist/Snap",
          event_detail_1 %in% c("Tip In", "Deflected") ~ "Tip In/Deflection",
          event_detail_1 %in%
            c("Backhand", "Between Legs", "Poke", "Bat", "Cradle", "Wrap Around") ~
            "Backhand/Other",
          T ~ event_detail_1
        ),
      is_counter_rush =
        ifelse(
          is_rush == 1,
          ifelse(
            event_team == home_team,
            ifelse(
              tidyr::replace_na(last_away_shot_is_rush, 0) == 1 &
                game_seconds - tidyr::replace_na(last_away_shot_secs, -10) <= 8,
              1,
              0
            ),
            ifelse(
              tidyr::replace_na(last_home_shot_is_rush, 0) == 1 &
                game_seconds - tidyr::replace_na(last_home_shot_secs, -10) <= 8,
              1,
              0
            )
          ),
          0
        ),
      is_reached_goalie_followup =
        (is_followup_shot == 1 & dplyr::lag(reached_goalie) == 1) |>
        as.integer() |>
        tidyr::replace_na(0),
      is_own_followup =
        (is_followup_shot == 1 & event_player_1 == dplyr::lag(event_player_1)) |>
        as.integer() |>
        tidyr::replace_na(0),
      followup_secs =
        ifelse(is_followup_shot == 1, game_seconds - dplyr::lag(game_seconds), 0) |>
        tidyr::replace_na(0),
      angle =
        ifelse(
          sign(shot_x) <= 0,
          abs(atan(ifelse(shot_y < 0, 0, shot_y) / shot_x) * (180 / pi)),
          180 - atan(ifelse(shot_y < 0, 0, shot_y) / shot_x) * (180 / pi)
        ),
      angle_change_velo =
        ifelse(
          is_followup_shot == 1,
          abs(
            ifelse(
              is.nan(angle),
              90,
              angle
            ) -
              ifelse(
                is.nan(dplyr::lag(angle)),
                90,
                dplyr::lag(angle)
              )
          ) /
            ifelse(followup_secs == 0, 0.5, followup_secs),
          0
        )
    ) |>
    dplyr::select(
      -c(
        last_home_shot_is_rush,
        last_home_shot_secs,
        last_away_shot_is_rush,
        last_away_shot_secs,
        angle
      )
    ) |>
    dplyr::ungroup()
}

pred_xg_shot_data <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202402") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_25 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202502") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_23 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202302") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_22 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202202") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_21 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202102") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_20 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202002") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

corsi_25 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202502", full.names = T) |>
  # head(1) |>
  purrr::map(
    function(f) {
      pbp <-
        f |>
        readr::read_csv(
          col_select = c(
            game_id:game_seconds, event_type, event_player_1, event_detail_1, event_detail_2,
            event_detail_3,
            event_team, home_team, away_team,
            coords_x, coords_y, event_team_zone,
            home_skaters_on, away_skaters_on, home_goalie, away_goalie,
            home_skater_strength_state, shift_id
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
        dplyr::mutate(fac_id = cumsum(event_type == "FAC"))

      pbp |>
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
            dplyr::lag(
              cumsum(event_type == "GOAL" & (event_team == home_team)) -
                cumsum(event_type == "GOAL" & (event_team == away_team))
            ) |>
            tidyr::replace_na(0),
          is_rush =
            purrr::pmap(
              list(
                fac = fac_id,
                shift = shift_id,
                secs = game_seconds,
                tm = event_team,
                x = coords_x
              ),
              function(fac, shift, secs, tm, x) {
                prior_events <-
                  pbp |>
                  dplyr::filter(
                    fac_id == fac,
                    shift_id <= shift,
                    game_seconds < secs,
                    game_seconds >= secs - 20,
                    (
                      event_type == "CHANGE" &
                        event_team != tm &
                        home_skater_strength_state == tidyr::replace_na(dplyr::lag(home_skater_strength_state), "")
                    ) |
                      (
                        event_type != "CHANGE" &
                          (abs(coords_x) < 25 | sign(coords_x) != sign(x))
                      )
                  )

                if (nrow(prior_events) == 0) {
                  tibble::tibble(is_rush = 0, rush_secs = 0)
                } else {
                  prior_events |>
                    dplyr::summarise(is_rush = 1, rush_secs = secs - max(game_seconds))
                }
              }
            )
        ) |>
        tidyr::unnest(is_rush) |>
        dplyr::select(-tmp)
    }
  ) |>
  dplyr::bind_rows()

corsi_25 |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(
    (event_type != "BLOCK" & event_team_zone == "O") |
      (event_type == "BLOCK" & event_team_zone == "D")
  ) |>
  # dplyr::filter(event_detail_2 != "Teammate Blocked") |>
  dplyr::mutate(
    x = coords_y * sign(coords_x),
    y = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(y > 0) |>
  dplyr::mutate(
    dist_to_center_goalline = sqrt(x**2 + y**2),
    angle_center = atan(y / abs(x)) * (180 / pi),
  ) |>
  dplyr::group_by(is_rush, rush_secs) |>
  dplyr::summarise(
    n = dplyr::n(),
    BLOCK = sum(event_type == "BLOCK") / n,
    # BLOCK_opp = sum(event_type == "BLOCK" & event_detail_2 == "") / n,
    # BLOCK_other = sum(event_type == "BLOCK" & event_detail_2 != "") / n,
    MISS = sum(event_type == "MISS") / n,
    SHOT = sum(event_type == "SHOT") / n,
    GOAL = sum(event_type == "GOAL") / n,
    mean_dist = mean(dist_to_center_goalline),
    mean_angle = mean(angle_center)
  ) |>
  dplyr::ungroup() |>
  # dplyr::mutate(
  #    similarity =
  #     1 /
  #     (1 + sqrt(
  #       (BLOCK_opp - sum(BLOCK_opp * (is_rush == 0)))**2 +
  #       (BLOCK_other - sum(BLOCK_other * (is_rush == 0)))**2 +
  #       (MISS - sum(MISS * (is_rush == 0)))**2 +
  #       (SHOT - sum(SHOT * (is_rush == 0)))**2 +
  #       (GOAL - sum(GOAL * (is_rush == 0)))**2
  #     ))
  # ) |>
  # dplyr::filter(is_rush == 1) |>
  tidyr::pivot_longer(c(BLOCK, MISS, SHOT, GOAL)) |>
  # tidyr::pivot_longer(c(BLOCK_opp, BLOCK_other, MISS, SHOT, GOAL)) |>
  # tidyr::pivot_longer(c(mean_dist, mean_angle)) |>
  dplyr::mutate(
    rush_secs = as.character(rush_secs),
    rush_secs = ifelse(is_rush == 0, "21+", rush_secs) |> factor(levels = c(as.character(1:20), "21+")),
    name = name |> factor(levels = c("BLOCK", "MISS", "SHOT", "GOAL") |> rev())
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = rush_secs, y = value, fill = name)) +
  # ggplot2::facet_wrap(ggplot2::vars(name), scales = "free", ncol = 1) +
  # ggplot2::geom_point() +
  ggplot2::geom_col() +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_viridis_d("") +
  ggplot2::scale_x_discrete("Seconds") +
  ggplot2::scale_y_continuous("", labels = scales::percent) + #, labels = scales::percent) +
  ggplot2::labs(
    title = "5-on-5 Shot Outcomes By Seconds Since Event Outside Offensive Zone",
    subtitle = "2025-26 Season",
    caption = "Data via NHL"
  ) +
  ggplot2::theme(legend.position = "bottom")

  # dplyr::ungroup() |>
  # dplyr::mutate(perc = n / sum(n)) |>
  # dplyr::group_by(is_rush) |>
  # dplyr::mutate(total = cumsum(n), total_perc = cumsum(perc)) |>
  # View()
  # dplyr::arrange((rush_perc))


pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(
    (event_type != "BLOCK" & event_team_zone == "O") |
      (event_type == "BLOCK" & event_team_zone == "D")
  ) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::group_by(is_rush, rush_secs) |>
  dplyr::tally() |>
  dplyr::ungroup() |>
  dplyr::mutate(season = "24-25", perc = n / sum(n)) |>
  dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
  dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1))) |>
  dplyr::bind_rows(
    corsi_18 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O") |
          (event_type == "BLOCK" & event_team_zone == "D")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "18-19", perc = n / sum(n)) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_19 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O") |
          (event_type == "BLOCK" & event_team_zone == "D")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "19-20", perc = n / sum(n)) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_20 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O") |
          (event_type == "BLOCK" & event_team_zone == "D")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "20-21", perc = n / sum(n)) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_21 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O") |
          (event_type == "BLOCK" & event_team_zone == "D")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "21-22", perc = n / sum(n)) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_22 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O") |
          (event_type == "BLOCK" & event_team_zone == "D")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "22-23", perc = n / sum(n)) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_23 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O") |
          (event_type == "BLOCK" & event_team_zone == "D")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "23-24", perc = n / sum(n)) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_25 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O") |
          (event_type == "BLOCK" & event_team_zone == "D")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "25-26", perc = n / sum(n)) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = season, y = perc, fill = rush_secs)) +
  ggplot2::geom_col() +
  ggplot2::scale_x_discrete("") +
  ggplot2::scale_y_continuous("Percent of All Corsi Attempts", labels = scales::percent) +
  ggplot2::scale_fill_viridis_d("Seconds Since Event Outside Offensive Zone") +
  ggplot2::labs(
    title = "Percent of All 5-on-5 Corsi Events Taken Off Rush (Play-by-Play)",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")




pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
  dplyr::filter(event_detail_2 != "Short") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
  dplyr::group_by(is_rush, rush_secs) |>
  dplyr::summarise(
    fsh_perc = sum(event_type == "GOAL") / dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(season = "24-25") |>
  dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1))) |>
  dplyr::bind_rows(
    corsi_18 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(event_detail_2 != "Short") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(
        fsh_perc = sum(event_type == "GOAL") / dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "18-19") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_19 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(event_detail_2 != "Short") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(
        fsh_perc = sum(event_type == "GOAL") / dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "19-20") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_20 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_detail_2 != "Short") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(
        fsh_perc = sum(event_type == "GOAL") / dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "20-21") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_21 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(event_detail_2 != "Short") |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(
        fsh_perc = sum(event_type == "GOAL") / dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "21-22") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_22 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(event_detail_2 != "Short") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(
        fsh_perc = sum(event_type == "GOAL") / dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "22-23") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_23 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(event_detail_2 != "Short") |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(
        fsh_perc = sum(event_type == "GOAL") / dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "23-24") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_25 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_detail_2 != "Short") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(
        fsh_perc = sum(event_type == "GOAL") / dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "25-26") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = as.integer(factor(season)), y = fsh_perc, color = rush_secs)) +
  ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
  ggplot2::scale_x_continuous(
    "",
    breaks = 1:8,
    minor_breaks = NULL,
    labels = c("18-19", "19-20", "20-21", "21-22", "22-23", "23-24", "24-25", "25-26")
  ) +
  ggplot2::scale_y_continuous(
    "Fenwick Sh%",
    labels = scales::percent, limits = c(0, NA)
  ) +
  ggplot2::scale_color_viridis_d("Seconds Since Event Outside Offensive Zone") +
  ggplot2::labs(
    title = "5-on-5 Fenwick Shooting Percentage of Attempts Taken Off Rush (Play-By-Play)",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")


pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
  dplyr::group_by(is_rush, rush_secs) |>
  dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)))) |>
  dplyr::ungroup() |>
  dplyr::mutate(season = "24-25") |>
  dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1))) |>
  dplyr::bind_rows(
    corsi_18 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)), na.rm = T)) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "18-19") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_19 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)))) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "19-20") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_20 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)))) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "20-21") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_21 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)))) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "21-22") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_22 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)))) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "22-23") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_23 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)))) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "23-24") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  dplyr::bind_rows(
    corsi_25 |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(event_team_zone == "O", event_type != "BLOCK") |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(is_rush == 1 & rush_secs <= 10) |>
      dplyr::group_by(is_rush, rush_secs) |>
      dplyr::summarise(mean_distance = mean(sqrt(((abs(coords_x) - 89)**2) + (coords_y**2)))) |>
      dplyr::ungroup() |>
      dplyr::mutate(season = "25-26") |>
      dplyr::mutate(rush_secs = factor(rush_secs, levels = c(10:1)))
  ) |>
  # View()
  ggplot2::ggplot(ggplot2::aes(x = as.integer(factor(season)), y = mean_distance, color = rush_secs)) +
  ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
  ggplot2::scale_x_continuous(
    "",
    breaks = 1:8,
    minor_breaks = NULL,
    labels = c("18-19", "19-20", "20-21", "21-22", "22-23", "23-24", "24-25", "25-26")
  ) +
  ggplot2::scale_y_continuous(
    "Average Distance"
    # labels = scales::percent,
    # limits = c(0, NA)
  ) +
  ggplot2::scale_color_viridis_d("Seconds Since Event Outside Offensive Zone") +
  ggplot2::labs(
    title = "Average Distance of 5-on-5 Fenwick Events Taken Off Rush (Play-By-Play)",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")



## tips and deflections

tip_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Tip In", "Deflected")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
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
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Tip In", "Deflected")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
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
    smoothed_regressed_on_perc =
      as.integer((sog_smoothed / att_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_on_perc) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    # direction = "up",
    legend_position = "bottom"
  ) +
  # ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  # ggplot2::geom_contour_filled(
  #   ggplot2::aes(x = x, y = y, z = on_perc),
  #   alpha = 0.7,
  #   bins = 6
  # ) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), alpha = 0.7, contour_var = "count") +
  # ggplot2::geom_raster(ggplot2::aes(x = coords_y, y = coords_x, fill = n), alpha = 0.7, interpolate = T) +
  ggplot2::scale_fill_viridis_d(
    name = "On Net Percentage",
    direction = 1, option = "A"
  )

## slapshots

slap_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Slap")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

slap_att_smoothed <- slap_att

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
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Slap")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
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
    smoothed_regressed_on_perc =
      as.integer((sog_smoothed / att_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_on_perc) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    # direction = "up",
    legend_position = "bottom"
  ) +
  # ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  # ggplot2::geom_contour_filled(
  #   ggplot2::aes(x = x, y = y, z = on_perc),
  #   alpha = 0.7,
  #   bins = 6
  # ) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), alpha = 0.7, contour_var = "count") +
  # ggplot2::geom_raster(ggplot2::aes(x = coords_y, y = coords_x, fill = n), alpha = 0.7, interpolate = T) +
  ggplot2::scale_fill_viridis_d(
    name = "On Net Percentage",
    direction = 1, option = "A"
  )




## snapshots

snap_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Snap")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

snap_att_smoothed <- snap_att

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
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Snap")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
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
    smoothed_regressed_on_perc =
      as.integer((sog_smoothed / att_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_on_perc) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    # direction = "up",
    legend_position = "bottom"
  ) +
  # ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  # ggplot2::geom_contour_filled(
  #   ggplot2::aes(x = x, y = y, z = on_perc),
  #   alpha = 0.7,
  #   bins = 6
  # ) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), alpha = 0.7, contour_var = "count") +
  # ggplot2::geom_raster(ggplot2::aes(x = coords_y, y = coords_x, fill = n), alpha = 0.7, interpolate = T) +
  ggplot2::scale_fill_viridis_d(
    name = "On Net Percentage",
    direction = 1, option = "A"
  )



## wristshots

wrist_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Wrist")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

wrist_att_smoothed <- wrist_att

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
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Wrist")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
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
    smoothed_regressed_on_perc =
      as.integer((sog_smoothed / att_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_on_perc) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    # direction = "up",
    legend_position = "bottom"
  ) +
  # ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  # ggplot2::geom_contour_filled(
  #   ggplot2::aes(x = x, y = y, z = on_perc),
  #   alpha = 0.7,
  #   bins = 6
  # ) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), alpha = 0.7, contour_var = "count") +
  # ggplot2::geom_raster(ggplot2::aes(x = coords_y, y = coords_x, fill = n), alpha = 0.7, interpolate = T) +
  ggplot2::scale_fill_viridis_d(
    name = "On Net Percentage",
    direction = 1, option = "A"
  )





## backshots

back_att <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Backhand")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

back_att_smoothed <- back_att

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
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_1 %in% c("Backhand")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  # dplyr::filter(tidyr::replace_na(event_detail_2, "") == "Defensive Deflection") |>
  # dplyr::group_by(event_type) |>
  # dplyr::tally()
  # dplyr::filter(event_detail_1 == "Cradle") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  # dplyr::group_by(x = x_hex, y = y_hex) |>
  dplyr::summarise(
    sog = sum(event_type != "MISS"),
    att = dplyr::n()
    # on_perc = as.integer((sog / att) * 100)
  ) |>
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
    smoothed_regressed_on_perc =
      as.integer((sog_smoothed / att_smoothed) * 100)
  ) |>
  tidyr::uncount(smoothed_regressed_on_perc) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    # direction = "up",
    legend_position = "bottom"
  ) +
  # ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  # ggplot2::geom_contour_filled(
  #   ggplot2::aes(x = x, y = y, z = on_perc),
  #   alpha = 0.7,
  #   bins = 6
  # ) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), alpha = 0.7, contour_var = "count") +
  # ggplot2::geom_raster(ggplot2::aes(x = coords_y, y = coords_x, fill = n), alpha = 0.7, interpolate = T) +
  ggplot2::scale_fill_viridis_d(
    name = "On Net Percentage",
    direction = 1, option = "A"
  )













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
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), contour_var = "count") +
  ggplot2::scale_fill_manual(
    "On Net Percentage",
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 8)
      )

      # scales::viridis_pal(alpha = 0.7, option = "A")(n = 9) |>
      # stringr::str_replace_all("#000004B3", "#FFFFFF00")
  ) +
  ggplot2::labs(
    title = "5-on-5 On Net Percentage by Shot Type (Unblocked Shots)",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )


x_on_data <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1,
    x = coords_y,
    y = coords_x
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::mutate(
    goalie = ifelse(event_team == home_team, away_goalie, home_goalie),
    total_on = dplyr::n(),
    avg_per_shooter = total_on / length(unique(event_player_1)),
    avg_per_goalie = total_on / length(unique(goalie)),
  ) |>
  dplyr::group_by(event_player_1) |>
  dplyr::mutate(
    shooter_weight =
      ifelse(
        dplyr::n() > avg_per_shooter,
        avg_per_shooter / dplyr::n(),
        dplyr::n() / avg_per_shooter
      )
  ) |>
  dplyr::group_by(goalie) |>
  dplyr::mutate(
    goalie_weight =
      ifelse(
        dplyr::n() > avg_per_goalie,
        avg_per_goalie / dplyr::n(),
        dplyr::n() / avg_per_goalie
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(event_detail_1 %in% c("Snap", "Slap", "Wrist", "Backhand", "Tip In", "Deflected")) |>
  dplyr::left_join(shot_blocker_density_smoothed) |>
  dplyr::mutate(
    event_detail_1 =
      ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1),
    is_snap = as.integer(event_detail_1 == "Snap"),
    is_slap = as.integer(event_detail_1 == "Slap"),
    is_backhand = as.integer(event_detail_1 == "Backhand"),
    is_tip = as.integer(event_detail_1 == "Tip In/Deflected"),
    is_on = as.integer(event_type != "MISS"),
    weight = as.integer(shooter_weight * goalie_weight * (10000/9)),
    dist_to_center_goalline = sqrt(x**2 + y**2),
    angle_center = atan(y / abs(x)),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    horiz_perc = horiz_angle / pi,
    vert_angle = atan(4 / dist_to_center_goalline),
    vert_angle_6_ft = atan(6 / dist_to_center_goalline),
    vert_perc = vert_angle / vert_angle_6_ft,
    shooting_target_perc = horiz_perc * vert_perc,
    dist_to_center_goalline = dist_to_center_goalline**2
  ) |>
  dplyr::select(
    is_on, x, y, is_snap:is_tip, dist_to_center_goalline, angle_center, horiz_perc,
    vert_perc, shooting_target_perc, shot_blocker_density_smoothed, weight
  )

x_on_data

set.seed(1138)
x_on_cv <-
  glmnet::cv.glmnet(
    model.matrix(
      is_on ~
        is_snap +
        is_slap +
        is_backhand +
        is_tip +
        horiz_perc +
        vert_perc +
        shot_blocker_density_smoothed,
        # (shot_blocker_density_smoothed * dist_to_center_goalline),
      x_on_data[seq(nrow(x_on_data)) %% 5 != 1, ]
    )[, -1],
    x_on_data[seq(nrow(x_on_data)) %% 5 != 1, ]$is_on,
    family = "binomial",
    alpha = 1,
    weights = x_on_data[seq(nrow(x_on_data)) %% 5 != 1, ]$weight
  )

x_on_model_min <-
  glmnet::glmnet(
    model.matrix(
      is_on ~
        is_snap +
        is_slap +
        is_backhand +
        is_tip +
        horiz_perc +
        vert_perc +
        shot_blocker_density_smoothed,
      # (shot_blocker_density_smoothed * dist_to_center_goalline),
      x_on_data[seq(nrow(x_on_data)) %% 5 != 1, ]
    )[, -1],
    x_on_data[seq(nrow(x_on_data)) %% 5 != 1, ]$is_on,
    family = "binomial",
    alpha = 1,
    lambda = x_on_cv$lambda.min,
    weights = x_on_data[seq(nrow(x_on_data)) %% 5 != 1, ]$weight
  )

x_on_model_min |>
  coef()

expected_on_5v5 <-
  tidyr::expand_grid(
    event_detail_1 = "Wrist",
    x = -42:42,
    y = 1:64
  ) |>
  dplyr::mutate(
    x_on =
      predict(
        x_on_model_min,
        model.matrix(
          is_on ~
            is_snap +
            is_slap +
            is_backhand +
            is_tip +
            horiz_perc +
            vert_perc +
            shot_blocker_density_smoothed,
          tidyr::expand_grid(
            is_on = 0,
            x = -42:42,
            y = 1:64
          ) |>
            dplyr::mutate(
              is_snap = 0,
              is_slap = 0,
              is_backhand = 0,
              is_tip = 0,
              horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
              vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
            ) |>
            dplyr::left_join(shot_blocker_density_smoothed)
        )[, -1],
        type = "response"
      ) |>
      as.double()
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      event_detail_1 = "Snap",
      x = -42:42,
      y = 1:64
    ) |>
      dplyr::mutate(
        x_on =
          predict(
            x_on_model_min,
            model.matrix(
              is_on ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                horiz_perc +
                vert_perc +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_on = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 1,
                  is_slap = 0,
                  is_backhand = 0,
                  is_tip = 0,
                  horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
                  vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
                ) |>
                dplyr::left_join(shot_blocker_density_smoothed)
            )[, -1],
            type = "response"
          ) |>
          as.double()
      )
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      event_detail_1 = "Slap",
      x = -42:42,
      y = 1:64
    ) |>
      dplyr::mutate(
        x_on =
          predict(
            x_on_model_min,
            model.matrix(
              is_on ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                horiz_perc +
                vert_perc +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_on = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 1,
                  is_backhand = 0,
                  is_tip = 0,
                  horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
                  vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
                ) |>
                dplyr::left_join(shot_blocker_density_smoothed)
            )[, -1],
            type = "response"
          ) |>
          as.double()
      )
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      event_detail_1 = "Backhand",
      x = -42:42,
      y = 1:64
    ) |>
      dplyr::mutate(
        x_on =
          predict(
            x_on_model_min,
            model.matrix(
              is_on ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                horiz_perc +
                vert_perc +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_on = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 0,
                  is_backhand = 1,
                  is_tip = 0,
                  horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
                  vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
                ) |>
                dplyr::left_join(shot_blocker_density_smoothed)
            )[, -1],
            type = "response"
          ) |>
          as.double()
      )
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      event_detail_1 = "Tip In/Deflection",
      x = -42:42,
      y = 1:64
    ) |>
      dplyr::mutate(
        x_on =
          predict(
            x_on_model_min,
            model.matrix(
              is_on ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                horiz_perc +
                vert_perc +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_on = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 0,
                  is_backhand = 0,
                  is_tip = 1,
                  horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
                  vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
                ) |>
                dplyr::left_join(shot_blocker_density_smoothed)
            )[, -1],
            type = "response"
          ) |>
          as.double()
      )
  )

expected_on_5v5 |>
  dplyr::mutate(
    # is_snap = 0,
    # is_slap = 0,
    # is_backhand = 0,
    # is_tip = 1,
    horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
    vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
  ) |>
  dplyr::left_join(shot_blocker_density_smoothed) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = x_on)) +
  ggplot2::scale_fill_viridis_c(option = "A", alpha = 0.7) +
  ggplot2::labs(
    title = "Estimated 5-on-5 Expected On (Given Through) Values by Attempt Location",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )


# predict(
#   x_on_model_min,
#   model.matrix(
#     is_on ~
#       is_snap +
#       is_slap +
#       is_backhand +
#       is_tip +
#       horiz_perc +
#       vert_perc +
#       shot_blocker_density_smoothed,
#     # (shot_blocker_density_smoothed * dist_to_center_goalline),
#     x_on_data
#   )[, -1],
#   type = "response"
# ) |>
#   summary()

