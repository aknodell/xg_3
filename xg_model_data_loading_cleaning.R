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
              tidyr::replace_na(0)
          ) |>
          dplyr::filter(
            home_skater_strength_state != "Penalty Shot" &
              !event_detail_1 %in% c("Own Goal", "Awarded")
          ) |>
          dplyr::mutate(
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
            is_off_turnover =
              purrr::pmap(
                list(
                  fac = fac_id,
                  secs = game_seconds,
                  tm = event_team,
                  x = period_standardized_x,
                  shooter = event_player_1,
                  z = shot_zone
                ),
                function(fac, secs, tm, x, shooter, z) {
                  if (tidyr::replace_na(z, "") != "O") {
                    tibble::tibble(
                      is_off_turnover = 0,
                      is_oz_turnover = 0,
                      shooter_same_turnover = 0,
                      turnover_secs = 0
                    )
                  } else {
                    prior_events <-
                      pbp |>
                      dplyr::filter(
                        fac_id == fac,
                        (event_type == "GIVE" & event_team != tm) |
                          (event_type %in% c("HIT", "TAKE") & event_team == tm),
                        game_seconds <= secs,
                        game_seconds >= secs - 8,
                        !is.na(shot_zone)
                      ) |>
                      dplyr::mutate(
                        same_zone_as_shot =
                          abs(period_standardized_x) > 25 &
                          sign(period_standardized_x) == sign(x)
                      ) |>
                      tail(1)

                    if (nrow(prior_events) > 0) {
                      prior_events |>
                        dplyr::transmute(
                          is_off_turnover = 1,
                          is_oz_turnover = as.integer(same_zone_as_shot),
                          shooter_same_turnover = as.integer(event_player_1 == shooter),
                          turnover_secs = secs - game_seconds
                        )
                    } else {
                      tibble::tibble(
                        is_off_turnover = 0,
                        is_oz_turnover = 0,
                        shooter_same_turnover = 0,
                        turnover_secs = 0
                      )
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
          tidyr::unnest(c(is_rush, is_off_turnover, is_off_faceoff)) |>
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

pred_xg_shot_data_19 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_201902") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_20 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202002") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_21 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202102") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_22 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202202") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_23 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202302") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_24 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202402") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()

pred_xg_shot_data_25 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202502") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  load_and_clean_shot_data()


pred_xg_shot_data_21 |>
  dplyr::mutate(
    in_rink =
      !(shot_y <= 17 & shot_x <= -14.5 & sqrt((shot_x - -14.5)**2 + (shot_y - 17)**2) > 28) &
      !(shot_y <= 17 & shot_x >= 14.5 & sqrt((shot_x - 14.5)**2 + (shot_y - 17)**2) > 28),
    shot_x = ifelse(!in_rink, shot_x - (sign(shot_x)), shot_x),
    in_rink =
      !(shot_y <= 17 & shot_x <= -14.5 & sqrt((shot_x - -14.5)**2 + (shot_y - 17)**2) > 28) &
      !(shot_y <= 17 & shot_x >= 14.5 & sqrt((shot_x - 14.5)**2 + (shot_y - 17)**2) > 28)
  ) |>
  dplyr::filter(!in_rink)

training_data <-
  pred_xg_shot_data_19 |>
  dplyr::mutate(season = "19-20") |>
  dplyr::bind_rows(
    pred_xg_shot_data_20 |>
      dplyr::mutate(season = "20-21")
  ) |>
  dplyr::bind_rows(
    pred_xg_shot_data_21 |>
      dplyr::mutate(season = "21-22")
  ) |>
  dplyr::bind_rows(
    pred_xg_shot_data_22 |>
      dplyr::mutate(season = "22-23")
  ) |>
  dplyr::bind_rows(
    pred_xg_shot_data_23 |>
      dplyr::mutate(season = "23-24")
  ) |>
  dplyr::bind_rows(
    pred_xg_shot_data_24 |>
      dplyr::mutate(season = "24-25")
  ) |>
  dplyr::bind_rows(
    pred_xg_shot_data_25 |>
      dplyr::mutate(season = "25-26")
  ) |>
  dplyr::mutate(
    is_goal = as.integer(event_type == "GOAL"),
    rush_velo = ifelse(rush_velo > 70, 70, rush_velo),
    point_shot =
      position_category == "D" &
      shot_type %in% c("Slap", "Wrist/Snap"),
    is_slap = as.integer(shot_type == "Slap"),
    is_tip = as.integer(shot_type == "Tip In/Deflection"),
    is_other = as.integer(shot_type == "Backhand/Other"),
    is_jam_play =
      as.integer(
        event_detail_1 == "Wrap Around" |
          (
            is_reached_goalie_followup &
              shot_type %in% c("Wrist/Snap", "Backhand/Other") &
              sqrt(shot_x**2 + shot_y**2) <= 5.5
          )

      ),
    is_leading =
      as.integer(
        (home_score_diff > 0 & event_team == home_team) |
          (home_score_diff < 0 & event_team == away_team)
      ),
    is_trailing =
      as.integer(
        (home_score_diff < 0 & event_team == home_team) |
          (home_score_diff > 0 & event_team == away_team)
      ),
    is_shell_off =
      as.integer(
        is_leading == 1 &
          game_period == 3 &
          abs(home_score_diff) <= 2
      ),
    is_shell_def =
      as.integer(
        (is_trailing == 1 &
        game_period == 3 &
        abs(home_score_diff) <= 2)
      ),
    play_for_tie =
      as.integer(
        home_score_diff == 0 &
          game_period == 3
      ),
    garbage_time =
      as.integer(
        (abs(home_score_diff) >= 5 & game_period == 1) |
          (abs(home_score_diff) >= 4 & game_period == 2) |
          (abs(home_score_diff) >= 3 & game_period == 3)
      ),
    in_rink =
      !(shot_y <= 17 & shot_x <= -14.5 & sqrt((shot_x - -14.5)**2 + (shot_y - 17)**2) > 28) &
      !(shot_y <= 17 & shot_x >= 14.5 & sqrt((shot_x - 14.5)**2 + (shot_y - 17)**2) > 28),
    shot_x = ifelse(!in_rink, shot_x - (sign(shot_x)), shot_x),
    in_rink =
      !(shot_y <= 17 & shot_x <= -14.5 & sqrt((shot_x - -14.5)**2 + (shot_y - 17)**2) > 28) &
      !(shot_y <= 17 & shot_x >= 14.5 & sqrt((shot_x - 14.5)**2 + (shot_y - 17)**2) > 28)
  ) |>
  dplyr::select(
    -c(
      game_period, game_seconds, event_detail_1, event_detail_2, event_detail_3,
      event_team, home_team, away_team, home_skaters_on, away_skaters_on,
      shift_id, fac_id, period_standardized_x, period_standardized_y,
      home_score_diff, in_rink
    )
  ) |>
  dplyr::left_join(
    nhl_db_con |>
      odbc::dbGetQuery(
        "select game_id, game_date from games where season >= 20192020 and session = 2"
      ) |>
      tibble::tibble() |>
      dplyr::arrange(game_date, game_id) |>
      tibble::rowid_to_column(var = "game_num")
  )

  # dplyr::mutate(
  #   shot_x = ifelse(event_type == "BLOCK", est_x, shot_x),
  #   shot_y = ifelse(event_type == "BLOCK", est_y, shot_y),
  #   dist_center = sqrt(shot_x**2 + shot_y**2),
  #   dist_near_post = sqrt((abs(shot_x) - 3)**2 + shot_y**2),
  #   dist_far_post = sqrt((abs(shot_x) + 3)**2 + shot_y**2),
  #   # slope_center
  #   angle_center = atan(abs(shot_x) / shot_y),
  #   angle_near_post = atan((abs(shot_x) - 3) / shot_y),
  #   angle_far_post = atan((abs(shot_x) + 3) / shot_y),
  #   h_angle = abs(angle_near_post - angle_far_post),
  #   length_adjacent = cos(h_angle / 2) * dist_near_post,
  #   width = 2 * sqrt(dist_near_post**2 - length_adjacent**2),
  #   height_far_post = 4 * (dist_near_post / dist_far_post),
  #   target_area = width * ((height_far_post + 4) / 2),
  #   # is_jam_play =
  #   #   abs(shot_y) <= 4 &
  #   #   shot_type %in% c("Wrist/Snap", "Backhand/Other") & (
  #   #     (is_rush == 1 & is_reached_goalie_followup == 1) |
  #   #       ()
  #   #   )
  # )

goalie_geometry <-
  tidyr::expand_grid(
    shot_x = -42:42,
    shot_y = 1:64
  ) |>
  dplyr::mutate(
    dist_center = sqrt(shot_x**2 + shot_y**2),
    slope_center = shot_y / shot_x,
    dist_in_crease =
      ifelse(
        abs(slope_center) >= (sqrt(36 - 16)/4),
        6,
        sqrt((abs(slope_center) * 4)**2 + 4**2)
      ),
    dist_near_post = sqrt((abs(shot_x) - 3)**2 + shot_y**2),
    dist_far_post = sqrt((abs(shot_x) + 3)**2 + shot_y**2),
    angle_center = atan(shot_y / abs(shot_x)) * (180 / pi),
    angle_near_post = atan((abs(shot_x) - 3) / shot_y),
    angle_far_post = atan((abs(shot_x) + 3) / shot_y),
    h_angle = abs(angle_near_post - angle_far_post),
    length_adjacent = cos(h_angle / 2) * dist_near_post,
    width_at_net = 2 * sqrt(dist_near_post**2 - length_adjacent**2),
    x_int =
      (-3 * (sqrt(36 - 16) - ((15/39)*3)) + ((15/39)*3)) /
      (slope_center - ((sqrt(36 - 16) - ((15/39)*3)) * sign(shot_x))),
    y_int = slope_center * (x_int),
    range_in_crease =
      dplyr::case_when(
        abs(slope_center) >= (sqrt(36 - 16)/4) ~
          6 - sqrt(((15/39)*3)**2 + 3**2),
        width_at_net < 2 ~ 0,
        T ~
          sqrt((x_int**2) + (y_int**2)) - sqrt(((15/39)*3)**2 + 3**2)
      ),
    optimal_goalie_distance =
      sqrt(((15/39)*3)**2 + 3**2) +
      (
        ((ifelse(dist_center > 42.5, 42.5, dist_center) - dist_in_crease) /
           (42.5 - dist_in_crease)) *
          range_in_crease
      ),
    optimal_goalie_x =
      dplyr::case_when(
        width_at_net < 2 ~ sign(shot_x) * 3,
        optimal_goalie_distance > dist_center ~ shot_x,
        T ~ shot_x * (optimal_goalie_distance / dist_center)
      ),
    optimal_goalie_y =
      dplyr::case_when(
        width_at_net < 2 ~
          ifelse(
            slope_center * optimal_goalie_x < 1,
            1,
            slope_center * optimal_goalie_x
          ),
        optimal_goalie_distance > dist_center ~ shot_y,
        T ~ shot_y * (optimal_goalie_distance / dist_center)
      ),
    width_at_goalie =
      width_at_net * ((dist_center - optimal_goalie_distance) / length_adjacent),
    optimal_width_coverage =
      dplyr::case_when(
        width_at_net <= 2 ~ 1,
        width_at_goalie <= 2 ~ 1,
        optimal_goalie_distance > dist_center ~ 1,
        T ~ 2 / width_at_goalie
      ),
    height_far_post = 4 * (dist_near_post / dist_far_post),
    target_area = width_at_net * ((height_far_post + 4) / 2),
    dist_to_goalie_optimal =
      ifelse(
        dist_center - optimal_goalie_distance < 0.5,
        0.5,
        dist_center - optimal_goalie_distance
      )
  ) |>
  dplyr::transmute(
    shot_x,
    shot_y,
    optimal_goalie_x,
    optimal_goalie_y,
    angle_center,
    dist_to_goalie_optimal,
    optimal_width_coverage,
    h_angle,
    width_at_net,
    avg_height = (height_far_post + 4) / 2,
    v_angle =
      atan(
        avg_height /
          (length_adjacent + ((dist_center - length_adjacent) / 2))
      ),
    target_area
  )
  # ggplot2::ggplot() +
  # off_zone_markings(show_behind_net = T) +
  # ggplot2::geom_tile(ggplot2::aes(x = shot_x, y = shot_y, fill = optimal_width_coverage), alpha = 0.7) +
  # ggforce::geom_circle(
  #   data = tibble::tibble(
  #     x = 0,
  #     y = 0,
  #     r = sqrt(((12/31)*3)**2 + 9)
  #   ),
  #   ggplot2::aes(x0 = x, y0 = y, r = r)
  # ) +
  # ggplot2::geom_vline(xintercept = -3) +
  # ggplot2::geom_vline(xintercept = 3) +
  # ggplot2::geom_abline(
  #   slope = (sqrt(36 - 16) - (12/31)*3),
  #   intercept = (-3 * (sqrt(36 - 16) - (12/31)*3)) + (12/31)*3
  # ) +
  # ggplot2::geom_abline(
  #   slope = -(sqrt(36 - 16) - (12/31)*3),
  #   intercept = (-3 * (sqrt(36 - 16) - (12/31)*3)) + (12/31)*3
  # ) +
  # ggplot2::scale_fill_viridis_c() +
  # ggplot2::labs(title = "X")


goalie_geometry |>
  # colnames()
  ggplot2::ggplot() +
  off_zone_markings(show_behind_net = T) +
  ggplot2::geom_tile(ggplot2::aes(x = shot_x, y = shot_y, fill = angle_center), alpha = 0.7) +
  ggplot2::scale_fill_viridis_c()


