training_data <-
  training_data |>
  dplyr::select(-c(est_x:shot_blocker_data_5v5_shot_type_point)) |>
  dplyr::filter(season >= "21-22") |>
  dplyr::group_by(game_date) |>
  tidyr::nest() |>
  dplyr::left_join(
    shot_blocker_density_ests_2,
    by = c("game_date" = "gm_dt")
  ) |>
  dplyr::mutate(
    data =
      purrr::map2(
        data,
        shot_blocker_data_5v5_basic,
        function(d, b) {
          d |>
            dplyr::left_join(
              b |>
                dplyr::select(
                  shot_x, shot_y, est_x, est_y, blockers_basic = blocker_dens
                ) |>
                dplyr::mutate(
                  home_skater_strength_state = "5v5",
                  event_team_strength = "EV"
                ),
              by = c("shot_x", "shot_y", "event_team_strength", "home_skater_strength_state")
            ) |>
            dplyr::mutate(
              est_x_basic = ifelse(event_type == "BLOCK", est_x, shot_x),
              est_y_basic = ifelse(event_type == "BLOCK", est_y, shot_y)
            ) |>
            dplyr::select(-c(est_x, est_y))
        }
      ),
    data =
      purrr::map2(
        data,
        shot_blocker_data_5v5_shot_type,
        function(d, b) {
          d |>
            dplyr::left_join(
              b |>
                dplyr::select(
                  shot_x, shot_y, shot_type, est_x, est_y, blockers_type = blocker_dens
                ) |>
                dplyr::mutate(
                  home_skater_strength_state = "5v5",
                  event_team_strength = "EV"
                ),
              by = c("shot_x", "shot_y", "shot_type", "event_team_strength", "home_skater_strength_state")
            ) |>
            dplyr::mutate(
              est_x_type = ifelse(event_type == "BLOCK", est_x, shot_x),
              est_y_type = ifelse(event_type == "BLOCK", est_y, shot_y)
            ) |>
            dplyr::select(-c(est_x, est_y))
        }
      ),
    data =
      purrr::map2(
        data,
        shot_blocker_data_5v5_shot_type_point,
        function(d, b) {
          d |>
            dplyr::left_join(
              b |>
                dplyr::select(
                  shot_x, shot_y, shot_type, point_shot, est_x, est_y, blockers_point = blocker_dens
                ) |>
                dplyr::mutate(
                  home_skater_strength_state = "5v5",
                  event_team_strength = "EV"
                ),
              by = c("shot_x", "shot_y", "shot_type", "point_shot", "event_team_strength", "home_skater_strength_state")
            ) |>
            dplyr::mutate(
              est_x_point = ifelse(event_type == "BLOCK", est_x, shot_x),
              est_y_point = ifelse(event_type == "BLOCK", est_y, shot_y)
            ) |>
            dplyr::select(-c(est_x, est_y))
        }
      )
  ) |>
  dplyr::select(-c(min:shot_blocker_data_5v5_shot_type_point)) |>
  tidyr::unnest(data) |>
  dplyr::ungroup()


training_data |>
  dplyr::filter(
    home_skater_strength_state == "5v5",
    event_team_strength == "EV",
    shot_y > 0,
    shot_zone == "O",
    position_category != "G"
  ) |>
  dplyr::group_by(season, is.na(est_x_basic), is.na(est_x_type), is.na(est_x_point)) |>
  dplyr::tally()


dynamic_xg_corsi <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # dplyr::group_by(season) |>
  # dplyr::filter(lubridate::day(gm_dt) == 12) |>
  # View()
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_basic" = "shot_x", "est_y_basic" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_basic <- get_min_model(mat, shots$is_goal)

          shots <-
            shots |>
            dplyr::select(-c(optimal_goalie_x:target_area)) |>
            dplyr::left_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_type <- get_min_model(mat, shots$is_goal)

          shots <-
            shots |>
            dplyr::select(-c(optimal_goalie_x:target_area)) |>
            dplyr::left_join(
              goalie_geometry,
              by = c("est_x_point" = "shot_x", "est_y_point" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_point <- get_min_model(mat, shots$is_goal)

          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing,
          #     shots
          #   )[, -1]
          #
          # min_score <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush,
          #     shots
          #   )[, -1]
          #
          # min_score_rush <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush +
          #       is_reached_goalie_followup,
          #     shots
          #   )[, -1]
          #
          # min_score_rush_rebound <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush +
          #       is_reached_goalie_followup +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_score_rush_rebound_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush +
          #       is_reached_goalie_followup +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush +
          #       is_reached_goalie_followup +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_rush_rebound_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_score_rush_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_rush_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_rush +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_rush_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_reached_goalie_followup,
          #     shots
          #   )[, -1]
          #
          # min_score_rebound <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_reached_goalie_followup +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_score_rebound_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_reached_goalie_followup +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_rebound_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_reached_goalie_followup +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_rebound_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_score_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_leading +
          #       is_trailing +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_score_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush,
          #     shots
          #   )[, -1]
          #
          # min_rush <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush +
          #       is_reached_goalie_followup,
          #     shots
          #   )[, -1]
          #
          # min_rush_rebound <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush +
          #       is_reached_goalie_followup +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_rush_rebound_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush +
          #       is_reached_goalie_followup +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush +
          #       is_reached_goalie_followup +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_rush_rebound_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_rush_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_rush_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_rush +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_rush_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_reached_goalie_followup,
          #     shots
          #   )[, -1]
          #
          # min_rebound <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_reached_goalie_followup +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_rebound_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_reached_goalie_followup +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_rebound_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_reached_goalie_followup +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_rebound_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_off_faceoff,
          #     shots
          #   )[, -1]
          #
          # min_fac <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_off_faceoff +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_fac_turn <- get_min_model(mat, shots$is_goal)
          #
          # mat <-
          #   model.matrix(
          #     is_goal ~
          #       dist_to_goalie_optimal +
          #       width_at_net +
          #       avg_height +
          #       is_slap +
          #       is_tip +
          #       is_other +
          #       is_off_turnover,
          #     shots
          #   )[, -1]
          #
          # min_turn <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            basic = list(min_basic), #
            type = list(min_type), #
            point = list(min_point), #
            # score = list(min_score), #
            # score_rush = list(min_score_rush), #
            # score_rush_rebound = list(min_score_rush_rebound), #
            # score_rush_rebound_fac = list(min_score_rush_rebound_fac), #
            # score_rush_rebound_fac_turn = list(min_score_rush_rebound_fac_turn), #
            # score_rush_rebound_turn = list(min_score_rush_rebound_turn), #
            # score_rush_fac = list(min_score_rush_fac), #
            # score_rush_fac_turn = list(min_score_rush_fac_turn), #
            # score_rush_turn = list(min_score_rush_turn), #
            # score_rebound = list(min_score_rebound), #
            # score_rebound_fac = list(min_score_rebound_fac), #
            # score_rebound_fac_turn = list(min_score_rebound_fac_turn), #
            # score_rebound_turn = list(min_score_rebound_turn), #
            # score_fac = list(min_score_fac), #
            # score_fac_turn = list(min_score_fac_turn), #
            # score_turn = list(min_score_turn), #
            # rush = list(min_rush), #
            # rush_rebound = list(min_rush_rebound), #
            # rush_rebound_fac = list(min_rush_rebound_fac), #
            # rush_rebound_fac_turn = list(min_rush_rebound_fac_turn), #
            # rush_rebound_turn = list(min_rush_rebound_turn), #
            # rush_fac = list(min_rush_fac), #
            # rush_fac_turn = list(min_rush_fac_turn), #
            # rush_turn = list(min_rush_turn), #
            # rebound = list(min_rebound), #
            # rebound_fac = list(min_rebound_fac), #
            # rebound_fac_turn = list(min_rebound_fac_turn), #
            # rebound_turn = list(min_rebound_turn), #
            # fac = list(min_fac), #
            # fac_turn = list(min_fac_turn), #
            # turn = list(min_turn) #
          )
        }
      )
  )

corsi_est_coord_metrics <-
  dynamic_xg_corsi |>
  # head() |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    basic =
      purrr::map2(
        basic,
        gm_dt,
        function(m, d) {
          shots <-
            training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_basic" = "shot_x", "est_y_basic" = "shot_y")
            ) |>
            dplyr::select(
              tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
            )

          xg = predict(
            m,
            model.matrix(
              is_goal ~ .,
              shots
            )[, -1],
            type = "response"
          ) |>
            as.double()
        }
      ),
    type =
      purrr::map2(
        type,
        gm_dt,
        function(m, d) {
          shots <-
            training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            ) |>
            dplyr::select(
              tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
            )

          xg = predict(
            m,
            model.matrix(
              is_goal ~ .,
              shots
            )[, -1],
            type = "response"
          ) |>
            as.double()
        }
      ),
    point =
      purrr::map2(
        point,
        gm_dt,
        function(m, d) {
          shots <-
            training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_point" = "shot_x", "est_y_point" = "shot_y")
            ) |>
            dplyr::select(
              tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
            )

          xg = predict(
            m,
            model.matrix(
              is_goal ~ .,
              shots
            )[, -1],
            type = "response"
          ) |>
            as.double()
        }
      ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
              # event_type != "BLOCK"
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  # View()
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      basic:point,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      basic:point,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      basic:point,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)"),
    metric = name |> stringr::str_extract("calib|ll|auc")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  )

corsi_est_coord_metrics |> View()




xg_values_corsi <-
  dynamic_xg_corsi |>
  # head() |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    basic =
      purrr::map2(
        basic,
        gm_dt,
        function(m, d) {
          shots <-
            training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_basic" = "shot_x", "est_y_basic" = "shot_y")
            ) |>
            dplyr::select(
              tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
            )

          xg = predict(
            m,
            model.matrix(
              is_goal ~ .,
              shots
            )[, -1],
            type = "response"
          ) |>
            as.double()
        }
      ),
    type =
      purrr::map2(
        type,
        gm_dt,
        function(m, d) {
          shots <-
            training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            ) |>
            dplyr::select(
              tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
            )

          xg = predict(
            m,
            model.matrix(
              is_goal ~ .,
              shots
            )[, -1],
            type = "response"
          ) |>
            as.double()
        }
      ),
    point =
      purrr::map2(
        point,
        gm_dt,
        function(m, d) {
          shots <-
            training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_point" = "shot_x", "est_y_point" = "shot_y")
            ) |>
            dplyr::select(
              tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
            )

          xg = predict(
            m,
            model.matrix(
              is_goal ~ .,
              shots
            )[, -1],
            type = "response"
          ) |>
            as.double()
        }
      ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
              # event_type != "BLOCK"
            ) |>
            dplyr::select(game_id, event_id, is_goal)
        }
      )
  ) |>
  tidyr::unnest()




team_predictiveness_corsi <-
  tibble::tibble(
    team_games = 10:72,
    metric = "net",
    xg_pred =
      purrr::map(
        team_games,
        function(gms) {
          pred_xg_shot_data_22 |>
            dplyr::bind_rows(pred_xg_shot_data_23) |>
            dplyr::bind_rows(pred_xg_shot_data_24) |>
            dplyr::bind_rows(pred_xg_shot_data_25) |>
            dplyr::mutate(home_score = sign(home_score_diff), is_home = event_team == home_team) |>
            dplyr::select(game_id, event_id, event_team, home_team, away_team, home_score, is_home) |>
            dplyr::left_join(xg_adj) |>
            dplyr::left_join(g_adj) |>
            dplyr::inner_join(xg_values_corsi) |>
            dplyr::group_by(
              season, game_id, event_team, gm_dt
            ) |>
            dplyr::summarise(
              goal_for = sum(is_goal * g_adj),
              dplyr::across(
                .cols = basic:point,
                .fns = function(x) sum(x * xg_adj),
                .names = "{.col}_xg_for"
              ),
              .groups = "drop"
            ) |>
            dplyr::group_by(season, event_team) |>
            dplyr::mutate(game_num = rank(gm_dt)) |>
            dplyr::group_by(game_id) |>
            dplyr::mutate(
              dplyr::across(
                tidyselect::ends_with("_for"),
                .fns = function(x) {
                  sum(x) - x
                },
                .names = "{.col}_against"
              )
            ) |>
            dplyr::rename_with(
              .cols = tidyselect::contains("_for_"),
              .fn = function(x) x |> stringr::str_remove("for_")
            ) |>
            dplyr::group_by(
              season, event_team, half = (game_num > gms) + 1
            ) |>
            dplyr::summarise(
              n = dplyr::n(),
              dplyr::across(
                c(tidyselect::ends_with("_for"), tidyselect::ends_with("_against")),
                sum
              ),
              .groups = "drop"
            ) |>
            dplyr::inner_join(
              readr::read_csv("nst_22.csv") |>
                dplyr::mutate(season = 20222023) |>
                dplyr::bind_rows(
                  readr::read_csv("nst_23.csv") |>
                    dplyr::mutate(season = 20232024)
                ) |>
                dplyr::bind_rows(
                  readr::read_csv("nst_24.csv") |>
                    dplyr::mutate(season = 20242025)
                ) |>
                dplyr::bind_rows(
                  readr::read_csv("nst_25.csv") |>
                    dplyr::mutate(season = 20252026)
                ) |>
                dplyr::mutate(
                  event_team =
                    stringr::str_replace_all(
                      Team,
                      c(
                        "Anaheim Ducks" = "ANA",
                        "Arizona Coyotes" = "ARI",
                        "Boston Bruins" = "BOS",
                        "Buffalo Sabres" = "BUF",
                        "Calgary Flames" = "CGY",
                        "Carolina Hurricanes" = "CAR",
                        "Chicago Blackhawks" = "CHI",
                        "Colorado Avalanche" = "COL",
                        "Columbus Blue Jackets" = "CBJ",
                        "Dallas Stars" = "DAL",
                        "Detroit Red Wings" = "DET",
                        "Edmonton Oilers" = "EDM",
                        "Florida Panthers" = "FLA",
                        "Los Angeles Kings" = "LAK",
                        "Minnesota Wild" = "MIN",
                        "Montreal Canadiens" = "MTL",
                        "Nashville Predators" = "NSH",
                        "New Jersey Devils" = "NJD",
                        "New York Islanders" = "NYI",
                        "New York Rangers" = "NYR",
                        "Ottawa Senators" = "OTT",
                        "Philadelphia Flyers" = "PHI",
                        "Pittsburgh Penguins" = "PIT",
                        "San Jose Sharks" = "SJS",
                        "Seattle Kraken" = "SEA",
                        "St Louis Blues" = "STL",
                        "Tampa Bay Lightning" = "TBL",
                        "Toronto Maple Leafs" = "TOR",
                        "Utah Hockey Club" = "UTA",
                        "Utah Mammoth" = "UTA",
                        "Vancouver Canucks" = "VAN",
                        "Vegas Golden Knights" = "VGK",
                        "Washington Capitals" = "WSH",
                        "Winnipeg Jets" = "WPG"
                      )
                    )
                ) |>
                dplyr::select(-c(tidyselect::ends_with("%"), `...3`)) |>
                dplyr::mutate(
                  game_date =
                    Game |>
                    stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") |>
                    lubridate::ymd()
                ) |>
                dplyr::group_by(season, event_team) |>
                dplyr::mutate(game_num = rank(game_date)) |>
                dplyr::group_by(
                  season, event_team, half = (game_num > gms) + 1
                ) |>
                dplyr::summarise(
                  n = dplyr::n(),
                  toi = sum(TOI),
                  dplyr::across(
                    c(tidyselect::ends_with("F"), tidyselect::ends_with("A")),
                    sum
                  ),
                  .groups = "drop"
                )
            ) |>
            # View()
            tidyr::pivot_longer(
              c(
                tidyselect::ends_with("_for"), tidyselect::ends_with("_against"),
                tidyselect::ends_with("F", ignore.case = F), tidyselect::ends_with("A", ignore.case = F)
              )
            ) |>
            dplyr::mutate(
              metric = name |> stringr::str_remove("((_for)|(_against)|(F)|(A))$"),
              direction = name |> stringr::str_extract("((for)|(against)|(F)|(A))$"),
              direction = ifelse(direction %in% c("for", "F"), "for", "against")
            ) |>
            dplyr::group_by(season, event_team, half, metric) |>
            dplyr::summarise(
              for_per = sum(value * (direction == "for")) / (sum(toi * (direction == "for")) / 60),
              against_per = sum(value * (direction == "against")) / (sum(toi * (direction == "against")) / 60),
              net_per = for_per - against_per,
              perc = sum(value * (direction %in% "for")) / sum(value),
              .groups = "drop"
            ) |>
            tidyr::unite("metric", metric, half) |>
            tidyr::pivot_wider(
              id_cols = c(season, event_team),
              names_from = metric,
              values_from = net_per
            ) |>
            dplyr::select(-c(season, event_team)) |>
            corrr::correlate() |>
            tidyr::pivot_longer(
              -c(term),
              names_to = "term_2"
            ) |>
            dplyr::filter(
              (
                stringr::str_remove(term, "_1") ==
                  stringr::str_remove(term_2, "_2")
              ) |
                (
                  stringr::str_detect(term, "_1") &
                    term_2 %in% c("G_1", "G_2")
                )
            ) |>
            dplyr::mutate(
              type =
                dplyr::case_when(
                  stringr::str_detect(term, "_1") &
                    term_2 %in% c("G_2") ~ "pred",
                  stringr::str_detect(term, "_1") &
                    term_2 %in% c("G_1") ~ "desc",
                  stringr::str_remove(term, "_1") ==
                    stringr::str_remove(term_2, "_2") ~ "auto",
                  T ~ "other"
                )
            ) |>
            dplyr::filter(!is.na(value))
        }
      )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      team_games = 10:72,
      metric = "for",
      xg_pred =
        purrr::map(
          team_games,
          function(gms) {
            pred_xg_shot_data_22 |>
              dplyr::bind_rows(pred_xg_shot_data_23) |>
              dplyr::bind_rows(pred_xg_shot_data_24) |>
              dplyr::bind_rows(pred_xg_shot_data_25) |>
              dplyr::mutate(home_score = sign(home_score_diff), is_home = event_team == home_team) |>
              dplyr::select(game_id, event_id, event_team, home_team, away_team, home_score, is_home) |>
              dplyr::left_join(xg_adj) |>
              dplyr::left_join(g_adj) |>
              dplyr::inner_join(xg_values_corsi) |>
              dplyr::group_by(
                season, game_id, event_team, gm_dt
              ) |>
              dplyr::summarise(
                goal_for = sum(is_goal * g_adj),
                dplyr::across(
                  .cols = basic:point,
                  .fns = function(x) sum(x * xg_adj),
                  .names = "{.col}_xg_for"
                ),
                .groups = "drop"
              ) |>
              dplyr::group_by(season, event_team) |>
              dplyr::mutate(game_num = rank(gm_dt)) |>
              dplyr::group_by(game_id) |>
              dplyr::mutate(
                dplyr::across(
                  tidyselect::ends_with("_for"),
                  .fns = function(x) {
                    sum(x) - x
                  },
                  .names = "{.col}_against"
                )
              ) |>
              dplyr::rename_with(
                .cols = tidyselect::contains("_for_"),
                .fn = function(x) x |> stringr::str_remove("for_")
              ) |>
              dplyr::group_by(
                season, event_team, half = (game_num > gms) + 1
              ) |>
              dplyr::summarise(
                n = dplyr::n(),
                dplyr::across(
                  c(tidyselect::ends_with("_for"), tidyselect::ends_with("_against")),
                  sum
                ),
                .groups = "drop"
              ) |>
              dplyr::inner_join(
                readr::read_csv("nst_22.csv") |>
                  dplyr::mutate(season = 20222023) |>
                  dplyr::bind_rows(
                    readr::read_csv("nst_23.csv") |>
                      dplyr::mutate(season = 20232024)
                  ) |>
                  dplyr::bind_rows(
                    readr::read_csv("nst_24.csv") |>
                      dplyr::mutate(season = 20242025)
                  ) |>
                  dplyr::bind_rows(
                    readr::read_csv("nst_25.csv") |>
                      dplyr::mutate(season = 20252026)
                  ) |>
                  dplyr::mutate(
                    event_team =
                      stringr::str_replace_all(
                        Team,
                        c(
                          "Anaheim Ducks" = "ANA",
                          "Arizona Coyotes" = "ARI",
                          "Boston Bruins" = "BOS",
                          "Buffalo Sabres" = "BUF",
                          "Calgary Flames" = "CGY",
                          "Carolina Hurricanes" = "CAR",
                          "Chicago Blackhawks" = "CHI",
                          "Colorado Avalanche" = "COL",
                          "Columbus Blue Jackets" = "CBJ",
                          "Dallas Stars" = "DAL",
                          "Detroit Red Wings" = "DET",
                          "Edmonton Oilers" = "EDM",
                          "Florida Panthers" = "FLA",
                          "Los Angeles Kings" = "LAK",
                          "Minnesota Wild" = "MIN",
                          "Montreal Canadiens" = "MTL",
                          "Nashville Predators" = "NSH",
                          "New Jersey Devils" = "NJD",
                          "New York Islanders" = "NYI",
                          "New York Rangers" = "NYR",
                          "Ottawa Senators" = "OTT",
                          "Philadelphia Flyers" = "PHI",
                          "Pittsburgh Penguins" = "PIT",
                          "San Jose Sharks" = "SJS",
                          "Seattle Kraken" = "SEA",
                          "St Louis Blues" = "STL",
                          "Tampa Bay Lightning" = "TBL",
                          "Toronto Maple Leafs" = "TOR",
                          "Utah Hockey Club" = "UTA",
                          "Utah Mammoth" = "UTA",
                          "Vancouver Canucks" = "VAN",
                          "Vegas Golden Knights" = "VGK",
                          "Washington Capitals" = "WSH",
                          "Winnipeg Jets" = "WPG"
                        )
                      )
                  ) |>
                  dplyr::select(-c(tidyselect::ends_with("%"), `...3`)) |>
                  dplyr::mutate(
                    game_date =
                      Game |>
                      stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") |>
                      lubridate::ymd()
                  ) |>
                  dplyr::group_by(season, event_team) |>
                  dplyr::mutate(game_num = rank(game_date)) |>
                  dplyr::group_by(
                    season, event_team, half = (game_num > gms) + 1
                  ) |>
                  dplyr::summarise(
                    n = dplyr::n(),
                    toi = sum(TOI),
                    dplyr::across(
                      c(tidyselect::ends_with("F"), tidyselect::ends_with("A")),
                      sum
                    ),
                    .groups = "drop"
                  )
              ) |>
              # View()
              tidyr::pivot_longer(
                c(
                  tidyselect::ends_with("_for"), tidyselect::ends_with("_against"),
                  tidyselect::ends_with("F", ignore.case = F), tidyselect::ends_with("A", ignore.case = F)
                )
              ) |>
              dplyr::mutate(
                metric = name |> stringr::str_remove("((_for)|(_against)|(F)|(A))$"),
                direction = name |> stringr::str_extract("((for)|(against)|(F)|(A))$"),
                direction = ifelse(direction %in% c("for", "F"), "for", "against")
              ) |>
              dplyr::group_by(season, event_team, half, metric) |>
              dplyr::summarise(
                for_per = sum(value * (direction == "for")) / (sum(toi * (direction == "for")) / 60),
                against_per = sum(value * (direction == "against")) / (sum(toi * (direction == "against")) / 60),
                net_per = for_per - against_per,
                perc = sum(value * (direction %in% "for")) / sum(value),
                .groups = "drop"
              ) |>
              tidyr::unite("metric", metric, half) |>
              tidyr::pivot_wider(
                id_cols = c(season, event_team),
                names_from = metric,
                values_from = for_per
              ) |>
              dplyr::select(-c(season, event_team)) |>
              corrr::correlate() |>
              tidyr::pivot_longer(
                -c(term),
                names_to = "term_2"
              ) |>
              dplyr::filter(
                (
                  stringr::str_remove(term, "_1") ==
                    stringr::str_remove(term_2, "_2")
                ) |
                  (
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_1", "G_2")
                  )
              ) |>
              dplyr::mutate(
                type =
                  dplyr::case_when(
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_2") ~ "pred",
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_1") ~ "desc",
                    stringr::str_remove(term, "_1") ==
                      stringr::str_remove(term_2, "_2") ~ "auto",
                    T ~ "other"
                  )
              ) |>
              dplyr::filter(!is.na(value))
          }
        )
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      team_games = 10:72,
      metric = "against",
      xg_pred =
        purrr::map(
          team_games,
          function(gms) {
            pred_xg_shot_data_22 |>
              dplyr::bind_rows(pred_xg_shot_data_23) |>
              dplyr::bind_rows(pred_xg_shot_data_24) |>
              dplyr::bind_rows(pred_xg_shot_data_25) |>
              dplyr::mutate(home_score = sign(home_score_diff), is_home = event_team == home_team) |>
              dplyr::select(game_id, event_id, event_team, home_team, away_team, home_score, is_home) |>
              dplyr::left_join(xg_adj) |>
              dplyr::left_join(g_adj) |>
              dplyr::inner_join(xg_values_corsi) |>
              dplyr::group_by(
                season, game_id, event_team, gm_dt
              ) |>
              dplyr::summarise(
                goal_for = sum(is_goal * g_adj),
                dplyr::across(
                  .cols = basic:point,
                  .fns = function(x) sum(x * xg_adj),
                  .names = "{.col}_xg_for"
                ),
                .groups = "drop"
              ) |>
              dplyr::group_by(season, event_team) |>
              dplyr::mutate(game_num = rank(gm_dt)) |>
              dplyr::group_by(game_id) |>
              dplyr::mutate(
                dplyr::across(
                  tidyselect::ends_with("_for"),
                  .fns = function(x) {
                    sum(x) - x
                  },
                  .names = "{.col}_against"
                )
              ) |>
              dplyr::rename_with(
                .cols = tidyselect::contains("_for_"),
                .fn = function(x) x |> stringr::str_remove("for_")
              ) |>
              dplyr::group_by(
                season, event_team, half = (game_num > gms) + 1
              ) |>
              dplyr::summarise(
                n = dplyr::n(),
                dplyr::across(
                  c(tidyselect::ends_with("_for"), tidyselect::ends_with("_against")),
                  sum
                ),
                .groups = "drop"
              ) |>
              dplyr::inner_join(
                readr::read_csv("nst_22.csv") |>
                  dplyr::mutate(season = 20222023) |>
                  dplyr::bind_rows(
                    readr::read_csv("nst_23.csv") |>
                      dplyr::mutate(season = 20232024)
                  ) |>
                  dplyr::bind_rows(
                    readr::read_csv("nst_24.csv") |>
                      dplyr::mutate(season = 20242025)
                  ) |>
                  dplyr::bind_rows(
                    readr::read_csv("nst_25.csv") |>
                      dplyr::mutate(season = 20252026)
                  ) |>
                  dplyr::mutate(
                    event_team =
                      stringr::str_replace_all(
                        Team,
                        c(
                          "Anaheim Ducks" = "ANA",
                          "Arizona Coyotes" = "ARI",
                          "Boston Bruins" = "BOS",
                          "Buffalo Sabres" = "BUF",
                          "Calgary Flames" = "CGY",
                          "Carolina Hurricanes" = "CAR",
                          "Chicago Blackhawks" = "CHI",
                          "Colorado Avalanche" = "COL",
                          "Columbus Blue Jackets" = "CBJ",
                          "Dallas Stars" = "DAL",
                          "Detroit Red Wings" = "DET",
                          "Edmonton Oilers" = "EDM",
                          "Florida Panthers" = "FLA",
                          "Los Angeles Kings" = "LAK",
                          "Minnesota Wild" = "MIN",
                          "Montreal Canadiens" = "MTL",
                          "Nashville Predators" = "NSH",
                          "New Jersey Devils" = "NJD",
                          "New York Islanders" = "NYI",
                          "New York Rangers" = "NYR",
                          "Ottawa Senators" = "OTT",
                          "Philadelphia Flyers" = "PHI",
                          "Pittsburgh Penguins" = "PIT",
                          "San Jose Sharks" = "SJS",
                          "Seattle Kraken" = "SEA",
                          "St Louis Blues" = "STL",
                          "Tampa Bay Lightning" = "TBL",
                          "Toronto Maple Leafs" = "TOR",
                          "Utah Hockey Club" = "UTA",
                          "Utah Mammoth" = "UTA",
                          "Vancouver Canucks" = "VAN",
                          "Vegas Golden Knights" = "VGK",
                          "Washington Capitals" = "WSH",
                          "Winnipeg Jets" = "WPG"
                        )
                      )
                  ) |>
                  dplyr::select(-c(tidyselect::ends_with("%"), `...3`)) |>
                  dplyr::mutate(
                    game_date =
                      Game |>
                      stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") |>
                      lubridate::ymd()
                  ) |>
                  dplyr::group_by(season, event_team) |>
                  dplyr::mutate(game_num = rank(game_date)) |>
                  dplyr::group_by(
                    season, event_team, half = (game_num > gms) + 1
                  ) |>
                  dplyr::summarise(
                    n = dplyr::n(),
                    toi = sum(TOI),
                    dplyr::across(
                      c(tidyselect::ends_with("F"), tidyselect::ends_with("A")),
                      sum
                    ),
                    .groups = "drop"
                  )
              ) |>
              # View()
              tidyr::pivot_longer(
                c(
                  tidyselect::ends_with("_for"), tidyselect::ends_with("_against"),
                  tidyselect::ends_with("F", ignore.case = F), tidyselect::ends_with("A", ignore.case = F)
                )
              ) |>
              dplyr::mutate(
                metric = name |> stringr::str_remove("((_for)|(_against)|(F)|(A))$"),
                direction = name |> stringr::str_extract("((for)|(against)|(F)|(A))$"),
                direction = ifelse(direction %in% c("for", "F"), "for", "against")
              ) |>
              dplyr::group_by(season, event_team, half, metric) |>
              dplyr::summarise(
                for_per = sum(value * (direction == "for")) / (sum(toi * (direction == "for")) / 60),
                against_per = sum(value * (direction == "against")) / (sum(toi * (direction == "against")) / 60),
                net_per = for_per - against_per,
                perc = sum(value * (direction %in% "for")) / sum(value),
                .groups = "drop"
              ) |>
              tidyr::unite("metric", metric, half) |>
              tidyr::pivot_wider(
                id_cols = c(season, event_team),
                names_from = metric,
                values_from = against_per
              ) |>
              dplyr::select(-c(season, event_team)) |>
              corrr::correlate() |>
              tidyr::pivot_longer(
                -c(term),
                names_to = "term_2"
              ) |>
              dplyr::filter(
                (
                  stringr::str_remove(term, "_1") ==
                    stringr::str_remove(term_2, "_2")
                ) |
                  (
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_1", "G_2")
                  )
              ) |>
              dplyr::mutate(
                type =
                  dplyr::case_when(
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_2") ~ "pred",
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_1") ~ "desc",
                    stringr::str_remove(term, "_1") ==
                      stringr::str_remove(term_2, "_2") ~ "auto",
                    T ~ "other"
                  )
              ) |>
              dplyr::filter(!is.na(value))
          }
        )
    )
  )


team_predictiveness_corsi |>
  dplyr::filter(metric == "net") |>
  tidyr::unnest(xg_pred) |>
  dplyr::filter(type == "pred") |>
  dplyr::group_by(term) |>
  dplyr::filter(
    term %in% c(
      "basic_xg_1",
      "type_xg_1",
      "point_xg_1",
      "xG_1",
      "C_1",
      "goal_1",
      "SC_1"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = team_games, y = value**2, color = term)) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(linewidth = 2, se = F) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_y_continuous(limits = c(0, NA)) +
  ggplot2::theme(legend.position = "bottom")

team_predictiveness_corsi |>
  dplyr::filter(metric == "for") |>
  tidyr::unnest(xg_pred) |>
  dplyr::filter(type == "pred") |>
  dplyr::group_by(term) |>
  dplyr::filter(
    term %in% c(
      "basic_xg_1",
      "type_xg_1",
      "point_xg_1",
      "xG_1",
      "C_1",
      "goal_1",
      "SC_1"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = team_games, y = value**2, color = term)) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(linewidth = 2, se = F) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_y_continuous(limits = c(0, NA)) +
  ggplot2::theme(legend.position = "bottom")

team_predictiveness_corsi |>
  dplyr::filter(metric == "against") |>
  tidyr::unnest(xg_pred) |>
  dplyr::filter(type == "pred") |>
  dplyr::group_by(term) |>
  dplyr::filter(
    term %in% c(
      "basic_xg_1",
      "type_xg_1",
      "point_xg_1",
      "xG_1",
      "C_1",
      "goal_1",
      "SC_1"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = team_games, y = value**2, color = term)) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(linewidth = 2, se = F) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_y_continuous(limits = c(0, NA)) +
  ggplot2::theme(legend.position = "bottom")

nst_games <-
  readr::read_csv("nst_22.csv") |>
  dplyr::mutate(season = 20222023) |>
  dplyr::bind_rows(
    readr::read_csv("nst_23.csv") |>
      dplyr::mutate(season = 20232024)
  ) |>
  dplyr::bind_rows(
    readr::read_csv("nst_24.csv") |>
      dplyr::mutate(season = 20242025)
  ) |>
  dplyr::bind_rows(
    readr::read_csv("nst_25.csv") |>
      dplyr::mutate(season = 20252026)
  ) |>
  dplyr::mutate(
    event_team =
      stringr::str_replace_all(
        Team,
        c(
          "Anaheim Ducks" = "ANA",
          "Arizona Coyotes" = "ARI",
          "Boston Bruins" = "BOS",
          "Buffalo Sabres" = "BUF",
          "Calgary Flames" = "CGY",
          "Carolina Hurricanes" = "CAR",
          "Chicago Blackhawks" = "CHI",
          "Colorado Avalanche" = "COL",
          "Columbus Blue Jackets" = "CBJ",
          "Dallas Stars" = "DAL",
          "Detroit Red Wings" = "DET",
          "Edmonton Oilers" = "EDM",
          "Florida Panthers" = "FLA",
          "Los Angeles Kings" = "LAK",
          "Minnesota Wild" = "MIN",
          "Montreal Canadiens" = "MTL",
          "Nashville Predators" = "NSH",
          "New Jersey Devils" = "NJD",
          "New York Islanders" = "NYI",
          "New York Rangers" = "NYR",
          "Ottawa Senators" = "OTT",
          "Philadelphia Flyers" = "PHI",
          "Pittsburgh Penguins" = "PIT",
          "San Jose Sharks" = "SJS",
          "Seattle Kraken" = "SEA",
          "St Louis Blues" = "STL",
          "Tampa Bay Lightning" = "TBL",
          "Toronto Maple Leafs" = "TOR",
          "Utah Hockey Club" = "UTA",
          "Utah Mammoth" = "UTA",
          "Vancouver Canucks" = "VAN",
          "Vegas Golden Knights" = "VGK",
          "Washington Capitals" = "WSH",
          "Winnipeg Jets" = "WPG"
        )
      )
  ) |>
  dplyr::select(-c(tidyselect::ends_with("%"), `...3`)) |>
  dplyr::mutate(
    game_date =
      Game |>
      stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") |>
      lubridate::ymd()
  ) |>
  dplyr::group_by(season, event_team) |>
  dplyr::mutate(game_num = rank(game_date)) |>
  dplyr::ungroup()








team_predictiveness_corsi_all <-
  tibble::tibble(
    team_games = 10:72,
    metric = "net",
    xg_pred =
      purrr::map(
        team_games,
        function(gms) {
          print(gms)
          pred_xg_shot_data_22 |>
            dplyr::bind_rows(pred_xg_shot_data_23) |>
            dplyr::bind_rows(pred_xg_shot_data_24) |>
            dplyr::bind_rows(pred_xg_shot_data_25) |>
            dplyr::mutate(home_score = sign(home_score_diff), is_home = event_team == home_team) |>
            dplyr::select(game_id, event_id, event_team, home_team, away_team, home_score, is_home) |>
            dplyr::left_join(xg_adj) |>
            dplyr::left_join(g_adj) |>
            dplyr::inner_join(xg_values_corsi) |>
            dplyr::group_by(
              season, game_id, event_team, gm_dt
            ) |>
            dplyr::summarise(
              goal_for = sum(is_goal * g_adj),
              dplyr::across(
                .cols = basic:point,
                .fns = function(x) sum(x * xg_adj),
                .names = "{.col}_xg_for"
              ),
              .groups = "drop"
            ) |>
            dplyr::group_by(season, event_team) |>
            dplyr::mutate(game_num = rank(gm_dt)) |>
            dplyr::group_by(game_id) |>
            dplyr::mutate(
              dplyr::across(
                tidyselect::ends_with("_for"),
                .fns = function(x) {
                  sum(x) - x
                },
                .names = "{.col}_against"
              )
            ) |>
            dplyr::rename_with(
              .cols = tidyselect::contains("_for_"),
              .fn = function(x) x |> stringr::str_remove("for_")
            ) |>
            dplyr::group_by(
              season, event_team, half = (game_num > gms) + 1
            ) |>
            dplyr::filter(half == 1) |>
            dplyr::summarise(
              n = dplyr::n(),
              dplyr::across(
                c(tidyselect::ends_with("_for"), tidyselect::ends_with("_against")),
                sum
              ),
              .groups = "drop"
            ) |>
            dplyr::inner_join(
              nst_games |>
                dplyr::group_by(
                  season, event_team, half = (game_num > gms) + 1
                ) |>
                dplyr::filter(half == 1) |>
                dplyr::summarise(
                  n = dplyr::n(),
                  toi = sum(TOI),
                  dplyr::across(
                    c(tidyselect::ends_with("F"), tidyselect::ends_with("A")),
                    sum
                  ),
                  .groups = "drop"
                )
            ) |>
            dplyr::bind_rows(
              nst_games |>
                dplyr::group_by(season, event_team, half = 2) |>
                dplyr::summarise(
                  n = dplyr::n(),
                  dplyr::across(TOI:SCA, sum),
                  .groups = "drop"
                ) |>
                dplyr::rename(toi = TOI)
            ) |>
            tidyr::pivot_longer(
              c(
                tidyselect::ends_with("_for"), tidyselect::ends_with("_against"),
                tidyselect::ends_with("F", ignore.case = F), tidyselect::ends_with("A", ignore.case = F)
              )
            ) |>
            dplyr::mutate(
              metric = name |> stringr::str_remove("((_for)|(_against)|(F)|(A))$"),
              direction = name |> stringr::str_extract("((for)|(against)|(F)|(A))$"),
              direction = ifelse(direction %in% c("for", "F"), "for", "against")
            ) |>
            dplyr::group_by(season, event_team, half, metric) |>
            dplyr::summarise(
              for_per = sum(value * (direction == "for")) / (sum(toi * (direction == "for")) / 60),
              against_per = sum(value * (direction == "against")) / (sum(toi * (direction == "against")) / 60),
              net_per = for_per - against_per,
              perc = sum(value * (direction %in% "for")) / sum(value),
              .groups = "drop"
            ) |>
            tidyr::unite("metric", metric, half) |>
            tidyr::pivot_wider(
              id_cols = c(season, event_team),
              names_from = metric,
              values_from = net_per
            ) |>
            dplyr::select(-c(season, event_team)) |>
            corrr::correlate() |>
            tidyr::pivot_longer(
              -c(term),
              names_to = "term_2"
            ) |>
            dplyr::filter(
              (
                stringr::str_detect(term, "_1") &
                  term_2 %in% c("G_2")
              )
            ) |>
            dplyr::mutate(
              type =
                dplyr::case_when(
                  stringr::str_detect(term, "_1") &
                    term_2 %in% c("G_2") ~ "pred",
                  stringr::str_detect(term, "_1") &
                    term_2 %in% c("G_1") ~ "desc",
                  stringr::str_remove(term, "_1") ==
                    stringr::str_remove(term_2, "_2") ~ "auto",
                  T ~ "other"
                )
            ) |>
            dplyr::filter(!is.na(value))
        }
      )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      team_games = 10:72,
      metric = "for",
      xg_pred =
        purrr::map(
          team_games,
          function(gms) {
            print(gms)
            pred_xg_shot_data_22 |>
              dplyr::bind_rows(pred_xg_shot_data_23) |>
              dplyr::bind_rows(pred_xg_shot_data_24) |>
              dplyr::bind_rows(pred_xg_shot_data_25) |>
              dplyr::mutate(home_score = sign(home_score_diff), is_home = event_team == home_team) |>
              dplyr::select(game_id, event_id, event_team, home_team, away_team, home_score, is_home) |>
              dplyr::left_join(xg_adj) |>
              dplyr::left_join(g_adj) |>
              dplyr::inner_join(xg_values_corsi) |>
              dplyr::group_by(
                season, game_id, event_team, gm_dt
              ) |>
              dplyr::summarise(
                goal_for = sum(is_goal * g_adj),
                dplyr::across(
                  .cols = basic:point,
                  .fns = function(x) sum(x * xg_adj),
                  .names = "{.col}_xg_for"
                ),
                .groups = "drop"
              ) |>
              dplyr::group_by(season, event_team) |>
              dplyr::mutate(game_num = rank(gm_dt)) |>
              dplyr::group_by(game_id) |>
              dplyr::mutate(
                dplyr::across(
                  tidyselect::ends_with("_for"),
                  .fns = function(x) {
                    sum(x) - x
                  },
                  .names = "{.col}_against"
                )
              ) |>
              dplyr::rename_with(
                .cols = tidyselect::contains("_for_"),
                .fn = function(x) x |> stringr::str_remove("for_")
              ) |>
              dplyr::group_by(
                season, event_team, half = (game_num > gms) + 1
              ) |>
              dplyr::filter(half == 1) |>
              dplyr::summarise(
                n = dplyr::n(),
                dplyr::across(
                  c(tidyselect::ends_with("_for"), tidyselect::ends_with("_against")),
                  sum
                ),
                .groups = "drop"
              ) |>
              dplyr::inner_join(
                nst_games |>
                  dplyr::group_by(
                    season, event_team, half = (game_num > gms) + 1
                  ) |>
                  dplyr::filter(half == 1) |>
                  dplyr::summarise(
                    n = dplyr::n(),
                    toi = sum(TOI),
                    dplyr::across(
                      c(tidyselect::ends_with("F"), tidyselect::ends_with("A")),
                      sum
                    ),
                    .groups = "drop"
                  )
              ) |>
              dplyr::bind_rows(
                nst_games |>
                  dplyr::group_by(season, event_team, half = 2) |>
                  dplyr::summarise(
                    n = dplyr::n(),
                    dplyr::across(TOI:SCA, sum),
                    .groups = "drop"
                  ) |>
                  dplyr::rename(toi = TOI)
              ) |>
              tidyr::pivot_longer(
                c(
                  tidyselect::ends_with("_for"), tidyselect::ends_with("_against"),
                  tidyselect::ends_with("F", ignore.case = F), tidyselect::ends_with("A", ignore.case = F)
                )
              ) |>
              dplyr::mutate(
                metric = name |> stringr::str_remove("((_for)|(_against)|(F)|(A))$"),
                direction = name |> stringr::str_extract("((for)|(against)|(F)|(A))$"),
                direction = ifelse(direction %in% c("for", "F"), "for", "against")
              ) |>
              dplyr::group_by(season, event_team, half, metric) |>
              dplyr::summarise(
                for_per = sum(value * (direction == "for")) / (sum(toi * (direction == "for")) / 60),
                against_per = sum(value * (direction == "against")) / (sum(toi * (direction == "against")) / 60),
                net_per = for_per - against_per,
                perc = sum(value * (direction %in% "for")) / sum(value),
                .groups = "drop"
              ) |>
              tidyr::unite("metric", metric, half) |>
              tidyr::pivot_wider(
                id_cols = c(season, event_team),
                names_from = metric,
                values_from = for_per
              ) |>
              dplyr::select(-c(season, event_team)) |>
              corrr::correlate() |>
              tidyr::pivot_longer(
                -c(term),
                names_to = "term_2"
              ) |>
              dplyr::filter(
                (
                  stringr::str_detect(term, "_1") &
                    term_2 %in% c("G_2")
                )
              ) |>
              dplyr::mutate(
                type =
                  dplyr::case_when(
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_2") ~ "pred",
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_1") ~ "desc",
                    stringr::str_remove(term, "_1") ==
                      stringr::str_remove(term_2, "_2") ~ "auto",
                    T ~ "other"
                  )
              ) |>
              dplyr::filter(!is.na(value))
          }
        )
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      team_games = 10:72,
      metric = "against",
      xg_pred =
        purrr::map(
          team_games,
          function(gms) {
            print(gms)
            pred_xg_shot_data_22 |>
              dplyr::bind_rows(pred_xg_shot_data_23) |>
              dplyr::bind_rows(pred_xg_shot_data_24) |>
              dplyr::bind_rows(pred_xg_shot_data_25) |>
              dplyr::mutate(home_score = sign(home_score_diff), is_home = event_team == home_team) |>
              dplyr::select(game_id, event_id, event_team, home_team, away_team, home_score, is_home) |>
              dplyr::left_join(xg_adj) |>
              dplyr::left_join(g_adj) |>
              dplyr::inner_join(xg_values_corsi) |>
              dplyr::group_by(
                season, game_id, event_team, gm_dt
              ) |>
              dplyr::summarise(
                goal_for = sum(is_goal * g_adj),
                dplyr::across(
                  .cols = basic:point,
                  .fns = function(x) sum(x * xg_adj),
                  .names = "{.col}_xg_for"
                ),
                .groups = "drop"
              ) |>
              dplyr::group_by(season, event_team) |>
              dplyr::mutate(game_num = rank(gm_dt)) |>
              dplyr::group_by(game_id) |>
              dplyr::mutate(
                dplyr::across(
                  tidyselect::ends_with("_for"),
                  .fns = function(x) {
                    sum(x) - x
                  },
                  .names = "{.col}_against"
                )
              ) |>
              dplyr::rename_with(
                .cols = tidyselect::contains("_for_"),
                .fn = function(x) x |> stringr::str_remove("for_")
              ) |>
              dplyr::group_by(
                season, event_team, half = (game_num > gms) + 1
              ) |>
              dplyr::filter(half == 1) |>
              dplyr::summarise(
                n = dplyr::n(),
                dplyr::across(
                  c(tidyselect::ends_with("_for"), tidyselect::ends_with("_against")),
                  sum
                ),
                .groups = "drop"
              ) |>
              dplyr::inner_join(
                nst_games |>
                  dplyr::group_by(
                    season, event_team, half = (game_num > gms) + 1
                  ) |>
                  dplyr::filter(half == 1) |>
                  dplyr::summarise(
                    n = dplyr::n(),
                    toi = sum(TOI),
                    dplyr::across(
                      c(tidyselect::ends_with("F"), tidyselect::ends_with("A")),
                      sum
                    ),
                    .groups = "drop"
                  )
              ) |>
              dplyr::bind_rows(
                nst_games |>
                  dplyr::group_by(season, event_team, half = 2) |>
                  dplyr::summarise(
                    n = dplyr::n(),
                    dplyr::across(TOI:SCA, sum),
                    .groups = "drop"
                  ) |>
                  dplyr::rename(toi = TOI)
              ) |>
              tidyr::pivot_longer(
                c(
                  tidyselect::ends_with("_for"), tidyselect::ends_with("_against"),
                  tidyselect::ends_with("F", ignore.case = F), tidyselect::ends_with("A", ignore.case = F)
                )
              ) |>
              dplyr::mutate(
                metric = name |> stringr::str_remove("((_for)|(_against)|(F)|(A))$"),
                direction = name |> stringr::str_extract("((for)|(against)|(F)|(A))$"),
                direction = ifelse(direction %in% c("for", "F"), "for", "against")
              ) |>
              dplyr::group_by(season, event_team, half, metric) |>
              dplyr::summarise(
                for_per = sum(value * (direction == "for")) / (sum(toi * (direction == "for")) / 60),
                against_per = sum(value * (direction == "against")) / (sum(toi * (direction == "against")) / 60),
                net_per = for_per - against_per,
                perc = sum(value * (direction %in% "for")) / sum(value),
                .groups = "drop"
              ) |>
              tidyr::unite("metric", metric, half) |>
              tidyr::pivot_wider(
                id_cols = c(season, event_team),
                names_from = metric,
                values_from = against_per
              ) |>
              dplyr::select(-c(season, event_team)) |>
              corrr::correlate() |>
              tidyr::pivot_longer(
                -c(term),
                names_to = "term_2"
              ) |>
              dplyr::filter(
                (
                  stringr::str_detect(term, "_1") &
                    term_2 %in% c("G_2")
                )
              ) |>
              dplyr::mutate(
                type =
                  dplyr::case_when(
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_2") ~ "pred",
                    stringr::str_detect(term, "_1") &
                      term_2 %in% c("G_1") ~ "desc",
                    stringr::str_remove(term, "_1") ==
                      stringr::str_remove(term_2, "_2") ~ "auto",
                    T ~ "other"
                  )
              ) |>
              dplyr::filter(!is.na(value))
          }
        )
    )
  )

team_predictiveness_corsi_all |>
  dplyr::filter(metric == "net") |>
  tidyr::unnest(xg_pred) |>
  dplyr::filter(type == "pred") |>
  dplyr::group_by(term) |>
  dplyr::filter(
    term %in% c(
      "basic_xg_1",
      "type_xg_1",
      "point_xg_1",
      "xG_1",
      "C_1",
      "goal_1",
      "SC_1"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = team_games, y = value**2, color = term)) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(linewidth = 2, se = F) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_y_continuous(limits = c(0, NA)) +
  ggplot2::theme(legend.position = "bottom")

team_predictiveness_corsi_all |>
  dplyr::filter(metric == "for") |>
  tidyr::unnest(xg_pred) |>
  dplyr::filter(type == "pred") |>
  dplyr::group_by(term) |>
  dplyr::filter(
    term %in% c(
      "basic_xg_1",
      "type_xg_1",
      "point_xg_1",
      "xG_1",
      "C_1",
      "goal_1",
      "SC_1"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = team_games, y = value**2, color = term)) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(linewidth = 2, se = F) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_y_continuous(limits = c(0, NA)) +
  ggplot2::theme(legend.position = "bottom")

team_predictiveness_corsi_all |>
  dplyr::filter(metric == "against") |>
  tidyr::unnest(xg_pred) |>
  dplyr::filter(type == "pred") |>
  dplyr::group_by(term) |>
  dplyr::filter(
    term %in% c(
      "basic_xg_1",
      "type_xg_1",
      "point_xg_1",
      "xG_1",
      "C_1",
      "goal_1",
      "SC_1"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = team_games, y = value**2, color = term)) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(linewidth = 2, se = F) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_y_continuous(limits = c(0, NA)) +
  ggplot2::theme(legend.position = "bottom")


dynamic_xg_corsi_context <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_basic <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing,
              shots
            )[, -1]

          min_score <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush,
              shots
            )[, -1]

          min_score_rush <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush +
                is_reached_goalie_followup,
              shots
            )[, -1]

          min_score_rush_rebound <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush +
                is_reached_goalie_followup +
                is_off_faceoff,
              shots
            )[, -1]

          min_score_rush_rebound_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush +
                is_reached_goalie_followup +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_score_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush +
                is_reached_goalie_followup +
                is_off_turnover,
              shots
            )[, -1]

          min_score_rush_rebound_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush +
                is_off_faceoff,
              shots
            )[, -1]

          min_score_rush_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_score_rush_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_rush +
                is_off_turnover,
              shots
            )[, -1]

          min_score_rush_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_reached_goalie_followup,
              shots
            )[, -1]

          min_score_rebound <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_reached_goalie_followup +
                is_off_faceoff,
              shots
            )[, -1]

          min_score_rebound_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_reached_goalie_followup +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_score_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_reached_goalie_followup +
                is_off_turnover,
              shots
            )[, -1]

          min_score_rebound_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_off_faceoff,
              shots
            )[, -1]

          min_score_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_score_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_off_turnover,
              shots
            )[, -1]

          min_score_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush,
              shots
            )[, -1]

          min_rush <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_reached_goalie_followup,
              shots
            )[, -1]

          min_rush_rebound <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_reached_goalie_followup +
                is_off_faceoff,
              shots
            )[, -1]

          min_rush_rebound_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_reached_goalie_followup +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_reached_goalie_followup +
                is_off_turnover,
              shots
            )[, -1]

          min_rush_rebound_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_off_faceoff,
              shots
            )[, -1]

          min_rush_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_rush_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_off_turnover,
              shots
            )[, -1]

          min_rush_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup,
              shots
            )[, -1]

          min_rebound <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_off_faceoff,
              shots
            )[, -1]

          min_rebound_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_off_turnover,
              shots
            )[, -1]

          min_rebound_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_faceoff,
              shots
            )[, -1]

          min_fac <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_faceoff +
                is_off_turnover,
              shots
            )[, -1]

          min_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover,
              shots
            )[, -1]

          min_turn <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            basic = list(min_basic), #
            score = list(min_score), #
            score_rush = list(min_score_rush), #
            score_rush_rebound = list(min_score_rush_rebound), #
            score_rush_rebound_fac = list(min_score_rush_rebound_fac), #
            score_rush_rebound_fac_turn = list(min_score_rush_rebound_fac_turn), #
            score_rush_rebound_turn = list(min_score_rush_rebound_turn), #
            score_rush_fac = list(min_score_rush_fac), #
            score_rush_fac_turn = list(min_score_rush_fac_turn), #
            score_rush_turn = list(min_score_rush_turn), #
            score_rebound = list(min_score_rebound), #
            score_rebound_fac = list(min_score_rebound_fac), #
            score_rebound_fac_turn = list(min_score_rebound_fac_turn), #
            score_rebound_turn = list(min_score_rebound_turn), #
            score_fac = list(min_score_fac), #
            score_fac_turn = list(min_score_fac_turn), #
            score_turn = list(min_score_turn), #
            rush = list(min_rush), #
            rush_rebound = list(min_rush_rebound), #
            rush_rebound_fac = list(min_rush_rebound_fac), #
            rush_rebound_fac_turn = list(min_rush_rebound_fac_turn), #
            rush_rebound_turn = list(min_rush_rebound_turn), #
            rush_fac = list(min_rush_fac), #
            rush_fac_turn = list(min_rush_fac_turn), #
            rush_turn = list(min_rush_turn), #
            rebound = list(min_rebound), #
            rebound_fac = list(min_rebound_fac), #
            rebound_fac_turn = list(min_rebound_fac_turn), #
            rebound_turn = list(min_rebound_turn), #
            fac = list(min_fac), #
            fac_turn = list(min_fac_turn), #
            turn = list(min_turn) #
          )
        }
      )
  )


corsi_context_metrics <-
  dynamic_xg_corsi_context |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    dplyr::across(
      -c(season:min),
      .fns =
        function(m) {
          purrr::map2(
            .x = m,
            .y = gm_dt,
            function(m, d) {
              shots <-
                training_data |>
                dplyr::filter(
                  game_date == d,
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5")
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
                ) |>
                dplyr::select(
                  tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
                )

              xg = predict(
                m,
                model.matrix(
                  is_goal ~ .,
                  shots
                )[, -1],
                type = "response"
              ) |>
                as.double()
            }
          )
        }
    ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  # View()
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      basic:turn,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      basic:turn,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      basic:turn,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)"),
    metric = name |> stringr::str_extract("calib|ll|auc")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  )

View(corsi_context_metrics)


dynamic_xg_corsi_context |>
  # dplyr::filter(lubridate::day(gm_dt) == 12) |>
  dplyr::ungroup() |>
  dplyr::select(gm_dt, xg_results) |>
  tidyr::unnest(xg_results) |>
  tidyr::pivot_longer(
    -c(gm_dt),
    # tidyselect::everything(),
    names_to = "model"
  ) |>
  dplyr::mutate(
    coefs =
      purrr::map(
        value,
        function(m) {
          coef(m) |>
            as.matrix() |>
            as.data.frame() |>
            tibble::rownames_to_column() |>
            tibble::as_tibble()
        }
      )
  ) |>
  dplyr::select(-c(value)) |>
  tidyr::unnest(coefs) |>
  dplyr::filter(
    rowname != "(Intercept)"
    # !rowname %in% c("(Intercept)", "is_slap", "is_tip", "is_other")
  ) |>
  # tidyr::pivot_wider(
  #   id_cols = c(model, gm_dt),
  #   values_from = s0,
  #   names_from = rowname
  # ) |>
  ggplot2::ggplot(ggplot2::aes(x = dplyr::dense_rank(gm_dt), y = s0, color = rowname)) +
  ggplot2::facet_wrap(ggplot2::vars(model), scales = "fixed", nrow = 4) +
  ggplot2::geom_hline(yintercept = 0, color = "red", linetype = 2) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::theme(legend.position = "bottom")





dynamic_xg_corsi_rush_testing <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )


          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo +
                is_counter_rush,
              shots
            )[, -1]

          min_rush_secs_velo_counter <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo,
              shots
            )[, -1]

          min_rush_secs_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                is_counter_rush,
              shots
            )[, -1]

          min_rush_secs_counter <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs,
              shots
            )[, -1]

          min_rush_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_velo +
                is_counter_rush,
              shots
            )[, -1]

          min_rush_velo_counter <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_velo,
              shots
            )[, -1]

          min_rush_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                is_counter_rush,
              shots
            )[, -1]

          min_rush_counter <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush,
              shots
            )[, -1]

          min_rush <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            rush_secs_velo_counter = list(min_rush_secs_velo_counter),
            rush_secs_velo = list(min_rush_secs_velo),
            rush_secs_counter = list(min_rush_secs_counter),
            rush_secs = list(min_rush_secs),
            rush_velo_counter = list(min_rush_velo_counter),
            rush_velo = list(min_rush_velo),
            rush_counter = list(min_rush_counter),
            rush = list(min_rush)
          )
        }
      )
  )

corsi_rush_metrics <-
  dynamic_xg_corsi_rush_testing |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    dplyr::across(
      -c(season:min),
      .fns =
        function(m) {
          purrr::map2(
            .x = m,
            .y = gm_dt,
            function(m, d) {
              shots <-
                training_data |>
                dplyr::filter(
                  game_date == d,
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5")
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
                ) |>
                dplyr::select(
                  tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
                )

              xg = predict(
                m,
                model.matrix(
                  is_goal ~ .,
                  shots
                )[, -1],
                type = "response"
              ) |>
                as.double()
            }
          )
        }
    ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      rush_secs_velo_counter:rush,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      rush_secs_velo_counter:rush,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      rush_secs_velo_counter:rush,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)"),
    metric = name |> stringr::str_extract("calib|ll|auc")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  )

View(corsi_rush_metrics)




dynamic_xg_corsi_fac_testing <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_faceoff +
                is_off_faceoff_win +
                faceoff_secs,
              shots
            )[, -1]

          min_fac_win_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_faceoff +
                is_off_faceoff_win,
              shots
            )[, -1]

          min_fac_win <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_faceoff +
                faceoff_secs,
              shots
            )[, -1]

          min_fac_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_faceoff,
              shots
            )[, -1]

          min_fac <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            fac_win_secs = list(min_fac_win_secs),
            fac_win = list(min_fac_win),
            fac_secs = list(min_fac_secs),
            fac = list(min_fac),
          )
        }
      )
  )

corsi_fac_metrics <-
  dynamic_xg_corsi_fac_testing |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    dplyr::across(
      -c(season:min),
      .fns =
        function(m) {
          purrr::map2(
            .x = m,
            .y = gm_dt,
            function(m, d) {
              shots <-
                training_data |>
                dplyr::filter(
                  game_date == d,
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5")
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
                ) |>
                dplyr::select(
                  tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
                )

              xg = predict(
                m,
                model.matrix(
                  is_goal ~ .,
                  shots
                )[, -1],
                type = "response"
              ) |>
                as.double()
            }
          )
        }
    ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      fac_win_secs:fac,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      fac_win_secs:fac,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      fac_win_secs:fac,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)"),
    metric = name |> stringr::str_extract("calib|ll|auc")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  )

View(corsi_fac_metrics)






dynamic_xg_corsi_rebound_testing <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_follow_own_secs_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs,
              shots
            )[, -1]

          min_rebound_follow_own_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_follow_own_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup,
              shots
            )[, -1]

          min_rebound_follow_own <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot +
                followup_secs +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_follow_secs_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot +
                followup_secs,
              shots
            )[, -1]

          min_rebound_follow_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_follow_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_followup_shot,
              shots
            )[, -1]

          min_rebound_follow <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_own_followup +
                followup_secs +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_own_secs_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_own_followup +
                followup_secs,
              shots
            )[, -1]

          min_rebound_own_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_own_followup +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_own_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                is_own_followup,
              shots
            )[, -1]

          min_rebound_own <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                followup_secs +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_secs_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                followup_secs,
              shots
            )[, -1]

          min_rebound_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup +
                angle_change_velo,
              shots
            )[, -1]

          min_rebound_velo <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_reached_goalie_followup,
              shots
            )[, -1]

          min_rebound <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            rebound_follow_own_secs_velo = list(min_rebound_follow_own_secs_velo),
            rebound_follow_own_secs = list(min_rebound_follow_own_secs),
            rebound_follow_own_velo = list(min_rebound_follow_own_velo),
            rebound_follow_own = list(min_rebound_follow_own),
            rebound_follow_secs_velo = list(min_rebound_follow_secs_velo),
            rebound_follow_secs = list(min_rebound_follow_secs),
            rebound_follow_velo = list(min_rebound_follow_velo),
            rebound_follow = list(min_rebound_follow),
            rebound_own_secs_velo = list(min_rebound_own_secs_velo),
            rebound_own_secs = list(min_rebound_own_secs),
            rebound_own_velo = list(min_rebound_own_velo),
            rebound_own = list(min_rebound_own),
            rebound_secs_velo = list(min_rebound_secs_velo),
            rebound_secs = list(min_rebound_secs),
            rebound_velo = list(min_rebound_velo),
            rebound = list(min_rebound)
          )
        }
      )
  )

corsi_rebound_metrics <-
  dynamic_xg_corsi_rebound_testing |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    dplyr::across(
      -c(season:min),
      .fns =
        function(m) {
          purrr::map2(
            .x = m,
            .y = gm_dt,
            function(m, d) {
              shots <-
                training_data |>
                dplyr::filter(
                  game_date == d,
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5")
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
                ) |>
                dplyr::select(
                  tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
                )

              xg = predict(
                m,
                model.matrix(
                  is_goal ~ .,
                  shots
                )[, -1],
                type = "response"
              ) |>
                as.double()
            }
          )
        }
    ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      rebound_follow_own_secs_velo:rebound,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      rebound_follow_own_secs_velo:rebound,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      rebound_follow_own_secs_velo:rebound,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)$"),
    metric = name |> stringr::str_extract("(calib|ll|auc)$")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  )

View(corsi_rebound_metrics)






dynamic_xg_corsi_turn_testing <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover +
                is_oz_turnover +
                shooter_same_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_turn_oz_same_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover +
                is_oz_turnover +
                shooter_same_turnover,
              shots
            )[, -1]

          min_turn_oz_same <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover +
                is_oz_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_turn_oz_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover +
                is_oz_turnover,
              shots
            )[, -1]

          min_turn_oz <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover +
                shooter_same_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_turn_same_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover +
                shooter_same_turnover,
              shots
            )[, -1]

          min_turn_same <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_turn_secs <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_turnover,
              shots
            )[, -1]

          min_turn <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            turn_oz_same_secs = list(min_turn_oz_same_secs),
            turn_oz_same = list(min_turn_oz_same),
            turn_oz_secs = list(min_turn_oz_secs),
            turn_oz = list(min_turn_oz),
            turn_same_secs = list(min_turn_same_secs),
            turn_same = list(min_turn_same),
            turn_secs = list(min_turn_secs),
            turn = list(min_turn)
          )
        }
      )
  )

corsi_turn_metrics <-
  dynamic_xg_corsi_turn_testing |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    dplyr::across(
      -c(season:min),
      .fns =
        function(m) {
          purrr::map2(
            .x = m,
            .y = gm_dt,
            function(m, d) {
              shots <-
                training_data |>
                dplyr::filter(
                  game_date == d,
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5")
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
                ) |>
                dplyr::select(
                  tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
                )

              xg = predict(
                m,
                model.matrix(
                  is_goal ~ .,
                  shots
                )[, -1],
                type = "response"
              ) |>
                as.double()
            }
          )
        }
    ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      turn_oz_same_secs:turn,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      turn_oz_same_secs:turn,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      turn_oz_same_secs:turn,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)$"),
    metric = name |> stringr::str_extract("(calib|ll|auc)$")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  )

View(corsi_turn_metrics)



dynamic_xg_corsi_score_testing <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_oshell_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_leading_trailing_oshell_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_oshell_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off +
                is_shell_def,
              shots
            )[, -1]

          min_leading_trailing_oshell_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_oshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off +
                play_for_tie,
              shots
            )[, -1]

          min_leading_trailing_oshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_oshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_off,
              shots
            )[, -1]

          min_leading_trailing_oshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_leading_trailing_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                is_shell_def,
              shots
            )[, -1]

          min_leading_trailing_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                play_for_tie,
              shots
            )[, -1]

          min_leading_trailing_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing +
                garbage_time,
              shots
            )[, -1]

          min_leading_trailing_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_trailing,
              shots
            )[, -1]

          min_leading_trailing <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_oshell_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_leading_oshell_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_leading_oshell_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off +
                is_shell_def,
              shots
            )[, -1]

          min_leading_oshell_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_oshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off +
                play_for_tie,
              shots
            )[, -1]

          min_leading_oshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off +
                garbage_time,
              shots
            )[, -1]

          min_leading_oshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_off,
              shots
            )[, -1]

          min_leading_oshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_leading_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_leading_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_shell_def,
              shots
            )[, -1]

          min_leading_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_leading_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                play_for_tie,
              shots
            )[, -1]

          min_leading_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                garbage_time,
              shots
            )[, -1]

          min_leading_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading,
              shots
            )[, -1]

          min_leading <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_trailing_oshell_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_trailing_oshell_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_trailing_oshell_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off +
                is_shell_def,
              shots
            )[, -1]

          min_trailing_oshell_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_trailing_oshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off +
                play_for_tie,
              shots
            )[, -1]

          min_trailing_oshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off +
                garbage_time,
              shots
            )[, -1]

          min_trailing_oshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_off,
              shots
            )[, -1]

          min_trailing_oshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_trailing_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_trailing_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_trailing_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                is_shell_def,
              shots
            )[, -1]

          min_trailing_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_trailing_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                play_for_tie,
              shots
            )[, -1]

          min_trailing_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing +
                garbage_time,
              shots
            )[, -1]

          min_trailing_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_trailing,
              shots
            )[, -1]

          min_trailing <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_oshell_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_oshell_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_oshell_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off +
                is_shell_def,
              shots
            )[, -1]

          min_oshell_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_oshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off +
                play_for_tie,
              shots
            )[, -1]

          min_oshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off +
                garbage_time,
              shots
            )[, -1]

          min_oshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_off,
              shots
            )[, -1]

          min_oshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_def +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_dshell_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_def +
                play_for_tie,
              shots
            )[, -1]

          min_dshell_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_def +
                garbage_time,
              shots
            )[, -1]

          min_dshell_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_shell_def,
              shots
            )[, -1]

          min_dshell <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                play_for_tie +
                garbage_time,
              shots
            )[, -1]

          min_tie_garbage <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                play_for_tie,
              shots
            )[, -1]

          min_tie <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                garbage_time,
              shots
            )[, -1]

          min_garbage <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            leading_trailing_oshell_dshell_tie_garbage = list(min_leading_trailing_oshell_dshell_tie_garbage),
            leading_trailing_oshell_dshell_tie = list(min_leading_trailing_oshell_dshell_tie),
            leading_trailing_oshell_dshell_garbage = list(min_leading_trailing_oshell_dshell_garbage),
            leading_trailing_oshell_dshell = list(min_leading_trailing_oshell_dshell),
            leading_trailing_oshell_tie_garbage = list(min_leading_trailing_oshell_tie_garbage),
            leading_trailing_oshell_tie = list(min_leading_trailing_oshell_tie),
            leading_trailing_oshell_garbage = list(min_leading_trailing_oshell_garbage),
            leading_trailing_oshell = list(min_leading_trailing_oshell),
            leading_trailing_dshell_tie_garbage = list(min_leading_trailing_dshell_tie_garbage),
            leading_trailing_dshell_tie = list(min_leading_trailing_dshell_tie),
            leading_trailing_dshell_garbage = list(min_leading_trailing_dshell_garbage),
            leading_trailing_dshell = list(min_leading_trailing_dshell),
            leading_trailing_tie_garbage = list(min_leading_trailing_tie_garbage),
            leading_trailing_tie = list(min_leading_trailing_tie),
            leading_trailing_garbage = list(min_leading_trailing_garbage),
            leading_trailing = list(min_leading_trailing),
            leading_oshell_dshell_tie_garbage = list(min_leading_oshell_dshell_tie_garbage),
            leading_oshell_dshell_tie = list(min_leading_oshell_dshell_tie),
            leading_oshell_dshell_garbage = list(min_leading_oshell_dshell_garbage),
            leading_oshell_dshell = list(min_leading_oshell_dshell),
            leading_oshell_tie_garbage = list(min_leading_oshell_tie_garbage),
            leading_oshell_tie = list(min_leading_oshell_tie),
            leading_oshell_garbage = list(min_leading_oshell_garbage),
            leading_oshell = list(min_leading_oshell),
            leading_dshell_tie_garbage = list(min_leading_dshell_tie_garbage),
            leading_dshell_tie = list(min_leading_dshell_tie),
            leading_dshell_garbage = list(min_leading_dshell_garbage),
            leading_dshell = list(min_leading_dshell),
            leading_tie_garbage = list(min_leading_tie_garbage),
            leading_tie = list(min_leading_tie),
            leading_garbage = list(min_leading_garbage),
            leading = list(min_leading),
            trailing_oshell_dshell_tie_garbage = list(min_trailing_oshell_dshell_tie_garbage),
            trailing_oshell_dshell_tie = list(min_trailing_oshell_dshell_tie),
            trailing_oshell_dshell_garbage = list(min_trailing_oshell_dshell_garbage),
            trailing_oshell_dshell = list(min_trailing_oshell_dshell),
            trailing_oshell_tie_garbage = list(min_trailing_oshell_tie_garbage),
            trailing_oshell_tie = list(min_trailing_oshell_tie),
            trailing_oshell_garbage = list(min_trailing_oshell_garbage),
            trailing_oshell = list(min_trailing_oshell),
            trailing_dshell_tie_garbage = list(min_trailing_dshell_tie_garbage),
            trailing_dshell_tie = list(min_trailing_dshell_tie),
            trailing_dshell_garbage = list(min_trailing_dshell_garbage),
            trailing_dshell = list(min_trailing_dshell),
            trailing_tie_garbage = list(min_trailing_tie_garbage),
            trailing_tie = list(min_trailing_tie),
            trailing_garbage = list(min_trailing_garbage),
            trailing = list(min_trailing),
            oshell_dshell_tie_garbage = list(min_oshell_dshell_tie_garbage),
            oshell_dshell_tie = list(min_oshell_dshell_tie),
            oshell_dshell_garbage = list(min_oshell_dshell_garbage),
            oshell_dshell = list(min_oshell_dshell),
            oshell_tie_garbage = list(min_oshell_tie_garbage),
            oshell_tie = list(min_oshell_tie),
            oshell_garbage = list(min_oshell_garbage),
            oshell = list(min_oshell),
            dshell_tie_garbage = list(min_dshell_tie_garbage),
            dshell_tie = list(min_dshell_tie),
            dshell_garbage = list(min_dshell_garbage),
            dshell = list(min_dshell),
            tie_garbage = list(min_tie_garbage),
            tie = list(min_tie),
            garbage = list(min_garbage)
          )
        }
      )
  )

corsi_score_metrics <-
  dynamic_xg_corsi_score_testing |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    dplyr::across(
      -c(season:min),
      .fns =
        function(m) {
          purrr::map2(
            .x = m,
            .y = gm_dt,
            function(m, d) {
              shots <-
                training_data |>
                dplyr::filter(
                  game_date == d,
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5")
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
                ) |>
                dplyr::select(
                  tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
                )

              xg = predict(
                m,
                model.matrix(
                  is_goal ~ .,
                  shots
                )[, -1],
                type = "response"
              ) |>
                as.double()
            }
          )
        }
    ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      leading_trailing_oshell_dshell_tie_garbage:garbage,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      leading_trailing_oshell_dshell_tie_garbage:garbage,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      leading_trailing_oshell_dshell_tie_garbage:garbage,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)$"),
    metric = name |> stringr::str_extract("(calib|ll|auc)$")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  )

View(corsi_score_metrics)



dynamic_xg_corsi_det_testing <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_rush +
                rush_secs +
                rush_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_score_rush_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_score_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_score_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_rush_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_score_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            det_score_rush_fac_turn = list(min_score_rush_fac_turn),
            det_score_fac_turn = list(min_score_fac_turn),
            det_score_rush_rebound_fac_turn = list(min_score_rush_rebound_fac_turn),
            det_rush_fac_turn = list(min_rush_fac_turn),
            det_score_rebound_fac_turn = list(min_score_rebound_fac_turn),
            det_fac_turn = list(min_fac_turn),
            det_rush_rebound_fac_turn = list(min_rush_rebound_fac_turn)
          )
        }
      )
  )

corsi_detailed_context_metrics <-
  dynamic_xg_corsi_det_testing |>
  # dplyr::filter(season > 20222023) |>
  tidyr::unnest(xg_results) |>
  dplyr::transmute(
    season,
    gm_dt,
    dplyr::across(
      -c(season:min),
      .fns =
        function(m) {
          purrr::map2(
            .x = m,
            .y = gm_dt,
            function(m, d) {
              shots <-
                training_data |>
                dplyr::filter(
                  game_date == d,
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5")
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
                ) |>
                dplyr::select(
                  tidyselect::any_of(c("is_goal", coef(m) |> rownames()))
                )

              xg = predict(
                m,
                model.matrix(
                  is_goal ~ .,
                  shots
                )[, -1],
                type = "response"
              ) |>
                as.double()
            }
          )
        }
    ),
    is_goal =
      purrr::map(
        gm_dt,
        function(d) {
          training_data |>
            dplyr::filter(
              game_date == d,
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5")
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      det_score_rush_fac_turn:det_rush_rebound_fac_turn,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      det_score_rush_fac_turn:det_rush_rebound_fac_turn,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      det_score_rush_fac_turn:det_rush_rebound_fac_turn,
      .fns = function(xg) {
        MLmetrics::AUC(
          xg,
          is_goal
        )
      },
      .names = "{.col}_auc"
    )
  ) |>
  tidyr::pivot_longer(tidyselect::everything()) |>
  dplyr::transmute(
    value,
    model = name |> stringr::str_remove("_(calib|ll|auc)$"),
    metric = name |> stringr::str_extract("(calib|ll|auc)$")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    names_from = metric,
    values_from = value
  ) |>
  View()

View(corsi_detailed_context_metrics)



dynamic_xg_corsi_det_testing |>
  # dplyr::filter(lubridate::day(gm_dt) == 12) |>
  dplyr::ungroup() |>
  dplyr::select(gm_dt, xg_results) |>
  tidyr::unnest(xg_results) |>
  dplyr::select(gm_dt, det_score_rebound_fac_turn) |>
  dplyr::mutate(
    coefs =
      purrr::map(
        det_score_rebound_fac_turn,
        function(m) {
          coef(m) |>
            as.matrix() |>
            as.data.frame() |>
            tibble::rownames_to_column() |>
            tibble::as_tibble()
        }
      )
  ) |>
  dplyr::select(-c(det_score_rebound_fac_turn)) |>
  tidyr::unnest(coefs) |>
  dplyr::filter(
    rowname != "(Intercept)"
    # !rowname %in% c("(Intercept)", "is_slap", "is_tip", "is_other")
  ) |>
  # tidyr::pivot_wider(
  #   id_cols = c(model, gm_dt),
  #   values_from = s0,
  #   names_from = rowname
  # ) |>
  ggplot2::ggplot(ggplot2::aes(x = dplyr::dense_rank(gm_dt), y = s0)) +
  ggplot2::facet_wrap(ggplot2::vars(rowname), scales = "fixed", nrow = 4) +
  ggplot2::geom_hline(yintercept = 0, color = "red", linetype = 2) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_color_viridis_d() +
  ggplot2::theme(legend.position = "bottom")







dynamic_xg_corsi_blocker_testing <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20192020 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 3262) |>
  dplyr::group_by(season, gm_dt) |>
  dplyr::summarise(min = min(game_num), .groups = "drop") |>
  # dplyr::group_by(season) |>
  # dplyr::filter(lubridate::day(gm_dt) == 12) |>
  # View()
  # head(1) |>
  dplyr::mutate(
    xg_results =
      purrr::pmap(
        list(
          dt = gm_dt,
          m = min
          # basic = shot_blocker_data_5v5_basic,
          # shot_type = shot_blocker_data_5v5_shot_type,
          # shot_type_point = shot_blocker_data_5v5_shot_type_point
        ),
        function(dt, m) {
          start_time <- Sys.time()

          print(
            "{dt} start" |>
              glue::glue()
          )

          shots <-
            training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("est_x_type" = "shot_x", "est_y_type" = "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_score_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_basic,
              shots
            )[, -1]

          min_score_rush_rebound_fac_turn_basic <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_type,
              shots
            )[, -1]

          min_score_rush_rebound_fac_turn_type <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_point,
              shots
            )[, -1]

          min_score_rush_rebound_fac_turn_point <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_score_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_basic,
              shots
            )[, -1]

          min_score_rebound_fac_turn_basic <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_type,
              shots
            )[, -1]

          min_score_rebound_fac_turn_type <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_leading +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_point,
              shots
            )[, -1]

          min_score_rebound_fac_turn_point <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_basic,
              shots
            )[, -1]

          min_rush_rebound_fac_turn_basic <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_type,
              shots
            )[, -1]

          min_rush_rebound_fac_turn_type <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                is_slap +
                is_tip +
                is_other +
                is_rush +
                rush_secs +
                rush_velo +
                is_reached_goalie_followup +
                is_followup_shot +
                is_own_followup +
                followup_secs +
                angle_change_velo +
                is_off_faceoff +
                faceoff_secs +
                is_off_turnover +
                turnover_secs +
                blockers_point,
              shots
            )[, -1]

          min_rush_rebound_fac_turn_point <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            det_score_rush_rebound_fac_turn = list(min_score_rush_rebound_fac_turn),
            det_score_rush_rebound_fac_turn_basic = list(min_score_rush_rebound_fac_turn_basic),
            det_score_rush_rebound_fac_turn_type = list(min_score_rush_rebound_fac_turn_type),
            det_score_rush_rebound_fac_turn_point = list(min_score_rush_rebound_fac_turn_point),
            det_score_rebound_fac_turn = list(min_score_rebound_fac_turn),
            det_score_rebound_fac_turn_basic = list(min_score_rebound_fac_turn_basic),
            det_score_rebound_fac_turn_type = list(min_score_rebound_fac_turn_type),
            det_score_rebound_fac_turn_point = list(min_score_rebound_fac_turn_point),
            det_rush_rebound_fac_turn = list(min_rush_rebound_fac_turn),
            det_rush_rebound_fac_turn_basic = list(min_rush_rebound_fac_turn_basic),
            det_rush_rebound_fac_turn_type = list(min_rush_rebound_fac_turn_type),
            det_rush_rebound_fac_turn_point = list(min_rush_rebound_fac_turn_point)
          )
        }
      )
  )

dynamic_xg_corsi_blocker_testing |>
  dplyr::ungroup() |>
  dplyr::select(xg_results) |>
  tidyr::unnest(xg_results) |>
  tidyr::pivot_longer(
    tidyselect::everything(),
    names_to = "model"
  ) |>
  dplyr::mutate(
    coefs =
      purrr::map(
        value,
        function(m) {
          coef(m) |>
            as.matrix() |>
            as.data.frame() |>
            tibble::rownames_to_column() |>
            tibble::as_tibble()
        }
      )
  ) |>
  dplyr::select(-c(value)) |>
  tidyr::unnest(coefs) |>
  dplyr::filter(
    !rowname %in% c("(Intercept)", "width_at_net", "avg_height", "is_slap", "is_tip", "is_other")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    values_from = s0,
    names_from = rowname
  ) |>
  View()

