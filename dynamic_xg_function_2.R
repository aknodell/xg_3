get_min_model <- function(mat, is_goal) {
  set.seed(1138)
  cv <-
    glmnet::cv.glmnet(
      mat,
      is_goal,
      family = "binomial",
      alpha = 0,
      parallel = T,
      type.measure = "mse"
    )

  set.seed(1138)
  # min_basic <-
  glmnet::glmnet(
    mat,
    is_goal,
    family = "binomial",
    alpha = 0,
    parallel = T,
    lambda = cv$lambda.min
  )
}

doParallel::registerDoParallel(cores = 10)

dynamic_xg_2 <-
# test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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



test |>
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
    !rowname %in% c("(Intercept)", "dist_to_goalie_optimal", "is_slap", "is_tip", "is_other")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    values_from = s0,
    names_from = rowname
  ) |>
  View()


test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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

          min_height_width <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                optimal_width_coverage +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_height_width_cover <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                optimal_width_coverage +
                h_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_height_width_cover_h <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                optimal_width_coverage +
                h_angle +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_height_width_cover_h_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                optimal_width_coverage +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_height_width_cover_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                h_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_height_width_h <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                h_angle +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_height_width_h_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                width_at_net +
                avg_height +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_height_width_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                optimal_width_coverage +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area_cover <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                optimal_width_coverage +
                h_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area_cover_h <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                optimal_width_coverage +
                h_angle +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area_cover_h_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                optimal_width_coverage +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area_cover_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                h_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area_h <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                h_angle +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area_h_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                target_area +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_area_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                optimal_width_coverage +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle_cover <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                optimal_width_coverage +
                h_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle_cover_h <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                optimal_width_coverage +
                h_angle +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle_cover_h_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                optimal_width_coverage +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle_cover_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                h_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle_h <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                h_angle +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle_h_v <- get_min_model(mat, shots$is_goal)

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
                v_angle +
                is_slap +
                is_tip +
                is_other,
              shots
            )[, -1]

          min_angle_v <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            height_width = list(min_height_width),
            height_width_cover = list(min_height_width_cover),
            height_width_cover_h = list(min_height_width_cover_h),
            height_width_cover_h_v = list(min_height_width_cover_h_v),
            height_width_cover_v = list(min_height_width_cover_v),
            height_width_h = list(min_height_width_h),
            height_width_h_v = list(min_height_width_h_v),
            height_width_v = list(min_height_width_v),
            area = list(min_area),
            area_cover = list(min_area_cover),
            area_cover_h = list(min_area_cover_h),
            area_cover_h_v = list(min_area_cover_h_v),
            area_cover_v = list(min_area_cover_v),
            area_h = list(min_area_h),
            area_h_v = list(min_area_h_v),
            area_v = list(min_area_v),
            angle = list(min_angle),
            angle_cover = list(min_angle_cover),
            angle_cover_h = list(min_angle_cover_h),
            angle_cover_h_v = list(min_angle_cover_h_v),
            angle_cover_v = list(min_angle_cover_v),
            angle_h = list(min_angle_h),
            angle_h_v = list(min_angle_h_v),
            angle_v = list(min_angle_v)
          )
        }
      )
  )

test |>
  tidyr::unnest(xg_results) |>
  dplyr::mutate(
    coefs =
      purrr::map(
        basic,
        function(m) {
          coef(m) |>
            as.matrix() |>
            as.data.frame() |>
            tibble::rownames_to_column() |>
            tibble::as_tibble()
        }
      )
  ) |>
  dplyr::select(-c(basic)) |>
  tidyr::unnest(coefs) |>
  # dplyr::group_by(rowname, sign(s0)) |>
  # dplyr::tally()
  tidyr::pivot_wider(
    # id_cols = rowname,
    id_cols = gm_dt,
    values_from = s0,
    # names_from = gm_dt
    names_from = rowname
  ) |>
  dplyr::select(-c(gm_dt, `(Intercept)`)) |>
  corrr::correlate() |>
  View()


# dynamic_xg_2 |>
location_metrics <-
  test |>
  # head(1) |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry |>
                    dplyr::mutate(
                      dist_to_center = sqrt((shot_x**2) + (shot_y**2)),
                      dist_to_center_2 = dist_to_center**2,
                      dist_to_center_3 = dist_to_center**3,
                      dist_to_goalie_optimal_2 = dist_to_goalie_optimal**2,
                      dist_to_goalie_optimal_3 = dist_to_goalie_optimal**3,
                    ),
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  dplyr::select(-c(season, gm_dt)) |>
  dplyr::summarise(
    dplyr::across(
      height_width:angle_v,
      .fns = function(xg) {
        sum(xg) / sum(is_goal)
      },
      .names = "{.col}_calib"
    ),
    dplyr::across(
      height_width:angle_v,
      .fns = function(xg) {
        MLmetrics::LogLoss(
          xg,
          is_goal
        )
      },
      .names = "{.col}_ll"
    ),
    dplyr::across(
      height_width:angle_v,
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

location_metrics |>
  tidyr::pivot_longer(
    c(ll, auc)
  ) |>
  dplyr::group_by(name) |>
  dplyr::mutate(
    val_norm = (value - min(value)) / (max(value) - min(value)),
    val_norm = ifelse(name == "ll", -1 * (val_norm -1), val_norm)
  ) |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    w_mean =
      sum(val_norm * ifelse(name == "ll", 2/3, 1/3))
  ) |>
  View()

  # ggplot2::ggplot(ggplot2::aes(x = model, y = val_norm)) +
  # ggplot2::facet_wrap(ggplot2::vars(name), scales = "free", ncol = 2) +
  # ggplot2::geom_point(size = 3) +
  # ggplot2::theme_minimal() +
  # ggplot2::theme(
  #   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  # )

# context_metrics <-
  dynamic_xg_2 |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry |>
                    dplyr::mutate(
                      dist_to_center = sqrt((shot_x**2) + (shot_y**2)),
                      dist_to_center_2 = dist_to_center**2,
                      dist_to_center_3 = dist_to_center**3,
                      dist_to_goalie_optimal_2 = dist_to_goalie_optimal**2,
                      dist_to_goalie_optimal_3 = dist_to_goalie_optimal**3,
                    ),
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
            ) |>
            dplyr::pull(is_goal)
        }
      )
  ) |>
  tidyr::unnest(tidyselect::everything()) |>
  View()
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

context_metrics |>
  View()
  # tidyr::pivot_longer(
  #   c(ll, auc)
  # ) |>
  # dplyr::group_by(name) |>
  # dplyr::mutate(
  #   val_norm = (value - min(value)) / (max(value) - min(value)),
  #   val_norm = ifelse(name == "ll", -1 * (val_norm -1), val_norm)
  # ) |>
  # # dplyr::group_by(model) |>
  # # dplyr::summarise(
  # #   w_mean =
  # #     sum(val_norm * ifelse(name == "ll", 2/3, 1/3))
  # # ) |>
  # # View()
  #
  # ggplot2::ggplot(ggplot2::aes(x = model, y = val_norm)) +
  # ggplot2::facet_wrap(ggplot2::vars(name), scales = "free", ncol = 2) +
  # ggplot2::geom_point(size = 3) +
  # ggplot2::theme_minimal() +
  # ggplot2::theme(
  #   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  # )



dynamic_xg_2 |>
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
  # View()






dynamic_xg_rush_testing <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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

dynamic_xg_rush_testing |>
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

rush_metrics <-
  dynamic_xg_rush_testing |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
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

View(rush_metrics)





dynamic_xg_fac_testing <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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

dynamic_xg_fac_testing |>
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

fac_metrics <-
  dynamic_xg_fac_testing |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
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

View(fac_metrics)





dynamic_xg_rebound_testing <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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

dynamic_xg_rebound_testing |>
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

rebound_metrics <-
  dynamic_xg_rebound_testing |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
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

View(rebound_metrics)



dynamic_xg_turn_testing <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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

dynamic_xg_turn_testing |>
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
    !rowname %in% c("(Intercept)", "dist_to_goalie_optimal", "width_at_net", "avg_height", "is_slap", "is_tip", "is_other")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    values_from = s0,
    names_from = rowname
  ) |>
  View()

turn_metrics <-
  dynamic_xg_turn_testing |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
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

View(turn_metrics)








rebound_metrics <-
  dynamic_xg_rebound_testing |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
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

View(rebound_metrics)



dynamic_xg_score_testing <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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

dynamic_xg_score_testing |>
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
    !rowname %in% c("(Intercept)", "dist_to_goalie_optimal", "width_at_net", "avg_height", "is_slap", "is_tip", "is_other")
  ) |>
  tidyr::pivot_wider(
    id_cols = model,
    values_from = s0,
    names_from = rowname
  ) |>
  View()

score_metrics <-
  dynamic_xg_score_testing |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
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

View(score_metrics)









dynamic_xg_det_testing <-
  # test <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select season, game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(season, gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
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
              game_num < m, game_num >= m - 1312,
              event_type != "BLOCK"
            ) |>
            dplyr::inner_join(
              goalie_geometry,
              by = c("shot_x", "shot_y")
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
                is_oz_turnover +
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
                is_oz_turnover +
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
                is_oz_turnover +
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
                is_oz_turnover +
                turnover_secs,
              shots
            )[, -1]

          min_rush_rebound_fac_turn <- get_min_model(mat, shots$is_goal)

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            det_score_rush_fac_turn = list(min_score_rush_fac_turn),
            det_score_rush_rebound_fac_turn = list(min_score_rush_rebound_fac_turn),
            det_rush_fac_turn = list(min_rush_fac_turn),
            det_rush_rebound_fac_turn = list(min_rush_rebound_fac_turn)
          )
        }
      )
  )

dynamic_xg_det_testing |>
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

detailed_context_metrics <-
  dynamic_xg_det_testing |>
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
                  home_skater_strength_state %in% c("5v5"),
                  event_type != "BLOCK"
                ) |>
                dplyr::inner_join(
                  goalie_geometry,
                  by = c("shot_x", "shot_y")
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
              home_skater_strength_state %in% c("5v5"),
              event_type != "BLOCK"
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

View(fac_metrics)
