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
  dplyr::summarise(min = min(game_num)) |>
  dplyr::group_by(season) |>
  dplyr::filter(lubridate::day(gm_dt) == 12) |>
  # View()
  # head() |>
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
              goalie_geometry |>
                dplyr::mutate(
                  dist_to_center = sqrt((shot_x**2) + (shot_y**2)),
                  dist_to_center_2 = dist_to_center**2,
                  dist_to_center_3 = dist_to_center**3,
                  dist_to_goalie_optimal_2 = dist_to_goalie_optimal**2,
                  dist_to_goalie_optimal_3 = dist_to_goalie_optimal**3,
                ),
              by = c("shot_x", "shot_y")
            )

          mat <-
            model.matrix(
              is_goal ~
                dist_to_goalie_optimal +
                angle_center +
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

          set.seed(1138)
          cv <-
            glmnet::cv.glmnet(
              mat,
              shots$is_goal,
              family = "binomial",
              alpha = 1,
              type.measure = "mse"
            )

          set.seed(1138)
          min_score_rush_rebound_fac_turn <-
            glmnet::glmnet(
              mat,
              shots$is_goal,
              family = "binomial",
              alpha = 1,
              lambda = cv$lambda.min
            )

          print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

          tibble::tibble(
            basic = list(min_basic),
            score = list(min_score),
            score_rush = list(min_score_rush),
            score_rush_rebound = list(min_score_rush_rebound),
            score_rush_rebound_fac = list(min_score_rush_rebound_fac),
            score_rush_rebound_fac_turn = list(min_score_rush_rebound_fac_turn),
            score_rush_rebound_turn = list(min_score_rush_rebound_turn),
            score_rush_fac = list(min_score_rush_fac),
            score_rush_fac_turn = list(min_score_rush_fac_turn),
            score_rush_turn = list(min_score_rush_turn),
            score_rebound = list(min_score_rebound),
            score_rebound_fac = list(min_score_rebound_fac),
            score_rebound_fac_turn = list(min_score_rebound_fac_turn),
            score_rebound_turn = list(min_score_rebound_turn),
            score_fac = list(min_score_fac),
            score_fac_turn = list(min_score_fac_turn),
            score_turn = list(min_score_turn),
            rush = list(min_rush),
            rush_rebound = list(min_rush_rebound),
            rush_rebound_fac = list(min_rush_rebound_fac),
            rush_rebound_fac_turn = list(min_rush_rebound_fac_turn),
            rush_rebound_turn = list(min_rush_rebound_turn),
            rush_fac = list(min_rush_fac),
            rush_fac_turn = list(min_rush_fac_turn),
            rush_turn = list(min_rush_turn),
            rebound = list(min_rebound),
            rebound_fac = list(min_rebound_fac),
            rebound_fac_turn = list(min_rebound_fac_turn),
            rebound_turn = list(min_rebound_turn),
            fac = list(min_fac),
            fac_turn = list(min_fac_turn),
            turn = list(min_turn)
          )
        }
      )
  )

test |>
  tidyr::unnest(xg_results) |>
  dplyr::mutate(
    coefs =
      purrr::map(
        m,
        function(m) {
          coef(m) |>
            as.matrix() |>
            as.data.frame() |>
            tibble::rownames_to_column() |>
            tibble::as_tibble()
        }
      )
  ) |>
  dplyr::select(-c(m)) |>
  tidyr::unnest(coefs) |>
  tidyr::pivot_wider(
    id_cols = rowname,
    values_from = s0,
    names_from = gm_dt
  ) |>
  View()

