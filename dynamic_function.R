training_data <-
  pred_xg_shot_data_21 |>
  dplyr::mutate(season = "21-22") |>
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
      dplyr::mutate(season = "25-25")
  ) |>
  dplyr::mutate(
    point_shot =
      position_category == "D" &
      shot_type %in% c("Slap", "Wrist/Snap")
  ) |>
  dplyr::left_join(
    nhl_db_con |>
      odbc::dbGetQuery(
        "select game_id, game_date from games where season >= 20212022 and session = 2"
      ) |>
      tibble::tibble() |>
      dplyr::arrange(game_date, game_id) |>
      tibble::rowid_to_column(var = "game_num")
  )

dynamic_xg <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
  dplyr::group_by(gm_dt) |>
  dplyr::summarise(min = min(game_num)) |>
  # head(2) |>
  dplyr::mutate(
    shot_data_5v5 =
      purrr::map2(
        gm_dt,
        min,
        function(dt, m) {
          print(
            "{dt} start" |>
              glue::glue()
          )

          training_data |>
            dplyr::filter(
              shot_y > 0,
              shot_zone == "O",
              position_category != "G",
              event_team_strength == "EV",
              home_skater_strength_state %in% c("5v5"),
              game_num < m, game_num >= m - 1312
            ) |>
            dplyr::mutate(
              game_weight =
                (
                  ((730 - as.integer(dt - game_date)) /
                    (
                      1 + (
                        (as.integer(dt - game_date) - 1) / 364
                      )
                    )
                  )**0.5
                ) /
                27
            )
        }
      ),
    shot_blocker_data_5v5_basic =
      purrr::map2(
        gm_dt,
        shot_data_5v5,
        function(dt, df_5v5) {
          print(
            "{dt} density" |>
              glue::glue()
          )

          att_density <-
            df_5v5 |>
            dplyr::group_by(
              event_team_strength,
              home_skater_strength_state
            ) |>
            tidyr::nest() |>
            dplyr::mutate(
              dens =
                purrr::map(
                  data,
                  function(df) {
                    MASS::kde2d(
                      x =
                        df |>
                        dplyr::filter(event_type != "BLOCK") |>
                        dplyr::pull(shot_x),
                      y =
                        df |>
                        dplyr::filter(event_type != "BLOCK") |>
                        dplyr::pull(shot_y),
                      lims = c(c(-42, 42), c(1, 64)),
                      n = c(85, 64)
                    ) |>
                      purrr::pluck("z") |>
                      tibble::as_tibble(.name_repair = "unique") |>
                      suppressMessages() |>
                      dplyr::mutate(shot_x = seq(-42, 42)) |>
                      tidyr::pivot_longer(-c(shot_x), names_to = "shot_y", values_to = "fen_z") |>
                      dplyr::mutate(shot_y = shot_y |> stringr::str_extract("\\d+") |> as.integer()) |>
                      dplyr::filter(
                        !(shot_y <= 17 & shot_x <= -14.5 & sqrt((shot_x - -14.5)**2 + (shot_y - 17)**2) > 28),
                        !(shot_y <= 17 & shot_x >= 14.5 & sqrt((shot_x - 14.5)**2 + (shot_y - 17)**2) > 28)
                      ) |>
                      dplyr::left_join(
                        MASS::kde2d(
                          x =
                            df |>
                            dplyr::filter(event_type == "BLOCK") |>
                            dplyr::pull(shot_x),
                          y =
                            df |>
                            dplyr::filter(event_type == "BLOCK") |>
                            dplyr::pull(shot_y),
                          lims = c(c(-42, 42), c(1, 64)),
                          n = c(85, 64)
                        ) |>
                          purrr::pluck("z") |>
                          tibble::as_tibble(.name_repair = "unique") |>
                          suppressMessages() |>
                          dplyr::mutate(shot_x = seq(-42, 42)) |>
                          tidyr::pivot_longer(-c(shot_x), names_to = "shot_y", values_to = "block_z") |>
                          dplyr::mutate(shot_y = shot_y |> stringr::str_extract("\\d+") |> as.integer()),
                        by = c("shot_x", "shot_y")
                      ) |>
                      dplyr::mutate(
                        fen_z = fen_z / sum(fen_z),
                        block_z = block_z / sum(block_z)
                      )
                  }
                )
            ) |>
            dplyr::select(-c(data)) |>
            tidyr::unnest(dens) |>
            dplyr::ungroup()

          att_density <-
            att_density |>
            dplyr::mutate(
              blocked_shooter_est =
                purrr::pmap(
                  list(
                    x = shot_x,
                    y = shot_y
                    # type = shot_type,
                    # point = point_shot
                  ),
                  function(x, y) {
                    slope_center <-  y / x

                    x_adj <-
                      dplyr::case_when(
                        x == 0 ~ 0,
                        abs(slope_center) >= 1 ~ x - (0.5 * slope_center**-1),
                        T ~ x - (0.5 * sign(x))
                      )

                    y_adj <-
                      ifelse(
                        abs(slope_center) >= 1,
                        y - 0.5,
                        y - (0.5 * abs(slope_center))
                      )

                    angle_center <- abs(atan(y_adj / abs(x_adj)) * (180 / pi))
                    angle_adj <- (((90 / angle_center) - 1)) * (10 / 57)

                    slope_left_post <- y_adj / (x_adj - (-3 - angle_adj))
                    intercept_left_post <- (slope_left_post * (3 + angle_adj))

                    slope_right_post <- y_adj / (x_adj - (3 + angle_adj))
                    intercept_right_post <- (slope_right_post * (-3 - angle_adj))

                    att_density |>
                      dplyr::filter(
                        # shot_type == type,
                        # point_shot == point,
                        shot_y >= y_adj,
                        !(
                          abs(x) > 3 &
                            ((sign(x) == -1 & shot_x > x) |
                               (sign(x) == 1 & shot_x < x))
                        ),
                        (
                          slope_left_post > 0 & slope_right_post > 0 &
                            shot_y >= ((shot_x * slope_left_post) + intercept_left_post) &
                            shot_y <= ((shot_x * slope_right_post) + intercept_right_post)
                        ) |
                          (
                            slope_left_post < 0 & slope_right_post < 0 &
                              shot_y <= ((shot_x * slope_left_post) + intercept_left_post) &
                              shot_y >= ((shot_x * slope_right_post) + intercept_right_post)
                          ) |
                          (
                            slope_left_post > 0 & slope_right_post < 0 &
                              shot_y >= ((shot_x * slope_left_post) + intercept_left_post) &
                              shot_y >= ((shot_x * slope_right_post) + intercept_right_post)
                          ) |
                          (shot_x == x & shot_y == y) |
                          (abs(shot_y - (shot_x * slope_center)) <= (0.5 * sqrt(2)))
                      ) |>
                      dplyr::summarise(
                        est_x =
                          weighted.mean(
                            shot_x,
                            fen_z**ifelse(
                              # point,
                              F,
                              0.4,
                              2.5
                            )
                          ) |> round(),
                        est_y =
                          weighted.mean(
                            shot_y,
                            fen_z**ifelse(
                              # point,
                              F,
                              0.4,
                              2.5
                            )
                          ) |> round()
                      )
                  }
                ),
              blocker_dens =
                purrr::pmap_dbl(
                  list(
                    x = shot_x,
                    y = shot_y
                    # type = shot_type,
                    # point = point_shot
                  ),
                  function(x, y) {
                    slope_center <-  y / x

                    x_adj <-
                      dplyr::case_when(
                        x == 0 ~ 0,
                        abs(slope_center) >= 1 ~ x + (0.5 * slope_center**-1),
                        T ~ x + (0.5 * sign(x))
                      )

                    y_adj <-
                      ifelse(
                        abs(slope_center) >= 1,
                        y + 0.5,
                        y + (0.5 * abs(slope_center))
                      )

                    angle_center <- abs(atan(y_adj / abs(x_adj)) * (180 / pi))
                    angle_adj <- (((90 / angle_center) - 1)) * (10 / 57)

                    slope_left_post <- y_adj / (x_adj - (-3 - angle_adj))
                    intercept_left_post <- (slope_left_post * (3 + angle_adj))

                    slope_right_post <- y_adj / (x_adj - (3 + angle_adj))
                    intercept_right_post <- (slope_right_post * (-3 - angle_adj))

                    att_density |>
                      dplyr::filter(
                        # shot_type == type,
                        # point_shot == point,
                        shot_y <= y_adj,
                        !(
                          abs(x) > 3 &
                            ((sign(x) == -1 & shot_x < x) |
                               (sign(x) == 1 & shot_x > x))
                        ),
                        (
                          slope_left_post > 0 & slope_right_post > 0 &
                            shot_y <= ((shot_x * slope_left_post) + intercept_left_post) &
                            shot_y >= ((shot_x * slope_right_post) + intercept_right_post)
                        ) |
                          (
                            slope_left_post < 0 & slope_right_post < 0 &
                              shot_y >= ((shot_x * slope_left_post) + intercept_left_post) &
                              shot_y <= ((shot_x * slope_right_post) + intercept_right_post)
                          ) |
                          (
                            slope_left_post > 0 & slope_right_post < 0 &
                              shot_y <= ((shot_x * slope_left_post) + intercept_left_post) &
                              shot_y <= ((shot_x * slope_right_post) + intercept_right_post)
                          ) |
                          (shot_x == x & shot_y == y) |
                          (abs(shot_y - (shot_x * slope_center)) <= (0.5 * sqrt(2)))
                      ) |>
                      dplyr::summarise(
                        blocker_dens = sum(block_z)
                      ) |>
                      dplyr::pull(blocker_dens)
                  }
                )
            ) |>
            dplyr::select(shot_x, shot_y, blocked_shooter_est, blocker_dens) |>
            tidyr::unnest(blocked_shooter_est) |>
            dplyr::ungroup()
        }
      ),
    shot_data_5v5 =
      purrr::map2(
        shot_data_5v5,
        shot_blocker_data_5v5_basic,
        function(shots, blocks) {
          shots <-
            shots |>
            dplyr::left_join(
              blocks |>
                dplyr::select(-blocker_dens),
              by = c("shot_x", "shot_y")
            ) |>
            dplyr::mutate(
              shot_x = ifelse(event_type == "BLOCK", est_x, shot_x),
              shot_y = ifelse(event_type == "BLOCK", est_y, shot_y),
              dist_center = sqrt(shot_x**2 + shot_y**2),
              dist_near_post = sqrt((abs(shot_x) - 3)**2 + shot_y**2),
              dist_far_post = sqrt((abs(shot_x) + 3)**2 + shot_y**2),
              angle_near_post = atan((abs(shot_x) - 3) / shot_y),
              angle_far_post = atan((abs(shot_x) + 3) / shot_y),
              h_angle = abs(angle_near_post - angle_far_post),
              l_adj = cos(h_angle / 2) * dist_near_post,
              width = 2 * sqrt(dist_near_post**2 - l_adj**2),
              height_far_post = 4 * (dist_near_post / dist_far_post),
              target_area = width * ((height_far_post + 4) / 2)
            ) |>
            dplyr::select(-c(dist_near_post:height_far_post)) |>
            dplyr::left_join(
              blocks |>
                dplyr::select(-c(est_x, est_y)),
              by = c("shot_x", "shot_y")
            )
        }
      ),
    xg_5v5_basic =
      purrr::map2(
        gm_dt,
        shot_data_5v5,
        function(dt, shots) {
          print(
            "{dt} model" |>
              glue::glue()
          )


          shots <-
            shots |>
            dplyr::filter(event_type != "BLOCK") |>
            dplyr::mutate(
              total_fen = dplyr::n(),
              avg_per_shooter = total_fen / length(unique(event_player_1)),
              avg_per_goalie = total_fen / length(unique(goalie)),
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
                ),
              total_weight = game_weight * shooter_weight * goalie_weight,
              total_weight = as.integer(total_weight / min(total_weight))
            ) |>
            dplyr::ungroup() |>
            dplyr::transmute(
              is_goal = as.integer(event_type == "GOAL"),
              dist_center,
              dist_center_2 = dist_center**2,
              dist_center_3 = dist_center**3,
              target_area,
              blocker_dens,
              total_weight
            ) |>
            dplyr::filter(
              !is.na(is_goal) &
                !is.na(dist_center) &
                !is.na(target_area) &
                !is.na(blocker_dens) &
                !is.na(total_weight)
            )

          model_mat <-
            model.matrix(
              is_goal ~
                (
                  (
                    dist_center +
                      dist_center_2 +
                      dist_center_3
                    ) * target_area
                ) +
                blocker_dens,
              shots
            )[, -1]

          set.seed(1138)
          cv <-
            glmnet::cv.glmnet(
              model_mat,
              shots$is_goal,
              family = "binomial",
              alpha = 0,
              weights = shots$total_weight,
              type.measure = "mse"
            )

          model_min <-
            glmnet::glmnet(
              model_mat,
              shots$is_goal,
              family = "binomial",
              alpha = 0,
              weights = shots$total_weight,
              lambda = cv$lambda.min
            )

          model_min
        }
      )
  ) |>
  dplyr::select(-c(shot_data_5v5))

training_data |>
  dplyr::filter(
    shot_y > 0,
    shot_zone == "O",
    position_category != "G",
    event_team_strength == "EV",
    home_skater_strength_state %in% c("5v5"),
    # game_date == "2022-10-07",
    event_type != "BLOCK"
  ) |>
  dplyr::group_by(gm_dt = game_date) |>
  tidyr::nest() |>
  dplyr::inner_join(dynamic_xg_test) |>
  dplyr::mutate(
    data =
      purrr::pmap(
        list(
          d = data,
          b = shot_blocker_data_5v5_basic,
          x = xg_5v5_basic
        ),
        function(d, b, x) {
          m <-
            d |>
            dplyr::left_join(
              b,
              by = c("shot_x", "shot_y")
            ) |>
            dplyr::mutate(
              shot_x = ifelse(event_type == "BLOCK", est_x, shot_x),
              shot_y = ifelse(event_type == "BLOCK", est_y, shot_y),
              dist_center = sqrt(shot_x**2 + shot_y**2),
              dist_near_post = sqrt((abs(shot_x) - 3)**2 + shot_y**2),
              dist_far_post = sqrt((abs(shot_x) + 3)**2 + shot_y**2),
              angle_near_post = atan((abs(shot_x) - 3) / shot_y),
              angle_far_post = atan((abs(shot_x) + 3) / shot_y),
              h_angle = abs(angle_near_post - angle_far_post),
              l_adj = cos(h_angle / 2) * dist_near_post,
              width = 2 * sqrt(dist_near_post**2 - l_adj**2),
              height_far_post = 4 * (dist_near_post / dist_far_post),
              target_area = width * ((height_far_post + 4) / 2)
            ) |>
            dplyr::select(-c(dist_near_post:height_far_post)) |>
            dplyr::ungroup() |>
            dplyr::transmute(
              is_goal = as.integer(event_type == "GOAL"),
              dist_center,
              dist_center_2 = dist_center**2,
              dist_center_3 = dist_center**3,
              target_area,
              blocker_dens,
              total_weight = 1
            )

          d |>
            dplyr::mutate(
              xg_basic =
                predict(
                  x,
                  model.matrix(
                    is_goal ~
                      (
                        (
                          dist_center +
                            dist_center_2 +
                            dist_center_3
                        ) * target_area
                      ) +
                      blocker_dens,
                    # (shot_blocker_density_smoothed * dist_to_center_goalline),
                    m
                  )[, -1],
                  type = "response"
                ) |>
                as.double()
                # print()
                # purrr::list_c()
            )
        }
      )
  ) |>
  dplyr::select(data) |>
  tidyr::unnest(data) |>
  dplyr::group_by(season) |>
  dplyr::summarise(
    gp = game_id |> unique() |> length(),
    g = sum(event_type == "GOAL"),
    xg = sum(xg_basic, na.rm = T)
  )
  View("5v5 dynamic xg test")

nhl_db_con |>
  odbc::dbGetQuery(
    "select game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
  ) |>
  tibble::tibble() |>
  dplyr::arrange(gm_dt, gm_id) |>
  tibble::rowid_to_column(var = "game_num") |>
  dplyr::filter(game_num > 1312) |>
  dplyr::group_by(gm_dt) |>
  dplyr::summarise(min = min(game_num))

  # dplyr::select(-c(shot_blocker_data_5v5_basic)) |>
  # tidyr::unnest(shot_data_5v5) |>
  # head(1) |>
  # tidyr::unnest(training_data) |>
  # View()





pred_xg_shot_data_21 |>
  dplyr::filter(
    shot_y > 0,
    shot_zone == "O",
    position_category != "G",
    event_team_strength == "EV",
    home_skater_strength_state %in% c("5v5")
    # event_team_strength == "PP",
    # home_skater_strength_state %in% c("4v3", "3v4")
  ) |>
  dplyr::mutate(
    point_shot =
      position_category == "D" &
      shot_type %in% c("Slap", "Wrist/Snap")
  ) |>
  dplyr::group_by(
    event_team_strength = "EV",
    home_skater_strength_state = "5v5",
    point_shot
    # shot_type
  ) |>
  tidyr::nest() |>
  # tail(1) |>
  # head(1) |>
  dplyr::mutate(
    att_density =
      purrr::map(
        data,
        function(df) {
          MASS::kde2d(
            x =
              df |>
              dplyr::filter(event_type != "BLOCK") |>
              dplyr::pull(shot_x),
            y =
              df |>
              dplyr::filter(event_type != "BLOCK") |>
              dplyr::pull(shot_y),
            lims = c(c(-42, 42), c(1, 64)),
            n = c(85, 64)
          ) |>
            purrr::pluck("z") |>
            tibble::as_tibble(.name_repair = "unique") |>
            dplyr::mutate(shot_x = seq(-42, 42)) |>
            tidyr::pivot_longer(-c(shot_x), names_to = "shot_y", values_to = "fen_z") |>
            dplyr::mutate(shot_y = shot_y |> stringr::str_extract("\\d+") |> as.integer()) |>
            dplyr::filter(
              !(shot_y <= 17 & shot_x <= -14.5 & sqrt((shot_x - -14.5)**2 + (shot_y - 17)**2) > 28),
              !(shot_y <= 17 & shot_x >= 14.5 & sqrt((shot_x - 14.5)**2 + (shot_y - 17)**2) > 28)
            ) |>
            dplyr::left_join(
              MASS::kde2d(
                x =
                  df |>
                  dplyr::filter(event_type == "BLOCK") |>
                  dplyr::pull(shot_x),
                y =
                  df |>
                  dplyr::filter(event_type == "BLOCK") |>
                  dplyr::pull(shot_y),
                lims = c(c(-42, 42), c(1, 64)),
                n = c(85, 64)
              ) |>
                purrr::pluck("z") |>
                tibble::as_tibble(.name_repair = "unique") |>
                dplyr::mutate(shot_x = seq(-42, 42)) |>
                tidyr::pivot_longer(-c(shot_x), names_to = "shot_y", values_to = "block_z") |>
                dplyr::mutate(shot_y = shot_y |> stringr::str_extract("\\d+") |> as.integer())
            ) |>
            dplyr::mutate(
              fen_z = fen_z / sum(fen_z),
              block_z = block_z / sum(block_z)
            )
        }
      )
  ) |>
  dplyr::select(-c(data)) |>
  # print() |>
  tidyr::unnest(att_density) |>
  dplyr::ungroup()


  dplyr::select(-c(tidyselect::any_of(c("est_x", "est_y")))) |>
  # dplyr::filter(
  #   shot_x == -30,
  #   # shot_y > 10,
  #   point_shot
  # ) |>
  # head() |>
  # dplyr::filter(shot_x %in% c(3, -3)) |>
  dplyr::mutate(
    blocked_shooter_est =
      purrr::pmap(
        list(
          x = shot_x,
          y = shot_y,
          type = shot_type,
          point = point_shot
        ),
        function(x, y, type, point) {
          slope_center <-  y / x

          x_adj <-
            dplyr::case_when(
              x == 0 ~ 0,
              abs(slope_center) >= 1 ~ x - (0.5 * slope_center**-1),
              T ~ x - (0.5 * sign(x))
            )

          y_adj <-
            ifelse(
              abs(slope_center) >= 1,
              y - 0.5,
              y - (0.5 * abs(slope_center))
            )

          angle_center <- abs(atan(y_adj / abs(x_adj)) * (180 / pi))
          angle_adj <- (((90 / angle_center) - 1)) * (10 / 57)

          slope_left_post <- y_adj / (x_adj - (-3 - angle_adj))
          intercept_left_post <- (slope_left_post * (3 + angle_adj))

          slope_right_post <- y_adj / (x_adj - (3 + angle_adj))
          intercept_right_post <- (slope_right_post * (-3 - angle_adj))

          shot_att_density_est_5v5 |>
            dplyr::filter(
              shot_type == type,
              point_shot == point,
              shot_y >= y_adj,
              !(
                abs(x) > 3 &
                  ((sign(x) == -1 & shot_x > x) |
                     (sign(x) == 1 & shot_x < x))
              ),
              (
                slope_left_post > 0 & slope_right_post > 0 &
                  shot_y >= ((shot_x * slope_left_post) + intercept_left_post) &
                  shot_y <= ((shot_x * slope_right_post) + intercept_right_post)
              ) |
                (
                  slope_left_post < 0 & slope_right_post < 0 &
                    shot_y <= ((shot_x * slope_left_post) + intercept_left_post) &
                    shot_y >= ((shot_x * slope_right_post) + intercept_right_post)
                ) |
                (
                  slope_left_post > 0 & slope_right_post < 0 &
                    shot_y >= ((shot_x * slope_left_post) + intercept_left_post) &
                    shot_y >= ((shot_x * slope_right_post) + intercept_right_post)
                ) |
                (shot_x == x & shot_y == y) |
                (abs(shot_y - (shot_x * slope_center)) <= (0.5 * sqrt(2)))
            ) |>
            # dplyr::mutate(
            #   slope_left = slope_left_post,
            #   intercept_left = intercept_left_post,
            #   slope_right = slope_right_post,
            #   intercept_right = intercept_right_post
            # )
            dplyr::summarise(
              est_x = weighted.mean(shot_x, fen_z**ifelse(point, 0.4, 2.5)) |> round(),
              est_y = weighted.mean(shot_y, fen_z**ifelse(point, 0.4, 2.5)) |> round()
            )
        }
      )
  ) |>
  # dplyr::select(shot_x_1 = shot_x, shot_y_1 = shot_y, blocked_shooter_est) |>
  tidyr::unnest(blocked_shooter_est) |>
  dplyr::ungroup()
