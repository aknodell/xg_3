shot_blocker_density_ests <-
  shot_blocker_density_ests |>
  dplyr::bind_rows(
    nhl_db_con |>
      odbc::dbGetQuery(
        "select game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
      ) |>
      tibble::tibble() |>
      dplyr::arrange(gm_dt, gm_id) |>
      tibble::rowid_to_column(var = "game_num") |>
      dplyr::filter(gm_dt > "2026-03-31") |>
      dplyr::group_by(gm_dt) |>
      dplyr::summarise(min = min(game_num))  |>
      # head(1) |>
      dplyr::mutate(
        shot_data_5v5 =
          purrr::map2(
            gm_dt,
            min,
            function(dt, m) {
              start_time <- Sys.time()

              print(
                "{dt} start" |>
                  glue::glue()
              )

              df_5v5 <-
                training_data |>
                dplyr::filter(
                  shot_y > 0,
                  shot_zone == "O",
                  position_category != "G",
                  event_team_strength == "EV",
                  home_skater_strength_state %in% c("5v5"),
                  game_num < m, game_num >= m - 1312
                )

              att_density_basic <-
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

              att_density_basic <-
                att_density_basic |>
                dplyr::mutate(
                  blocked_shooter_est =
                    purrr::pmap(
                      list(
                        x = shot_x,
                        y = shot_y
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

                        att_density_basic |>
                          dplyr::filter(
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

                        att_density_basic |>
                          dplyr::filter(
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

              att_density_shot_type <-
                df_5v5 |>
                dplyr::group_by(
                  shot_type
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
                dplyr::select(-c(data))

              att_density_shot_type <-
                att_density_shot_type |>
                tidyr::unnest(dens) |>
                dplyr::select(-c(fen_z, block_z)) |>
                dplyr::left_join(
                  att_density_shot_type,
                  by = c("shot_type")
                ) |>
                dplyr::mutate(
                  blocked_shooter_est =
                    purrr::pmap(
                      list(
                        x = shot_x,
                        y = shot_y,
                        d = dens
                      ),
                      function(x, y, d) {
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

                        d |>
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
                        y = shot_y,
                        d = dens
                      ),
                      function(x, y, d) {
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

                        d |>
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
                dplyr::select(shot_type, shot_x, shot_y, blocked_shooter_est, blocker_dens) |>
                tidyr::unnest(blocked_shooter_est) |>
                dplyr::ungroup()

              att_density_shot_type_point <-
                df_5v5 |>
                dplyr::group_by(
                  shot_type,
                  point_shot
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
                dplyr::select(-c(data))

              att_density_shot_type_point <-
                att_density_shot_type_point |>
                tidyr::unnest(dens) |>
                dplyr::select(-c(fen_z, block_z)) |>
                dplyr::left_join(
                  att_density_shot_type_point,
                  by = c("shot_type", "point_shot")
                ) |>
                dplyr::mutate(
                  blocked_shooter_est =
                    purrr::pmap(
                      list(
                        x = shot_x,
                        y = shot_y,
                        d = dens,
                        point = point_shot
                      ),
                      function(x, y, d, point) {
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

                        d |>
                          dplyr::filter(
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
                                  point,
                                  # F,
                                  0.4,
                                  2.5
                                )
                              ) |> round(),
                            est_y =
                              weighted.mean(
                                shot_y,
                                fen_z**ifelse(
                                  point,
                                  # F,
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
                        y = shot_y,
                        d = dens
                      ),
                      function(x, y, d) {
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

                        d |>
                          dplyr::filter(
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
                dplyr::select(shot_type, point_shot, shot_x, shot_y, blocked_shooter_est, blocker_dens) |>
                tidyr::unnest(blocked_shooter_est) |>
                dplyr::ungroup()

              print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

              tibble::tibble(
                shot_blocker_data_5v5_basic = list(att_density_basic),
                shot_blocker_data_5v5_shot_type = list(att_density_shot_type),
                shot_blocker_data_5v5_shot_type_point = list(att_density_shot_type_point)
              )
            }
          )
      ) |>
      tidyr::unnest(shot_data_5v5)
  )

