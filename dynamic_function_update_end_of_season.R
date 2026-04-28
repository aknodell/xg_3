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

dynamic_xg <-
  dynamic_xg |>
  dplyr::bind_rows(
    nhl_db_con |>
      odbc::dbGetQuery(
        "select game_id gm_id, game_date gm_dt from games where season >= 20212022 and session = 2"
      ) |>
      tibble::tibble() |>
      dplyr::arrange(gm_dt, gm_id) |>
      tibble::rowid_to_column(var = "game_num") |>
      dplyr::filter(gm_dt > "2026-04-01") |>
      dplyr::group_by(gm_dt) |>
      dplyr::summarise(min = min(game_num)) |>
      dplyr::left_join(shot_blocker_density_ests) |>
      # head(1) |>
      dplyr::mutate(
        shot_data_5v5 =
          purrr::pmap(
            list(
              dt = gm_dt,
              m = min,
              bb = shot_blocker_data_5v5_basic,
              bst = shot_blocker_data_5v5_shot_type,
              bstp = shot_blocker_data_5v5_shot_type_point
            ),
            function(dt, m, bb, bst, bstp) {
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
                  game_num < m, game_num >= m - 1312,
                  event_type != "BLOCK"
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
                ) |>
                dplyr::left_join(
                  bb |>
                    dplyr::transmute(
                      shot_x, shot_y, block_dens_basic = blocker_dens
                    ),
                  by = c("shot_x", "shot_y")
                ) |>
                dplyr::left_join(
                  bst |>
                    dplyr::transmute(
                      shot_x, shot_y, shot_type, block_dens_shot_type = blocker_dens
                    ),
                  by = c("shot_x", "shot_y", "shot_type")
                ) |>
                dplyr::left_join(
                  bstp |>
                    dplyr::transmute(
                      shot_x, shot_y, shot_type, point_shot, block_dens_shot_type_point = blocker_dens
                    ),
                  by = c("shot_x", "shot_y", "shot_type", "point_shot")
                )

              df_5v5 <-
                df_5v5 |>
                dplyr::mutate(
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
                dplyr::select(-c(dist_near_post:height_far_post))

              shots <-
                df_5v5 |>
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
                  player_weight = shooter_weight * goalie_weight,
                  total_weight = game_weight * player_weight,
                  total_weight = as.integer(total_weight / min(total_weight)),
                  game_weight_int = as.integer(game_weight / min(game_weight)),
                  player_weight_int = as.integer(player_weight / min(player_weight)),
                  total_weight_int = as.integer(total_weight / min(total_weight))
                ) |>
                dplyr::ungroup() |>
                dplyr::transmute(
                  is_goal = as.integer(event_type == "GOAL"),
                  dist_center,
                  dist_center_2 = dist_center**2,
                  dist_center_3 = dist_center**3,
                  target_area,
                  is_slap = as.integer(shot_type == "Slap"),
                  is_tip = as.integer(shot_type == "Tip In/Deflection"),
                  is_other = as.integer(shot_type == "Backhand/Other"),
                  is_rush,
                  rush_secs,
                  rush_velo = ifelse(rush_velo > 70, 70, rush_velo),
                  block_dens_basic,
                  block_dens_shot_type,
                  block_dens_shot_type_point,
                  is_off_faceoff,
                  is_off_faceoff_win,
                  faceoff_secs,
                  is_followup_shot,
                  is_reached_goalie_followup,
                  is_own_followup,
                  followup_secs,
                  angle_change_velo,
                  player_weight,
                  game_weight,
                  total_weight,
                  player_weight_int,
                  game_weight_int,
                  total_weight_int
                ) |>
                dplyr::filter(
                  !is.na(is_goal) &
                    !is.na(dist_center) &
                    !is.na(target_area) &
                    !is.na(total_weight_int) &
                    !is.na(block_dens_shot_type_point)
                )

              model_mat_basic <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    block_dens_basic,
                  shots
                )[, -1]

              set.seed(1138)
              cv_basic <-
                glmnet::cv.glmnet(
                  model_mat_basic,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$total_weight,
                  type.measure = "mse"
                )

              model_min_basic <-
                glmnet::glmnet(
                  model_mat_basic,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$total_weight,
                  lambda = cv_basic$lambda.min
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
                    ),
                  shots
                )[, -1]

              set.seed(1138)
              cv_un_weight <-
                glmnet::cv.glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  type.measure = "mse"
                )

              model_min_un_weight <-
                glmnet::glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  lambda = cv_un_weight$lambda.min
                )

              set.seed(1138)
              cv_game_weight <-
                glmnet::cv.glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_game_weight <-
                glmnet::glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_game_weight$lambda.min
                )

              set.seed(1138)
              cv_player_weight <-
                glmnet::cv.glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$player_weight_int,
                  type.measure = "mse"
                )

              model_min_player_weight <-
                glmnet::glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$player_weight_int,
                  lambda = cv_player_weight$lambda.min
                )

              set.seed(1138)
              cv_total_weight <-
                glmnet::cv.glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$total_weight_int,
                  type.measure = "mse"
                )

              model_min_total_weight <-
                glmnet::glmnet(
                  model_mat,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$total_weight_int,
                  lambda = cv_total_weight$lambda.min
                )

              model_mat_shot_type_game_weight <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other,
                  shots
                )[, -1]

              set.seed(1138)
              cv_shot_type_game_weight <-
                glmnet::cv.glmnet(
                  model_mat_shot_type_game_weight,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_shot_type_game_weight <-
                glmnet::glmnet(
                  model_mat_shot_type_game_weight,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_shot_type_game_weight$lambda.min
                )

              model_mat_rush_basic <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    is_rush,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_basic <-
                glmnet::cv.glmnet(
                  model_mat_rush_basic,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_basic <-
                glmnet::glmnet(
                  model_mat_rush_basic,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_basic$lambda.min
                )

              model_mat_rush_secs <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_secs),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_secs <-
                glmnet::cv.glmnet(
                  model_mat_rush_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_secs <-
                glmnet::glmnet(
                  model_mat_rush_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_secs$lambda.min
                )

              model_mat_rush_velo <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo <-
                glmnet::glmnet(
                  model_mat_rush_velo,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo$lambda.min
                )

              model_mat_rush_secs_velo <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo * rush_secs),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_secs_velo <-
                glmnet::cv.glmnet(
                  model_mat_rush_secs_velo,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_secs_velo <-
                glmnet::glmnet(
                  model_mat_rush_secs_velo,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_secs_velo$lambda.min
                )

              model_mat_rush_velo_blocker_basic <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_basic,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic$lambda.min
                )

              model_mat_rush_velo_blocker_shot_type <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_shot_type <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_shot_type,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_shot_type <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_shot_type,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_shot_type$lambda.min
                )

              model_mat_rush_velo_blocker_shot_type_point <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_shot_type_point <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_shot_type_point,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_shot_type_point <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_shot_type_point,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_shot_type_point$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    (is_off_faceoff),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    (is_off_faceoff + is_off_faceoff_win),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_secs <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    (is_off_faceoff * faceoff_secs),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_secs <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_secs <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_secs$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_follow <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    is_followup_shot,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_follow <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_follow <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_follow$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_own_follow <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    (is_followup_shot + is_own_followup),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_own_follow <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_own_follow <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_own_follow$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_reach_follow <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    is_reached_goalie_followup,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_reach_follow <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_reach_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_reach_follow <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_reach_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_reach_follow$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    (is_reached_goalie_followup + is_own_followup),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    ((is_followup_shot + is_reached_goalie_followup) * is_own_followup),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    (is_followup_shot + is_reached_goalie_followup + is_own_followup),
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    (is_followup_shot + is_reached_goalie_followup + is_own_followup) * followup_secs,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs$lambda.min
                )

              model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo <-
                model.matrix(
                  is_goal ~
                    (
                      (
                        dist_center +
                          dist_center_2 +
                          dist_center_3
                      ) * target_area
                    ) +
                    is_slap +
                    is_tip +
                    is_other +
                    (is_rush * rush_velo) +
                    block_dens_shot_type_point +
                    ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                    (is_followup_shot + is_reached_goalie_followup + is_own_followup) * angle_change_velo,
                  shots
                )[, -1]

              set.seed(1138)
              cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo <-
                glmnet::cv.glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  type.measure = "mse"
                )

              model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo <-
                glmnet::glmnet(
                  model_mat_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo,
                  shots$is_goal,
                  family = "binomial",
                  alpha = 0,
                  weights = shots$game_weight_int,
                  lambda = cv_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo$lambda.min
                )

              print("time: {(Sys.time() - start_time) |> hms::as_hms() |> round() |> hms::as_hms()}" |> glue::glue())

              tibble::tibble(
                xg_5v5_basic = list(model_min_basic),
                xg_5v5_basic_un_weight = list(model_min_un_weight),
                xg_5v5_basic_game_weight = list(model_min_game_weight),
                xg_5v5_basic_player_weight = list(model_min_player_weight),
                xg_5v5_basic_total_weight = list(model_min_total_weight),
                xg_5v5_shot_type_game_weight = list(model_min_shot_type_game_weight),
                xg_5v5_rush_basic = list(model_min_rush_basic),
                xg_5v5_rush_secs = list(model_min_rush_secs),
                xg_5v5_rush_velo = list(model_min_rush_velo),
                xg_5v5_rush_secs_velo = list(model_min_rush_secs_velo),
                xg_5v5_rush_velo_blocker_basic = list(model_min_rush_velo_blocker_basic),
                xg_5v5_rush_velo_blocker_shot_type = list(model_min_rush_velo_blocker_shot_type),
                xg_5v5_rush_velo_blocker_shot_type_point = list(model_min_rush_velo_blocker_shot_type_point),
                xg_5v5_rush_velo_blocker_basic_fac = list(model_min_rush_velo_blocker_basic_fac),
                xg_5v5_rush_velo_blocker_basic_fac_win = list(model_min_rush_velo_blocker_basic_fac_win),
                xg_5v5_rush_velo_blocker_basic_fac_secs = list(model_min_rush_velo_blocker_basic_fac_secs),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs = list(model_min_rush_velo_blocker_basic_fac_win_secs),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_follow = list(model_min_rush_velo_blocker_basic_fac_win_secs_follow),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_follow = list(model_min_rush_velo_blocker_basic_fac_win_secs_own_follow),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_reach_follow = list(model_min_rush_velo_blocker_basic_fac_win_secs_reach_follow),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow = list(model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int = list(model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all = list(model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs = list(model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs),
                xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo = list(model_min_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo)
              )
            }
          )
      ) |>
      dplyr::select(
        -c(
          shot_blocker_data_5v5_basic,
          shot_blocker_data_5v5_shot_type,
          shot_blocker_data_5v5_shot_type_point
        )
      ) |>
      tidyr::unnest(shot_data_5v5)
  )

training_data <-
  training_data |>
  dplyr::left_join(
    training_data |>
      dplyr::filter(
        shot_y > 0,
        shot_zone == "O",
        position_category != "G",
        event_team_strength == "EV",
        home_skater_strength_state %in% c("5v5"),
        event_type != "BLOCK"
      ) |>
      dplyr::group_by(gm_dt = game_date) |>
      tidyr::nest() |>
      dplyr::inner_join(dynamic_xg) |>
      dplyr::left_join(shot_blocker_density_ests) |>
      dplyr::mutate(
        data =
          purrr::pmap(
            list(
              d = data,
              b = shot_blocker_data_5v5_basic,
              x = xg_5v5_basic,
              xu = xg_5v5_basic_un_weight,
              xg = xg_5v5_basic_game_weight,
              xp = xg_5v5_basic_player_weight,
              xt = xg_5v5_basic_total_weight
            ),
            function(d, b, x, xu, xg, xp, xt) {
              d <-
                d |>
                dplyr::inner_join(
                  b,
                  by = c("shot_x", "shot_y")
                )

              m <-
                d |>
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
                  blocker_dens
                )

              d |>
                dplyr::transmute(
                  # season,
                  game_id,
                  event_id,
                  # xg_basic =
                  #   predict(
                  #     x,
                  #     model.matrix(
                  #       is_goal ~
                  #         (
                  #           (
                  #             dist_center +
                  #               dist_center_2 +
                  #               dist_center_3
                  #           ) * target_area
                  #         ) +
                  #         blocker_dens,
                  #       m
                  #     )[, -1],
                  #     type = "response"
                  #   ) |>
                  #   as.double(),
                  xg_unweighted =
                    predict(
                      xu,
                      model.matrix(
                        is_goal ~
                          (
                            (
                              dist_center +
                                dist_center_2 +
                                dist_center_3
                            ) * target_area
                          ),
                        m
                      )[, -1],
                      type = "response"
                    ) |>
                    as.double(),
                  xg_game_weight =
                    predict(
                      xg,
                      model.matrix(
                        is_goal ~
                          (
                            (
                              dist_center +
                                dist_center_2 +
                                dist_center_3
                            ) * target_area
                          ),
                        m
                      )[, -1],
                      type = "response"
                    ) |>
                    as.double(),
                  xg_player_weight =
                    predict(
                      xp,
                      model.matrix(
                        is_goal ~
                          (
                            (
                              dist_center +
                                dist_center_2 +
                                dist_center_3
                            ) * target_area
                          ),
                        m
                      )[, -1],
                      type = "response"
                    ) |>
                    as.double(),
                  xg_total_weight =
                    predict(
                      xt,
                      model.matrix(
                        is_goal ~
                          (
                            (
                              dist_center +
                                dist_center_2 +
                                dist_center_3
                            ) * target_area
                          ),
                        m
                      )[, -1],
                      type = "response"
                    ) |>
                    as.double()
                )
            }
          )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(data) |>
      tidyr::unnest(data) |>
      dplyr::full_join(
        training_data |>
          dplyr::filter(
            shot_y > 0,
            shot_zone == "O",
            position_category != "G",
            event_team_strength == "EV",
            home_skater_strength_state %in% c("5v5"),
            event_type != "BLOCK"
          ) |>
          dplyr::group_by(gm_dt = game_date) |>
          tidyr::nest() |>
          dplyr::inner_join(dynamic_xg) |>
          dplyr::mutate(
            data =
              purrr::pmap(
                list(
                  d = data,
                  x = xg_5v5_shot_type_game_weight
                ),
                function(d, x) {
                  m <-
                    d |>
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
                      is_slap = as.integer(shot_type == "Slap"),
                      is_tip = as.integer(shot_type == "Tip In/Deflection"),
                      is_other = as.integer(shot_type == "Backhand/Other")
                    )

                  d |>
                    dplyr::transmute(
                      game_id,
                      event_id,
                      xg_shot_type =
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
                              is_slap +
                              is_tip +
                              is_other,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double()
                    )
                }
              )
          ) |>
          dplyr::select(data) |>
          tidyr::unnest(data) |>
          dplyr::ungroup(),
        by = c("game_id", "event_id")
      ) |>
      dplyr::full_join(
        training_data |>
          dplyr::filter(
            shot_y > 0,
            shot_zone == "O",
            position_category != "G",
            event_team_strength == "EV",
            home_skater_strength_state %in% c("5v5"),
            event_type != "BLOCK"
          ) |>
          dplyr::group_by(gm_dt = game_date) |>
          tidyr::nest() |>
          dplyr::inner_join(dynamic_xg) |>
          dplyr::mutate(
            data =
              purrr::pmap(
                list(
                  d = data,
                  x_br = xg_5v5_rush_basic,
                  x_rs = xg_5v5_rush_secs,
                  x_rv = xg_5v5_rush_velo,
                  x_rvs = xg_5v5_rush_secs_velo
                ),
                function(d, b, x_br, x_rs, x_rv, x_rvs) {
                  m <-
                    d |>
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
                      is_slap = as.integer(shot_type == "Slap"),
                      is_tip = as.integer(shot_type == "Tip In/Deflection"),
                      is_other = as.integer(shot_type == "Backhand/Other"),
                      is_rush,
                      rush_secs,
                      rush_velo = ifelse(rush_velo > 70, 70, rush_velo)
                    )

                  d |>
                    dplyr::transmute(
                      game_id,
                      event_id,
                      xg_rush =
                        predict(
                          x_br,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              is_rush,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_rush_secs =
                        predict(
                          x_rs,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_secs),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_rush_velo =
                        predict(
                          x_rv,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_rush_velo_secs =
                        predict(
                          x_rvs,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_secs * rush_velo),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double()
                    )
                }
              )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(data) |>
          tidyr::unnest(data),
        by = c("game_id", "event_id")
      ) |>
      dplyr::full_join(
        training_data |>
          dplyr::filter(
            shot_y > 0,
            shot_zone == "O",
            position_category != "G",
            event_team_strength == "EV",
            home_skater_strength_state %in% c("5v5"),
            event_type != "BLOCK"
          ) |>
          dplyr::group_by(gm_dt = game_date) |>
          tidyr::nest() |>
          dplyr::inner_join(dynamic_xg) |>
          dplyr::inner_join(shot_blocker_density_ests) |>
          dplyr::mutate(
            data =
              purrr::pmap(
                list(
                  d = data,
                  b = shot_blocker_data_5v5_basic,
                  st = shot_blocker_data_5v5_shot_type,
                  stp = shot_blocker_data_5v5_shot_type_point,
                  xb = xg_5v5_rush_velo_blocker_basic,
                  xst = xg_5v5_rush_velo_blocker_shot_type,
                  xstp = xg_5v5_rush_velo_blocker_shot_type_point
                ),
                function(d, b, st, stp, xb, xst, xstp) {
                  d <-
                    d |>
                    dplyr::inner_join(
                      b |>
                        dplyr::transmute(
                          shot_x, shot_y, block_dens_basic = blocker_dens
                        ),
                      by = c("shot_x", "shot_y")
                    ) |>
                    dplyr::inner_join(
                      st |>
                        dplyr::transmute(
                          shot_x, shot_y, shot_type, block_dens_shot_type = blocker_dens
                        ),
                      by = c("shot_x", "shot_y", "shot_type")
                    ) |>
                    dplyr::inner_join(
                      stp |>
                        dplyr::transmute(
                          shot_x, shot_y, shot_type, point_shot, block_dens_shot_type_point = blocker_dens
                        ),
                      by = c("shot_x", "shot_y", "shot_type", "point_shot")
                    )

                  m <-
                    d |>
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
                      is_slap = as.integer(shot_type == "Slap"),
                      is_tip = as.integer(shot_type == "Tip In/Deflection"),
                      is_other = as.integer(shot_type == "Backhand/Other"),
                      is_rush,
                      # rush_secs,
                      rush_velo = ifelse(rush_velo > 70, 70, rush_velo),
                      block_dens_basic,
                      block_dens_shot_type,
                      block_dens_shot_type_point
                    ) |>
                    dplyr::filter(
                      !is.na(is_goal) &
                        !is.na(dist_center) &
                        !is.na(target_area) &
                        !is.na(block_dens_basic)
                    )

                  d |>
                    dplyr::transmute(
                      game_id,
                      event_id,
                      xg_blockers_basic =
                        predict(
                          xb,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_basic,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_blockers_shot_type =
                        predict(
                          xst,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_blockers_shot_type_point =
                        predict(
                          xstp,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double()
                    )
                }
              )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(data) |>
          tidyr::unnest(data),
        by = c("game_id", "event_id")
      ) |>
      dplyr::full_join(
        training_data |>
          dplyr::filter(
            shot_y > 0,
            shot_zone == "O",
            position_category != "G",
            event_team_strength == "EV",
            home_skater_strength_state %in% c("5v5"),
            event_type != "BLOCK"
          ) |>
          dplyr::group_by(gm_dt = game_date) |>
          tidyr::nest() |>
          dplyr::inner_join(dynamic_xg) |>
          dplyr::inner_join(shot_blocker_density_ests) |>
          dplyr::mutate(
            data =
              purrr::pmap(
                list(
                  d = data,
                  stp = shot_blocker_data_5v5_shot_type_point,
                  xf = xg_5v5_rush_velo_blocker_basic_fac,
                  xfw = xg_5v5_rush_velo_blocker_basic_fac_win,
                  xfs = xg_5v5_rush_velo_blocker_basic_fac_secs,
                  xfws = xg_5v5_rush_velo_blocker_basic_fac_win_secs
                ),
                function(d,stp, xf, xfw, xfs, xfws) {
                  d <-
                    d |>
                    dplyr::inner_join(
                      stp |>
                        dplyr::transmute(
                          shot_x, shot_y, shot_type, point_shot, block_dens_shot_type_point = blocker_dens
                        ),
                      by = c("shot_x", "shot_y", "shot_type", "point_shot")
                    )

                  m <-
                    d |>
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
                      is_slap = as.integer(shot_type == "Slap"),
                      is_tip = as.integer(shot_type == "Tip In/Deflection"),
                      is_other = as.integer(shot_type == "Backhand/Other"),
                      is_rush,
                      # rush_secs,
                      rush_velo = ifelse(rush_velo > 70, 70, rush_velo),
                      block_dens_shot_type_point,
                      is_off_faceoff,
                      is_off_faceoff_win,
                      faceoff_secs
                    ) |>
                    dplyr::filter(
                      !is.na(is_goal) &
                        !is.na(dist_center) &
                        !is.na(target_area) &
                        !is.na(block_dens_shot_type_point)
                    )

                  d |>
                    dplyr::transmute(
                      game_id,
                      event_id,
                      xg_fac =
                        predict(
                          xf,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              is_off_faceoff,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_fac_win =
                        predict(
                          xfw,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              (is_off_faceoff + is_off_faceoff_win),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_fac_secs =
                        predict(
                          xfs,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              (is_off_faceoff * faceoff_secs),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_fac_win_secs =
                        predict(
                          xfws,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double()
                    )
                }
              )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(data) |>
          tidyr::unnest(data),
        by = c("game_id", "event_id")
      ) |>
      dplyr::full_join(
        training_data |>
          dplyr::filter(
            shot_y > 0,
            shot_zone == "O",
            position_category != "G",
            event_team_strength == "EV",
            home_skater_strength_state %in% c("5v5"),
            event_type != "BLOCK"
          ) |>
          dplyr::group_by(gm_dt = game_date) |>
          tidyr::nest() |>
          dplyr::inner_join(dynamic_xg) |>
          dplyr::inner_join(shot_blocker_density_ests) |>
          dplyr::mutate(
            data =
              purrr::pmap(
                list(
                  d = data,
                  stp = shot_blocker_data_5v5_shot_type_point,
                  xf = xg_5v5_rush_velo_blocker_basic_fac_win_secs_follow,
                  xof = xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_follow,
                  xrf = xg_5v5_rush_velo_blocker_basic_fac_win_secs_reach_follow,
                  xorf = xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow,
                  xorfi = xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_int
                ),
                function(d,stp, xf, xof, xrf, xorf, xorfi) {
                  d <-
                    d |>
                    dplyr::inner_join(
                      stp |>
                        dplyr::transmute(
                          shot_x, shot_y, shot_type, point_shot, block_dens_shot_type_point = blocker_dens
                        ),
                      by = c("shot_x", "shot_y", "shot_type", "point_shot")
                    )

                  m <-
                    d |>
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
                      is_slap = as.integer(shot_type == "Slap"),
                      is_tip = as.integer(shot_type == "Tip In/Deflection"),
                      is_other = as.integer(shot_type == "Backhand/Other"),
                      is_rush,
                      # rush_secs,
                      rush_velo = ifelse(rush_velo > 70, 70, rush_velo),
                      block_dens_shot_type_point,
                      is_off_faceoff,
                      is_off_faceoff_win,
                      faceoff_secs,
                      is_followup_shot,
                      is_reached_goalie_followup,
                      is_own_followup
                    ) |>
                    dplyr::filter(
                      !is.na(is_goal) &
                        !is.na(dist_center) &
                        !is.na(target_area) &
                        !is.na(block_dens_shot_type_point)
                    )

                  d |>
                    dplyr::transmute(
                      game_id,
                      event_id,
                      xg_follow =
                        predict(
                          xf,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              is_followup_shot,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_own_follow =
                        predict(
                          xof,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              (is_followup_shot + is_own_followup),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_reach_follow =
                        predict(
                          xrf,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              is_reached_goalie_followup,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_own_reach_follow =
                        predict(
                          xorf,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              (is_reached_goalie_followup + is_own_followup),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_own_reach_follow_int =
                        predict(
                          xorfi,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              ((is_followup_shot + is_reached_goalie_followup) * is_own_followup),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double()
                    )
                }
              )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(data) |>
          tidyr::unnest(data),
        by = c("game_id", "event_id")
      ) |>
      dplyr::full_join(
        training_data |>
          dplyr::filter(
            shot_y > 0,
            shot_zone == "O",
            position_category != "G",
            event_team_strength == "EV",
            home_skater_strength_state %in% c("5v5"),
            event_type != "BLOCK"
          ) |>
          dplyr::group_by(gm_dt = game_date) |>
          tidyr::nest() |>
          dplyr::inner_join(dynamic_xg) |>
          dplyr::inner_join(shot_blocker_density_ests) |>
          dplyr::mutate(
            data =
              purrr::pmap(
                list(
                  d = data,
                  stp = shot_blocker_data_5v5_shot_type_point,
                  xorfa = xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all
                ),
                function(d, stp, xorfa) {
                  d <-
                    d |>
                    dplyr::inner_join(
                      stp |>
                        dplyr::transmute(
                          shot_x, shot_y, shot_type, point_shot, block_dens_shot_type_point = blocker_dens
                        ),
                      by = c("shot_x", "shot_y", "shot_type", "point_shot")
                    )

                  m <-
                    d |>
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
                      is_slap = as.integer(shot_type == "Slap"),
                      is_tip = as.integer(shot_type == "Tip In/Deflection"),
                      is_other = as.integer(shot_type == "Backhand/Other"),
                      is_rush,
                      # rush_secs,
                      rush_velo = ifelse(rush_velo > 70, 70, rush_velo),
                      block_dens_shot_type_point,
                      is_off_faceoff,
                      is_off_faceoff_win,
                      faceoff_secs,
                      is_followup_shot,
                      is_reached_goalie_followup,
                      is_own_followup
                    ) |>
                    dplyr::filter(
                      !is.na(is_goal) &
                        !is.na(dist_center) &
                        !is.na(target_area) &
                        !is.na(block_dens_shot_type_point)
                    )

                  d |>
                    dplyr::transmute(
                      game_id,
                      event_id,
                      xg_own_reach_follow_all =
                        predict(
                          xorfa,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              (is_followup_shot + is_reached_goalie_followup + is_own_followup),
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double()
                    )
                }
              )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(data) |>
          tidyr::unnest(data),
        by = c("game_id", "event_id")
      ) |>
      dplyr::full_join(
        training_data |>
          dplyr::filter(
            shot_y > 0,
            shot_zone == "O",
            position_category != "G",
            event_team_strength == "EV",
            home_skater_strength_state %in% c("5v5"),
            event_type != "BLOCK"
          ) |>
          dplyr::group_by(gm_dt = game_date) |>
          tidyr::nest() |>
          dplyr::inner_join(dynamic_xg) |>
          dplyr::inner_join(shot_blocker_density_ests) |>
          dplyr::mutate(
            data =
              purrr::pmap(
                list(
                  d = data,
                  stp = shot_blocker_data_5v5_shot_type_point,
                  xs = xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_secs,
                  xv = xg_5v5_rush_velo_blocker_basic_fac_win_secs_own_reach_follow_all_velo
                ),
                function(d,stp, xs, xv) {
                  d <-
                    d |>
                    dplyr::inner_join(
                      stp |>
                        dplyr::transmute(
                          shot_x, shot_y, shot_type, point_shot, block_dens_shot_type_point = blocker_dens
                        ),
                      by = c("shot_x", "shot_y", "shot_type", "point_shot")
                    )

                  m <-
                    d |>
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
                      is_slap = as.integer(shot_type == "Slap"),
                      is_tip = as.integer(shot_type == "Tip In/Deflection"),
                      is_other = as.integer(shot_type == "Backhand/Other"),
                      is_rush,
                      # rush_secs,
                      rush_velo = ifelse(rush_velo > 70, 70, rush_velo),
                      block_dens_shot_type_point,
                      is_off_faceoff,
                      is_off_faceoff_win,
                      faceoff_secs,
                      is_followup_shot,
                      is_reached_goalie_followup,
                      is_own_followup,
                      followup_secs,
                      angle_change_velo
                    ) |>
                    dplyr::filter(
                      !is.na(is_goal) &
                        !is.na(dist_center) &
                        !is.na(target_area) &
                        !is.na(block_dens_shot_type_point)
                    )

                  d |>
                    dplyr::transmute(
                      game_id,
                      event_id,
                      xg_own_reach_follow_all_secs =
                        predict(
                          xs,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              (is_followup_shot + is_reached_goalie_followup + is_own_followup) * followup_secs,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double(),
                      xg_own_reach_follow_all_velo =
                        predict(
                          xv,
                          model.matrix(
                            is_goal ~
                              (
                                (
                                  dist_center +
                                    dist_center_2 +
                                    dist_center_3
                                ) * target_area
                              ) +
                              is_slap +
                              is_tip +
                              is_other +
                              (is_rush * rush_velo) +
                              block_dens_shot_type_point +
                              ((is_off_faceoff + is_off_faceoff_win) * faceoff_secs) +
                              (is_followup_shot + is_reached_goalie_followup + is_own_followup) * angle_change_velo,
                            m
                          )[, -1],
                          type = "response"
                        ) |>
                        as.double()
                    )
                }
              )
          ) |>
          dplyr::ungroup() |>
          dplyr::select(data) |>
          tidyr::unnest(data),
        by = c("game_id", "event_id")
      ) |>
      # colnames()
      dplyr::select(
        game_id, event_id,
        xg_game_weight,
        xg_shot_type,
        xg_rush_velo,
        xg_blockers_basic,
        xg_blockers_shot_type_point,
        xg_fac_win_secs,
        xg_own_reach_follow_all_secs
      )
  )
