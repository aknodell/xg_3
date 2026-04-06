shot_att_density_est_5v5 <-
  pred_xg_shot_data_22 |>
  # dplyr::mutate(season = 2024) |>
  # dplyr::bind_rows(
  #   pred_xg_shot_data_23 |>
  #     dplyr::mutate(season = 2023)
  # ) |>
  # dplyr::bind_rows(
  #   pred_xg_shot_data_25 |>
  #     dplyr::mutate(season = 2025)
  # ) |>
  # dplyr::bind_rows(
  #   pred_xg_shot_data_22 |>
  #     dplyr::mutate(season = 2022)
  # ) |>
  # dplyr::bind_rows(
  #   pred_xg_shot_data_21 |>
  #     dplyr::mutate(season = 2021)
  # ) |>
  # dplyr::bind_rows(
  #   pred_xg_shot_data_20 |>
  #     dplyr::mutate(season = 2020)
  # ) |>
  # dplyr::filter(event_type != "BLOCK", !is.na(goalie)) |>
  # dplyr::group_by(season, shot_y) |>
  # dplyr::tally() |>
  # ggplot2::ggplot(ggplot2::aes(x = shot_y, y = n)) +
  # ggplot2::facet_wrap(ggplot2::vars(season), scales = "free", nrow = 2) +
  # ggplot2::geom_col(width = 1) +
  # ggplot2::geom_vline(xintercept = 0, color = "red") +
  # ggplot2::geom_vline(xintercept = 64.5, color = "blue")
  # # dplyr::mutate(
  # #   diff = n - dplyr::lag(n)
  # # ) |>
  # View()
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
    point_shot,
    shot_type
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

# shot_att_density_est_5v5 |>
#   dplyr::group_by(shot_type, point_shot) |>
#   dplyr::mutate(
#     fen_z = (fen_z - min(fen_z)) / (max(fen_z) - min(fen_z)),
#     block_z = (block_z - min(block_z)) / (max(block_z) - min(block_z))
#   ) |>
#   ggplot2::ggplot() +
#   off_zone_markings() +
#   ggplot2::facet_wrap(ggplot2::vars(shot_type, point_shot)) +
#   ggplot2::geom_tile(ggplot2::aes(x = shot_x, y = shot_y, fill = fen_z), alpha = 0.5) +
#   ggplot2::scale_fill_viridis_c(option = "D") +
#   ggnewscale::new_scale_fill() +
#   ggplot2::geom_tile(ggplot2::aes(x = shot_x, y = shot_y, fill = block_z), alpha = 0.3) +
#   ggplot2::scale_fill_viridis_c(option = "A")
  # dplyr::tally()
  # dplyr::summarise(
  #   sum(event_type %in% c("BLOCK")) / dplyr::n()
  # )


shot_att_density_est_5v5 <-
  shot_att_density_est_5v5 |>
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
  # dplyr::group_by(shot_x_1, shot_y_1) |>
  # dplyr::mutate(
  #   est_x = weighted.mean(shot_x, fen_z**2),
  #   est_y = weighted.mean(shot_y, fen_z**2)
  # ) |>
  # ggplot2::ggplot() +
  # off_zone_markings(show_behind_net = T) +
  # ggplot2::facet_wrap(ggplot2::vars(shot_x_1, shot_y_1)) +
  # ggplot2::geom_tile(ggplot2::aes(x = shot_x, y = shot_y, fill = fen_z), alpha = 0.5) +
  # ggplot2::geom_point(ggplot2::aes(x = shot_x_1, y = shot_y_1)) +
  # ggplot2::geom_point(ggplot2::aes(x = est_x, y = est_y)) +
  # ggplot2::geom_abline(ggplot2::aes(slope = slope_left, intercept = intercept_left)) +
  # ggplot2::geom_abline(ggplot2::aes(slope = slope_right, intercept = intercept_right)) +
  # ggplot2::scale_fill_viridis_c(option = "D")




# shot_blocker_density_est_5v5 <-
shot_att_density_est_5v5 |>
  dplyr::select(-c(tidyselect::any_of(c("est_x", "est_y")))) |>
  dplyr::filter(
    abs(shot_x) <= 6,
    shot_y == 60,
    point_shot,
    shot_type == "Slap"
  ) |>
  # head() |>
  # dplyr::filter(shot_x %in% c(3, -3)) |>
  dplyr::mutate(
    blocker_dens =
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

          shot_att_density_est_5v5 |>
            dplyr::filter(
              shot_type == type,
              point_shot == point,
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
            # dplyr::summarise(
            #   blocker_dens = sum(block_z)
            # ) |>
            # dplyr::pull(blocker_dens)
            dplyr::mutate(
              slope_left = slope_left_post,
              intercept_left = intercept_left_post,
              slope_right = slope_right_post,
              intercept_right = intercept_right_post
            )
        }
      )
  ) |>
  # dplyr::group_by(shot_type, point_shot) |>
  # dplyr::mutate(blocker_dens = blocker_dens / max(blocker_dens)) |>
  # ggplot2::ggplot() +
  # off_zone_markings(show_behind_net = T) +
  # ggplot2::facet_wrap(ggplot2::vars(shot_type, point_shot)) +
  # ggplot2::geom_tile(ggplot2::aes(x = shot_x, y = shot_y, fill = blocker_dens), alpha = 0.7)


  # View()
  dplyr::select(shot_x_1 = shot_x, shot_y_1 = shot_y, blocker_dens) |>
  tidyr::unnest(blocker_dens) |>
  # dplyr::ungroup()
# dplyr::group_by(shot_x_1, shot_y_1) |>
# dplyr::mutate(
#   est_x = weighted.mean(shot_x, fen_z**2),
#   est_y = weighted.mean(shot_y, fen_z**2)
# ) |>
ggplot2::ggplot() +
off_zone_markings(show_behind_net = T) +
ggplot2::facet_wrap(ggplot2::vars(shot_x_1, shot_y_1)) +
ggplot2::geom_tile(ggplot2::aes(x = shot_x, y = shot_y, fill = block_z), alpha = 0.5) +
ggplot2::geom_point(ggplot2::aes(x = shot_x_1, y = shot_y_1)) +
# ggplot2::geom_point(ggplot2::aes(x = est_x, y = est_y)) +
ggplot2::geom_abline(ggplot2::aes(slope = slope_left, intercept = intercept_left)) +
ggplot2::geom_abline(ggplot2::aes(slope = slope_right, intercept = intercept_right)) +
ggplot2::scale_fill_viridis_c(option = "D")

# View()


pred_xg_shot_data_22 |>
  dplyr::filter(
    shot_y > 0,
    shot_zone == "O",
    position_category != "G",
    event_team_strength == "EV",
    home_skater_strength_state %in% c("5v5"),
    # event_team_strength == "PP",
    # home_skater_strength_state %in% c("4v3", "3v4")
  ) |>
  dplyr::mutate(
    event_type =
      (event_type == "BLOCK") |>
      ifelse("Estimated Blocked Shot Origins", "Fenwick Origins"),
    point_shot =
      position_category == "D" &
      shot_type %in% c("Slap", "Wrist/Snap")
  ) |>
  # dplyr::filter(event_type == "BLOCK") |>
  dplyr::left_join(
    shot_att_density_est_5v5
  ) |>
  dplyr::mutate(
    est_x = ifelse(event_type == "Estimated Blocked Shot Origins", est_x, shot_x),
    est_y = ifelse(event_type == "Estimated Blocked Shot Origins", est_y, shot_y)
  ) |>
  dplyr::bind_rows(
    pred_xg_shot_data_22 |>
      dplyr::filter(
        shot_y > 0,
        shot_zone == "O",
        position_category != "G",
        event_team_strength == "EV",
        home_skater_strength_state %in% c("5v5"),
        event_type == "BLOCK"
        # event_team_strength == "PP",
        # home_skater_strength_state %in% c("4v3", "3v4")
      ) |>
      dplyr::mutate(
        event_type = "Location Blocked",
        point_shot =
          position_category == "D" &
          shot_type %in% c("Slap", "Wrist/Snap"),
        est_x = shot_x,
        est_y = shot_y
      )
  ) |>
  dplyr::mutate(
    event_type =
      event_type |>
      factor(
        levels = c(
          "Fenwick Origins",
          "Location Blocked",
          "Estimated Blocked Shot Origins"
        )
      )
  ) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::facet_wrap(
    ggplot2::vars(
      event_type,
      # (event_type == "BLOCK") |>
      #   ifelse("Estimated Blocked Shot Origins", "Fenwick Origins"),
      shot_type,
      ifelse(point_shot, "'Point Shots' (Shooter is Defenseman)", "Non-Point Shots"),
    ),
    nrow = 3
  ) +
  ggplot2::geom_density_2d_filled(
    ggplot2::aes(x = est_x, y = est_y),
    contour_var = "ndensity",
    bins = 10
    # values = colorRampPalette(c("white","blue"))(10)
  ) +
  # )
  ggplot2::scale_fill_manual(
    values =
      # colorRampPalette(c("white","blue"))(10) |>
      # stringr::str_c("80") |>
      # stringr::str_replace("#FFFFFF80", "#FFFFFF00")
    c(
      "#FFFFFF00",
      scales::viridis_pal(alpha = 0.7, option = "A")(n = 9)
    )
  ) +
  ggplot2::labs(
    title = "5-on-5 Estimated Blocked Shot Origins vs Fenwick Shot Origins",
    subtitle = "2022-23 Season",
    caption = "Data via NHL"
  )


