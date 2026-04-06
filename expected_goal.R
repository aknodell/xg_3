x_goal_data <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type %in% c("SHOT", "GOAL")) |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  # dplyr::filter(event_detail_1 %in% c("Backhand")) |>
  # dplyr::filter(event_detail_2 != "Defensive Deflection") |>
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
    is_goal = as.integer(event_type == "GOAL"),
    weight = as.integer(shooter_weight * goalie_weight * 615),
    dist_to_center_goalline = sqrt(x**2 + y**2),
    dist_post_1 = sqrt((abs(x) - 3)**2 + y**2),
    # angle_outside_post_1 = atan((x - 6) / y),
    # angle_outside_post_2 = atan((x + 6) / y),
    # angle_center = atan(y / abs(x)),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    # horiz_perc = horiz_angle / pi,
    # outside_horiz_angle = abs(angle_outside_post_1 - angle_outside_post_2),
    rad = cos(horiz_angle / 2) * dist_post_1,
    rad_2 = rad**2,
    rad_3 = rad**3,
    width = 2 * sqrt(dist_post_1**2 - rad**2),
    # horiz_perc =
      # ((horiz_angle / outside_horiz_angle) +
    # (width / (width + 6))) /
    # 2,
    # horiz_perc =
    #   ifelse(
    #     horiz_angle / oustide_horiz_angle
    #   ),
    vert_angle = atan(4 / dist_to_center_goalline),
    # vert_angle_6_ft = atan(6 / dist_to_center_goalline),
    # vert_perc = vert_angle / vert_angle_6_ft,
    height = tan(vert_angle) * rad,
    max_area_to_cover = width * height
    # shooting_target_perc = horiz_perc * vert_perc,
    # dist_to_center_goalline = dist_to_center_goalline**2
  ) |>
  dplyr::select(
    is_goal, x, y, is_snap:is_tip, rad, rad_2, rad_3, height, width, max_area_to_cover,
    shot_blocker_density_smoothed, weight
  )


set.seed(1138)
x_goal_cv <-
  glmnet::cv.glmnet(
    model.matrix(
      is_goal ~
        is_snap +
        is_slap +
        is_backhand +
        is_tip +
        # rad +
        rad_2 +
        rad_3 +
        width +
        height +
        # max_area_to_cover +
        shot_blocker_density_smoothed,
      # (shot_blocker_density_smoothed * dist_to_center_goalline),
      x_goal_data[seq(nrow(x_goal_data)) %% 5 != 1, ]
    )[, -1],
    x_goal_data[seq(nrow(x_goal_data)) %% 5 != 1, ]$is_goal,
    family = "binomial",
    alpha = 1,
    weights = x_goal_data[seq(nrow(x_goal_data)) %% 5 != 1, ]$weight
  )

x_goal_model_min <-
  glmnet::glmnet(
    model.matrix(
      is_goal ~
        is_snap +
        is_slap +
        is_backhand +
        is_tip +
        # rad +
        rad_2 +
        rad_3 +
        width +
        height +
        # max_area_to_cover +
        shot_blocker_density_smoothed,
      # (shot_blocker_density_smoothed * dist_to_center_goalline),
      x_goal_data[seq(nrow(x_goal_data)) %% 5 != 1, ]
    )[, -1],
    x_goal_data[seq(nrow(x_goal_data)) %% 5 != 1, ]$is_goal,
    family = "binomial",
    alpha = 1,
    lambda = x_goal_cv$lambda.min,
    weights = x_goal_data[seq(nrow(x_goal_data)) %% 5 != 1, ]$weight
  )

x_goal_model_min |>
  coef()

predict(
  x_goal_model_min,
  model.matrix(
    is_goal ~
      is_snap +
      is_slap +
      is_backhand +
      is_tip +
      # rad +
      rad_2 +
      rad_3 +
      width +
      height +
      # max_area_to_cover +
      shot_blocker_density_smoothed,
    # (shot_blocker_density_smoothed * dist_to_center_goalline),
    x_goal_data
  )[, -1],
  type = "response"
) |>
  summary()


expected_goal_5v5 <-
  tidyr::expand_grid(
    event_detail_1 = "Wrist",
    x = -42:42,
    y = 1:64
  ) |>
  dplyr::mutate(
    x_goal =
      predict(
        x_goal_model_min,
        model.matrix(
          is_goal ~
            is_snap +
            is_slap +
            is_backhand +
            is_tip +
            rad_2 +
            rad_3 +
            width +
            height +
            shot_blocker_density_smoothed,
          tidyr::expand_grid(
            is_goal = 0,
            x = -42:42,
            y = 1:64
          ) |>
            dplyr::mutate(
              is_snap = 0,
              is_slap = 0,
              is_backhand = 0,
              is_tip = 0,
              rad = cos(abs(atan((x - 3) / y) - atan((x + 3) / y)) / 2) * sqrt((abs(x) - 3)**2 + y**2),
              rad_2 = rad**2,
              rad_3 = rad**3,
              width = 2 * sqrt(sqrt((abs(x) - 3)**2 + y**2)**2 - rad**2),
              height = tan(atan(4 / sqrt(x**2 + y**2))) * rad,
            ) |>
            dplyr::select(-rad) |>
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
        x_goal =
          predict(
            x_goal_model_min,
            model.matrix(
              is_goal ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                rad_2 +
                rad_3 +
                width +
                height +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_goal = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 1,
                  is_slap = 0,
                  is_backhand = 0,
                  is_tip = 0,
                  rad = cos(abs(atan((x - 3) / y) - atan((x + 3) / y)) / 2) * sqrt((abs(x) - 3)**2 + y**2),
                  rad_2 = rad**2,
                  rad_3 = rad**3,
                  width = 2 * sqrt(sqrt((abs(x) - 3)**2 + y**2)**2 - rad**2),
                  height = tan(atan(4 / sqrt(x**2 + y**2))) * rad,
                ) |>
                dplyr::select(-rad) |>
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
        x_goal =
          predict(
            x_goal_model_min,
            model.matrix(
              is_goal ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                rad_2 +
                rad_3 +
                width +
                height +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_goal = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 1,
                  is_backhand = 0,
                  is_tip = 0,
                  rad = cos(abs(atan((x - 3) / y) - atan((x + 3) / y)) / 2) * sqrt((abs(x) - 3)**2 + y**2),
                  rad_2 = rad**2,
                  rad_3 = rad**3,
                  width = 2 * sqrt(sqrt((abs(x) - 3)**2 + y**2)**2 - rad**2),
                  height = tan(atan(4 / sqrt(x**2 + y**2))) * rad,
                ) |>
                dplyr::select(-rad) |>
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
        x_goal =
          predict(
            x_goal_model_min,
            model.matrix(
              is_goal ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                rad_2 +
                rad_3 +
                width +
                height +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_goal = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 0,
                  is_backhand = 1,
                  is_tip = 0,
                  rad = cos(abs(atan((x - 3) / y) - atan((x + 3) / y)) / 2) * sqrt((abs(x) - 3)**2 + y**2),
                  rad_2 = rad**2,
                  rad_3 = rad**3,
                  width = 2 * sqrt(sqrt((abs(x) - 3)**2 + y**2)**2 - rad**2),
                  height = tan(atan(4 / sqrt(x**2 + y**2))) * rad,
                ) |>
                dplyr::select(-rad) |>
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
        x_goal =
          predict(
            x_goal_model_min,
            model.matrix(
              is_goal ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                rad_2 +
                rad_3 +
                width +
                height +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_goal = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 0,
                  is_backhand = 0,
                  is_tip = 1,
                  rad = cos(abs(atan((x - 3) / y) - atan((x + 3) / y)) / 2) * sqrt((abs(x) - 3)**2 + y**2),
                  rad_2 = rad**2,
                  rad_3 = rad**3,
                  width = 2 * sqrt(sqrt((abs(x) - 3)**2 + y**2)**2 - rad**2),
                  height = tan(atan(4 / sqrt(x**2 + y**2))) * rad,
                ) |>
                dplyr::select(-rad) |>
                dplyr::left_join(shot_blocker_density_smoothed)
            )[, -1],
            type = "response"
          ) |>
          as.double()
      )
  )

expected_goal_5v5 |>
  # dplyr::mutate(
  #   # is_snap = 0,
  #   # is_slap = 0,
  #   # is_backhand = 0,
  #   # is_tip = 1,
  #   horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
  #   vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
  # ) |>
  # dplyr::left_join(shot_blocker_density_smoothed) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = x_goal)) +
  ggplot2::scale_fill_viridis_c(option = "A", alpha = 0.7) +
  ggplot2::labs(
    title = "Estimated 5-on-5 Expected Goal (Given On Goal) Values by Attempt Location",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )


corsi_expected_vals <-
  expected_through_5v5 |>
  dplyr::left_join(
    expected_on_5v5
  ) |>
  dplyr::left_join(
    expected_goal_5v5
  ) |>
  dplyr::mutate(
    fenwick_xg = x_goal * x_on,
    corsi_xg = x_goal * x_on * x_thru,
  )

corsi_expected_vals |>
  # dplyr::mutate(
  #   # is_snap = 0,
  #   # is_slap = 0,
  #   # is_backhand = 0,
  #   # is_tip = 1,
  #   horiz_perc = abs(atan((x - 3) / y) - atan((x + 3) / y)) / pi,
  #   vert_perc = atan(4 / sqrt(x**2 + y**2)) / atan(6 / sqrt(x**2 + y**2))
  # ) |>
  # dplyr::left_join(shot_blocker_density_smoothed) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = x_on)) +
  ggplot2::scale_fill_viridis_c(option = "A", alpha = 0.7) +
  ggplot2::labs(
    title = "5-on-5 Corsi Expected Goal Values by Attempt Location",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )


