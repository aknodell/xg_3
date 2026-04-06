blocked_shot_xg_vals <-
  tidyr::expand_grid(
    # event_detail_1 = "Backhand",
    event_detail_1 = c("Backhand", "Slap", "Snap", "Tip In/Deflection", "Wrist"),
    x = -42:42,
    y = 1:64
  ) |>
  # head() |>
  dplyr::mutate(
    slope_to_left_post = y / (x + 3.01),
    slope_to_right_post = y / (x - 3.01),
    left_intercept = 3 * slope_to_left_post,
    right_intercept = -3 * slope_to_right_post
  ) |>
  dplyr::mutate(
    expected_values =
      purrr::pmap(
        list(
          type = event_detail_1,
          left_slope = slope_to_left_post,
          right_slope = slope_to_right_post,
          left_int = left_intercept,
          right_int = right_intercept
        ),
        function(type, left_slope, right_slope, left_int, right_int) {
          coords <- {
            if (sign(left_slope) == -1 & sign(right_slope) == -1) {
              corsi_expected_vals |>
                dplyr::filter(
                  event_detail_1 == type,
                  y <= (x * left_slope) + left_int,
                  y >= (x * right_slope) + right_int
                )
            } else if (sign(left_slope) == 1 & sign(right_slope) == 1) {
              corsi_expected_vals |>
                dplyr::filter(
                  event_detail_1 == type,
                  y >= (x * left_slope) + left_int,
                  y <= (x * right_slope) + right_int
                )
            } else {
              corsi_expected_vals |>
                dplyr::filter(
                  event_detail_1 == type,
                  y >= (x * left_slope) + left_int,
                  y >= (x * right_slope) + right_int
                )
            }
          }

          if (type == "Backhand") {
            coords |>
              dplyr::left_join(
                back_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01)
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                xthru = weighted.mean(xthru, atts_smoothed_non_zero),
                x_on = weighted.mean(x_on, atts_smoothed_non_zero),
                x_goal = weighted.mean(x_goal, atts_smoothed_non_zero)
              )
          } else if (type == "Slap") {
            coords |>
              dplyr::left_join(
                slap_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01)
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                xthru = weighted.mean(xthru, atts_smoothed_non_zero),
                x_on = weighted.mean(x_on, atts_smoothed_non_zero),
                x_goal = weighted.mean(x_goal, atts_smoothed_non_zero)
              )

          } else if (type == "Snap") {
            coords |>
              dplyr::left_join(
                snap_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01)
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                xthru = weighted.mean(xthru, atts_smoothed_non_zero),
                x_on = weighted.mean(x_on, atts_smoothed_non_zero),
                x_goal = weighted.mean(x_goal, atts_smoothed_non_zero)
              )
          } else if (type == "Tip In/Deflection") {
            coords |>
              dplyr::left_join(
                tip_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01)
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                xthru = weighted.mean(xthru, atts_smoothed_non_zero),
                x_on = weighted.mean(x_on, atts_smoothed_non_zero),
                x_goal = weighted.mean(x_goal, atts_smoothed_non_zero)
              )

          } else if (type == "Wrist") {
            coords |>
              dplyr::left_join(
                wrist_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01)
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                xthru = weighted.mean(xthru, atts_smoothed_non_zero),
                x_on = weighted.mean(x_on, atts_smoothed_non_zero),
                x_goal = weighted.mean(x_goal, atts_smoothed_non_zero)
              )
          }
        }
      )
  ) |>
  dplyr::select(x, y, event_detail_1, expected_values) |>
  tidyr::unnest(expected_values)

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

# xg_24 <-
corsi_25 |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(
    (event_type != "BLOCK" & event_team_zone == "O") |
      (event_type == "BLOCK" & event_team_zone == "D")
  ) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::mutate(
    event_detail_1 =
      dplyr::case_when(
        event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
        event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
        T ~ event_detail_1
      ),
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1,
    x = coords_y,
    y = coords_x
  ) |>
  dplyr::filter(y > 0) |>
  dplyr::left_join(
    blocked_shot_est_coords |>
      dplyr::mutate(
        est_x = as.integer(round(est_x)),
        est_y = as.integer(round(est_y))
      )
  ) |>
  dplyr::mutate(
    x = ifelse(event_type == "BLOCK", est_x, x),
    y = ifelse(event_type == "BLOCK", est_y, y)
  ) |>
  dplyr::left_join(
    corsi_expected_vals
    # blocked_shot_xg_vals |>
    #   dplyr::mutate(event_type = "BLOCK") |>
    #   dplyr::bind_rows(
    #     corsi_expected_vals |>
    #       dplyr::mutate(event_type = "MISS")
    #   ) |>
    #   dplyr::bind_rows(
    #     corsi_expected_vals |>
    #       dplyr::mutate(event_type = "SHOT")
    #   ) |>
    #   dplyr::bind_rows(
    #     corsi_expected_vals |>
    #       dplyr::mutate(event_type = "GOAL")
    #   )
  ) |>
  # dplyr::group_by(event_type == "BLOCK") |>
  # dplyr::summarise(
  #   count = dplyr::n(),
  #   xblocks = sum(1 - x_thru)
  # )
  # dplyr::mutate(
  #   xblock = 1 - xthru,
  #   xblock = xblock / 1.14,
  #   xthru = 1 - xblock,
  #   corsi_xg = xthru * x_on * x_goal,
  #   fenwick_xg = x_on * x_goal
  # ) |>
  # dplyr::group_by(event_type == "BLOCK") |>
  # dplyr::summarise(
  #   count = dplyr::n(),
  #   xblocks = sum(1 - xthru)
  # )
  # dplyr::summarise(
  #   sum(corsi_xg),
  #   sum(fenwick_xg * (event_type != "BLOCK")),
  #   sum(x_goal * (event_type %in% c("GOAL", "SHOT"))),
  #   sum(event_type == "GOAL")
  # )
  dplyr::summarise(
    gp = unique(game_id) |> length(),
    corsi = dplyr::n(),
    x_thru = sum(x_thru),
    thru = sum(event_type != "BLOCK"),
    thru_over_x = thru - x_thru,
    x_on = sum((event_type != "BLOCK") * x_on),
    on = sum(event_type %in% c("SHOT", "GOAL")),
    on_over_x = on - x_on,
    corsi_xg = sum(corsi_xg),
    fenwick_xg = sum((event_type != "BLOCK") * fenwick_xg),
    sog_xg = sum((event_type %in% c("SHOT", "GOAL")) * x_goal),
    g = sum(event_type == "GOAL"),
    corsi_g_over_x = g - corsi_xg,
    fenwick_g_over_x = g - fenwick_xg,
    sog_g_over_x = g - sog_xg
    # x_g = sum
  ) |>
  as.list()
  View("25-26 Summary")


corsi_25 |>
  # dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(
    (event_type != "BLOCK" & event_team_zone == "O") |
      (event_type == "BLOCK" & event_team_zone == "D")
  ) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::mutate(
    event_detail_1 =
      dplyr::case_when(
        event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
        event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
        T ~ event_detail_1
      ),
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1,
    x = coords_y,
    y = coords_x
  ) |>
  dplyr::filter(y > 0) |>
  dplyr::left_join(
    blocked_shot_est_coords |>
      dplyr::mutate(
        est_x = as.integer(round(est_x)),
        est_y = as.integer(round(est_y))
      )
  ) |>
  dplyr::mutate(
    x = ifelse(event_type == "BLOCK", est_x, x),
    y = ifelse(event_type == "BLOCK", est_y, y)
  ) |>
  dplyr::left_join(
    corsi_expected_vals
  ) |>
  dplyr::mutate(
    goalie = ifelse(event_team == home_team, away_goalie, home_goalie)
  ) |>
  dplyr::filter(goalie == 8479979) |>
  dplyr::tally()
  # dplyr::group_by(game_id) |>
  dplyr::summarise(
    gp = unique(game_id) |> length(),
    corsi = dplyr::n(),
    x_thru = sum(x_thru),
    thru = sum(event_type != "BLOCK"),
    thru_over_x = thru - x_thru,
    x_on = sum((event_type != "BLOCK") * x_on),
    on = sum(event_type %in% c("SHOT", "GOAL")),
    on_over_x = on - x_on,
    corsi_xg = sum(corsi_xg),
    fenwick_xg = sum((event_type != "BLOCK") * fenwick_xg),
    sog_xg = sum((event_type %in% c("SHOT", "GOAL")) * x_goal),
    g = sum(event_type == "GOAL"),
    corsi_g_over_x = g - corsi_xg,
    fenwick_g_over_x = g - fenwick_xg,
    sog_g_over_x = g - sog_xg
    # x_g = sum
  ) |>
  View("25-26 Summary")

corsi_23 |>
  dplyr::filter(event_player_1 == 8481581, home_skater_strength_state == "5v5") |>
  dplyr::filter(
    (event_type != "BLOCK" & event_team_zone == "O")
  ) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::mutate(
    season = "2023-24",
    event_detail_1 =
      dplyr::case_when(
        event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
        event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
        T ~ event_detail_1
      ),
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1,
    x = coords_y,
    y = coords_x
  ) |>
  dplyr::bind_rows(
    pred_xg_shot_data |>
      dplyr::filter(event_player_1 == 8481581, home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::mutate(
        season = "2024-25",
        event_detail_1 =
          dplyr::case_when(
            event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
            event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
            T ~ event_detail_1
          ),
        coords_y = coords_y * sign(coords_x),
        coords_x = (coords_x * sign(coords_x) - 89) * -1,
        x = coords_y,
        y = coords_x
      )
  ) |>
  dplyr::bind_rows(
    corsi_25 |>
      dplyr::filter(event_player_1 == 8481581, home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type != "BLOCK" & event_team_zone == "O")
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::mutate(
        season = "2025-26",
        event_detail_1 =
          dplyr::case_when(
            event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
            event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
            T ~ event_detail_1
          ),
        coords_y = coords_y * sign(coords_x),
        coords_x = (coords_x * sign(coords_x) - 89) * -1,
        x = coords_y,
        y = coords_x
      )
  ) |>
  dplyr::filter(y > 0) |>
  # dplyr::left_join(
  #   blocked_shot_est_coords |>
  #     dplyr::mutate(
  #       est_x = as.integer(round(est_x)),
  #       est_y = as.integer(round(est_y))
  #     )
  # ) |>
  # dplyr::mutate(
  #   x = ifelse(event_type == "BLOCK", est_x, x),
  #   y = ifelse(event_type == "BLOCK", est_y, y)
  # ) |>
  ggplot2::ggplot() +
  off_zone_markings(show_behind_net = T, direction = "up") +
  ggplot2::facet_wrap(ggplot2::vars(season)) +
  ggplot2::geom_density2d_filled(ggplot2::aes(x = x, y = y), bins = 9) +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), size = 2, alpha = 0.3) +
  ggplot2::scale_fill_manual(
    "Shooting Percentage",
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 8)
      )
  ) +
  ggplot2::labs(
    title = "Where Does Thomas Harley Shoot the Puck From?",
    subtitle = "5-on-5",
    caption = "Puck Over the Glass   |   Data via NHL"
  )



  tidyr::pivot_longer(
    c(home_team, away_team),
    values_to = "team"
  ) |>
  dplyr::group_by(team) |>
  dplyr::summarise(
    corsi_for = sum(event_team == team),
    corsi_xg_for = sum(corsi_xg * (event_team == team)),
    fenwick_xg_for = sum(fenwick_xg * (event_team == team) * (event_type != "BLOCK")),
    sog_xg_for = sum(x_goal * (event_team == team) * (event_type %in% c("SHOT", "GOAL"))),
    g_for = sum((event_team == team) * (event_type == "GOAL")),
    corsi_against = sum(event_team != team),
    corsi_xg_against = sum(corsi_xg * (event_team != team)),
    fenwick_xg_against = sum(fenwick_xg * (event_team != team) * (event_type != "BLOCK")),
    sog_xg_against = sum(x_goal * (event_team != team) * (event_type %in% c("SHOT", "GOAL"))),
    g_against = sum((event_team != team) * (event_type == "GOAL")),
    corsi_perc = corsi_for / (corsi_for + corsi_against),
    corsi_xg_perc = corsi_xg_for / (corsi_xg_for + corsi_xg_against),
    fenwick_xg_perc = fenwick_xg_for / (fenwick_xg_for + fenwick_xg_against),
    sog_xg_perc = sog_xg_for / (sog_xg_for + sog_xg_against),
    g_perc = g_for / (g_for + g_against),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(g_perc)) |>
  dplyr::mutate(team = team |> factor() |> forcats::fct_inorder()) |>
  tidyr::pivot_longer(
    c(
      corsi_perc,
      corsi_xg_perc,
      # fenwick_xg_perc,
      # sog_xg_perc,
      g_perc
    )
  ) |>
  dplyr::mutate(
    name = name |> factor(levels = c("corsi_perc", "corsi_xg_perc", "fenwick_xg_perc", "sog_xg_perc", "g_perc")),
    value = value - 0.5
  ) |>
  # dplyr::arrange(dplyr::desc(corsi_xg_for)) |>
  # dplyr::mutate(team = team |> factor() |> forcats::fct_inorder()) |>
  # tidyr::pivot_longer(c(corsi_xg_for, fenwick_xg_for, sog_xg_for, g_for)) |>
  # dplyr::mutate(
  #   name = name |> factor(levels = c("corsi_xg_for", "fenwick_xg_for", "sog_xg_for", "g_for"))
  # ) |>
  # dplyr::arrange(corsi_xg_against) |>
  # dplyr::mutate(team = team |> factor() |> forcats::fct_inorder()) |>
  # tidyr::pivot_longer(c(corsi_xg_against, fenwick_xg_against, sog_xg_against, g_against)) |>
  # dplyr::mutate(
  #   name = name |> factor(levels = c("corsi_xg_against", "fenwick_xg_against", "sog_xg_against", "g_against")),
  #   value = value * -1
  # ) |>
  ggplot2::ggplot(ggplot2::aes(x = team, y = value, fill = name)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_col(position = "dodge", width = 0.8) +
  # ggplot2::geom_col(ggplot2::aes(y = corsi_xg_perc), width = 0.8, fill = viridisLite::viridis(5, option = "D")[4]) +
  # ggplot2::geom_col(ggplot2::aes(y = fenwick_xg_perc), width = 0.6, fill = viridisLite::viridis(5, option = "D")[3]) +
  # ggplot2::geom_col(ggplot2::aes(y = sog_xg_perc), width = 0.4, fill = viridisLite::viridis(5, option = "D")[2]) +
  # ggplot2::geom_col(ggplot2::aes(y = g_perc), width = 0.2, fill = viridisLite::viridis(5, option = "D")[1]) +
  ggplot2::scale_fill_viridis_d("") +
  ggplot2::scale_x_discrete("") +
  ggplot2::scale_y_continuous("Share Above/Below 50%", labels = scales::percent) +
  ggplot2::labs(
    title = "5-on-5 xG Share by Sub-Model",
    subtitle = "2025-26 Season",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  )





g_adj <-
  tibble::tribble(
    ~home_score, ~is_home, ~g_adj,
    -1, 0, 1.071,
    -1, 1, 0.938,
    0, 0, 1.061,
    0, 1, 0.945,
    1, 0, 1.012,
    1, 1, 0.988
  )

xg_adj <-
  tibble::tribble(
    ~home_score, ~is_home, ~xg_adj,
    -1, F, 1.091,
    -1, T, 0.923,
    0, F, 1.051,
    0, T, 0.954,
    1, F, 1.010,
    1, T, 0.991
  )

corsi_adj <-
  tibble::tribble(
    ~home_score, ~is_home, ~c_adj,
    -3, F, 1.23,
    -3, T, 0.843,
    -2, F, 1.182,
    -2, T, 0.866,
    -1, F, 1.127,
    -1, T, 0.899,
    0, F, 1.032,
    0, T, 0.970,
    1, F, 0.952,
    1, T, 1.053,
    2, F, 0.913,
    2, T, 1.105,
    3, F, 0.891,
    3, T, 1.140
  )

corsi_25 |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(
    (event_type != "BLOCK" & event_team_zone == "O") |
      (event_type == "BLOCK" & event_team_zone == "D")
  ) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::mutate(
    event_detail_1 =
      dplyr::case_when(
        event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
        event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
        T ~ event_detail_1
      ),
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1,
    x = coords_y,
    y = coords_x
  ) |>
  dplyr::filter(y > 0) |>
  dplyr::left_join(
    blocked_shot_est_coords |>
      dplyr::mutate(
        est_x = as.integer(round(est_x)),
        est_y = as.integer(round(est_y))
      )
  ) |>
  dplyr::mutate(
    x = ifelse(event_type == "BLOCK", est_x, x),
    y = ifelse(event_type == "BLOCK", est_y, y)
  ) |>
  dplyr::left_join(corsi_expected_vals) |>
  dplyr::mutate(
    is_home = event_team == home_team,
    home_score =
      dplyr::case_when(
        home_score_diff < -3 ~ -3,
        home_score_diff > 3 ~ 3,
        T ~ home_score_diff
      )
  ) |>
  dplyr::left_join(corsi_adj) |>
  dplyr::mutate(home_score = sign(home_score)) |>
  dplyr::left_join(xg_adj) |>
  dplyr::left_join(g_adj) |>
  tidyr::pivot_longer(
    c(home_team, away_team),
    values_to = "team"
  ) |>
  dplyr::group_by(team) |>
  dplyr::summarise(
    corsi_for = sum((event_team == team) * c_adj),
    corsi_xg_for = sum(corsi_xg * (event_team == team) * xg_adj),
    fenwick_xg_for = sum(fenwick_xg * (event_team == team) * (event_type != "BLOCK")),
    sog_xg_for = sum(x_goal * (event_team == team) * (event_type %in% c("SHOT", "GOAL"))),
    g_for = sum((event_team == team) * (event_type == "GOAL") * g_adj),
    corsi_against = sum((event_team != team) * c_adj),
    corsi_xg_against = sum(corsi_xg * (event_team != team) * xg_adj),
    fenwick_xg_against = sum(fenwick_xg * (event_team != team) * (event_type != "BLOCK")),
    sog_xg_against = sum(x_goal * (event_team != team) * (event_type %in% c("SHOT", "GOAL"))),
    g_against = sum((event_team != team) * (event_type == "GOAL") * g_adj),
    corsi_perc = corsi_for / (corsi_for + corsi_against),
    corsi_xg_perc = corsi_xg_for / (corsi_xg_for + corsi_xg_against),
    fenwick_xg_perc = fenwick_xg_for / (fenwick_xg_for + fenwick_xg_against),
    sog_xg_perc = sog_xg_for / (sog_xg_for + sog_xg_against),
    g_perc = g_for / (g_for + g_against),
    .groups = "drop"
  ) |>
  # View()
  dplyr::arrange(dplyr::desc(g_perc)) |>
  dplyr::mutate(team = team |> factor() |> forcats::fct_inorder()) |>
  tidyr::pivot_longer(
    c(
      corsi_perc,
      corsi_xg_perc,
      # fenwick_xg_perc,
      # sog_xg_perc,
      g_perc
    )
  ) |>
  dplyr::mutate(
    name =
      name |>
      stringr::str_replace_all(
        c(
          "corsi_perc" = "Corsi %",
          "corsi_xg_perc" = "Corsi-Based xG %",
          "g_perc" = "Goals %"
        )
      ) |>
      factor(levels = c("Corsi %", "Corsi-Based xG %", "fenwick_xg_perc", "sog_xg_perc", "Goals %")),
    value = value - 0.5
  ) |>
  # dplyr::arrange(dplyr::desc(corsi_xg_for)) |>
  # dplyr::mutate(team = team |> factor() |> forcats::fct_inorder()) |>
  # tidyr::pivot_longer(c(corsi_xg_for, fenwick_xg_for, sog_xg_for, g_for)) |>
  # dplyr::mutate(
  #   name = name |> factor(levels = c("corsi_xg_for", "fenwick_xg_for", "sog_xg_for", "g_for"))
  # ) |>
  # dplyr::arrange(corsi_xg_against) |>
  # dplyr::mutate(team = team |> factor() |> forcats::fct_inorder()) |>
  # tidyr::pivot_longer(c(corsi_xg_against, fenwick_xg_against, sog_xg_against, g_against)) |>
  # dplyr::mutate(
  #   name = name |> factor(levels = c("corsi_xg_against", "fenwick_xg_against", "sog_xg_against", "g_against")),
  #   value = value * -1
  # ) |>
  ggplot2::ggplot(ggplot2::aes(x = team, y = value, fill = name)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_col(position = "dodge", width = 0.8) +
  # ggplot2::geom_col(ggplot2::aes(y = corsi_xg_perc), width = 0.8, fill = viridisLite::viridis(5, option = "D")[4]) +
  # ggplot2::geom_col(ggplot2::aes(y = fenwick_xg_perc), width = 0.6, fill = viridisLite::viridis(5, option = "D")[3]) +
  # ggplot2::geom_col(ggplot2::aes(y = sog_xg_perc), width = 0.4, fill = viridisLite::viridis(5, option = "D")[2]) +
  # ggplot2::geom_col(ggplot2::aes(y = g_perc), width = 0.2, fill = viridisLite::viridis(5, option = "D")[1]) +
  ggplot2::scale_fill_viridis_d("") +
  ggplot2::scale_x_discrete("") +
  ggplot2::scale_y_continuous("Share Above/Below 50%", labels = scales::percent) +
  ggplot2::labs(
    title = "5-on-5 Shot Metrics",
    subtitle = "2025-26 Season, Score & Venue Adjusted",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  )




