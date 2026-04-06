pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  # dplyr::filter(event_type == "BLOCK") |>
  # dplyr::filter(event_team_zone == "D") |>
  # dplyr::filter(is_rush == 0) |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(shot_zone == "O") |>
  dplyr::mutate(
    event_detail_1 =
      dplyr::case_when(
        event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
        event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
        T ~ event_detail_1
      )
    # x = coords_y * sign(coords_x),
    # y = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(shot_y > 0) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::group_by(position_category, event_detail_1) |>
  dplyr::mutate(count = "n = {formatC(dplyr::n(), digits = 0, big.mark = ',', format = 'f')}" |> glue::glue()) |>
  # dplyr::filter(
  #   event_detail_1 %in%
  #     c(
  #       "Snap",
  #       "Slap",
  #       "Wrist",
  #       "Backhand",
  #       "Tip In",
  #       "Deflected"
  #     )
  # ) |>
  # dplyr::mutate(
    # rush_secs = as.character(rush_secs),
    # rush_secs = ifelse(is_rush == 0, "21+", rush_secs) |> factor(levels = c(as.character(0:20), "21+")),
    # event_detail_1 =
    #   ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1)
  # ) |>
  # dplyr::filter(event_detail_1 == "Tip") |>
  # dplyr::group_by(event_detail_1, position_category) |>
  # dplyr::group_by(rush_secs) |>
  # dplyr::tally()
  # dplyr::mutate(
  #   perc = n / sum(n),
  #   cum_perc = cumsum(perc)
  # )
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    legend_position = "none"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(position_category, event_detail_1, count), ncol = 5) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = shot_x, y = shot_y), contour_var = "ndensity", bins = 10) +
  ggplot2::scale_fill_manual(
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 9)
      )
    # scales::viridis_pal(alpha = 0.7, option = "A")(n = 8) |>
    # stringr::str_replace_all("#000004B3", "#FFFFFF00")
  ) +
  ggplot2::labs(
    title = "5-on-5 Unblocked Shot Distribution By Shooter Position",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )
  # ggplot2::theme(legend.position = "none")


pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  # dplyr::filter(event_type == "BLOCK") |>
  # dplyr::filter(event_team_zone == "D") |>
  # dplyr::filter(is_rush == 0) |>
  dplyr::filter(event_type == "BLOCK") |>
  dplyr::filter(shot_zone == "O") |>
  dplyr::mutate(
    event_detail_1 =
      dplyr::case_when(
        event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
        event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
        T ~ event_detail_1
      )
    # x = coords_y * sign(coords_x),
    # y = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(shot_y > 0) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::group_by(position_category, event_detail_1) |>
  dplyr::mutate(count = "n = {formatC(dplyr::n(), digits = 0, big.mark = ',', format = 'f')}" |> glue::glue()) |>
  # dplyr::filter(
  #   event_detail_1 %in%
  #     c(
  #       "Snap",
  #       "Slap",
  #       "Wrist",
  #       "Backhand",
  #       "Tip In",
  #       "Deflected"
  #     )
  # ) |>
  # dplyr::mutate(
  # rush_secs = as.character(rush_secs),
  # rush_secs = ifelse(is_rush == 0, "21+", rush_secs) |> factor(levels = c(as.character(0:20), "21+")),
  # event_detail_1 =
  #   ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1)
  # ) |>
  # dplyr::filter(event_detail_1 == "Tip") |>
  # dplyr::group_by(event_detail_1, position_category) |>
  # dplyr::group_by(rush_secs) |>
  # dplyr::tally()
  # dplyr::mutate(
  #   perc = n / sum(n),
  #   cum_perc = cumsum(perc)
  # )
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    legend_position = "none"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(position_category, event_detail_1, count), ncol = 5) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = shot_x, y = shot_y), contour_var = "ndensity", bins = 10) +
  ggplot2::scale_fill_manual(
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 9)
      )
    # scales::viridis_pal(alpha = 0.7, option = "A")(n = 8) |>
    # stringr::str_replace_all("#000004B3", "#FFFFFF00")
  ) +
  ggplot2::labs(
    title = "5-on-5 Blocked Shot Distribution By Shooter Position",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )

MASS::kde2d(
  x =
    pred_xg_shot_data |>
    dplyr::mutate(
      event_detail_1 =
        dplyr::case_when(
          event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
          event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
          T ~ event_detail_1
        )
    ) |>
    dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
    dplyr::filter(event_type == "BLOCK") |>
    dplyr::filter(shot_zone == "O") |>
    dplyr::filter(home_skater_strength_state == "5v5") |>
    dplyr::filter(shot_y > 0) |>
    dplyr::filter(position_category == "F" | event_detail_1 == c("Backhand")) |>
    dplyr::pull(shot_x),
  y =
    pred_xg_shot_data |>
    dplyr::mutate(
      event_detail_1 =
        dplyr::case_when(
          event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
          event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
          T ~ event_detail_1
        )
    ) |>
    dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
    dplyr::filter(event_type == "BLOCK") |>
    dplyr::filter(shot_zone == "O") |>
    dplyr::filter(home_skater_strength_state == "5v5") |>
    dplyr::filter(shot_y > 0) |>
    dplyr::filter(position_category == "F" | event_detail_1 == c("Backhand")) |>
    dplyr::pull(shot_y),
  lims = c(c(-42, 42), c(0, 64)),
  n = c(85, 65)
) |>
  purrr::pluck("z") |>
  tibble::as_tibble(.name_repair = "unique") |>
  dplyr::mutate(x = seq(-42, 42)) |>
  tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
  dplyr::mutate(
    type = "Point (Implied)",
    y = y |> stringr::str_extract("\\d+") |> as.integer() |> magrittr::subtract(1),
    density = z / sum(z)
  ) |>
  dplyr::bind_rows(
    MASS::kde2d(
      x =
        pred_xg_shot_data |>
        dplyr::mutate(
          event_detail_1 =
            dplyr::case_when(
              event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
              event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
              T ~ event_detail_1
            )
        ) |>
        dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
        dplyr::filter(event_type == "BLOCK") |>
        dplyr::filter(shot_zone == "O") |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::filter(shot_y > 0) |>
        dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
        dplyr::pull(shot_x),
      y =
        pred_xg_shot_data |>
        dplyr::mutate(
          event_detail_1 =
            dplyr::case_when(
              event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
              event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
              T ~ event_detail_1
            )
        ) |>
        dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
        dplyr::filter(event_type == "BLOCK") |>
        dplyr::filter(shot_zone == "O") |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::filter(shot_y > 0) |>
        dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
        dplyr::pull(shot_y),
      lims = c(c(-42, 42), c(0, 64)),
      n = c(85, 65)
    ) |>
      purrr::pluck("z") |>
      tibble::as_tibble(.name_repair = "unique") |>
      dplyr::mutate(x = seq(-42, 42)) |>
      tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
      dplyr::mutate(
        type = "Low (Implied)",
        y = y |> stringr::str_extract("\\d+") |> as.integer() |> magrittr::subtract(1),
        density = z / sum(z)
      )
  ) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::facet_wrap(ggplot2::vars(type), ncol = 2) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = density), alpha = 0.7) +
  ggplot2::scale_fill_viridis_c(option = "A")


MASS::kde2d(
  x =
    pred_xg_shot_data |>
    dplyr::mutate(
      event_detail_1 =
        dplyr::case_when(
          event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
          event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
          T ~ event_detail_1
        )
    ) |>
    dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
    dplyr::filter(event_type == "BLOCK") |>
    dplyr::filter(shot_zone == "O") |>
    dplyr::filter(home_skater_strength_state == "5v5") |>
    dplyr::filter(shot_y > 0) |>
    dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
    dplyr::pull(shot_x),
  y =
    pred_xg_shot_data |>
    dplyr::mutate(
      event_detail_1 =
        dplyr::case_when(
          event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
          event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
          T ~ event_detail_1
        )
    ) |>
    dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
    dplyr::filter(event_type == "BLOCK") |>
    dplyr::filter(shot_zone == "O") |>
    dplyr::filter(home_skater_strength_state == "5v5") |>
    dplyr::filter(shot_y > 0) |>
    dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
    dplyr::pull(shot_y),
  lims = c(c(-42, 42), c(0, 64)),
  n = c(85, 65)
) |>
  purrr::pluck("z") |>
  tibble::as_tibble(.name_repair = "unique") |>
  dplyr::mutate(x = seq(-42, 42)) |>
  tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
  dplyr::mutate(
    y = y |> stringr::str_extract("\\d+") |> as.integer() |> magrittr::subtract(1),
    density = z / sum(z)
  ) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = density), alpha = 0.7) +
  ggplot2::scale_fill_viridis_c(option = "A")


non_rush_density_fenwick <-
  MASS::kde2d(
    x =
      pred_xg_shot_data |>
      dplyr::filter(event_type != "BLOCK") |>
      dplyr::filter(event_team_zone == "O") |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(is_rush == 0) |>
      dplyr::mutate(
        x = coords_y * sign(coords_x),
        y = (coords_x * sign(coords_x) - 89) * -1
      ) |>
      dplyr::filter(y > 0) |>
      dplyr::filter(
        event_detail_1 %in%
          c(
            "Snap",
            "Slap",
            "Wrist",
            "Backhand",
            "Tip In",
            "Deflected"
          )
      ) |>
      dplyr::pull(x),
    y =
      pred_xg_shot_data |>
      dplyr::filter(event_type != "BLOCK") |>
      dplyr::filter(event_team_zone == "O") |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(is_rush == 0) |>
      dplyr::mutate(
        x = coords_y * sign(coords_x),
        y = (coords_x * sign(coords_x) - 89) * -1
      ) |>
      dplyr::filter(y > 0) |>
      dplyr::filter(
        event_detail_1 %in%
          c(
            "Snap",
            "Slap",
            "Wrist",
            "Backhand",
            "Tip In",
            "Deflected"
          )
      ) |>
      dplyr::pull(y),
    lims = c(c(-42, 42), c(1, 64)),
    n = c(3, 3)
  ) |>
  purrr::pluck("z") |>
  tibble::as_tibble(.name_repair = "unique") |>
  dplyr::mutate(x = seq(-42, 42, by = 42)) |>
  tidyr::pivot_longer(-c(x), names_to = "y", values_to = "density") |>
  dplyr::mutate(
    y = y |> stringr::str_extract("\\d") |> as.integer(),
    density = density / sum(density)
  )


pred_xg_shot_data |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(is_rush == 1) |>
  dplyr::mutate(
    x = coords_y * sign(coords_x),
    y = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(y > 0) |>
  # dplyr::filter(
  #   event_detail_1 %in%
  #     c(
  #       "Snap",
  #       "Slap",
  #       "Wrist",
  #       "Backhand",
  #       "Tip In",
  #       "Deflected"
  #     )
  # ) |>
  dplyr::group_by(rush_secs) |>
  tidyr::nest() |>
  dplyr::arrange(rush_secs) |>
  # head(1) |>
  dplyr::mutate(
    similarity =
      purrr::map_dbl(
        data,
        function(d) {
          MASS::kde2d(
            x = d$x,
            y = d$y,
            lims = c(c(-42, 42), c(1, 64)),
            n = c(3, 3)
          ) |>
            purrr::pluck("z") |>
            tibble::as_tibble(.name_repair = "unique") |>
            dplyr::mutate(x = seq(-42, 42, by = 42)) |>
            tidyr::pivot_longer(-c(x), names_to = "y", values_to = "density") |>
            dplyr::mutate(
              y = y |> stringr::str_extract("\\d") |> as.integer(),
              density_1 = density / sum(density)
            ) |>
            dplyr::left_join(non_rush_density_fenwick) |>
            dplyr::mutate(pt_dist = (density_1 - density)**2) |>
            dplyr::summarise(
              similarity = sum(pt_dist),
              similarity = sqrt(similarity),
              similarity = 1 / (1 + similarity)
            ) |>
            dplyr::pull(similarity)
        }
      )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = rush_secs, y = similarity)) +
  ggplot2::geom_point()
  View()




gganimate::animate(
  pred_xg_shot_data |>
    dplyr::filter(home_skater_strength_state == "5v5") |>
    # dplyr::filter(event_type == "BLOCK") |>
    # dplyr::filter(event_team_zone == "D") |>
    # dplyr::filter(is_rush == 0) |>
    dplyr::filter(event_type != "BLOCK") |>
    dplyr::filter(event_team_zone == "O") |>
    dplyr::mutate(
      x = coords_y * sign(coords_x),
      y = (coords_x * sign(coords_x) - 89) * -1
    ) |>
    dplyr::filter(y > 0) |>
    dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
    dplyr::filter(
      event_detail_1 %in%
        c(
          "Snap",
          "Slap",
          "Wrist",
          "Backhand",
          "Tip In",
          "Deflected"
        )
    ) |>
    dplyr::mutate(
      rush_secs = as.character(rush_secs),
      rush_secs = ifelse(is_rush == 0, "21+", rush_secs) |> factor(levels = c(as.character(0:20), "21+")),
      event_detail_1 =
        ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1)
    ) |>
    ggplot2::ggplot() +
    off_zone_markings(
      show_behind_net = T,
      show_neutral_zone = F,
      big_net = F,
      legend_position = "bottom"
    ) +
    # ggplot2::facet_wrap(ggplot2::vars(rush_secs)) +
    ggplot2::geom_density_2d_filled(ggplot2::aes(x = x, y = y), bins = 9) +
    ggplot2::scale_fill_manual(
      values =
        c(
          "#FFFFFF00",
          scales::viridis_pal(alpha = 0.7, option = "A")(n = 8)
        )
      # scales::viridis_pal(alpha = 0.7, option = "A")(n = 8) |>
      # stringr::str_replace_all("#000004B3", "#FFFFFF00")
    ) +
    ggplot2::labs(
      title = "5-on-5 Unblocked Shot Distribution {closest_state} Seconds Since Outside of Zone Event",
      subtitle = "2024-25 Season",
      caption = "Data via NHL"
    ) +
    ggplot2::theme(legend.position = "none") +
    gganimate::transition_states(rush_secs, transition_length = 0, state_length = 1),
  nframes = 22 * 10,
    # edge_viz |>
    # httr::content(type = "text/json", encoding = "UTF-8") |>
    # jsonlite::fromJSON() |>
    # tibble::tibble() |>
    # nrow(),
  fps = 10,
  height = 600,
  width = 750,
  units = "px",
  renderer = gganimate::gifski_renderer("heat_maps.gif")
)



blocks <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type == "BLOCK") |>
  dplyr::filter(event_team_zone == "D") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::tally() |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  dplyr::mutate(n = tidyr::replace_na(n, 0)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = n) |>
  tibble::column_to_rownames(var = "y")

blocks_smoothed <- blocks

for (row in seq(nrow(blocks))) {
  for (col in seq(length(blocks))) {
    blocks_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, blocks[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, blocks[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, blocks[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, blocks[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, blocks[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, blocks[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, blocks[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, blocks[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, blocks[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, blocks[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, blocks[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, blocks[row, col + 1] / 2) +
         ##
         blocks[row, col])
  }
}

blocks_smoothed <-
  blocks_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "blocks_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    blocks_smoothed_perc = blocks_smoothed / sum(blocks_smoothed)
  )
# dplyr::group_by(sign(x)) |>
# dplyr::summarise(sum(blocks_smoothed), sum(blocks_smoothed_perc))



## intercept notes
# when between posts (-2:2), shift both slopes intercept positive for both
# outside posts, intercept for far post is positive, near post is negative
# lined up with post,
# category 1: intercept = 2
# category 2: intercept = slope + 1
# category 3: intercept = 2*slope

## category notes
# outside left post:
# include coord if
# y >= left post function(x)
# y <= right post function(x)
# not outside right post
# not left of location
# not above location
# outside right post:
# opposite of outside of left post
# between posts (-2:2)
# include coord if
# y >= left post function(x)
# y <= right post function(x)
# not above location
# lined up with post


shot_blocker_density <-
  tidyr::expand_grid(
    x = -42:42,
    y = 1:64
  ) |>
  dplyr::mutate(
    angle_left_post = atan(y / abs(x + 3)) * (180 / pi),
    angle_right_post = atan(y / abs(x - 3)) * (180 / pi),
    slope_to_left_post = y / (x + 3.01 + ((angle_left_post) / 45)),
    slope_to_right_post = y / (x - 3.01 - ((angle_right_post) / 45)),
    # slope_to_left_post = y / (x + 3),
    # slope_to_right_post = y / (x - 3)1,
    # left_category = dplyr::case_when(
    #   abs(slope_to_left_post) <= 0.5 ~ 1,
    #   abs(slope_to_left_post) > 0.5 & abs(slope_to_left_post) < 2 ~ 2,
    #   abs(slope_to_left_post) >= 2 ~ 3
    # ),
    # right_category = dplyr::case_when(
    #   abs(slope_to_right_post) <= 0.5 ~ 1,
    #   abs(slope_to_right_post) > 0.5 & abs(slope_to_right_post) < 2 ~ 2,
    #   abs(slope_to_right_post) >= 2 ~ 3
    # ),
    left_intercept =
      ifelse(
        x <= -3,
        (-2 * ((90 - angle_left_post) / 90)) + (2 * slope_to_left_post * (angle_left_post / 90)),
        (2 * ((90 - angle_left_post) / 90)) + (2 * slope_to_left_post * (angle_left_post / 90))
      ) +
      (3 * slope_to_left_post),
    right_intercept =
      ifelse(
        x >= 3,
        (-2 * ((90 - angle_right_post) / 90)) - (2 * slope_to_right_post * (angle_right_post / 90)),
        (2 * ((90 - angle_right_post) / 90)) - (2 * slope_to_right_post * (angle_right_post / 90))
      ) -
      (3 * slope_to_right_post)

    #   dplyr::case_when(
    #     abs(x) >= 3 ~
    #       dplyr::case_when(
    #         left_category == 1 ~ 2 * sign(slope_to_left_post),
    #         left_category == 2 ~ slope_to_left_post + sign(slope_to_left_post),
    #         left_category == 3 ~ 2 * slope_to_left_post
    #       ),
    #     T ~ dplyr::case_when(
    #       left_category == 1 ~ 2,
    #       left_category == 2 ~ 1 + abs(slope_to_left_post),
    #       left_category == 3 ~ 2 * abs(slope_to_left_post)
    #     ) + y,
    #   ) + (3 * slope_to_left_post),
    # right_intercept =
    #   dplyr::case_when(
    #     abs(x) >= 3 ~
    #       dplyr::case_when(
    #         right_category == 1 ~ 2 * -sign(slope_to_right_post),
    #         right_category == 2 ~ -slope_to_right_post - sign(slope_to_right_post),
    #         right_category == 3 ~ 2 * -slope_to_right_post
    #       ),
    #     T ~ dplyr::case_when(
    #       left_category == 1 ~ 2,
    #       left_category == 2 ~ 1 + abs(slope_to_right_post),
    #       left_category == 3 ~ 2 * abs(slope_to_right_post)
    #     ) + y,
    #   ) - (3 * slope_to_right_post)
  ) |>
  # dplyr::group_by(
  #   sign(slope_to_left_post), sign(slope_to_right_post)
  # ) |>
  # dplyr::tally()
  # dplyr::filter(x == 3) |>
  # View()
  dplyr::mutate(
    shot_blocker_density =
      purrr::pmap_dbl(
        list(
          shot_x = x,
          shot_y = y,
          left_slope = slope_to_left_post,
          right_slope = slope_to_right_post,
          left_int = left_intercept,
          right_int = right_intercept,
          left_angle = angle_left_post,
          right_angle = angle_right_post
        ),
        function(shot_x, shot_y, left_slope, right_slope, left_int, right_int, left_angle, right_angle) {
          if (sign(left_slope) == -1 & sign(right_slope) == -1) {
            blocks_smoothed |>
              dplyr::filter(
                x >= min(shot_x, -5),
                x < 3 + ((right_angle) / 45),
                # x < 5,
                y <= max(shot_y, 2)
              ) |>
              dplyr::filter(
                y >= (x * left_slope) + left_int,
                y <= (x * right_slope) + right_int
              ) |>
              dplyr::pull(blocks_smoothed_perc) |>
              sum()
          } else if (sign(left_slope) == 1 & sign(right_slope) == 1) {
            blocks_smoothed |>
              dplyr::filter(
                x <= max(shot_x, 5),
                x > -3 - ((left_angle) / 45),
                # x > -5,
                y <= shot_y
              ) |>
              dplyr::filter(
                y <= (x * left_slope) + left_int,
                y >= (x * right_slope) + right_int
              ) |>
              dplyr::pull(blocks_smoothed_perc) |>
              sum()
          } else {
            blocks_smoothed |>
              dplyr::filter(
                y <= shot_y
                # abs(x) <= 5
              ) |>
              dplyr::filter(
                y <= (x * left_slope) + left_int,
                y <= (x * right_slope) + right_int
              ) |>
              dplyr::pull(blocks_smoothed_perc) |>
              sum()
          }

          # if (shot_x < -3) {
          #   blocks_smoothed |>
          #     dplyr::filter(
          #       x >= min(shot_x, -5),
          #       x < 3 + ((90 - right_angle) / 45),
          #       # x < 5,
          #       y <= shot_y
          #     ) |>
          #     dplyr::filter(
          #       y >= (x * left_slope) + left_intercept,
          #       y <= (x * right_slope) + right_intercept
          #     ) |>
          #     dplyr::pull(blocks_smoothed_perc) |>
          #     sum()
          # } else if (shot_x > 3) {
          #   blocks_smoothed |>
          #     dplyr::filter(
          #       x <= max(shot_x, 5),
          #       x > -3 - ((90 - left_angle) / 45),
          #       # x > -5,
          #       y <= shot_y
          #     ) |>
          #     dplyr::filter(
          #       y <= (x * left_slope) + left_intercept,
          #       y >= (x * right_slope) + right_intercept
          #     ) |>
          #     dplyr::pull(blocks_smoothed_perc) |>
          #     sum()
          # } else if (abs(shot_x) < 3) {
          #   blocks_smoothed |>
          #     dplyr::filter(
          #       y <= shot_y,
          #       abs(x) <= 5
          #     ) |>
          #     dplyr::filter(
          #       y <= (x * left_slope) + left_intercept,
          #       y <= (x * right_slope) + right_intercept
          #     ) |>
          #     dplyr::pull(blocks_smoothed_perc) |>
          #     sum()
          # } else if (shot_x == -3) {
          #   blocks_smoothed |>
          #     dplyr::filter(
          #       y <= shot_y,
          #       abs(x) <= 5
          #     ) |>
          #     dplyr::filter(
          #       y <= (x * right_slope) + right_intercept,
          #       y <= (x * (shot_y / 2)) + (5 * (shot_y / 2))
          #     ) |>
          #     dplyr::pull(blocks_smoothed_perc) |>
          #     sum()
          # } else if (shot_x == 3) {
          #   blocks_smoothed |>
          #     dplyr::filter(
          #       y <= shot_y,
          #       abs(x) <= 5
          #     ) |>
          #     dplyr::filter(
          #       y <= (x * left_slope) + left_intercept,
          #       y <= (x * (shot_y / -2)) + (5 * (shot_y / 2))
          #     ) |>
          #     dplyr::pull(blocks_smoothed_perc) |>
          #     sum()
          # }
        }
      )
  ) |>
  dplyr::select(x, y, shot_blocker_density) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = shot_blocker_density) |>
  tibble::column_to_rownames(var = "y")


shot_blocker_density_smoothed <- shot_blocker_density

for (row in seq(nrow(shot_blocker_density))) {
  for (col in seq(length(shot_blocker_density))) {
    shot_blocker_density_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, shot_blocker_density[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, shot_blocker_density[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, shot_blocker_density[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, shot_blocker_density[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, shot_blocker_density[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, shot_blocker_density[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, shot_blocker_density[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, shot_blocker_density[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, shot_blocker_density[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, shot_blocker_density[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, shot_blocker_density[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, shot_blocker_density[row, col + 1] / 2) +
         ##
         shot_blocker_density[row, col]) /
      (
        6 +
          # 2 up or down
          ifelse(row >= 3 & row <= 62, 1, 0) +
          # 2 left or right
          ifelse(col >= 3 & col <= 83, 1, 0) +
          # 1 up or down
          ifelse(row >=2 & row <= 63, 1, 0) +
          # 1 up or down
          ifelse(col >= 2 & col <= 84, 1, 0) +
          # diagonals
          ifelse(row >= 2 & col >= 2, 1, 0) +
          ifelse(row >= 2 & col <= 84, 1, 0) +
          ifelse(row <= 63 & col >= 2, 1, 0) +
          ifelse(row <= 63 & col <= 84, 1, 0)
      )
  }
}

shot_blocker_density_smoothed <-
  shot_blocker_density_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "shot_blocker_density_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y)
  )

shot_blocker_density_smoothed |>
# shot_blocker_density |>
  # tibble::rownames_to_column(var = "y") |>
  # tidyr::pivot_longer(-c(y), names_to = "x", values_to = "shot_blocker_density_smoothed") |>
  # dplyr::mutate(
  #   x = as.integer(x),
  #   y = as.integer(y)
  # ) |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T
  ) +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = shot_blocker_density_smoothed)) +
  ggplot2::scale_fill_viridis_c(option = "A", alpha = 0.7) +
  ggplot2::labs(
    title = "Estimated 5-on-5 Shot Blocker Density by Attempt Location",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )





atts <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(event_team_zone == "O") |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(event_detail_2 != "Defensive Deflection") |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::group_by(x = coords_y, y = coords_x) |>
  dplyr::summarise(
    att = dplyr::n()
  ) |>
  dplyr::full_join(
    tidyr::expand_grid(
      x = -42:42,
      y = 1:64
    )
  ) |>
  dplyr::mutate(att = tidyr::replace_na(att, 0)) |>
  dplyr::arrange(x, dplyr::desc(y)) |>
  tidyr::pivot_wider(id_cols = y, names_from = x, values_from = att) |>
  tibble::column_to_rownames(var = "y")

atts_smoothed <- atts

for (row in seq(nrow(atts))) {
  for (col in seq(length(atts))) {
    atts_smoothed[row, col] <-
      # 2 above
      (ifelse(row < 3, 0, atts[row - 2, col] / 3) +
         ## 2 below
         ifelse(row > 62, 0, atts[row + 2, col] / 3) +
         ## 2 left
         ifelse(col < 3, 0, atts[row, col - 2] / 3) +
         ## 2 right
         ifelse(col > 83, 0, atts[row, col + 2] / 3) +
         ## diagonal up left
         ifelse(col < 2 | row < 2, 0, atts[row - 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal up right
         ifelse(col > 84 | row < 2, 0, atts[row - 1, col + 1] / (sqrt(2) + 1)) +
         ## diagonal down left
         ifelse(col < 2 | row > 63, 0, atts[row + 1, col - 1] / (sqrt(2) + 1)) +
         ## diagonal down right
         ifelse(col > 84 | row > 63, 0, atts[row + 1, col + 1] / (sqrt(2) + 1)) +
         ## 1 above
         ifelse(row < 2, 0, atts[row - 1, col] / 2) +
         ## 1 below
         ifelse(row > 63, 0, atts[row + 1, col] / 2) +
         ## 1 left
         ifelse(col < 2, 0, atts[row, col - 1] / 2) +
         ## 1 right
         ifelse(col > 84, 0, atts[row, col + 1] / 2) +
         ##
         atts[row, col]) /
      (
        6 +
          # 2 up or down
          ifelse(row >= 3 & row <= 62, 1, 0) +
          # 2 left or right
          ifelse(col >= 3 & col <= 83, 1, 0) +
          # 1 up or down
          ifelse(row >=2 & row <= 63, 1, 0) +
          # 1 up or down
          ifelse(col >= 2 & col <= 84, 1, 0) +
          # diagonals
          ifelse(row >= 2 & col >= 2, 1, 0) +
          ifelse(row >= 2 & col <= 84, 1, 0) +
          ifelse(row <= 63 & col >= 2, 1, 0) +
          ifelse(row <= 63 & col <= 84, 1, 0)
      )
  }
}

atts_smoothed <-
  atts_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "atts_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    atts_smoothed_perc = atts_smoothed / sum(atts_smoothed)
  )


atts_smoothed |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T
  ) +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = atts_smoothed_perc)) +
  ggplot2::scale_fill_viridis_c(option = "A", alpha = 0.7) +
  ggplot2::labs(
    title = "Estimated 5-on-5 Attempt Density by Attempt Location",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )

wrist_att_smoothed <-
  wrist_att_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "atts_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    atts_smoothed_perc = atts_smoothed / sum(atts_smoothed)
  )

shot_blocker_density_smoothed |>
  dplyr::left_join(wrist_att_smoothed) |>
  dplyr::summarise(weighted.mean(shot_blocker_density_smoothed, atts_smoothed_perc))


shot_blocker_density_smoothed |>
  dplyr::mutate(
    event_detail_1 = "Wrist",
    xthru = 1 - (shot_blocker_density_smoothed / (0.0360 / 0.344))
  )

slap_att_smoothed <-
  slap_att_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "atts_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    atts_smoothed_perc = atts_smoothed / sum(atts_smoothed)
  )


shot_blocker_density_smoothed |>
  dplyr::left_join(slap_att_smoothed) |>
  dplyr::summarise(weighted.mean(shot_blocker_density_smoothed, atts_smoothed_perc))


shot_blocker_density_smoothed |>
  dplyr::mutate(
    event_detail_1 = "Slap",
    xthru = 1 - (shot_blocker_density_smoothed / (0.0539 / 0.323))
  )

snap_att_smoothed <-
  snap_att_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "atts_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    atts_smoothed_perc = atts_smoothed / sum(atts_smoothed)
  )


shot_blocker_density_smoothed |>
  dplyr::left_join(snap_att_smoothed) |>
  dplyr::summarise(weighted.mean(shot_blocker_density_smoothed, atts_smoothed_perc))


shot_blocker_density_smoothed |>
  dplyr::mutate(
    event_detail_1 = "Snap",
    xthru = 1 - (shot_blocker_density_smoothed / (0.0361 / 0.302))
  )

back_att_smoothed <-
  back_att_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "atts_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    atts_smoothed_perc = atts_smoothed / sum(atts_smoothed)
  )

shot_blocker_density_smoothed |>
  dplyr::left_join(back_att_smoothed) |>
  dplyr::summarise(weighted.mean(shot_blocker_density_smoothed, atts_smoothed_perc))


shot_blocker_density_smoothed |>
  dplyr::mutate(
    event_detail_1 = "Backhand",
    xthru = 1 - (shot_blocker_density_smoothed / (0.0140 / 0.148))
  )

tip_att_smoothed <-
  tip_att_smoothed |>
  tibble::rownames_to_column(var = "y") |>
  tidyr::pivot_longer(-c(y), names_to = "x", values_to = "atts_smoothed") |>
  dplyr::mutate(
    x = as.integer(x),
    y = as.integer(y),
    atts_smoothed_perc = atts_smoothed / sum(atts_smoothed)
  )

shot_blocker_density_smoothed |>
  dplyr::left_join(tip_att_smoothed) |>
  dplyr::summarise(weighted.mean(shot_blocker_density_smoothed, atts_smoothed_perc))


shot_blocker_density_smoothed |>
  dplyr::mutate(
    event_detail_1 = "Tip In/Deflection",
    xthru = 1 - (shot_blocker_density_smoothed / (0.0164 / 0.0928))
  )

expected_through_5v5 <-
  shot_blocker_density_smoothed |>
  dplyr::mutate(
    event_detail_1 = "Backhand",
    xthru = 1 - (shot_blocker_density_smoothed / (0.0140 / 0.148))
  ) |>
  dplyr::bind_rows(
    shot_blocker_density_smoothed |>
      dplyr::mutate(
        event_detail_1 = "Tip In/Deflection",
        xthru = 1 - (shot_blocker_density_smoothed / (0.0164 / 0.0928))
      )
  ) |>
  dplyr::bind_rows(
    shot_blocker_density_smoothed |>
      dplyr::mutate(
        event_detail_1 = "Wrist",
        xthru = 1 - (shot_blocker_density_smoothed / (0.0360 / 0.344))
      )
  ) |>
  dplyr::bind_rows(
    shot_blocker_density_smoothed |>
      dplyr::mutate(
        event_detail_1 = "Snap",
        xthru = 1 - (shot_blocker_density_smoothed / (0.0361 / 0.302))
      )
  ) |>
  dplyr::bind_rows(
    shot_blocker_density_smoothed |>
      dplyr::mutate(
        event_detail_1 = "Slap",
        xthru = 1 - (shot_blocker_density_smoothed / (0.0539 / 0.323))
      )
  )

expected_through_5v5 |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = xthru)) +
  ggplot2::scale_fill_viridis_c(option = "A", alpha = 0.7) +
  ggplot2::labs(
    title = "Estimated 5-on-5 Expected Through (Avoid Shot Blocker) Values by Attempt Location",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )

blocked_shot_est_coords <-
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
          block_x = x,
          block_y = y,
          type = event_detail_1,
          left_slope = slope_to_left_post,
          right_slope = slope_to_right_post,
          left_int = left_intercept,
          right_int = right_intercept
        ),
        function(block_x, block_y, type, left_slope, right_slope, left_int, right_int) {
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
                    dist = 1 / (sqrt((block_x - x)**2 + (block_y - y)**2) + 1),
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01),
                    weight = dist**1.1 * atts_smoothed_non_zero**2
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                est_x = weighted.mean(x, weight),
                est_y = weighted.mean(y, weight)
              )
          } else if (type == "Slap") {
            coords |>
              dplyr::left_join(
                slap_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    dist = 1 / (sqrt((block_x - x)**2 + (block_y - y)**2) + 1),
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01),
                    weight = dist**1.1 * atts_smoothed_non_zero**2
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                est_x = weighted.mean(x, weight),
                est_y = weighted.mean(y, weight)
              )
          } else if (type == "Snap") {
            coords |>
              dplyr::left_join(
                snap_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    dist = 1 / (sqrt((block_x - x)**2 + (block_y - y)**2) + 1),
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01),
                    weight = dist**1.1 * atts_smoothed_non_zero**2
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                est_x = weighted.mean(x, weight),
                est_y = weighted.mean(y, weight)
              )
          } else if (type == "Tip In/Deflection") {
            coords |>
              dplyr::left_join(
                tip_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    dist = 1 / (sqrt((block_x - x)**2 + (block_y - y)**2) + 1),
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01),
                    weight = dist**1.1 * atts_smoothed_non_zero**2
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                est_x = weighted.mean(x, weight),
                est_y = weighted.mean(y, weight)
              )
          } else if (type == "Wrist") {
            coords |>
              dplyr::left_join(
                wrist_att_smoothed |>
                  dplyr::transmute(
                    x,
                    y,
                    dist = 1 / (sqrt((block_x - x)**2 + (block_y - y)**2) + 1),
                    atts_smoothed_non_zero = (atts_smoothed + 0.01) / sum(atts_smoothed + 0.01),
                    weight = dist**1.1 * atts_smoothed_non_zero**2
                  ),
                by = c("x", "y")
              ) |>
              dplyr::summarise(
                est_x = weighted.mean(x, weight),
                est_y = weighted.mean(y, weight)
              )
          }
        }
      )
  ) |>
  dplyr::select(x, y, event_detail_1, expected_values) |>
  tidyr::unnest(expected_values)

x_thru_data <-
  pred_xg_shot_data |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(
    (event_type != "BLOCK" & event_team_zone == "O") |
      (event_type == "BLOCK" & event_team_zone == "D")) |>
  dplyr::filter() |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::mutate(
    coords_y = coords_y * sign(coords_x),
    coords_x = (coords_x * sign(coords_x) - 89) * -1,
    x = coords_y,
    y = coords_x
  ) |>
  dplyr::filter(coords_x > 0) |>
  dplyr::mutate(
    total_att = dplyr::n(),
    avg_per_shooter = total_att / length(unique(event_player_1))
  ) |>
  dplyr::group_by(event_player_1) |>
  dplyr::mutate(
    event_detail_1 =
      ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflection", event_detail_1),
    shooter_weight =
      ifelse(
        dplyr::n() > avg_per_shooter,
        avg_per_shooter / dplyr::n(),
        dplyr::n() / avg_per_shooter
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(event_detail_1 %in% c("Snap", "Slap", "Wrist", "Backhand", "Tip In/Deflection")) |>
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
  dplyr::left_join(shot_blocker_density_smoothed) |>
  dplyr::mutate(
    is_snap = as.integer(event_detail_1 == "Snap"),
    is_slap = as.integer(event_detail_1 == "Slap"),
    is_backhand = as.integer(event_detail_1 == "Backhand"),
    is_tip = as.integer(event_detail_1 == "Tip In/Deflection"),
    is_thru = as.integer(event_type != "BLOCK"),
    weight = as.integer(shooter_weight / min(shooter_weight))
  ) |>
  dplyr::select(
    is_thru, event_type, coords_y, coords_x, x, y, is_snap:is_tip,
    shot_blocker_density_smoothed, weight
  )

x_thru_data

set.seed(1138)
x_thru_cv <-
  glmnet::cv.glmnet(
    model.matrix(
      is_thru ~
        is_snap +
        is_slap +
        is_backhand +
        is_tip +
        shot_blocker_density_smoothed,
      x_thru_data[seq(nrow(x_thru_data)) %% 5 != 1, ]
    )[, -1],
    x_thru_data[seq(nrow(x_thru_data)) %% 5 != 1, ]$is_thru,
    family = "binomial",
    alpha = 1,
    weights = x_thru_data[seq(nrow(x_thru_data)) %% 5 != 1, ]$weight
  )

x_thru_model_min <-
  glmnet::glmnet(
    model.matrix(
      is_thru ~
        is_snap +
        is_slap +
        is_backhand +
        is_tip +
        shot_blocker_density_smoothed,
      x_thru_data[seq(nrow(x_thru_data)) %% 5 != 1, ]
    )[, -1],
    x_thru_data[seq(nrow(x_thru_data)) %% 5 != 1, ]$is_thru,
    family = "binomial",
    alpha = 1,
    lambda = x_thru_cv$lambda.min,
    weights = x_thru_data[seq(nrow(x_thru_data)) %% 5 != 1, ]$weight
  )

x_thru_model_min |>
  coef()

expected_through_5v5 <-
  tidyr::expand_grid(
    event_detail_1 = "Wrist",
    x = -42:42,
    y = 1:64
  ) |>
  dplyr::mutate(
    x_thru =
      predict(
        x_thru_model_min,
        model.matrix(
          is_thru ~
            is_snap +
            is_slap +
            is_backhand +
            is_tip +
            shot_blocker_density_smoothed,
          tidyr::expand_grid(
            is_thru = 0,
            x = -42:42,
            y = 1:64
          ) |>
            dplyr::mutate(
              is_snap = 0,
              is_slap = 0,
              is_backhand = 0,
              is_tip = 0
            ) |>
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
        x_thru =
          predict(
            x_thru_model_min,
            model.matrix(
              is_thru ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_thru = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 1,
                  is_slap = 0,
                  is_backhand = 0,
                  is_tip = 0
                ) |>
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
        x_thru =
          predict(
            x_thru_model_min,
            model.matrix(
              is_thru ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_thru = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 1,
                  is_backhand = 0,
                  is_tip = 0
                ) |>
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
        x_thru =
          predict(
            x_thru_model_min,
            model.matrix(
              is_thru ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_thru = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 0,
                  is_backhand = 1,
                  is_tip = 0
                ) |>
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
        x_thru =
          predict(
            x_thru_model_min,
            model.matrix(
              is_thru ~
                is_snap +
                is_slap +
                is_backhand +
                is_tip +
                shot_blocker_density_smoothed,
              tidyr::expand_grid(
                is_thru = 0,
                x = -42:42,
                y = 1:64
              ) |>
                dplyr::mutate(
                  is_snap = 0,
                  is_slap = 0,
                  is_backhand = 0,
                  is_tip = 1
                ) |>
                dplyr::left_join(shot_blocker_density_smoothed)
            )[, -1],
            type = "response"
          ) |>
          as.double()
      )
  )

expected_through_5v5 |>
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    legend_position = "bottom"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(event_detail_1)) +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = x_thru)) +
  ggplot2::scale_fill_viridis_c(option = "A", alpha = 0.7) +
  ggplot2::labs(
    title = "Estimated 5-on-5 Expected Through Values by Attempt Location",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )








################################################################################
##### V2
################################################################################

shot_type_characteristic_summary <-
  pred_xg_shot_data |>
  dplyr::bind_rows(pred_xg_shot_data_25) |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(
    (event_type == "BLOCK" & event_team_zone == "D") |
      (event_type != "BLOCK" & event_team_zone == "O"),
    shot_y > 0,
    event_detail_1 != "Own Goal",
    event_detail_1 != "Awarded"
  ) |>
  dplyr::filter(position_category %in% c("F", "D")) |>
  dplyr::mutate(
    position_category =
      ifelse(event_detail_1 %in% c("Wrist", "Snap", "Slap"), position_category, "All"),
    event_detail_1 =
      ifelse(event_detail_1 %in% c("Backhand", "Poke", "Bat", "Between Legs"), "Backhand/Poke/Bat/Between Legs", event_detail_1),
    event_detail_1 =
      ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1),
    event_detail_1 =
      ifelse(event_detail_1 %in% c("Wrist", "Snap", "Wrap Around", "Cradle"), "Wrist/Snap/Wrap Around/Cradle", event_detail_1)
  ) |>
  dplyr::filter(position_category != "D") |>
  # dplyr::mutate(
  #   event_detail_1 =
  #     ifelse(
  #       event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs"),
  #       "Cradle/Wrap Around/Between Legs",
  #       event_detail_1
  #     ),
  #   event_detail_1 =
  #     ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1),
  #   event_detail_1 =
  #     ifelse(event_detail_1 %in% c("Poke", "Bat"), "Bat/Poke", event_detail_1)
  # ) |>
  dplyr::group_by(event_detail_1, position_category) |>
  dplyr::left_join(
    pred_xg_shot_data |>
      dplyr::bind_rows(pred_xg_shot_data_25) |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(
        (event_type == "BLOCK" & event_team_zone == "D") |
          (event_type != "BLOCK" & event_team_zone == "O"),
        shot_y > 0,
        event_detail_1 != "Own Goal",
        event_detail_1 != "Awarded"
      ) |>
      dplyr::filter(position_category %in% c("F", "D")) |>
      dplyr::mutate(
        position_category =
          ifelse(event_detail_1 %in% c("Wrist", "Snap", "Slap"), position_category, "All"),
        event_detail_1 =
          ifelse(event_detail_1 %in% c("Backhand", "Poke", "Bat", "Between Legs"), "Backhand/Poke/Bat/Between Legs", event_detail_1),
        event_detail_1 =
          ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1),
        event_detail_1 =
          ifelse(event_detail_1 %in% c("Wrist", "Snap", "Wrap Around", "Cradle"), "Wrist/Snap/Wrap Around/Cradle", event_detail_1)
      ) |>
      dplyr::filter(position_category != "D") |>
      dplyr::group_by(event_detail_1, position_category) |>
      dplyr::summarise(
        n_f = sum(event_type != "BLOCK"),
        .groups = "drop"
      ) |>
      dplyr::group_by(event_detail_1, position_category) |>
      # dplyr::mutate(group = event_detatil_1)
      dplyr::summarise(
        group = "{event_detail_1}_{position_category}" |>
          # ifelse(
          #   any(n_f < 100000),
          #   "{event_detail_1}_All",
          #   "{event_detail_1}_{position_category}"
          # ) |>
          glue::glue()
      )
  ) |>
  dplyr::group_by(group) |>
  dplyr::summarise(
    n = dplyr::n(),
    n_f = sum(event_type != "BLOCK"),
    block_perc = sum(event_type == "BLOCK") / n,
    fsh_perc = sum(event_type == "GOAL") / (sum(event_type != "BLOCK")),
    dist_mean = mean(sqrt(shot_x**2 + shot_y**2 + ifelse(event_type == "BLOCK", NA, 0)), na.rm = T),
    dist_sd = sd(sqrt(shot_x**2 + shot_y**2 + ifelse(event_type == "BLOCK", NA, 0)), na.rm = T),
    angle_mean = mean(atan(shot_y / abs(shot_x)) * (180 / pi + ifelse(event_type == "BLOCK", NA, 0)), na.rm = T),
    angle_sd = sd(atan(shot_y / abs(shot_x)) * (180 / pi + ifelse(event_type == "BLOCK", NA, 0)), na.rm = T),
    .groups = "drop"
  )

shot_type_characteristic_summary |>
  dplyr::mutate(
    dplyr::across(
      block_perc:angle_sd,
      .fns = function(x) {
        (x - mean(x)) / sd(x)
      }
      # .names = "{.col}_std"
    ),
    similarity =
      shot_type_characteristic_summary |>
      dplyr::mutate(
        dplyr::across(
          block_perc:angle_sd,
          .fns = function(x) {
            (x - mean(x)) / sd(x)
          }
          # .names = "{.col}_std"
        )
      ) |>
      list(),
    similarity =
      purrr::pmap(
        list(
          df = similarity,
          blk = block_perc,
          fsh = fsh_perc,
          dist_m = dist_mean,
          dist_s = dist_sd,
          angle_m = angle_mean,
          angle_s = angle_sd
        ),
        function(df, blk, fsh, dist_m, dist_s, angle_m, angle_s) {
          df |>
            dplyr::transmute(
              group_2 = group,
              dist =
                sqrt(
                  (block_perc - blk)**2 + (fsh_perc - fsh)**2 + (dist_mean - dist_m)**2 +
                    (dist_sd - dist_s)**2 + (angle_mean - angle_m)**2 + (angle_sd - angle_s)**2
                )
            )
        }
      )
  ) |>
  tidyr::unnest(similarity) |>
  dplyr::mutate(
    similarity = 1 / (1 + dist),
    similarity = (similarity - min(similarity)) / (max(similarity) - min(similarity)),
    similarity = ifelse(similarity == 1, NA, similarity),
    # similarity = ifelse(group >= group_2, NA, similarity)
  ) |>
  # View()
  # dplyr::group_by(group_2) |>
  # dplyr::filter(group %in% c("Backhand_All", "Tip In_All", "Deflected_All", "Slap_All", "Snap_All", "Wrist_All")) |>
  # dplyr::filter(similarity == max(similarity, na.rm = T)) |>
  ggplot2::ggplot(ggplot2::aes(x = group, y = group_2, fill = similarity)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_viridis_c(option = "A", limits = c(0, 1))



pred_xg_shot_data |>
  dplyr::bind_rows(pred_xg_shot_data) |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  # dplyr::filter(event_type == "BLOCK") |>
  # dplyr::filter(event_team_zone == "D") |>
  dplyr::filter(position_category != "G") |>
  # dplyr::filter(is_rush == 0) |>
  dplyr::filter(event_type != "BLOCK") |>
  dplyr::filter(shot_zone == "O") |>
  # dplyr::mutate(
  #   event_detail_1 =
  #     dplyr::case_when(
  #       event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
  #       event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
  #       T ~ event_detail_1
  #     )
  #   # x = coords_y * sign(coords_x),
  #   # y = (coords_x * sign(coords_x) - 89) * -1
  # ) |>
  dplyr::filter(shot_y > 0) |>
  # dplyr::filter(is_off_faceoff == 1) |>
  # dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::mutate(
    event_detail_1 =
      ifelse(event_detail_1 %in% c("Cradle", "Wrap Around"), "Cradle/Wrap Around", event_detail_1)
  ) |>
  dplyr::group_by(position_category, event_detail_1) |>
  dplyr::mutate(
    count = "n = {formatC(dplyr::n(), digits = 0, big.mark = ',', format = 'f')}" |> glue::glue()
  ) |>
  # dplyr::filter(
  #   event_detail_1 %in%
  #     c(
  #       "Snap",
  #       "Slap",
  #       "Wrist",
  #       "Backhand",
  #       "Tip In",
  #       "Deflected"
  #     )
  # ) |>
  # dplyr::mutate(
  # rush_secs = as.character(rush_secs),
  # rush_secs = ifelse(is_rush == 0, "21+", rush_secs) |> factor(levels = c(as.character(0:20), "21+")),
  # event_detail_1 =
  #   ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1)
  # ) |>
  # dplyr::filter(event_detail_1 == "Tip") |>
  # dplyr::group_by(event_detail_1, position_category) |>
  # dplyr::group_by(rush_secs) |>
  # dplyr::tally()
  # dplyr::mutate(
  #   perc = n / sum(n),
  #   cum_perc = cumsum(perc)
  # )
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = T,
    show_neutral_zone = F,
    big_net = F,
    legend_position = "none"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(is_reached_goalie_followup), nrow = 2) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = shot_x, y = shot_y), contour_var = "ndensity", bins = 10) +
  ggplot2::scale_fill_manual(
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 9)
      )
    # scales::viridis_pal(alpha = 0.7, option = "A")(n = 8) |>
    # stringr::str_replace_all("#000004B3", "#FFFFFF00")
  ) +
  ggplot2::labs(
    title = "5-on-5 Unblocked Shot Distribution By Shooter Position",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )
# ggplot2::theme(legend.position = "none")

readr::read_csv("../scraper_testing/clean_files/pbp_2025020001.csv") |>
  dplyr::mutate(
    fac_id = cumsum(event_type == "FAC") |> tidyr::replace_na(0)
  ) |>
  dplyr::filter(
    stringr::str_detect(event_description, "GOALIE STOPPED") |
      event_type %in% c("SHOT", "MISS", "BLOCK", "GOAL")
  ) |>
  dplyr::group_by(fac_id) |>
  dplyr::mutate(
    reached_goalie =
      event_type %in% c("SHOT", "GOAL") |
      (event_type == "MISS" & event_detail_2 != "Short"),
    is_frozen =
      (reached_goalie &
         dplyr::lead(stringr::str_detect(event_description, "GOALIE STOPPED")) &
         event_team != dplyr::lead(event_team) &
         (dplyr::lead(game_seconds) - game_seconds) <= 3) |>
      tidyr::replace_na(F)
  ) |>
  # dplyr::filter(event_type == "STOP" | dplyr::lead(event_type) == "STOP") |>
  View()



pred_xg_shot_data |>
  # dplyr::filter(event_team_strength == "PP") |>
  dplyr::filter(home_skater_strength_state == "5v5") |>
  dplyr::filter(is_frozen == 1) |>
  dplyr::filter(event_type != "BLOCK") |>
  # dplyr::filter(event_type == "BLOCK") |>
  # dplyr::filter(event_detail_3 == "Flub") |>
  dplyr::filter(shot_zone == "O") |>
  dplyr::filter(shot_y > 0) |>
  dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
  dplyr::filter(position_category != "G") |>
  # dplyr::filter(is_rush == 0) |>
  # dplyr::filter(shot_zone == "O") |>
  dplyr::mutate(
    event_detail_1 =
      dplyr::case_when(
        event_detail_1 %in% c("Wrist", "Snap") ~ "Wrist/Snap",
        event_detail_1 %in% c("Tip In", "Deflected") ~ "Tip In/Deflection",
        event_detail_1 %in% c("Backhand", "Between Legs", "Poke", "Bat", "Cradle", "Wrap Around") ~ "Backhand/Other",
        T ~ event_detail_1
      )
    # x = coords_y * sign(coords_x),
    # y = (coords_x * sign(coords_x) - 89) * -1
  ) |>
  dplyr::group_by(event_detail_1, position_category) |>
  # dplyr::summarise(perc_flub = sum(event_detail_3 == "Flub") / dplyr::n())
  dplyr::group_by(position_category, event_detail_1) |>
  dplyr::mutate(count = "n = {formatC(dplyr::n(), digits = 0, big.mark = ',', format = 'f')}" |> glue::glue()) |>
  # dplyr::filter(
  #   event_detail_1 %in%
  #     c(
  #       "Snap",
  #       "Slap",
  #       "Wrist",
  #       "Backhand",
  #       "Tip In",
  #       "Deflected"
  #     )
  # ) |>
  # dplyr::mutate(
  # rush_secs = as.character(rush_secs),
  # rush_secs = ifelse(is_rush == 0, "21+", rush_secs) |> factor(levels = c(as.character(0:20), "21+")),
  # event_detail_1 =
  #   ifelse(event_detail_1 %in% c("Tip In", "Deflected"), "Tip In/Deflected", event_detail_1)
  # ) |>
  # dplyr::filter(event_detail_1 == "Tip") |>
  # dplyr::group_by(event_detail_1, position_category) |>
  # dplyr::group_by(rush_secs) |>
  # dplyr::tally()
  # dplyr::mutate(
  #   perc = n / sum(n),
  #   cum_perc = cumsum(perc)
  # )
  ggplot2::ggplot() +
  off_zone_markings(
    show_behind_net = F,
    show_neutral_zone = F,
    big_net = F,
    legend_position = "none"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(position_category, event_detail_1, count), nrow = 2) +
  ggplot2::geom_density_2d_filled(ggplot2::aes(x = shot_x, y = shot_y), contour_var = "ndensity", bins = 10) +
  ggplot2::scale_fill_manual(
    values =
      c(
        "#FFFFFF00",
        scales::viridis_pal(alpha = 0.7, option = "A")(n = 9)
      )
    # scales::viridis_pal(alpha = 0.7, option = "A")(n = 8) |>
    # stringr::str_replace_all("#000004B3", "#FFFFFF00")
  ) +
  ggplot2::labs(
    title = "5-on-5 Blocked Shot Distribution By Shooter Position",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  )

shot_blocker_density_est <-
  MASS::kde2d(
    x =
      pred_xg_shot_data |>
      dplyr::mutate(
        event_detail_1 =
          dplyr::case_when(
            event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
            event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
            T ~ event_detail_1
          )
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(event_type == "BLOCK") |>
      dplyr::filter(shot_zone == "O") |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(shot_y > 0) |>
      dplyr::filter(position_category == "F" | event_detail_1 == c("Backhand")) |>
      dplyr::pull(shot_x),
    y =
      pred_xg_shot_data |>
      dplyr::mutate(
        event_detail_1 =
          dplyr::case_when(
            event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
            event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
            T ~ event_detail_1
          )
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(event_type == "BLOCK") |>
      dplyr::filter(shot_zone == "O") |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(shot_y > 0) |>
      dplyr::filter(position_category == "F" | event_detail_1 == c("Backhand")) |>
      dplyr::pull(shot_y),
    lims = c(c(-42, 42), c(0, 64)),
    n = c(85, 65)
  ) |>
  purrr::pluck("z") |>
  tibble::as_tibble(.name_repair = "unique") |>
  dplyr::mutate(x = seq(-42, 42)) |>
  tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
  dplyr::mutate(
    type = "Low (Implied)",
    y = y |> stringr::str_extract("\\d+") |> as.integer() |> magrittr::subtract(1),
    density = z / sum(z)
  ) |>
  dplyr::bind_rows(
    MASS::kde2d(
      x =
        pred_xg_shot_data |>
        dplyr::mutate(
          event_detail_1 =
            dplyr::case_when(
              event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
              event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
              T ~ event_detail_1
            )
        ) |>
        dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
        dplyr::filter(event_type == "BLOCK") |>
        dplyr::filter(shot_zone == "O") |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::filter(shot_y > 0) |>
        dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
        dplyr::pull(shot_x),
      y =
        pred_xg_shot_data |>
        dplyr::mutate(
          event_detail_1 =
            dplyr::case_when(
              event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
              event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
              T ~ event_detail_1
            )
        ) |>
        dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
        dplyr::filter(event_type == "BLOCK") |>
        dplyr::filter(shot_zone == "O") |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::filter(shot_y > 0) |>
        dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
        dplyr::pull(shot_y),
      lims = c(c(-42, 42), c(0, 64)),
      n = c(85, 65)
    ) |>
      purrr::pluck("z") |>
      tibble::as_tibble(.name_repair = "unique") |>
      dplyr::mutate(x = seq(-42, 42)) |>
      tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
      dplyr::mutate(
        type = "Point (Implied)",
        y = y |> stringr::str_extract("\\d+") |> as.integer() |> magrittr::subtract(1),
        density = z / sum(z)
      )
  )

shot_blocker_density_est |>
  dplyr::filter(
    !(
      (abs(x) <= 4 & y <= 4) |
        (abs(x) <= 3 & y == 5)
    )
  ) |>
  dplyr::group_by(type) |>
  dplyr::mutate(density = z / sum(z)) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::facet_wrap(ggplot2::vars(type), ncol = 2) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = density), alpha = 0.7) +
  ggplot2::scale_fill_viridis_c(option = "A")


shot_att_density_est <-
  MASS::kde2d(
    x =
      pred_xg_shot_data |>
      dplyr::mutate(
        event_detail_1 =
          dplyr::case_when(
            event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
            event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
            T ~ event_detail_1
          )
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(event_type != "BLOCK") |>
      dplyr::filter(shot_zone == "O") |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(shot_y > 0) |>
      dplyr::filter((position_category == "F" | event_detail_1 == c("Backhand")) & event_detail_1 != "Tip In/Deflection") |>
      dplyr::pull(shot_x),
    y =
      pred_xg_shot_data |>
      dplyr::mutate(
        event_detail_1 =
          dplyr::case_when(
            event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
            event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
            T ~ event_detail_1
          )
      ) |>
      dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
      dplyr::filter(event_type != "BLOCK") |>
      dplyr::filter(shot_zone == "O") |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::filter(shot_y > 0) |>
      dplyr::filter((position_category == "F" | event_detail_1 == c("Backhand")) & event_detail_1 != "Tip In/Deflection") |>
      dplyr::pull(shot_y),
    lims = c(c(-42, 42), c(1, 64)),
    n = c(85, 64)
  ) |>
  purrr::pluck("z") |>
  tibble::as_tibble(.name_repair = "unique") |>
  dplyr::mutate(x = seq(-42, 42)) |>
  tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
  dplyr::mutate(
    type = "Low (Implied)",
    y = y |> stringr::str_extract("\\d+") |> as.integer(),
    density = z / sum(z)
  ) |>
  dplyr::bind_rows(
    MASS::kde2d(
      x =
        pred_xg_shot_data |>
        dplyr::mutate(
          event_detail_1 =
            dplyr::case_when(
              event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
              event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
              T ~ event_detail_1
            )
        ) |>
        dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
        dplyr::filter(event_type != "BLOCK") |>
        dplyr::filter(shot_zone == "O") |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::filter(shot_y > 0) |>
        dplyr::filter(position_category == "D" & !event_detail_1 %in% c("Backhand", "Tip In/Deflection")) |>
        dplyr::pull(shot_x),
      y =
        pred_xg_shot_data |>
        dplyr::mutate(
          event_detail_1 =
            dplyr::case_when(
              event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
              event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
              T ~ event_detail_1
            )
        ) |>
        dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
        dplyr::filter(event_type != "BLOCK") |>
        dplyr::filter(shot_zone == "O") |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::filter(shot_y > 0) |>
        dplyr::filter(position_category == "D" & !event_detail_1 %in% c("Backhand", "Tip In/Deflection")) |>
        dplyr::pull(shot_y),
      lims = c(c(-42, 42), c(1, 64)),
      n = c(85, 64)
    ) |>
      purrr::pluck("z") |>
      tibble::as_tibble(.name_repair = "unique") |>
      dplyr::mutate(x = seq(-42, 42)) |>
      tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
      dplyr::mutate(
        type = "Point (Implied)",
        y = y |> stringr::str_extract("\\d+") |> as.integer(),
        density = z / sum(z)
      )
  )

shot_att_density_est |>
  # dplyr::filter(
  #   !(
  #     y < 17 &
  #       x < -14.5 &
  #       sqrt(
  #         (17 - (y - 0.5)) ** 2 + (-14.5 - (x - 0.5)) ** 2
  #       ) > 28
  #   ),
  #   !(
  #     y < 17 &
  #       x > 14.5 &
  #       sqrt(
  #         (17 - (y - 0.5)) ** 2 + (14.5 - (x + 0.5)) ** 2
  #       ) > 28
  #   )
  # ) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::facet_wrap(ggplot2::vars(type), ncol = 2) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = density), alpha = 0.7) +
  ggplot2::scale_fill_viridis_c(option = "A")

puck_path_density_est <-
  shot_att_density_est |>
  # dplyr::filter(x == -42, y == 64) |>
  # tail() |>
  # head(1) |>
  dplyr::mutate(
    dens =
      purrr::pmap(
        list(
          shot_x = x,
          shot_y = y,
          density = density
        ),
        function(shot_x, shot_y, density) {
          center_angle <- atan(shot_y / abs(shot_x)) * (180 / pi)
          center_dist <- sqrt(shot_y**2 + shot_x**2)

          perp_slope <- (-shot_y / shot_x)**-1
          perp_intercept <- shot_y + (shot_x * - perp_slope)

          angle_margin_of_error <- 3.1# + ((90 - center_angle) / 90)
          distance_margin_of_error <- (center_dist / sqrt(64**2 + 42**2)) * sqrt(2)

          angle_left_post <- atan(shot_y / abs(shot_x + angle_margin_of_error)) * (180 / pi)
          angle_right_post <- atan(shot_y / abs(shot_x - angle_margin_of_error)) * (180 / pi)

          hyp_left_post <- sqrt(shot_y**2 + (shot_x + angle_margin_of_error)**2)
          left_ratio <- distance_margin_of_error / hyp_left_post
          left_x_offset <- shot_y * left_ratio
          left_y_offset <- abs(shot_x + angle_margin_of_error) * left_ratio

          hyp_right_post <- sqrt(shot_y**2 + (shot_x - angle_margin_of_error)**2)
          right_ratio <- distance_margin_of_error / hyp_right_post
          right_x_offset <- shot_y * right_ratio
          right_y_offset <- abs(shot_x - angle_margin_of_error) * right_ratio


          left_slope <- (shot_y / (shot_x + angle_margin_of_error))
          right_slope <- (shot_y / (shot_x - angle_margin_of_error))
          left_intercept <- (angle_margin_of_error * left_slope)
          right_intercept <- (-angle_margin_of_error * right_slope)

          coords <-
            tidyr::expand_grid(
              x = -42:42,
              y = 0:64
            ) |>
            dplyr::filter(
              y <= (x * perp_slope) + perp_intercept
                # x <= ifelse(shot_x > -5, 42, 5) &
                # x >= ifelse(shot_x < 5, -42, -5)
            ) |>
            dplyr::mutate(
              dist = 1- ((sqrt((shot_x - x)**2 + (shot_y - y)**2)) / (sqrt((85)**2 + (64)**2))),
              # dist_net =
              x_coord = (y - perp_intercept) / perp_slope,
              above_left_slope =
                y >= ((x * left_slope) + left_intercept - (left_y_offset)),# + (left_slope * left_x_offset)),
              below_left_slope =
                y <= ((x * left_slope) + left_intercept + (left_y_offset)), # + (left_slope * left_x_offset)),
              above_right_slope =
                y >= ((x * right_slope) + right_intercept - (right_y_offset)), # - (right_slope * right_x_offset)),
              below_right_slope =
                y <= ((x * right_slope) + right_intercept + (right_y_offset)), # - (right_slope * right_x_offset)),
              angle_left =
                ifelse(
                  below_left_slope,
                  center_angle +
                    (atan(
                      abs(shot_x - x) /
                        abs(shot_y - y)
                    ) *
                    (180 / pi) *
                    ifelse(x < shot_x, -1, 1)),
                  (90 - center_angle) +
                  atan(
                    abs(shot_y - y) /
                      abs(shot_x - x)
                  ) *
                    (180 / pi) *
                    ifelse(y < shot_y, 1, -1)
                ),
              angle_right =
                ifelse(
                  below_right_slope,
                  center_angle +
                    (atan(
                      abs(shot_x - x) /
                        abs(shot_y - y)
                    ) *
                      (180 / pi) *
                      ifelse(x < shot_x, 1, -1)),
                  (90 - center_angle) +
                    atan(
                      abs(shot_y - y) /
                        abs(shot_x - x)
                    ) *
                      (180 / pi) *
                      ifelse(y > shot_y, -1, 1)
                ),
              dens_weight =
                dplyr::case_when(
                  sign(left_slope) == -1 &
                    sign(right_slope) == -1 &
                    above_left_slope &
                    below_right_slope ~
                    1,
                  sign(left_slope) == 1 &
                    sign(right_slope) == 1 &
                    below_left_slope &
                    above_right_slope ~
                    1,
                  sign(left_slope) == 1 &
                    sign(right_slope) == -1 &
                    below_left_slope &
                    below_right_slope ~
                    1,
                  T ~ (
                    # 1
                    # closer coords have more weight
                    dist ** 0.3
                  ) * (
                    # angle / 90
                    dplyr::case_when(
                      sign(left_slope) == 1 & above_left_slope ~
                        ((angle_left) / (90 - abs(angle_left_post - center_angle))),
                      sign(right_slope) == -1 & above_right_slope ~
                        ((angle_right) / (90 - abs(center_angle - angle_right_post))),
                      sign(left_slope) == -1 & below_left_slope ~
                        ((angle_left) / (90 - abs(angle_left_post - center_angle))),
                      sign(right_slope) == 1 & below_right_slope ~
                        ((angle_right) / (90 - abs(angle_right_post - center_angle))),
                      T ~ 0
                    ) ** 7
                  ) *
                    0
                ),
              dens = density * dens_weight
            )

          coords |>
            dplyr::transmute(
              puck_path_x = x,
              puck_path_y = y,
              above_left_slope,
              angle_right,
              angle_left,
              dens_weight,
              dens = dens
            )
        }
      )
  ) |>
  # print() |>
  tidyr::unnest(dens) |>
  # View()
  dplyr::group_by(type, puck_path_x, puck_path_y) |>
  dplyr::summarise(puck_path_density = sum(dens), .groups = "drop") |>
  dplyr::group_by(type) |>
  dplyr::mutate(density = puck_path_density / sum(puck_path_density)) |>
  dplyr::ungroup()

puck_path_density_est |>
  dplyr::filter(
    !(
      (abs(puck_path_x) <= 4 & puck_path_y <= 4) |
        (abs(puck_path_x) <= 3 & puck_path_y == 5)
    )
  ) |>
  dplyr::group_by(type) |>
  dplyr::mutate(density = puck_path_density / sum(puck_path_density)) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::facet_wrap(ggplot2::vars(type), ncol = 2) +
  ggplot2::geom_tile(ggplot2::aes(x = puck_path_x, y = puck_path_y, fill = density), alpha = 0.7) +
  ggplot2::scale_fill_viridis_c(option = "A")


shot_blocker_density_est |>
  dplyr::filter(
    !(
      (abs(x) <= 4 & y <= 4) |
        (abs(x) <= 3 & y == 5)
    )
  ) |>
  dplyr::group_by(type) |>
  dplyr::mutate(density = z / sum(z)) |>
  dplyr::left_join(
    puck_path_density_est |>
      dplyr::filter(
        !(
          (abs(puck_path_x) <= 4 & puck_path_y <= 4) |
            (abs(puck_path_x) <= 3 & puck_path_y == 5)
        )
      ) |>
      dplyr::group_by(type) |>
      dplyr::mutate(density = puck_path_density / sum(puck_path_density)),
    by = c("x" = "puck_path_x", "y" = "puck_path_y", "type")
  ) |>
  # View()
  dplyr::mutate(
    shot_blockers_over_path = density.x - tidyr::replace_na(density.y, 0)
  ) |>
  # View()
  # summary()
  # dplyr::filter(abs(shot_blockers_over_path) <= max(shot_blockers_over_path)) |>
  # ggplot2::ggplot(ggplot2::aes(x = shot_blockers_over_path)) +
  # ggplot2::facet_wrap(ggplot2::vars(type), ncol = 2) +
  # ggplot2::geom_histogram()
    # summary()
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::facet_wrap(ggplot2::vars(type), ncol = 2) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = shot_blockers_over_path), alpha = 0.7) +
  ggplot2::scale_fill_gradient2(
    low = "blue3", high = "red3", mid = "white", midpoint = 0
  )
  ggplot2::scale_fill_viridis_c(option = "A")




MASS::kde2d(
  x =
    pred_xg_shot_data |>
    dplyr::mutate(
      event_detail_1 =
        dplyr::case_when(
          event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
          event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
          T ~ event_detail_1
        )
    ) |>
    dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
    dplyr::filter(event_type == "BLOCK") |>
    dplyr::filter(shot_zone == "O") |>
    dplyr::filter(home_skater_strength_state == "5v5") |>
    dplyr::filter(shot_y > 0) |>
    dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
    dplyr::pull(shot_x),
  y =
    pred_xg_shot_data |>
    dplyr::mutate(
      event_detail_1 =
        dplyr::case_when(
          event_detail_1 %in% c("Tip In", "Deflected", "Poke", "Bat") ~ "Tip In/Deflection",
          event_detail_1 %in% c("Cradle", "Wrap Around", "Between Legs") ~ "Backhand",
          T ~ event_detail_1
        )
    ) |>
    dplyr::filter(!event_detail_1 %in% c("Own Goal", "Awarded")) |>
    dplyr::filter(event_type == "BLOCK") |>
    dplyr::filter(shot_zone == "O") |>
    dplyr::filter(home_skater_strength_state == "5v5") |>
    dplyr::filter(shot_y > 0) |>
    dplyr::filter(position_category == "D" & event_detail_1 != "Backhand") |>
    dplyr::pull(shot_y),
  lims = c(c(-42, 42), c(0, 64)),
  n = c(85, 65)
) |>
  purrr::pluck("z") |>
  tibble::as_tibble(.name_repair = "unique") |>
  dplyr::mutate(x = seq(-42, 42)) |>
  tidyr::pivot_longer(-c(x), names_to = "y", values_to = "z") |>
  dplyr::mutate(
    y = y |> stringr::str_extract("\\d+") |> as.integer() |> magrittr::subtract(1),
    density = z / sum(z)
  ) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = density), alpha = 0.7) +
  ggplot2::scale_fill_viridis_c(option = "A")






