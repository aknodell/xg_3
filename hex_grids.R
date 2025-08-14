coord_map <-
  tidyr::expand_grid(
    x = -43:43,
    y = 1:64
  ) |>
  dplyr::filter(abs(x) > 3)


radius <- 3.501

hex_grid_3_5_vertical <-
  tidyr::expand_grid(
    x =
      c(
        seq(-4 - radius, -43 + radius, by = -radius * 2),
        seq(4 + radius, 43 - radius, by = radius * 2)
      ),
    y = seq(1 + radius, 64 - radius, by = (radius/sqrt(3))*2*3)
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      x =
        c(
          seq(-4 - (2 * radius), -43 + radius, by = -radius * 2),
          seq(4 + (2 * radius), 43 - radius, by = radius * 2)
        ),
      y = seq(1 + radius + (radius * sqrt(3)), 64 - radius, by = (radius/sqrt(3))*2*3)
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      x = 0,
      y = c(3, seq(from = 6 + radius, to = 64 - radius, by = radius * 2))
    )
  ) |>
  dplyr::mutate(
    dist_to_center_goalline = sqrt(x**2 + y**2),
    dist_post_1 = sqrt((abs(x) - 3)**2 + y**2),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    vert_angle = atan(4 / dist_to_center_goalline),
    radius = cos(horiz_angle / 2) * dist_post_1,
    width = 2 * sqrt(dist_post_1**2 - radius**2),
    height = tan(vert_angle) * radius,
    max_area_to_cover = width * height,
    shooting_target_perc =
      (horiz_angle / pi) *
      (vert_angle / (pi / 2)),
  ) |>
  dplyr::arrange(radius, x) |>
  tibble::rowid_to_column(var = "hex_id") |>
  dplyr::select(
    hex_id,
    x_hex = x,
    y_hex = y,
    radius,
    max_area_to_cover,
    shooting_target_perc,
  )

hex_grid_3_5_horizontal <-
  tidyr::expand_grid(
    x =
      c(
        seq(-4 - radius, -43 + radius, by = -(radius/sqrt(3))*2*3),
        seq(4 + radius, 43 - radius, by = (radius/sqrt(3))*2*3)
      ),
    y = seq(1 + radius, 64 -radius, by = radius * 2)
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      x =
        c(
          seq(-4 - (radius) - ((radius/sqrt(3))*3), -43 + radius, by = -(radius/sqrt(3))*2*3),
          seq(4 + (radius) + ((radius/sqrt(3))*3), 43 - radius, by = (radius/sqrt(3))*2*3)
        ),
      y = seq(1 + (2 * radius), 64 - radius, by = radius * 2)
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      x = 0,
      y = c(3, seq(from = 6 + radius, to = 64 - radius, by = radius * 2))
    )
  ) |>
  dplyr::mutate(
    dist_to_center_goalline = sqrt(x**2 + y**2),
    dist_post_1 = sqrt((abs(x) - 3)**2 + y**2),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    vert_angle = atan(4 / dist_to_center_goalline),
    radius = cos(horiz_angle / 2) * dist_post_1,
    width = 2 * sqrt(dist_post_1**2 - radius**2),
    height = tan(vert_angle) * radius,
    max_area_to_cover = width * height,
    shooting_target_perc =
      (horiz_angle / pi) *
      (vert_angle / (pi / 2)),
  ) |>
  dplyr::arrange(radius, x) |>
  tibble::rowid_to_column(var = "hex_id") |>
  dplyr::select(
    hex_id,
    x_hex = x,
    y_hex = y,
    radius,
    max_area_to_cover,
    shooting_target_perc,
  )

radius <- 3.001

hex_grid_3_vertical <-
  tidyr::expand_grid(
    x =
      c(
        seq(-4 - radius, -43 + radius, by = -radius * 2),
        seq(4 + radius, 43 - radius, by = radius * 2)
      ),
    y = seq(1 + radius, 64 - radius, by = (radius/sqrt(3))*2*3)
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      x =
        c(
          seq(-4 - (2 * radius), -43 + radius, by = -radius * 2),
          seq(4 + (2 * radius), 43 - radius, by = radius * 2)
        ),
      y = seq(1 + radius + (radius * sqrt(3)), 64 - radius, by = (radius/sqrt(3))*2*3)
    )
  ) |>
  dplyr::filter(!(abs(x) == max(x) & y == min(y))) |>
  dplyr::bind_rows(
    tibble::tibble(
      x = 0,
      y = c(3, seq(from = 6 + radius, to = 64 - radius, by = radius * 2))
    )
  ) |>
  dplyr::mutate(
    dist_to_center_goalline = sqrt(x**2 + y**2),
    dist_post_1 = sqrt((abs(x) - 3)**2 + y**2),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    vert_angle = atan(4 / dist_to_center_goalline),
    radius = cos(horiz_angle / 2) * dist_post_1,
    width = 2 * sqrt(dist_post_1**2 - radius**2),
    height = tan(vert_angle) * radius,
    max_area_to_cover = width * height,
    shooting_target_perc =
      (horiz_angle / pi) *
      (vert_angle / (pi / 2)),
  ) |>
  dplyr::arrange(radius, x) |>
  tibble::rowid_to_column(var = "hex_id") |>
  dplyr::select(
    hex_id,
    x_hex = x,
    y_hex = y,
    radius,
    max_area_to_cover,
    shooting_target_perc,
  )

hex_grid_3_horizontal <-
  tidyr::expand_grid(
    x =
      c(
        seq(-4 - radius, -43 + radius, by = -(radius/sqrt(3))*2*3),
        seq(4 + radius, 43 - radius, by = (radius/sqrt(3))*2*3)
      ),
    y = seq(1 + radius, 64 - radius, by = radius * 2)
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      x =
        c(
          seq(-4 - (radius) - ((radius/sqrt(3))*3), -43 + radius, by = -(radius/sqrt(3))*2*3),
          seq(4 + (radius) + ((radius/sqrt(3))*3), 43 - radius, by = (radius/sqrt(3))*2*3)
        ),
      y = seq(1 + (2 * radius), 64 - radius, by = radius * 2)
    )
  ) |>
  dplyr::filter(!(abs(x) == max(x) & y == min(y))) |>
  dplyr::bind_rows(
    tibble::tibble(
      x = 0,
      y = c(3, seq(from = 6 + radius, to = 64 - radius, by = radius * 2))
    )
  ) |>
  dplyr::mutate(
    dist_to_center_goalline = sqrt(x**2 + y**2),
    dist_post_1 = sqrt((abs(x) - 3)**2 + y**2),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    vert_angle = atan(4 / dist_to_center_goalline),
    radius = cos(horiz_angle / 2) * dist_post_1,
    width = 2 * sqrt(dist_post_1**2 - radius**2),
    height = tan(vert_angle) * radius,
    max_area_to_cover = width * height,
    shooting_target_perc =
      (horiz_angle / pi) *
      (vert_angle / (pi / 2)),
  ) |>
  dplyr::arrange(radius, x) |>
  tibble::rowid_to_column(var = "hex_id") |>
  dplyr::select(
    hex_id,
    x_hex = x,
    y_hex = y,
    radius,
    max_area_to_cover,
    shooting_target_perc,
  )

radius <- 2.501

hex_grid_2_5_vertical <-
  tidyr::expand_grid(
    x =
      c(
        seq(-4 - radius, -43 + radius, by = -radius * 2),
        seq(4 + radius, 43 - radius, by = radius * 2)
      ),
    y = seq(1 + radius, 64 - radius, by = (radius/sqrt(3))*2*3)
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      x =
        c(
          seq(-4 - (2 * radius), -43 + radius, by = -radius * 2),
          seq(4 + (2 * radius), 43 - radius, by = radius * 2)
        ),
      y = seq(1 + radius + (radius * sqrt(3)), 64 - radius, by = (radius/sqrt(3))*2*3)
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      x = 0,
      y = c(3, seq(from = 6 + radius, to = 64 - radius, by = radius * 2))
    )
  ) |>
  dplyr::mutate(
    dist_to_center_goalline = sqrt(x**2 + y**2),
    dist_post_1 = sqrt((abs(x) - 3)**2 + y**2),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    vert_angle = atan(4 / dist_to_center_goalline),
    radius = cos(horiz_angle / 2) * dist_post_1,
    width = 2 * sqrt(dist_post_1**2 - radius**2),
    height = tan(vert_angle) * radius,
    max_area_to_cover = width * height,
    shooting_target_perc =
      (horiz_angle / pi) *
      (vert_angle / (pi / 2)),
  ) |>
  dplyr::arrange(radius, x) |>
  tibble::rowid_to_column(var = "hex_id") |>
  dplyr::select(
    hex_id,
    x_hex = x,
    y_hex = y,
    radius,
    max_area_to_cover,
    shooting_target_perc,
  )

hex_grid_2_5_horizontal <-
  tidyr::expand_grid(
    x =
      c(
        seq(-4 - radius, -43 + radius, by = -(radius/sqrt(3))*2*3),
        seq(4 + radius, 43 - radius, by = (radius/sqrt(3))*2*3)
      ),
    y = seq(1 + radius, 64 - radius, by = radius * 2)
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      x =
        c(
          seq(-4 - (radius) - ((radius/sqrt(3))*3), -43 + radius, by = -(radius/sqrt(3))*2*3),
          seq(4 + (radius) + ((radius/sqrt(3))*3), 43 - radius, by = (radius/sqrt(3))*2*3)
        ),
      y = seq(1 + (2 * radius), 64 - radius, by = radius * 2)
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      x = 0,
      y = c(3, seq(from = 6 + radius, to = 64 - radius, by = radius * 2))
    )
  ) |>
  dplyr::mutate(
    dist_to_center_goalline = sqrt(x**2 + y**2),
    dist_post_1 = sqrt((abs(x) - 3)**2 + y**2),
    angle_post_1 = atan((x - 3) / y),
    angle_post_2 = atan((x + 3) / y),
    horiz_angle = abs(angle_post_1 - angle_post_2),
    vert_angle = atan(4 / dist_to_center_goalline),
    radius = cos(horiz_angle / 2) * dist_post_1,
    width = 2 * sqrt(dist_post_1**2 - radius**2),
    height = tan(vert_angle) * radius,
    max_area_to_cover = width * height,
    shooting_target_perc =
      (horiz_angle / pi) *
      (vert_angle / (pi / 2)),
  ) |>
  dplyr::arrange(radius, x) |>
  tibble::rowid_to_column(var = "hex_id") |>
  dplyr::select(
    hex_id,
    x_hex = x,
    y_hex = y,
    radius,
    max_area_to_cover,
    shooting_target_perc,
  )

coord_map_lookup_table <-
  coord_map |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      x = -3:3,
      y = 0:64
    )
  ) |>
  dplyr::mutate(
    nearest_cell_3_5_vertical =
      purrr::map2(
        x, y,
        function(x1, y1) {
          # in the slot
          if (abs(x1) <= 3) {
            # in the crease
            if (
              y1 <= 5 |
              (x1 == 0 & y1 <= 6)
            ) {
              hex_grid_3_5_vertical |>
                dplyr::filter(hex_id == 1)
            } else {
              hex_grid_3_5_vertical |>
                dplyr::filter(x_hex == 0, y_hex != 3) |>
                dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
                dplyr::filter(dist == min(dist)) |>
                dplyr::select(-dist)
            }
          } else {
            hex_grid_3_5_vertical |>
              dplyr::filter(sign(x_hex) == sign(x1)) |>
              dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
              dplyr::filter(dist == min(dist)) |>
              dplyr::select(-dist)
          }
        }
      ),
    nearest_cell_3_vertical =
      purrr::map2(
        x, y,
        function(x1, y1) {
          # in the slot
          if (abs(x1) <= 3) {
            # in the crease
            if (
              y1 <= 5 |
              (x1 == 0 & y1 <= 6)
            ) {
              hex_grid_3_vertical |>
                dplyr::filter(hex_id == 1)
            } else {
              hex_grid_3_vertical |>
                dplyr::filter(x_hex == 0, y_hex != 3) |>
                dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
                dplyr::filter(dist == min(dist)) |>
                dplyr::select(-dist)
            }
          } else {
            hex_grid_3_vertical |>
              dplyr::filter(sign(x_hex) == sign(x1)) |>
              dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
              dplyr::filter(dist == min(dist)) |>
              dplyr::select(-dist)
          }
        }
      ),
    nearest_cell_2_5_vertical =
      purrr::map2(
        x, y,
        function(x1, y1) {
          # in the slot
          if (abs(x1) <= 3) {
            # in the crease
            if (
              y1 <= 5 |
              (x1 == 0 & y1 <= 6)
            ) {
              hex_grid_2_5_vertical |>
                dplyr::filter(hex_id == 1)
            } else {
              hex_grid_2_5_vertical |>
                dplyr::filter(x_hex == 0, y_hex != 3) |>
                dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
                dplyr::filter(dist == min(dist)) |>
                dplyr::select(-dist)
            }
          } else {
            hex_grid_2_5_vertical |>
              dplyr::filter(sign(x_hex) == sign(x1)) |>
              dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
              dplyr::filter(dist == min(dist)) |>
              dplyr::select(-dist)
          }
        }
      ),
    nearest_cell_3_5_horizontal =
      purrr::map2(
        x, y,
        function(x1, y1) {
          # in the slot
          if (abs(x1) <= 3) {
            # in the crease
            if (
              y1 <= 5 |
              (x1 == 0 & y1 <= 6)
            ) {
              hex_grid_3_5_horizontal |>
                dplyr::filter(hex_id == 1)
            } else {
              hex_grid_3_5_horizontal |>
                dplyr::filter(x_hex == 0, y_hex != 3) |>
                dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
                dplyr::filter(dist == min(dist)) |>
                dplyr::select(-dist)
            }
          } else {
            hex_grid_3_5_horizontal |>
              dplyr::filter(sign(x_hex) == sign(x1)) |>
              dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
              dplyr::filter(dist == min(dist)) |>
              dplyr::select(-dist)
          }
        }
      ),
    nearest_cell_3_horizontal =
      purrr::map2(
        x, y,
        function(x1, y1) {
          # in the slot
          if (abs(x1) <= 3) {
            # in the crease
            if (
              y1 <= 5 |
              (x1 == 0 & y1 <= 6)
            ) {
              hex_grid_3_horizontal |>
                dplyr::filter(hex_id == 1)
            } else {
              hex_grid_3_horizontal |>
                dplyr::filter(x_hex == 0, y_hex != 3) |>
                dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
                dplyr::filter(dist == min(dist)) |>
                dplyr::select(-dist)
            }
          } else {
            hex_grid_3_horizontal |>
              dplyr::filter(sign(x_hex) == sign(x1)) |>
              dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
              dplyr::filter(dist == min(dist)) |>
              dplyr::select(-dist)
          }
        }
      ),
    nearest_cell_2_5_horizontal =
      purrr::map2(
        x, y,
        function(x1, y1) {
          # in the slot
          if (abs(x1) <= 3) {
            # in the crease
            if (
              y1 <= 5 |
              (x1 == 0 & y1 <= 6)
            ) {
              hex_grid_2_5_horizontal |>
                dplyr::filter(hex_id == 1)
            } else {
              hex_grid_2_5_horizontal |>
                dplyr::filter(x_hex == 0, y_hex != 3) |>
                dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
                dplyr::filter(dist == min(dist)) |>
                dplyr::select(-dist)
            }
          } else {
            hex_grid_2_5_horizontal |>
              dplyr::filter(sign(x_hex) == sign(x1)) |>
              dplyr::mutate(dist = sqrt((x_hex - x1)**2 + (y_hex - y1)**2)) |>
              dplyr::filter(dist == min(dist)) |>
              dplyr::select(-dist)
          }
        }
      )
  )

coord_map_lookup_table |>
  tidyr::unnest(nearest_cell_3_5_vertical) |>
  dplyr::mutate(
    hex_id = hex_id / max(hex_id),
    method = "3.5 ft Radius, Vertical Alignment"
  ) |>
  dplyr::bind_rows(
    coord_map_lookup_table |>
      tidyr::unnest(nearest_cell_3_vertical) |>
      dplyr::mutate(
        hex_id = hex_id / max(hex_id),
        method = "3 ft Radius, Vertical Alignment")
  ) |>
  dplyr::bind_rows(
    coord_map_lookup_table |>
      tidyr::unnest(nearest_cell_2_5_vertical) |>
      dplyr::mutate(
        hex_id = hex_id / max(hex_id),
        method = "2.5 ft Radius, Vertical Alignment")
  ) |>
  dplyr::bind_rows(
    coord_map_lookup_table |>
      tidyr::unnest(nearest_cell_3_5_horizontal) |>
      dplyr::mutate(
        hex_id = hex_id / max(hex_id),
        method = "3.5 ft Radius, Horizontal Alignment")
  ) |>
  dplyr::bind_rows(
    coord_map_lookup_table |>
      tidyr::unnest(nearest_cell_3_horizontal) |>
      dplyr::mutate(
        hex_id = hex_id / max(hex_id),
        method = "3 ft Radius, Horizontal Alignment")
  ) |>
  dplyr::bind_rows(
    coord_map_lookup_table |>
      tidyr::unnest(nearest_cell_2_5_horizontal) |>
      dplyr::mutate(
        hex_id = hex_id / max(hex_id),
        method = "2.5 ft Radius, Horizontal Alignment")
  ) |>
  dplyr::mutate(method = method |> factor() |> forcats::fct_inorder()) |>
  ggplot2::ggplot() +
  ggplot2::facet_wrap(
    ggplot2::vars(method),
    ncol = 3
  ) +
  off_zone_markings(show_behind_net = T, show_neutral_zone = T, big_net = T) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex_id), alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = x_hex, y = y_hex), size = 3, color = "white") +
  ggplot2::scale_fill_viridis_c()



coord_map_lookup_table |>
  tidyr::unnest(nearest_cell_3_5_vertical) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex_id), alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = x_hex, y = y_hex), size = 3, color = "white") +
  ggplot2::scale_fill_viridis_c()

coord_map_lookup_table |>
  tidyr::unnest(nearest_cell_3_vertical) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex_id), alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = x_hex, y = y_hex), size = 3, color = "white") +
  ggplot2::scale_fill_viridis_c()

coord_map_lookup_table |>
  tidyr::unnest(nearest_cell_2_5_vertical) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex_id), alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = x_hex, y = y_hex), size = 3, color = "white") +
  ggplot2::scale_fill_viridis_c()

coord_map_lookup_table |>
  tidyr::unnest(nearest_cell_3_5_horizontal) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex_id), alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = x_hex, y = y_hex), size = 3, color = "white") +
  ggplot2::scale_fill_viridis_c()

coord_map_lookup_table |>
  tidyr::unnest(nearest_cell_3_horizontal) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex_id), alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = x_hex, y = y_hex), size = 3, color = "white") +
  ggplot2::scale_fill_viridis_c()

coord_map_lookup_table |>
  tidyr::unnest(nearest_cell_2_5_horizontal) |>
  ggplot2::ggplot() +
  off_zone_markings() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex_id), alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = x_hex, y = y_hex), size = 3, color = "white") +
  ggplot2::scale_fill_viridis_c()


