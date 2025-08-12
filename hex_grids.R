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
          seq(-4 - (2 * radius), -43  - radius, by = -radius * 2),
          seq(4 + (2 * radius), 43 - radius, by = radius * 2)
        ),
      y = seq(1 + radius + (radius * sqrt(3)), 64 - radius, by = (radius/sqrt(3))*2*3)
    )
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
  )

coord_map_lookup_table <-
  coord_map |>
  dplyr::mutate(
    nearest_cell_3_5_horizontal =
      purrr::map2(
        x, y,
        function(x1, y1) {
          hex_grid_3_5_horizontal |>
            dplyr::mutate(
              dist = sqrt((x - x1)**2 + (y - y1)**2)
            ) |>
            dplyr::filter(dist == min(dist))
        }
      ),
    nearest_cell_3_5_vertical =
      purrr::map2(
        x, y,
        function(x1, y1) {
          hex_grid_3_5_vertical |>
            dplyr::mutate(
              dist = sqrt((x - x1)**2 + (y - y1)**2)
            ) |>
            dplyr::filter(dist == min(dist))
        }
      ),
    nearest_cell_3_horizontal =
      purrr::map2(
        x, y,
        function(x1, y1) {
          hex_grid_3_horizontal |>
            dplyr::mutate(
              dist = sqrt((x - x1)**2 + (y - y1)**2)
            ) |>
            dplyr::filter(dist == min(dist))
        }
      ),
    nearest_cell_3_vertical =
      purrr::map2(
        x, y,
        function(x1, y1) {
          hex_grid_3_vertical |>
            dplyr::mutate(
              dist = sqrt((x - x1)**2 + (y - y1)**2)
            ) |>
            dplyr::filter(dist == min(dist))
        }
      ),
    nearest_cell_2_5_horizontal =
      purrr::map2(
        x, y,
        function(x1, y1) {
          hex_grid_2_5_horizontal |>
            dplyr::mutate(
              dist = sqrt((x - x1)**2 + (y - y1)**2)
            ) |>
            dplyr::filter(dist == min(dist))
        }
      ),
    nearest_cell_2_5_vertical =
      purrr::map2(
        x, y,
        function(x1, y1) {
          hex_grid_2_5_vertical |>
            dplyr::mutate(
              dist = sqrt((x - x1)**2 + (y - y1)**2)
            ) |>
            dplyr::filter(dist == min(dist))
        }
      )
  )






