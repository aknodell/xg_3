off_zone_markings <- function(legend_position = "none") {
  list(
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = c(-22, 22),
          y = 20,
          r = 1
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "red",
      color = "red"
    ),
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = c(-22, 22),
          y = 20,
          r = 15
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "red",
      color = "red",
      alpha = 0
    ),
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = 0,
          y = 0,
          r = 6
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "lightblue",
      color = "red"
    ),
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          xmin = c(-6.5, 4),
          xmax = c(-4, 6.5),
          ymin = 0,
          ymax = 6
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "white"
    ),
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          x =
            c(
              -26, -26, -21, -21, 18, 18, 23, 23,
              (-22 - (sqrt(15**2 - 2**2))), (-22 - sqrt(15**2 - 2**2)),
              (-22 + (sqrt(15**2 - 2**2))), (-22 + sqrt(15**2 - 2**2)),
              (22 - (sqrt(15**2 - 2**2))), (22 - sqrt(15**2 - 2**2)),
              (22 + (sqrt(15**2 - 2**2))), (22 + sqrt(15**2 - 2**2)),
              -23, -23, -21, -21, 21, 21, 23, 23,
              -23, -23, -21, -21, 21, 21, 23, 23,
              -4, 4,
              -42.5
            ),
          xend =
            c(
              -23, -23, -18, -18, 21, 21, 26, 26,
              (-22 - (sqrt(15**2 - 2**2)) - 2), (-22 - sqrt(15**2 - 2**2) - 2),
              (-22 + (sqrt(15**2 - 2**2)) + 2), (-22 + sqrt(15**2 - 2**2) + 2),
              (22 - (sqrt(15**2 - 2**2)) - 2), (22 - sqrt(15**2 - 2**2) - 2),
              (22 + (sqrt(15**2 - 2**2)) + 2), (22 + sqrt(15**2 - 2**2) + 2),
              -23, -23, -21, -21, 21, 21, 23, 23,
              -23, -23, -21, -21, 21, 21, 23, 23,
              -4, 4,
              42.5
            ),
          y =
            c(
              22, 18, 22, 18, 22, 18, 22, 18,
              22, 18, 22, 18, 22, 18, 22, 18,
              18, 18, 26, 26, 18, 18, 26, 26,
              26, 26, 18, 18, 26, 26, 18, 18,
              sqrt(6**2 - 4**2), sqrt(6**2 - 4**2),
              0
            ),
          yend =
            c(
              22, 18, 22, 18, 22, 18, 22, 18,
              22, 18, 22, 18, 22, 18, 22, 18,
              14, 14, 22, 22, 14, 14, 22, 22,
              22, 22, 14, 14, 22, 22, 14, 14,
              0, 0,
              0
            )
        ),
      mapping =
        ggplot2::aes(
          x = x, y = y, xend = xend, yend = yend
        ),
      color = "red"
    ),
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          y = 64,
          yend = 0,
          x = c(-42.5, 42.5)
        ),
      mapping = ggplot2::aes(
        x = x, y = y, yend = yend
      ),
      color = "black"
    ),
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          xmin = -42.5,
          xmax = 42.5,
          ymin = 63,
          ymax = 64
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "blue"
    ),
    ggplot2::coord_fixed(
      ylim = c(0, 64), xlim = c(-42.5, 42.5), expand = F
    ),
    ggplot2::theme_minimal(),
    ggplot2::theme(
      legend.position = legend_position,
      panel.spacing.x = ggplot2::unit(2, "lines"),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
  )
}
