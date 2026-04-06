off_zone_markings <-
  function(legend_position = "none", show_behind_net = F, show_neutral_zone = F, big_net = F, direction = "down") {
    net_radius <- ifelse(big_net, 20/12, 18/12)
    net_depth <- ifelse(big_net, 44/12, 40/12)
    net_max_width <- ifelse(big_net, 96/12, 88/12)

    net_curve_center_x <- (net_max_width / 2) - net_radius
    net_curve_center_y <- net_depth - net_radius
    net_post_x_diff = 3 - net_curve_center_x

    right_goal_joint_front_x <-
      (net_curve_center_x) +
      sqrt(
        net_radius**2 -
          (
            sin(
              (pi / 2) -
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
                atan(net_post_x_diff / net_curve_center_y)
            ) *
              net_radius
          )**2
      )
    left_goal_joint_front_x <-
      (-net_curve_center_x) -
      sqrt(
        net_radius**2 -
          (
            sin(
              (pi / 2) -
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
                atan(net_post_x_diff / net_curve_center_y)
            ) *
              net_radius
          )**2
      )
    goal_joint_front_y <-
      (-net_curve_center_y) + (
        sin(
          (pi / 2) -
            acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
            atan(net_post_x_diff / net_curve_center_y)
        ) *
          net_radius
      )
    right_goal_joint_back_x <- net_curve_center_x
    left_goal_joint_back_x <- -net_curve_center_x
    goal_joint_back_y <- -net_depth

    xlims <- c(-42.5, 42.5)
    ylims <-
      c(
        ifelse(show_behind_net, -11, 0),
        ifelse(show_neutral_zone, 89.5, 64)
      )

    if (direction == "up") {
      xlims <- rev(xlims)
      ylims <- rev(ylims)
    }

    list(
      # center line
      ggplot2::geom_rect(
        data =
          tibble::tibble(
            xmin = -42.5,
            xmax = 42.5,
            ymin = 88.5,
            ymax = 89.5
          ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "#C8102E"
      ),
      # white markings on center line
      ggplot2::geom_rect(
        data = tibble::tibble(
          xmin = seq(-42.5, 42.5, by = 2),
          xmax = seq(-41.5, 42.5, by = 2),
          ymin = 88.5,
          ymax = 89.5
        ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "white"
      ),
      # blue line
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
        fill = "#0033A0"
      ),
      # faceoff dot red centers
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = c(-22, 22, -22, 22),
            y = c(20, 20, 69, 69),
            r = 1
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#C8102E",
        color = "#C8102E"
      ),
      # faceoff dot white spaces
      ggplot2::geom_rect(
        data =
          tibble::tibble(
            xmin = c(-23, -23, 21, 21, -23, -23, 21, 21),
            xmax = c(21, 21, 23, 23, 21, 21, 23, 23),
            ymin = c(20.75, 19, 20.75, 19, 69.75, 68, 69.75, 68),
            ymax = c(21, 19.25, 21, 19.25, 70, 68.25, 70, 68.25)
          ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "white"
      ),
      # faceoff dot outside lines
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = c(-22, 22, -22, 22),
            y = c(20, 20, 69, 69),
            r = 1
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#C8102E",
        color = "#C8102E",
        alpha = 0
      ),
      # center faceoff dot
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = 0,
            y = 89,
            r = 0.5
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#0033A0",
        color = "white"
      ),
      # center_circle
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = 0,
            y = 89,
            r = 15
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#0033A0",
        color = "#0033A0",
        alpha = 0
      ),
      # faceoff circles
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = c(-22, 22),
            y = c(20, 20),
            r = 15
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#C8102E",
        color = "#C8102E",
        alpha = 0
      ),
      # goalie crease
      ggforce::geom_circle(
        data =
          tibble::tibble(
            x = 0,
            y = 0,
            r = 6
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        fill = "#41B6E6",
        color = "#C8102E"
      ),
      # white areas to cover edges of goalie crease
      ggplot2::geom_rect(
        data =
          tibble::tibble(
            xmin = c(-6.5, 4, -7),
            xmax = c(-4, 6.5, 7),
            ymin = c(0, 0, -7),
            ymax = c(6, 6, 0)
          ),
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),
        fill = "white"
      ),
      # net color (curved sections)
      ggforce::geom_circle(
        tibble::tibble(
          x = c(-net_curve_center_x, net_curve_center_x),
          y = -net_curve_center_y,
          r = net_radius
        ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
        linewidth = 0,
        color = "lightgrey",
        fill = "lightgrey"
      ),
      # net color (straight sections)
      ggplot2::geom_polygon(
        data =
          tibble::tibble(
            x =
              c(
                # left post
                -3,
                # right post
                3,
                right_goal_joint_front_x,
                right_goal_joint_back_x,
                left_goal_joint_back_x,
                left_goal_joint_front_x
              ),
            y = c(
              0,
              0,
              goal_joint_front_y,
              goal_joint_back_y,
              goal_joint_back_y,
              goal_joint_front_y
            )
          ),
        mapping = ggplot2::aes(x = x, y = y),
        fill = "lightgrey"
      ),
      # goal straight lines
      ggplot2::geom_segment(
        data =
          tibble::tibble(
            x = c(-3, left_goal_joint_back_x, 3),
            xend =
              c(
                left_goal_joint_front_x,
                right_goal_joint_back_x,
                right_goal_joint_front_x
              ),
            y = c(0, goal_joint_back_y, 0),
            yend =
              c(
                goal_joint_front_y,
                goal_joint_back_y,
                goal_joint_front_y
              )
          ),
        mapping = ggplot2::aes(
          x = x, xend = xend, y = y, yend = yend
        ),
        color = "black"
      ),
      # goal curves
      ggforce::geom_arc(
        data =
          tibble::tibble(
            x = c(-net_curve_center_x, net_curve_center_x),
            y = -net_curve_center_y,
            r = net_radius,
            start =
              c(
                pi,
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) +
                  atan(net_post_x_diff / net_curve_center_y)
              ),
            end =
              c(
                (2*pi) -
                  acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
                  atan(net_post_x_diff / net_curve_center_y),
                pi
              )
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
        color = "black"
      ),
      # red lines
      ggplot2::geom_segment(
        data =
          tibble::tibble(
            x =
              c(
                # center line border
                -42.5, -42.5,
                # inside hash marks (horizontal)
                -22 - (23/6), -22 - (23/6), -22 + (5/6), -22 + (5/6),
                22 - (23/6), 22 - (23/6),  22 + (5/6), 22 + (5/6),
                # inside hash marks (vertical)
                -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
                # outside hashmarks
                (-22 - (sqrt(15**2 - 2.875**2))), (-22 - sqrt(15**2 - 2.875**2)),
                (-22 + (sqrt(15**2 - 2.875**2))), (-22 + sqrt(15**2 - 2.875**2)),
                (22 - (sqrt(15**2 - 2.875**2))), (22 - sqrt(15**2 - 2.875**2)),
                (22 + (sqrt(15**2 - 2.875**2))), (22 + sqrt(15**2 - 2.875**2)),
                # sides of goalie crease
                -4, 4,
                # goal line
                -sqrt(28**2 - 17**2) - 14.5,
                # trapezoid
                -11, 11,
                # crease hash marks
                -4, 4
              ),
            xend =
              c(
                # center line border
                42.5, 42.5,
                # inside hash marks (horizontal)
                -22 - (5/6), -22 - (5/6), -22 + (23/6), -22 + (23/6),
                22 - (5/6), 22 - (5/6),  22 + (23/6), 22 + (23/6),
                # inside hash marks (vertical)
                -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
                # outside hashmarks
                (-22 - (sqrt(15**2 - 2.875**2)) - 2), (-22 - sqrt(15**2 - 2.875**2) - 2),
                (-22 + (sqrt(15**2 - 2.875**2)) + 2), (-22 + sqrt(15**2 - 2.875**2) + 2),
                (22 - (sqrt(15**2 - 2.875**2)) - 2), (22 - sqrt(15**2 - 2.875**2) - 2),
                (22 + (sqrt(15**2 - 2.875**2)) + 2), (22 + sqrt(15**2 - 2.875**2) + 2),
                # sides of goalie crease
                -4, 4,
                # goal line
                sqrt(28**2 - 17**2) + 14.5,
                # trapezoid
                -14, 14,
                # crease hash marks
                -43/12, 43/12
              ),
            y =
              c(
                # center line border
                89.5, 88.5,
                # inside hash marks (horizontal)
                22, 18, 22, 18, 22, 18, 22, 18,
                # inside hash marks (vertical)
                18, 26, 18, 26, 26, 18, 26, 18,
                # outside hashmarks
                22.875, 17.125, 22.875, 17.125, 22.875, 17.125, 22.875, 17.125,
                # sides of goalie crease
                sqrt(6**2 - 4**2), sqrt(6**2 - 4**2),
                # goal line
                0,
                # trapezoid
                0, 0,
                # crease hash marks
                4, 4
              ),
            yend =
              c(
                # center line border
                89.5, 88.5,
                # inside hash marks (horizontal)
                22, 18, 22, 18, 22, 18, 22, 18,
                # inside hash marks (vertical)
                14, 22, 14, 22, 22, 14, 22, 14,
                # outside hashmarks
                22.875, 17.125, 22.875, 17.125, 22.875, 17.125, 22.875, 17.125,
                # sides of goalie crease
                0, 0,
                # goal line
                0,
                # trapezoid
                -11, -11,
                # crease hash marks
                4, 4
              )
          ),
        mapping =
          ggplot2::aes(
            x = x, y = y, xend = xend, yend = yend
          ),
        color = "#C8102E"
      ),
      # rink straight borders
      ggplot2::geom_segment(
        data =
          tibble::tibble(
            y = c(89.5, 89.5, -11),
            yend = c(17, 17, -11),
            x = c(-42.5, 42.5, -14.5),
            xend = c(-42.5, 42.5, 14.5)
          ),
        mapping = ggplot2::aes(
          x = x, xend = xend, y = y, yend = yend
        ),
        color = "black",
        linewidth = 1
      ),
      # rink corners
      ggforce::geom_arc(
        data =
          tibble::tibble(
            x = c(14.5, -14.5),
            y = 17,
            r = 28,
            start = c(pi / 2, 3 * pi / 2),
            end = c(pi)
          ),
        mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
        color = "black",
        linewidth = 1
      ),
      ggplot2::coord_fixed(
        xlim = xlims,
        ylim = ylims,
        expand = F
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




full_rink_markings_vert <- function(legend_position = "none", center_line = 0, big_net = F) {
  net_radius <- ifelse(big_net, 20/12, 18/12)
  net_depth <- ifelse(big_net, 44/12, 40/12)
  net_max_width <- ifelse(big_net, 96/12, 88/12)

  net_curve_center_x <- (net_max_width / 2) - net_radius
  net_curve_center_y <- net_depth - net_radius
  net_post_x_diff = 3 - net_curve_center_x

  right_goal_joint_front_x <-
    (net_curve_center_x) +
    sqrt(
      net_radius**2 -
        (
          sin(
            (pi / 2) -
              acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
              atan(net_post_x_diff / net_curve_center_y)
          ) *
            net_radius
        )**2
    )
  left_goal_joint_front_x <-
    (-net_curve_center_x) -
    sqrt(
      net_radius**2 -
        (
          sin(
            (pi / 2) -
              acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
              atan(net_post_x_diff / net_curve_center_y)
          ) *
            net_radius
        )**2
    )
  goal_joint_front_y <-
    (-net_curve_center_y) + (
      sin(
        (pi / 2) -
          acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
          atan(net_post_x_diff / net_curve_center_y)
      ) *
        net_radius
    )
  right_goal_joint_back_x <- net_curve_center_x
  left_goal_joint_back_x <- -net_curve_center_x
  goal_joint_back_y <- -net_depth

  xlims <- c(-42.5, 42.5)
  ylims <- c(center_line - 100, center_line + 100)

  list(
    # center line
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          xmin = -42.5,
          xmax = 42.5,
          ymin = center_line - 0.5,
          ymax = center_line + 0.5
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "#C8102E"
    ),
    # white markings on center line
    ggplot2::geom_rect(
      data = tibble::tibble(
        xmin = seq(-42.5, 42.5, by = 2),
        xmax = seq(-41.5, 42.5, by = 2),
        ymin = center_line - 0.5,
        ymax = center_line + 0.5
      ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "white"
    ),
    # blue lines
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          xmin = -42.5,
          xmax = 42.5,
          ymin = c(center_line + 24, center_line - 25),
          ymax = c(center_line + 25, center_line - 24)
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "#0033A0"
    ),
    # faceoff dot red centers
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = c(-22, 22, -22, 22, -22, 22, -22, 22),
          y = c(
            center_line + 20, center_line + 20, center_line + 69, center_line + 69,
            center_line - 20, center_line - 20, center_line - 69, center_line - 69
          ),
          r = 1
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#C8102E",
      color = "#C8102E"
    ),
    # faceoff dot white spaces
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          xmin = c(-23, -23, 21, 21, -23, -23, 21, 21, -23, -23, 21, 21, -23, -23, 21, 21),
          xmax = c(21, 21, 23, 23, 21, 21, 23, 23, 21, 21, 23, 23, 21, 21, 23, 23),
          ymin =
            c(
              center_line + 68, center_line + 69.75, center_line + 68, center_line + 69.75,
              center_line - 68.25, center_line - 70, center_line - 68.25, center_line - 70,
              # 20.75, 19, 20.75, 19,
              center_line + 19, center_line + 20.75, center_line + 19, center_line + 20.75,
              center_line - 19.25, center_line - 21, center_line - 19.25, center_line - 21
              # 69.75, 68, 69.75, 68
            ),
          ymax =
            c(
              center_line + 68.25, center_line + 70, center_line + 68.25, center_line + 70,
              center_line - 68, center_line - 69.75, center_line - 68, center_line - 69.75,
              # 21, 19.25, 21, 19.25,
              center_line + 19.25, center_line + 21, center_line + 19.25, center_line + 21,
              center_line - 19, center_line - 20.75, center_line - 19, center_line - 20.75
              # 70, 68.25, 70, 68.25
            )
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "white"
    ),
    # faceoff dot outside lines
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = c(-22, 22, -22, 22, -22, 22, -22, 22),
          y = c(
            center_line + 20, center_line + 20, center_line + 69, center_line + 69,
            center_line - 20, center_line - 20, center_line - 69, center_line - 69
          ),
          r = 1
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#C8102E",
      color = "#C8102E",
      alpha = 0
    ),
    # center faceoff dot
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = 0,
          y = center_line,
          r = 0.5
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#0033A0",
      color = "white"
    ),
    # center_circle
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = 0,
          y = center_line,
          r = 15
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#0033A0",
      color = "#0033A0",
      alpha = 0
    ),
    # faceoff circles
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = c(-22, 22, -22, 22),
          y = c(center_line + 69, center_line + 69, center_line - 69, center_line - 69),
          r = 15
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#C8102E",
      color = "#C8102E",
      alpha = 0
    ),
    # goalie crease
    ggforce::geom_circle(
      data =
        tibble::tibble(
          x = 0,
          y = c(center_line + 89, center_line - 89),
          r = 6
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#41B6E6",
      color = "#C8102E"
    ),
    # white areas to cover edges of goalie crease
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          xmin = c(-6.5, 4, -7, -6.5, 4, -7),
          xmax = c(-4, 6.5, 7, -4, 6.5, 7),
          ymin =
            c(
              center_line + 89, center_line + 89, center_line + 96,
              center_line - 89, center_line - 89, center_line - 96
            ),
          ymax =
            c(
              center_line + 75, center_line + 75, center_line + 89,
              center_line - 75, center_line - 75, center_line - 89
            )
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "white"
    ),
    # # net color (curved sections)
    ggforce::geom_circle(
      tibble::tibble(
        x = c(-net_curve_center_x, net_curve_center_x, -net_curve_center_x, net_curve_center_x),
        y =
          c(
            center_line + 89 + abs(net_curve_center_y), center_line + 89 + abs(net_curve_center_y),
            center_line - 89 - abs(net_curve_center_y), center_line - 89 - abs(net_curve_center_y)
          ),
        r = net_radius
      ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      linewidth = 0,
      color = "lightgrey",
      fill = "lightgrey"
    ),
    # net color (straight sections)
    ggplot2::geom_polygon(
      data =
        tibble::tibble(
          x =
            c(
              # left post
              -3,
              # right post
              3,
              right_goal_joint_front_x,
              right_goal_joint_back_x,
              left_goal_joint_back_x,
              left_goal_joint_front_x
            ),
          y = c(
            center_line + 89,
            center_line + 89,
            center_line + 89 + abs(goal_joint_front_y),
            center_line + 89 + abs(goal_joint_back_y),
            center_line + 89 + abs(goal_joint_back_y),
            center_line + 89 + abs(goal_joint_front_y)
          )
        ),
      mapping = ggplot2::aes(x = x, y = y),
      fill = "lightgrey"
    ),
    ggplot2::geom_polygon(
      data =
        tibble::tibble(
          x =
            c(
              # left post
              -3,
              # right post
              3,
              right_goal_joint_front_x,
              right_goal_joint_back_x,
              left_goal_joint_back_x,
              left_goal_joint_front_x
            ),
          y = c(
            center_line - 89,
            center_line - 89,
            center_line - 89 - abs(goal_joint_front_y),
            center_line - 89 - abs(goal_joint_back_y),
            center_line - 89 - abs(goal_joint_back_y),
            center_line - 89 - abs(goal_joint_front_y)
          )
        ),
      mapping = ggplot2::aes(x = x, y = y),
      fill = "lightgrey"
    ),
    # goal straight lines
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          x =
            c(
              -3, left_goal_joint_back_x, 3,
              -3, left_goal_joint_back_x, 3
            ),
          xend =
            c(
              left_goal_joint_front_x,
              right_goal_joint_back_x,
              right_goal_joint_front_x,
              left_goal_joint_front_x,
              right_goal_joint_back_x,
              right_goal_joint_front_x
            ),
          y =
            c(
              center_line + 89, center_line + 89 + abs(goal_joint_back_y), center_line + 89,
              center_line - 89, center_line - 89 - abs(goal_joint_back_y), center_line - 89
            ),
          yend =
            c(
              center_line + 89 + abs(goal_joint_front_y),
              center_line + 89 + abs(goal_joint_back_y),
              center_line + 89 + abs(goal_joint_front_y),
              center_line - 89 - abs(goal_joint_front_y),
              center_line - 89 - abs(goal_joint_back_y),
              center_line - 89 - abs(goal_joint_front_y)
            )
        ),
      mapping = ggplot2::aes(
        x = x, xend = xend, y = y, yend = yend
      ),
      color = "black"
    ),
    # goal curves
    ggforce::geom_arc(
      data =
        tibble::tibble(
          x =
            c(
              -net_curve_center_x, net_curve_center_x,
              -net_curve_center_x, net_curve_center_x
            ),
          y =
            c(
              center_line - 89 - abs(net_curve_center_y),
              center_line - 89 - abs(net_curve_center_y),
              center_line + 89 + abs(net_curve_center_y),
              center_line + 89 + abs(net_curve_center_y)
            ),
          r = net_radius,
          start =
            c(
              pi,
              acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) +
                atan(net_post_x_diff / net_curve_center_y),
              pi +
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) +
                atan(net_post_x_diff / net_curve_center_y),
              0
              # pi,
              # acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) +
                # atan(net_post_x_diff / net_curve_center_y)
            ),
          end =
            c(
              (2*pi) -
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
                atan(net_post_x_diff / net_curve_center_y),
              pi,
              2*pi,
              (pi / 2) +
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) +
                atan(net_post_x_diff / net_curve_center_y)
              # (2*pi) -
              #   acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
              #   atan(net_post_x_diff / net_curve_center_y),
              # acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
              #   atan(net_post_x_diff / net_curve_center_y)
            )
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
      color = "black"
    ),
    # # red lines
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          x =
            c(
              # center line border
              -42.5, -42.5,
              # inside hash marks (horizontal)
              -22 - (23/6), -22 - (23/6), -22 + (5/6), -22 + (5/6),
              22 - (23/6), 22 - (23/6),  22 + (5/6), 22 + (5/6),
              -22 - (23/6), -22 - (23/6), -22 + (5/6), -22 + (5/6),
              22 - (23/6), 22 - (23/6),  22 + (5/6), 22 + (5/6),
              # inside hash marks (vertical)
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              # # outside hashmarks
              (-22 - (sqrt(15**2 - 2.875**2))), (-22 - sqrt(15**2 - 2.875**2)),
              (-22 + (sqrt(15**2 - 2.875**2))), (-22 + sqrt(15**2 - 2.875**2)),
              (22 - (sqrt(15**2 - 2.875**2))), (22 - sqrt(15**2 - 2.875**2)),
              (22 + (sqrt(15**2 - 2.875**2))), (22 + sqrt(15**2 - 2.875**2)),
              (-22 - (sqrt(15**2 - 2.875**2))), (-22 - sqrt(15**2 - 2.875**2)),
              (-22 + (sqrt(15**2 - 2.875**2))), (-22 + sqrt(15**2 - 2.875**2)),
              (22 - (sqrt(15**2 - 2.875**2))), (22 - sqrt(15**2 - 2.875**2)),
              (22 + (sqrt(15**2 - 2.875**2))), (22 + sqrt(15**2 - 2.875**2)),
              # sides of goalie crease
              -4, 4, -4, 4,
              # goal line
              -sqrt(28**2 - 17**2) - 14.5, -sqrt(28**2 - 17**2) - 14.5,
              # trapezoid
              -11, 11, -11, 11,
              # crease hash marks
              -4, 4, -4, 4
            ),
          xend =
            c(
              # center line border
              42.5, 42.5,
              # inside hash marks (horizontal)
              -22 - (5/6), -22 - (5/6), -22 + (23/6), -22 + (23/6),
              22 - (5/6), 22 - (5/6),  22 + (23/6), 22 + (23/6),
              -22 - (5/6), -22 - (5/6), -22 + (23/6), -22 + (23/6),
              22 - (5/6), 22 - (5/6),  22 + (23/6), 22 + (23/6),
              # inside hash marks (vertical)
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              # # outside hashmarks
              (-22 - (sqrt(15**2 - 2.875**2)) - 2), (-22 - sqrt(15**2 - 2.875**2) - 2),
              (-22 + (sqrt(15**2 - 2.875**2)) + 2), (-22 + sqrt(15**2 - 2.875**2) + 2),
              (22 - (sqrt(15**2 - 2.875**2)) - 2), (22 - sqrt(15**2 - 2.875**2) - 2),
              (22 + (sqrt(15**2 - 2.875**2)) + 2), (22 + sqrt(15**2 - 2.875**2) + 2),
              (-22 - (sqrt(15**2 - 2.875**2)) - 2), (-22 - sqrt(15**2 - 2.875**2) - 2),
              (-22 + (sqrt(15**2 - 2.875**2)) + 2), (-22 + sqrt(15**2 - 2.875**2) + 2),
              (22 - (sqrt(15**2 - 2.875**2)) - 2), (22 - sqrt(15**2 - 2.875**2) - 2),
              (22 + (sqrt(15**2 - 2.875**2)) + 2), (22 + sqrt(15**2 - 2.875**2) + 2),
              # sides of goalie crease
              -4, 4, -4, 4,
              # goal line
              sqrt(28**2 - 17**2) + 14.5, sqrt(28**2 - 17**2) + 14.5,
              # trapezoid
              -14, 14, -14, 14,
              # crease hash marks
              -43/12, 43/12, -43/12, 43/12
            ),
          y =
            c(
              # center line border
              center_line + 0.5, center_line - 0.5,
              # inside hash marks (horizontal)
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              # inside hash marks (vertical)
              center_line + 71, center_line + 63, center_line + 71, center_line + 63,
              center_line + 63, center_line + 71, center_line + 63, center_line + 71,
              center_line - 71, center_line - 63, center_line - 71, center_line - 63,
              center_line - 63, center_line - 71, center_line - 63, center_line - 71,
              # outside hashmarks
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              # sides of goalie crease
              center_line + (89 - sqrt(6**2 - 4**2)), center_line + (89 - sqrt(6**2 - 4**2)),
              center_line - (89 - sqrt(6**2 - 4**2)), center_line - (89 - sqrt(6**2 - 4**2)),
              # goal line
              center_line + 89, center_line - 89,
              # trapezoid
              center_line + 89, center_line + 89, center_line - 89, center_line - 89,
              # crease hash marks
              center_line + 85, center_line + 85, center_line - 85, center_line - 85
            ),
          yend =
            c(
              # center line border
              center_line + 0.5, center_line - 0.5,
              # inside hash marks (horizontal)
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              # inside hash marks (vertical)
              center_line + 75, center_line + 67, center_line + 75, center_line + 67,
              center_line + 67, center_line + 75, center_line + 67, center_line + 75,
              center_line - 75, center_line - 67, center_line - 75, center_line - 67,
              center_line - 67, center_line - 75, center_line - 67, center_line - 75,
              # # outside hashmarks
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              # sides of goalie crease
              center_line + 89, center_line + 89, center_line - 89, center_line - 89,
              # goal line
              center_line + 89, center_line - 89,
              # # trapezoid
              center_line + 100, center_line + 100, center_line - 100, center_line - 100,
              # crease hash marks
              center_line + 85, center_line + 85, center_line - 85, center_line - 85
            )
        ),
      mapping =
        ggplot2::aes(
          x = x, y = y, xend = xend, yend = yend
        ),
      color = "#C8102E"
    ),
    # rink straight borders
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          x = c(-42.5, 42.5, -14.5, -14.5),
          xend = c(-42.5, 42.5, 14.5, 14.5),
          y = c(center_line + 72, center_line + 72, center_line - 100, center_line + 100),
          yend = c(center_line - 72, center_line - 72, center_line - 100, center_line + 100)
        ),
      mapping = ggplot2::aes(
        x = x, xend = xend, y = y, yend = yend
      ),
      color = "black",
      linewidth = 1
    ),
    # rink corners
    ggforce::geom_arc(
      data =
        tibble::tibble(
          x = c(14.5, 14.5, -14.5, -14.5),
          y = c(center_line + 72, center_line - 72, center_line - 72, center_line + 72),
          r = 28,
          start =
            c(
              0,
              pi / 2,
              pi,
              3 * pi / 2
            ),
          end =
            c(
              pi / 2,
              pi,
              3 * pi / 2,
              2 * pi
            )
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
      color = "black",
      linewidth = 1
    ),
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims,
      expand = F
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



full_rink_markings_horiz <- function(legend_position = "none", center_line = 0, big_net = F) {
  net_radius <- ifelse(big_net, 20/12, 18/12)
  net_depth <- ifelse(big_net, 44/12, 40/12)
  net_max_width <- ifelse(big_net, 96/12, 88/12)

  net_curve_center_x <- (net_max_width / 2) - net_radius
  net_curve_center_y <- net_depth - net_radius
  net_post_x_diff = 3 - net_curve_center_x

  right_goal_joint_front_x <-
    (net_curve_center_x) +
    sqrt(
      net_radius**2 -
        (
          sin(
            (pi / 2) -
              acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
              atan(net_post_x_diff / net_curve_center_y)
          ) *
            net_radius
        )**2
    )
  left_goal_joint_front_x <-
    (-net_curve_center_x) -
    sqrt(
      net_radius**2 -
        (
          sin(
            (pi / 2) -
              acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
              atan(net_post_x_diff / net_curve_center_y)
          ) *
            net_radius
        )**2
    )
  goal_joint_front_y <-
    (-net_curve_center_y) + (
      sin(
        (pi / 2) -
          acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)) -
          atan(net_post_x_diff / net_curve_center_y)
      ) *
        net_radius
    )
  right_goal_joint_back_x <- net_curve_center_x
  left_goal_joint_back_x <- -net_curve_center_x
  goal_joint_back_y <- -net_depth

  xlims <- c(center_line - 100, center_line + 100)
  ylims <- c(-42.5, 42.5)

  list(
    # center line
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          ymin = -42.5,
          ymax = 42.5,
          xmin = center_line - 0.5,
          xmax = center_line + 0.5
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "#C8102E"
    ),
    # white markings on center line
    ggplot2::geom_rect(
      data = tibble::tibble(
        ymin = seq(-42.5, 42.5, by = 2),
        ymax = seq(-41.5, 42.5, by = 2),
        xmin = center_line - 0.5,
        xmax = center_line + 0.5
      ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "white"
    ),
    # blue lines
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          ymin = -42.5,
          ymax = 42.5,
          xmin = c(center_line + 24, center_line - 25),
          xmax = c(center_line + 25, center_line - 24)
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "#0033A0"
    ),
    # faceoff dot red centers
    ggforce::geom_circle(
      data =
        tibble::tibble(
          y = c(-22, 22, -22, 22, -22, 22, -22, 22),
          x = c(
            center_line + 20, center_line + 20, center_line + 69, center_line + 69,
            center_line - 20, center_line - 20, center_line - 69, center_line - 69
          ),
          r = 1
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#C8102E",
      color = "#C8102E"
    ),
    # faceoff dot white spaces
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          ymin = c(-23, -23, 21, 21, -23, -23, 21, 21, -23, -23, 21, 21, -23, -23, 21, 21),
          ymax = c(21, 21, 23, 23, 21, 21, 23, 23, 21, 21, 23, 23, 21, 21, 23, 23),
          xmin =
            c(
              center_line + 68, center_line + 69.75, center_line + 68, center_line + 69.75,
              center_line - 68.25, center_line - 70, center_line - 68.25, center_line - 70,
              # 20.75, 19, 20.75, 19,
              center_line + 19, center_line + 20.75, center_line + 19, center_line + 20.75,
              center_line - 19.25, center_line - 21, center_line - 19.25, center_line - 21
              # 69.75, 68, 69.75, 68
            ),
          xmax =
            c(
              center_line + 68.25, center_line + 70, center_line + 68.25, center_line + 70,
              center_line - 68, center_line - 69.75, center_line - 68, center_line - 69.75,
              # 21, 19.25, 21, 19.25,
              center_line + 19.25, center_line + 21, center_line + 19.25, center_line + 21,
              center_line - 19, center_line - 20.75, center_line - 19, center_line - 20.75
              # 70, 68.25, 70, 68.25
            )
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "white"
    ),
    # faceoff dot outside lines
    ggforce::geom_circle(
      data =
        tibble::tibble(
          y = c(-22, 22, -22, 22, -22, 22, -22, 22),
          x = c(
            center_line + 20, center_line + 20, center_line + 69, center_line + 69,
            center_line - 20, center_line - 20, center_line - 69, center_line - 69
          ),
          r = 1
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#C8102E",
      color = "#C8102E",
      alpha = 0
    ),
    # center faceoff dot
    ggforce::geom_circle(
      data =
        tibble::tibble(
          y = 0,
          x = center_line,
          r = 0.5
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#0033A0",
      color = "white"
    ),
    # center_circle
    ggforce::geom_circle(
      data =
        tibble::tibble(
          y = 0,
          x = center_line,
          r = 15
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#0033A0",
      color = "#0033A0",
      alpha = 0
    ),
    # faceoff circles
    ggforce::geom_circle(
      data =
        tibble::tibble(
          y = c(-22, 22, -22, 22),
          x = c(center_line + 69, center_line + 69, center_line - 69, center_line - 69),
          r = 15
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#C8102E",
      color = "#C8102E",
      alpha = 0
    ),
    # goalie crease
    ggforce::geom_circle(
      data =
        tibble::tibble(
          y = 0,
          x = c(center_line + 89, center_line - 89),
          r = 6
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      fill = "#41B6E6",
      color = "#C8102E"
    ),
    # white areas to cover edges of goalie crease
    ggplot2::geom_rect(
      data =
        tibble::tibble(
          ymin = c(-6.5, 4, -7, -6.5, 4, -7),
          ymax = c(-4, 6.5, 7, -4, 6.5, 7),
          xmin =
            c(
              center_line + 89, center_line + 89, center_line + 96,
              center_line - 89, center_line - 89, center_line - 96
            ),
          xmax =
            c(
              center_line + 75, center_line + 75, center_line + 89,
              center_line - 75, center_line - 75, center_line - 89
            )
        ),
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ),
      fill = "white"
    ),
    # # net color (curved sections)
    ggforce::geom_circle(
      tibble::tibble(
        y = c(-net_curve_center_x, net_curve_center_x, -net_curve_center_x, net_curve_center_x),
        x =
          c(
            center_line + 89 + abs(net_curve_center_y), center_line + 89 + abs(net_curve_center_y),
            center_line - 89 - abs(net_curve_center_y), center_line - 89 - abs(net_curve_center_y)
          ),
        r = net_radius
      ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r),
      linewidth = 0,
      color = "lightgrey",
      fill = "lightgrey"
    ),
    # net color (straight sections)
    ggplot2::geom_polygon(
      data =
        tibble::tibble(
          y =
            c(
              # left post
              -3,
              # right post
              3,
              right_goal_joint_front_x,
              right_goal_joint_back_x,
              left_goal_joint_back_x,
              left_goal_joint_front_x
            ),
          x = c(
            center_line + 89,
            center_line + 89,
            center_line + 89 + abs(goal_joint_front_y),
            center_line + 89 + abs(goal_joint_back_y),
            center_line + 89 + abs(goal_joint_back_y),
            center_line + 89 + abs(goal_joint_front_y)
          )
        ),
      mapping = ggplot2::aes(x = x, y = y),
      fill = "lightgrey"
    ),
    ggplot2::geom_polygon(
      data =
        tibble::tibble(
          y =
            c(
              # left post
              -3,
              # right post
              3,
              right_goal_joint_front_x,
              right_goal_joint_back_x,
              left_goal_joint_back_x,
              left_goal_joint_front_x
            ),
          x = c(
            center_line - 89,
            center_line - 89,
            center_line - 89 - abs(goal_joint_front_y),
            center_line - 89 - abs(goal_joint_back_y),
            center_line - 89 - abs(goal_joint_back_y),
            center_line - 89 - abs(goal_joint_front_y)
          )
        ),
      mapping = ggplot2::aes(x = x, y = y),
      fill = "lightgrey"
    ),
    # goal straight lines
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          y =
            c(
              -3, left_goal_joint_back_x, 3,
              -3, left_goal_joint_back_x, 3
            ),
          yend =
            c(
              left_goal_joint_front_x,
              right_goal_joint_back_x,
              right_goal_joint_front_x,
              left_goal_joint_front_x,
              right_goal_joint_back_x,
              right_goal_joint_front_x
            ),
          x =
            c(
              center_line + 89, center_line + 89 + abs(goal_joint_back_y), center_line + 89,
              center_line - 89, center_line - 89 - abs(goal_joint_back_y), center_line - 89
            ),
          xend =
            c(
              center_line + 89 + abs(goal_joint_front_y),
              center_line + 89 + abs(goal_joint_back_y),
              center_line + 89 + abs(goal_joint_front_y),
              center_line - 89 - abs(goal_joint_front_y),
              center_line - 89 - abs(goal_joint_back_y),
              center_line - 89 - abs(goal_joint_front_y)
            )
        ),
      mapping = ggplot2::aes(
        x = x, xend = xend, y = y, yend = yend
      ),
      color = "black"
    ),
    # goal curves
    ggforce::geom_arc(
      data =
        tibble::tibble(
          y =
            c(
              -net_curve_center_x,
              net_curve_center_x,
              -net_curve_center_x,
              net_curve_center_x
            ),
          x =
            c(
              center_line - 89 - abs(net_curve_center_y),
              center_line - 89 - abs(net_curve_center_y),
              center_line + 89 + abs(net_curve_center_y),
              center_line + 89 + abs(net_curve_center_y)
            ),
          r = net_radius,
          start =
            c(
              pi -
                # atan(net_post_x_diff / net_curve_center_y) -
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)),
              3 * pi / 2,
              pi / 2,
              0 -
                # atan(net_post_x_diff / net_curve_center_y) -
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2))
            ),
          end =
            c(
              3 * pi / 2,
              2 * pi -
                atan(net_post_x_diff / net_curve_center_y) +
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)),
              pi -
                atan(net_post_x_diff / net_curve_center_y) +
                acos(net_radius / sqrt(net_curve_center_y**2 + net_post_x_diff**2)),
              pi / 2
            )
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
      color = "black"
    ),
    # red lines
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          y =
            c(
              # center line border
              -42.5, -42.5,
              # inside hash marks (horizontal)
              -22 - (23/6), -22 - (23/6), -22 + (5/6), -22 + (5/6),
              22 - (23/6), 22 - (23/6),  22 + (5/6), 22 + (5/6),
              -22 - (23/6), -22 - (23/6), -22 + (5/6), -22 + (5/6),
              22 - (23/6), 22 - (23/6),  22 + (5/6), 22 + (5/6),
              # inside hash marks (vertical)
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              # # outside hashmarks
              (-22 - (sqrt(15**2 - 2.875**2))), (-22 - sqrt(15**2 - 2.875**2)),
              (-22 + (sqrt(15**2 - 2.875**2))), (-22 + sqrt(15**2 - 2.875**2)),
              (22 - (sqrt(15**2 - 2.875**2))), (22 - sqrt(15**2 - 2.875**2)),
              (22 + (sqrt(15**2 - 2.875**2))), (22 + sqrt(15**2 - 2.875**2)),
              (-22 - (sqrt(15**2 - 2.875**2))), (-22 - sqrt(15**2 - 2.875**2)),
              (-22 + (sqrt(15**2 - 2.875**2))), (-22 + sqrt(15**2 - 2.875**2)),
              (22 - (sqrt(15**2 - 2.875**2))), (22 - sqrt(15**2 - 2.875**2)),
              (22 + (sqrt(15**2 - 2.875**2))), (22 + sqrt(15**2 - 2.875**2)),
              # sides of goalie crease
              -4, 4, -4, 4,
              # goal line
              -sqrt(28**2 - 17**2) - 14.5, -sqrt(28**2 - 17**2) - 14.5,
              # trapezoid
              -11, 11, -11, 11,
              # crease hash marks
              -4, 4, -4, 4
            ),
          yend =
            c(
              # center line border
              42.5, 42.5,
              # inside hash marks (horizontal)
              -22 - (5/6), -22 - (5/6), -22 + (23/6), -22 + (23/6),
              22 - (5/6), 22 - (5/6),  22 + (23/6), 22 + (23/6),
              -22 - (5/6), -22 - (5/6), -22 + (23/6), -22 + (23/6),
              22 - (5/6), 22 - (5/6),  22 + (23/6), 22 + (23/6),
              # inside hash marks (vertical)
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              -22 - (5/6), -22 + (5/6), 22 - (5/6), 22 + (5/6), -22 - (5/6), -22 + (5/6),  22 - (5/6), 22 + (5/6),
              # # outside hashmarks
              (-22 - (sqrt(15**2 - 2.875**2)) - 2), (-22 - sqrt(15**2 - 2.875**2) - 2),
              (-22 + (sqrt(15**2 - 2.875**2)) + 2), (-22 + sqrt(15**2 - 2.875**2) + 2),
              (22 - (sqrt(15**2 - 2.875**2)) - 2), (22 - sqrt(15**2 - 2.875**2) - 2),
              (22 + (sqrt(15**2 - 2.875**2)) + 2), (22 + sqrt(15**2 - 2.875**2) + 2),
              (-22 - (sqrt(15**2 - 2.875**2)) - 2), (-22 - sqrt(15**2 - 2.875**2) - 2),
              (-22 + (sqrt(15**2 - 2.875**2)) + 2), (-22 + sqrt(15**2 - 2.875**2) + 2),
              (22 - (sqrt(15**2 - 2.875**2)) - 2), (22 - sqrt(15**2 - 2.875**2) - 2),
              (22 + (sqrt(15**2 - 2.875**2)) + 2), (22 + sqrt(15**2 - 2.875**2) + 2),
              # sides of goalie crease
              -4, 4, -4, 4,
              # goal line
              sqrt(28**2 - 17**2) + 14.5, sqrt(28**2 - 17**2) + 14.5,
              # trapezoid
              -14, 14, -14, 14,
              # crease hash marks
              -43/12, 43/12, -43/12, 43/12
            ),
          x =
            c(
              # center line border
              center_line + 0.5, center_line - 0.5,
              # inside hash marks (horizontal)
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              # inside hash marks (vertical)
              center_line + 71, center_line + 63, center_line + 71, center_line + 63,
              center_line + 63, center_line + 71, center_line + 63, center_line + 71,
              center_line - 71, center_line - 63, center_line - 71, center_line - 63,
              center_line - 63, center_line - 71, center_line - 63, center_line - 71,
              # outside hashmarks
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              # sides of goalie crease
              center_line + (89 - sqrt(6**2 - 4**2)), center_line + (89 - sqrt(6**2 - 4**2)),
              center_line - (89 - sqrt(6**2 - 4**2)), center_line - (89 - sqrt(6**2 - 4**2)),
              # goal line
              center_line + 89, center_line - 89,
              # trapezoid
              center_line + 89, center_line + 89, center_line - 89, center_line - 89,
              # crease hash marks
              center_line + 85, center_line + 85, center_line - 85, center_line - 85
            ),
          xend =
            c(
              # center line border
              center_line + 0.5, center_line - 0.5,
              # inside hash marks (horizontal)
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line + 67, center_line + 71, center_line + 67, center_line + 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              center_line - 67, center_line - 71, center_line - 67, center_line - 71,
              # inside hash marks (vertical)
              center_line + 75, center_line + 67, center_line + 75, center_line + 67,
              center_line + 67, center_line + 75, center_line + 67, center_line + 75,
              center_line - 75, center_line - 67, center_line - 75, center_line - 67,
              center_line - 67, center_line - 75, center_line - 67, center_line - 75,
              # # outside hashmarks
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line + 66.125, center_line + 71.875, center_line + 66.125, center_line + 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              center_line - 66.125, center_line - 71.875, center_line - 66.125, center_line - 71.875,
              # sides of goalie crease
              center_line + 89, center_line + 89, center_line - 89, center_line - 89,
              # goal line
              center_line + 89, center_line - 89,
              # # trapezoid
              center_line + 100, center_line + 100, center_line - 100, center_line - 100,
              # crease hash marks
              center_line + 85, center_line + 85, center_line - 85, center_line - 85
            )
        ),
      mapping =
        ggplot2::aes(
          x = x, y = y, xend = xend, yend = yend
        ),
      color = "#C8102E"
    ),
    # rink straight borders
    ggplot2::geom_segment(
      data =
        tibble::tibble(
          y = c(-42.5, 42.5, -14.5, -14.5),
          yend = c(-42.5, 42.5, 14.5, 14.5),
          x = c(center_line + 72, center_line + 72, center_line - 100, center_line + 100),
          xend = c(center_line - 72, center_line - 72, center_line - 100, center_line + 100)
        ),
      mapping = ggplot2::aes(
        x = x, xend = xend, y = y, yend = yend
      ),
      color = "black",
      linewidth = 1
    ),
    # rink corners
    ggforce::geom_arc(
      data =
        tibble::tibble(
          y = c(14.5, 14.5, -14.5, -14.5),
          x = c(center_line + 72, center_line - 72, center_line - 72, center_line + 72),
          r = 28,
          start =
            c(
              0,
              3 * pi / 2,
              pi,
              pi / 2
            ),
          end =
            c(
              pi / 2,
              2 * pi,
              3 * pi / 2,
              pi
            )
        ),
      mapping = ggplot2::aes(x0 = x, y0 = y, r = r, start = start, end = end),
      color = "black",
      linewidth = 1
    ),
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims,
      expand = F
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


