schedule_2026 <-
  nhlPbpScrapeR::get_full_season_schedule_api(20252026)

nhl_db_con |>
  odbc::dbAppendTable(
    name = "games",
    value = schedule_2026 |>
      dplyr::filter(game_date < lubridate::today()) |>
      dplyr::arrange(game_date, game_id)
  )

edge_goals <-
  nhl_db_con |>
  odbc::dbGetQuery(
    "select game_id, session, game_date from games where season >= 20232024 and session >= 2 order by game_id"
  ) |>
  tibble::tibble() |>
  dplyr::filter(game_date < lubridate::today()) |>
  # head(1) |>
  dplyr::mutate(
    replay_urls =
      purrr::map(
        game_id,
        function(game_id) {
          print(game_id)
          Sys.sleep(0.6)

          "https://api-web.nhle.com/v1/gamecenter/{game_id}/landing" |>
            glue::glue() |>
            httr::GET() |>
            httr::content(type = "text/json", encoding = "UTF-8") |>
            jsonlite::fromJSON() |>
            purrr::pluck("summary", "scoring") |>
            tidyr::unnest(c(periodDescriptor, goals)) |>
            tidyr::unnest_wider(teamAbbrev) |>
            dplyr::select(
              tidyselect::any_of(
                c(
                  "number",
                  "timeInPeriod",
                  "situationCode",
                  "strength",
                  "default",
                  "playerId",
                  "goalModifier",
                  "pptReplayUrl",
                  "homeTeamDefendingSide",
                  "isHome"
                )
              )
            )
        }
      )
  ) |>
  tidyr::unnest(replay_urls) |>
  dplyr::transmute(
    game_id,
    game_period = number,
    timeInPeriod,
    game_seconds =
      timeInPeriod |>
      purrr::map2_int(
        game_period,
        function(tm, per) {
          tm |>
            stringr::str_split(":") |>
            purrr::flatten_chr() |>
            as.integer() |>
            magrittr::multiply_by(c(60, 1)) |>
            sum() |>
            magrittr::add((per - 1) * 1200)
        }
      ),
    event_team = default,
    event_player_1 = playerId,
    home_team_def_zone = homeTeamDefendingSide,
    is_home = isHome,
    situationCode,
    strength,
    goalModifier,
    pptReplayUrl
  )

# all_goals <-

time_before_goals_5v5 <-
  edge_goals |>
  dplyr::filter(!is.na(pptReplayUrl), situationCode == "1551", goalModifier == "none") |>
  # head() |>
  dplyr::mutate(
    filepath =
      "../scraper_testing/edge_animations/{game_id}_{pptReplayUrl |> stringr::str_extract('ev\\\\d+')}.csv" |>
      glue::glue(),
    side =
      ifelse(home_team_def_zone == "left", 1, -1) *
      ifelse(is_home, 1, -1),
    time_before_goal =
      purrr::map2(
        filepath,
        side,
        function(f, s) {
          anim <-
            f |>
            readr::read_csv(
              col_select =
                c(timeStamp, teamAbbrev, coords_x, coords_y, playerId),
              col_types =
                readr::cols(
                  teamAbbrev = readr::col_character(),
                  .default = readr::col_double()
                )
            )

          puck <-
            anim |>
            dplyr::filter(playerId == 0) |>
            dplyr::transmute(
              timeStamp,
              puck_x = coords_x,
              puck_y = coords_y,
              puck_in_zone = ((coords_x * s) >= 25),
              puck_in_zone_instance =
                cumsum(puck_in_zone & tidyr::replace_na(dplyr::lag(puck_in_zone) == F, F)),
              puck_in_net =
                cumsum(
                  (abs(coords_x) >= 88.8 & abs(coords_x) <= (89.2 + (40/12))) &
                    abs(coords_y) <= 3.2
                ) > 0
            ) |>
            dplyr::filter(!puck_in_net)

          if (nrow(puck) == 0) {
            tibble::tibble()
          } else {
            puck |>
              dplyr::summarise(
                total_time_before_goal = max(timeStamp) - min(timeStamp) + 1,
                time_in_zone_before_goal =
                  max(timeStamp) -
                  min(
                    timeStamp *
                      ifelse(
                        puck_in_zone_instance == max(puck_in_zone_instance),
                        1,
                        2
                      )
                  ) +
                  1
              )
          #   anim |>
          #     dplyr::filter(playerId != 0) |>
          #     dplyr::inner_join(puck, by = "timeStamp") |>
          #     dplyr::group_by(timeStamp, puck_x, puck_y) |>
          #     dplyr::summarise(
          #       attackers_at_puck =
          #         sum(teamAbbrev == t & (abs((coords_x * s) - (puck_x * s)) <= 10)),
          #       # defenders_in_zone =
          #       #   sum(teamAbbrev != t & (coords_x * side) >= 25) - 1,
          #       defenders_below_puck =
          #         sum(teamAbbrev != t & ((coords_x * s) >= ((puck_x * s)))) - 1,
          #       .groups = "drop"
          #     ) |>
          #     # dplyr::filter(attackers_in_zone > 0) |>
          #     dplyr::mutate(
          #       puck_entry_timeStamp = timeStamp - min(timeStamp),
          #       player_entry_timeStamp = timeStamp - min(timeStamp * (ifelse(attackers_at_puck == 0, 2, 1)))
          #     ) |>
          #     dplyr::filter(puck_entry_timeStamp <= 9) |>
          #     dplyr::filter(player_entry_timeStamp <= 4) |>
          #     dplyr::summarise(
          #       attackers = round(weighted.mean(attackers_at_puck, (c(1:dplyr::n()) / 10)**0.5) |> tail(dplyr::n())),
          #       defenders = round(weighted.mean(defenders_below_puck, (c(1:dplyr::n()) / 10)**0.5) |> tail(dplyr::n())),
          #       rush = "{attackers}v{defenders}" |> glue::glue()
          #     )
          # }
          #
          #
          #
          # anim <-
          #   f |>
          #   readr::read_csv() |>
          #   dplyr::filter(playerId == 0) |>
          #   dplyr::mutate(
          #     puck_in_net =
          #       playerId == 0 & (
          #         (abs(coords_x) >= 88.8 & abs(coords_x) <= (89.2 + (40/12))) &
          #           abs(coords_y) <= 3.2
          #       ),
          #     side = s,
          #     puck_in_net = cumsum(puck_in_net) > 0
          #   )
          #
          # if (sum(anim$puck_in_net) == 0) {
          #   tibble::tibble()
          # } else {
          #   anim |>
          #     dplyr::filter(!puck_in_net) |>
          #     dplyr::mutate(
          #       puck_in_zone = playerId == 0 & ((coords_x * side) >= 25),
          #       in_zone_instance = cumsum(!puck_in_zone),
          #       in_zone_timestamp = ifelse(in_zone_instance == max(in_zone_instance) & puck_in_zone, timeStamp, NA_integer_)
          #     ) |>
          #     # dplyr::filter(in_zone_inst)
          #     # View()
          #     dplyr::summarise(
          #       total_time_before_goal = range(timeStamp) |> magrittr::multiply_by(c(-1, 1)) |> sum() + 1,
          #       time_in_zone_before_goal =
          #         c(
          #           min(in_zone_timestamp, na.rm = T),
          #           max(timeStamp)
          #         ) |>
          #         magrittr::multiply_by(c(-1, 1)) |>
          #         sum() + 1
          #     )
          }
        }
      )
  ) |>
  tidyr::unnest(time_before_goal)

time_before_goals_5v5 |>
  dplyr::filter(total_time_before_goal >= 60) |>
  dplyr::mutate(
    season = game_id %/% 1000000,
    session = (game_id %/% 10000) %% 10,
    all_in_zone = total_time_before_goal == time_in_zone_before_goal,
    zone_time_before_goal =
      ifelse(all_in_zone, "Unknown", as.character(time_in_zone_before_goal)) |>
      factor(
        levels = c(as.character(0:115), "Unknown")
      )
  ) |>
  dplyr::group_by(
    season,
    session,
    zone_time_before_goal
  ) |>
  dplyr::tally() |>
  dplyr::full_join(
    tidyr::expand_grid(
      season = c(2023, 2024, 2025),
      session = c(2, 3),
      zone_time_before_goal =
        c(as.character(0:115), "Unknown") |>
        factor(
          levels = c(as.character(0:115), "Unknown")
        )
    ) |>
      dplyr::filter(!(season == 2025 & session == 3))
  ) |>
  dplyr::group_by(season, session) |>
  dplyr::arrange(zone_time_before_goal) |>
  dplyr::mutate(
    perc = tidyr::replace_na(n, 0) / sum(n, na.rm = T),
    cumulative_share = cumsum(perc)
  ) |>
  dplyr::filter(
    zone_time_before_goal != "Unknown"
    # session == 3
  ) |>
  dplyr::filter(as.numeric(zone_time_before_goal) <= 70) |>
  dplyr::mutate(
    zone_time_before_goal = as.numeric(zone_time_before_goal),
    season =
      dplyr::case_when(
        season == 2023 ~ "2023-24",
        season == 2024 ~ "2024-25",
        season == 2025 ~ "2025-26"
      )
  ) |>
  # dplyr::filter(season == 2023, session == 2) |>
  # View()
  ggplot2::ggplot(ggplot2::aes(x = zone_time_before_goal, y = cumulative_share, color = season, linetype = factor(session))) +
  # ggplot2::facet_wrap(ggplot2::vars(season)) +
  # ggplot2::geom_col(width = 1, alpha = 0.2, position = "identity") +
  ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
  ggplot2::scale_x_continuous(
    "Seconds Since Zone Entry",
    breaks =
      seq(0, 70, by = 10),
      # as.character() |>
      # factor(levels = c(as.character(0:115), "Unknown")),
    labels = as.character(0:7)
  ) +
  ggplot2::scale_y_continuous("Cumulative Percent of 5-on-5 Goals", labels = scales::percent) +
  ggplot2::scale_color_viridis_d(
    "Season"
  ) +
  ggplot2::labs(
    title = "Cumulative Share of 5-on-5 Goals by Time Since Offensive Zone Entry",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")
  View()



# time_before_goals_5v5 <-
#   edge_goals |>
#   dplyr::filter(!is.na(pptReplayUrl), situationCode == "1551", goalModifier == "none") |>
#   # head() |>
#   dplyr::mutate(
#     filepath =
#       "../scraper_testing/edge_animations/{game_id}_{pptReplayUrl |> stringr::str_extract('ev\\\\d+')}.csv" |>
#       glue::glue(),
#     side =
#       ifelse(home_team_def_zone == "left", 1, -1) *
#       ifelse(is_home, 1, -1),
#     time_before_goal =
#       purrr::map2(
#         filepath,
#         side,
#         function(f, s) {
#           anim <-
#             f |>
#             readr::read_csv() |>
#             dplyr::filter(playerId == 0) |>
#             dplyr::mutate(
#               puck_in_net =
#                 playerId == 0 & (
#                   (abs(coords_x) >= 88.8 & abs(coords_x) <= (89.2 + (40/12))) &
#                     abs(coords_y) <= 3.2
#                 ),
#               side = s,
#               puck_in_net = cumsum(puck_in_net) > 0
#             )
#
#           if (sum(anim$puck_in_net) == 0) {
#             tibble::tibble()
#           } else {
#             anim |>
#               dplyr::filter(!puck_in_net) |>
#               dplyr::mutate(
#                 puck_in_zone = playerId == 0 & ((coords_x * side) >= 25),
#                 in_zone_instance = cumsum(!puck_in_zone),
#                 in_zone_timestamp = ifelse(in_zone_instance == max(in_zone_instance) & puck_in_zone, timeStamp, NA_integer_)
#               ) |>
#               # dplyr::filter(in_zone_inst)
#               # View()
#               dplyr::summarise(
#                 total_time_before_goal = range(timeStamp) |> magrittr::multiply_by(c(-1, 1)) |> sum() + 1,
#                 time_in_zone_before_goal =
#                   c(
#                     min(in_zone_timestamp, na.rm = T),
#                     max(timeStamp)
#                   ) |>
#                   magrittr::multiply_by(c(-1, 1)) |>
#                   sum() + 1
#               )
#           }
#         }
#       )
#   ) |>
#   tidyr::unnest(time_before_goal)

rush_goals_by_type <-
  time_before_goals_5v5 |>
  dplyr::filter(
    total_time_before_goal >= 60 &
    time_in_zone_before_goal <= 40 &
      time_in_zone_before_goal != total_time_before_goal
  ) |>
  dplyr::mutate(
    season = game_id %/% 1000000,
    session = (game_id %/% 10000) %% 10,
  ) |>
  # tail(1) |>
  dplyr::mutate(
    rush_type =
      purrr::pmap(
        list(
          f = filepath,
          t = event_team,
          s = side
        ),
        function(f, t, s) {
          anim <-
            f |>
            readr::read_csv(
              col_select =
                c(timeStamp, teamAbbrev, coords_x, coords_y, playerId),
              col_types =
                readr::cols(
                  teamAbbrev = readr::col_character(),
                  .default = readr::col_double()
                )
            )

          puck <-
            anim |>
            dplyr::filter(playerId == 0) |>
            dplyr::transmute(
              timeStamp,
              puck_x = coords_x,
              puck_y = coords_y,
              puck_in_zone = ((coords_x * s) >= 25),
              puck_in_zone_instance =
                cumsum(puck_in_zone & tidyr::replace_na(dplyr::lag(puck_in_zone) == F, F)),
              puck_in_net =
                cumsum(
                  (abs(coords_x) >= 88.8 & abs(coords_x) <= (89.2 + (40/12))) &
                    abs(coords_y) <= 3.2
                ) > 0
            ) |>
            dplyr::filter(
              puck_in_zone &
                !puck_in_net &
                puck_in_zone_instance == max(puck_in_zone_instance)
            ) |>
            dplyr::select(timeStamp, puck_x, puck_y)

          if (nrow(puck) == 0) {
            tibble::tibble()
          } else {
            anim |>
              dplyr::filter(playerId != 0) |>
              dplyr::inner_join(puck, by = "timeStamp") |>
              dplyr::group_by(timeStamp, puck_x, puck_y) |>
              dplyr::summarise(
                attackers_at_puck =
                  sum(teamAbbrev == t & (abs((coords_x * s) - (puck_x * s)) <= 10)),
                # defenders_in_zone =
                #   sum(teamAbbrev != t & (coords_x * side) >= 25) - 1,
                defenders_below_puck =
                  sum(teamAbbrev != t & ((coords_x * s) >= ((puck_x * s)))) - 1,
                defenders_below_puck = ifelse(defenders_below_puck == -1, 0, defenders_below_puck),
                .groups = "drop"
              ) |>
              # dplyr::filter(attackers_in_zone > 0) |>
              dplyr::mutate(
                puck_entry_timeStamp = timeStamp - min(timeStamp),
                player_entry_timeStamp = timeStamp - min(timeStamp * (ifelse(attackers_at_puck == 0, 2, 1)))
              ) |>
              dplyr::filter(puck_entry_timeStamp <= 9) |>
              dplyr::filter(player_entry_timeStamp <= 4) |>
              dplyr::summarise(
                attackers = round(weighted.mean(attackers_at_puck, (c(1:dplyr::n()) / 10)**0.5) |> tail(dplyr::n())),
                defenders = round(weighted.mean(defenders_below_puck, (c(1:dplyr::n()) / 10)**0.5) |> tail(dplyr::n())),
                rush = "{attackers}v{defenders}" |> glue::glue()
              )
          }
        }
      )
  ) |>
  tidyr::unnest(rush_type)
  # head(10) |>
  # View()

rush_goals_by_type |>
  dplyr::mutate(game_id = game_id %% 10000) |>
  dplyr::filter(session == 2) |>
  dplyr::mutate(
    rush =
      dplyr::case_when(
        rush %in% c("1v0", "2v0", "3v0", "4v0", "5v0") ~ "Breakaway",
        rush %in% c("2v1", "3v1", "3v2") ~ rush,
        T ~ "Other"
      )
  ) |>
  dplyr::group_by(
    season,
    session,
    players = attackers
  ) |>
  # dplyr::summarise(gp = max(game_id), n = dplyr::n(), n / gp)
  dplyr::tally() |>
  dplyr::mutate(perc = n / sum(n)) |>
  dplyr::group_by(season) |>
  dplyr::mutate(att_total = cumsum(perc)) |>
  dplyr::select(season, players, att_total) |>
  dplyr::full_join(
    rush_goals_by_type |>
      dplyr::filter(session == 2) |>
      dplyr::mutate(
        rush =
          dplyr::case_when(
            rush %in% c("1v0", "2v0", "3v0", "4v0", "5v0") ~ "Breakaway",
            rush %in% c("2v1", "3v1", "3v2") ~ rush,
            T ~ "Other"
          )
      ) |>
      dplyr::group_by(
        season,
        session,
        players = defenders
      ) |>
      dplyr::tally() |>
      dplyr::mutate(perc = n / sum(n)) |>
      dplyr::group_by(season) |>
      dplyr::mutate(def_total = cumsum(perc)) |>
      dplyr::select(season, players, def_total)
  ) |>
  dplyr::mutate(att_total = att_total |> tidyr::replace_na(1)) |>
  tidyr::pivot_longer(
    c(att_total, def_total)
  ) |>
  dplyr::mutate(group = "{season}{name}" |> glue::glue()) |>
  ggplot2::ggplot(ggplot2::aes(x = players, y = value, group = group, color = season, linetype = name)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_color_viridis_c()



rush_goals_by_type |>
  dplyr::mutate(game_id = game_id %% 10000) |>
  dplyr::filter(session == 2) |>
  dplyr::mutate(
    rush =
      dplyr::case_when(
        rush %in% c("1v0", "2v0", "3v0", "4v0", "5v0") ~ "Breakaway",
        rush %in% c("2v1", "3v1", "3v2") ~ rush,
        T ~ "Other"
      )
  ) |>
  dplyr::group_by(
    season,
    session,
    rush_adv = attackers - defenders
  ) |>
  dplyr::tally() |>
  dplyr::mutate(total = cumsum(n)) |>
  dplyr::group_by(season) |>
  dplyr::mutate(perc = total / sum(n)) |>
  ggplot2::ggplot(ggplot2::aes(x = rush_adv, y = perc, color = season, group = season)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_color_viridis_c()


time_before_goals_5v5 |>
  dplyr::filter(total_time_before_goal >= 60) |>
  dplyr::mutate(
    season = game_id %/% 1000000,
    session = (game_id %/% 10000) %% 10,
    all_in_zone = total_time_before_goal == time_in_zone_before_goal,
    zone_time_before_goal =
      ifelse(all_in_zone, "Unknown", as.character(time_in_zone_before_goal)) |>
      factor(
        levels = c(as.character(0:115), "Unknown")
      )
  ) |>
  dplyr::group_by(
    season,
    session,
    zone_time_before_goal
  ) |>
  dplyr::tally() |>
  dplyr::full_join(
    tidyr::expand_grid(
      season = c(2023, 2024, 2025),
      session = c(2, 3),
      zone_time_before_goal =
        c(as.character(0:115), "Unknown") |>
        factor(
          levels = c(as.character(0:115), "Unknown")
        )
    ) |>
      dplyr::filter(!(season == 2025 & session == 3))
  ) |>
  dplyr::group_by(season, session) |>
  dplyr::arrange(zone_time_before_goal) |>
  dplyr::mutate(
    perc = tidyr::replace_na(n, 0) / sum(n, na.rm = T),
    cumulative_share = cumsum(perc)
  ) |>
  dplyr::filter(
    zone_time_before_goal != "Unknown"
    # session == 3
  ) |>
  dplyr::filter(as.numeric(zone_time_before_goal) <= 70) |>
  dplyr::mutate(
    zone_time_before_goal = as.numeric(zone_time_before_goal),
    season =
      dplyr::case_when(
        season == 2023 ~ "2023-24",
        season == 2024 ~ "2024-25",
        season == 2025 ~ "2025-26"
      )
  ) |>
  # dplyr::filter(season == 2023, session == 2) |>
  # View()
  ggplot2::ggplot(ggplot2::aes(x = zone_time_before_goal, y = cumulative_share, color = season, linetype = factor(session))) +
  # ggplot2::facet_wrap(ggplot2::vars(season)) +
  # ggplot2::geom_col(width = 1, alpha = 0.2, position = "identity") +
  ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
  ggplot2::scale_x_continuous(
    "Seconds Since Zone Entry",
    breaks =
      seq(0, 70, by = 10),
      # as.character() |>
      # factor(levels = c(as.character(0:115), "Unknown")),
    labels = as.character(0:7)
  ) +
  ggplot2::scale_y_continuous("Cumulative Percent of 5-on-5 Goals Scored", labels = scales::percent) +
  ggplot2::scale_color_viridis_d(
    "Season"
  ) +
  ggplot2::scale_linetype_manual(
    "Session",
    values = c(`2` = "solid", `3` = "dashed"),
    labels = c("Regular", "Playoffs")
  ) +
  ggplot2::labs(
    title = "Cumulative Share of 5-on-5 Goals by Time Since Offensive Zone Entry",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")



time_before_goals_pp <-
  edge_goals |>
  dplyr::filter(!is.na(pptReplayUrl), strength == "pp", goalModifier == "none") |>
  # head() |>
  dplyr::mutate(
    filepath =
      "../scraper_testing/edge_animations/{game_id}_{pptReplayUrl |> stringr::str_extract('ev\\\\d+')}.csv" |>
      glue::glue(),
    side =
      ifelse(home_team_def_zone == "left", 1, -1) *
      ifelse(is_home, 1, -1),
    time_before_goal =
      purrr::map2(
        filepath,
        side,
        function(f, s) {
          anim <-
            f |>
            readr::read_csv(
              col_types =
                readr::cols(
                  teamAbbrev = readr::col_character(),
                  puck_in_net = readr::col_logical(),
                  .default = readr::col_double()
                )
            ) |>
            dplyr::filter(playerId == 0) |>
            dplyr::mutate(
              puck_in_net =
                playerId == 0 & (
                  (abs(coords_x) >= 88.8 & abs(coords_x) <= (89.2 + (40/12))) &
                    abs(coords_y) <= 3.2
                ),
              side = s,
              puck_in_net = cumsum(puck_in_net) > 0
            )

          if (sum(anim$puck_in_net) == 0) {
            tibble::tibble()
          } else {
            anim |>
              dplyr::filter(!puck_in_net) |>
              dplyr::mutate(
                puck_in_zone = playerId == 0 & ((coords_x * side) >= 25),
                in_zone_instance = cumsum(!puck_in_zone),
                in_zone_timestamp = ifelse(in_zone_instance == max(in_zone_instance) & puck_in_zone, timeStamp, NA_integer_)
              ) |>
              # dplyr::filter(in_zone_inst)
              # View()
              dplyr::summarise(
                total_time_before_goal = range(timeStamp) |> magrittr::multiply_by(c(-1, 1)) |> sum() + 1,
                time_in_zone_before_goal =
                  c(
                    min(in_zone_timestamp, na.rm = T),
                    max(timeStamp)
                  ) |>
                  magrittr::multiply_by(c(-1, 1)) |>
                  sum() + 1
              )
          }
        }
      )
  ) |>
  tidyr::unnest(time_before_goal)

time_before_goals_pp |>
  View()

time_before_goals_pp |>
  dplyr::filter(total_time_before_goal >= 60) |>
  dplyr::mutate(
    season = game_id %/% 1000000,
    session = (game_id %/% 10000) %% 10,
    all_in_zone = total_time_before_goal == time_in_zone_before_goal,
    zone_time_before_goal =
      ifelse(all_in_zone, "Unknown", as.character(time_in_zone_before_goal)) |>
      factor(
        levels = c(as.character(0:115), "Unknown")
      )
  ) |>
  dplyr::group_by(
    season,
    session,
    zone_time_before_goal
  ) |>
  dplyr::tally() |>
  dplyr::full_join(
    tidyr::expand_grid(
      season = c(2023, 2024, 2025),
      session = c(2, 3),
      zone_time_before_goal =
        c(as.character(0:115), "Unknown") |>
        factor(
          levels = c(as.character(0:115), "Unknown")
        )
    ) |>
      dplyr::filter(!(season == 2025 & session == 3))
  ) |>
  dplyr::group_by(season, session) |>
  dplyr::arrange(zone_time_before_goal) |>
  dplyr::mutate(
    perc = tidyr::replace_na(n, 0) / sum(n, na.rm = T),
    cumulative_share = cumsum(perc)
  ) |>
  dplyr::filter(
    zone_time_before_goal != "Unknown"
    # session == 3
  ) |>
  dplyr::filter(as.numeric(zone_time_before_goal) <= 70) |>
  dplyr::mutate(
    zone_time_before_goal = as.numeric(zone_time_before_goal),
    season =
      dplyr::case_when(
        season == 2023 ~ "2023-24",
        season == 2024 ~ "2024-25",
        season == 2025 ~ "2025-26"
      )
  ) |>
  # dplyr::filter(season == 2023, session == 2) |>
  # View()
  ggplot2::ggplot(ggplot2::aes(x = zone_time_before_goal, y = cumulative_share, color = season, linetype = factor(session))) +
  # ggplot2::facet_wrap(ggplot2::vars(season)) +
  # ggplot2::geom_col(width = 1, alpha = 0.2, position = "identity") +
  ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
  ggplot2::scale_x_continuous(
    "Seconds Since Zone Entry",
    breaks =
      seq(0, 70, by = 10),
    # as.character() |>
    # factor(levels = c(as.character(0:115), "Unknown")),
    labels = as.character(0:7)
  ) +
  ggplot2::scale_y_continuous("Cumulative Percent of Power Play Goals Scored", labels = scales::percent) +
  ggplot2::scale_color_viridis_d(
    "Season"
  ) +
  ggplot2::scale_linetype_manual(
    "Session",
    values = c(`2` = "solid", `3` = "dashed"),
    labels = c("Regular", "Playoffs")
  ) +
  ggplot2::labs(
    title = "Cumulative Share of Power Play Goals by Time Since Offensive Zone Entry",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")



time_before_goals_sh <-
  edge_goals |>
  dplyr::filter(!is.na(pptReplayUrl), strength == "sh", goalModifier == "none") |>
  # head() |>
  dplyr::mutate(
    filepath =
      "../scraper_testing/edge_animations/{game_id}_{pptReplayUrl |> stringr::str_extract('ev\\\\d+')}.csv" |>
      glue::glue(),
    side =
      ifelse(home_team_def_zone == "left", 1, -1) *
      ifelse(is_home, 1, -1),
    time_before_goal =
      purrr::map2(
        filepath,
        side,
        function(f, s) {
          anim <-
            f |>
            readr::read_csv(
              col_types =
                readr::cols(
                  teamAbbrev = readr::col_character(),
                  puck_in_net = readr::col_logical(),
                  .default = readr::col_double()
                )
            ) |>
            dplyr::filter(playerId == 0) |>
            dplyr::mutate(
              puck_in_net =
                playerId == 0 & (
                  (abs(coords_x) >= 88.8 & abs(coords_x) <= (89.2 + (40/12))) &
                    abs(coords_y) <= 3.2
                ),
              side = s,
              puck_in_net = cumsum(puck_in_net) > 0
            )

          if (sum(anim$puck_in_net) == 0) {
            tibble::tibble()
          } else {
            anim |>
              dplyr::filter(!puck_in_net) |>
              dplyr::mutate(
                puck_in_zone = playerId == 0 & ((coords_x * side) >= 25),
                in_zone_instance = cumsum(!puck_in_zone),
                in_zone_timestamp = ifelse(in_zone_instance == max(in_zone_instance) & puck_in_zone, timeStamp, NA_integer_)
              ) |>
              # dplyr::filter(in_zone_inst)
              # View()
              dplyr::summarise(
                total_time_before_goal = range(timeStamp) |> magrittr::multiply_by(c(-1, 1)) |> sum() + 1,
                time_in_zone_before_goal =
                  c(
                    min(in_zone_timestamp, na.rm = T),
                    max(timeStamp)
                  ) |>
                  magrittr::multiply_by(c(-1, 1)) |>
                  sum() + 1
              )
          }
        }
      )
  ) |>
  tidyr::unnest(time_before_goal)

time_before_goals_sh |>
  View()

time_before_goals_sh |>
  dplyr::filter(total_time_before_goal >= 60) |>
  dplyr::mutate(
    season = game_id %/% 1000000,
    session = (game_id %/% 10000) %% 10,
    all_in_zone = total_time_before_goal == time_in_zone_before_goal,
    zone_time_before_goal =
      ifelse(all_in_zone, "Unknown", as.character(time_in_zone_before_goal)) |>
      factor(
        levels = c(as.character(0:115), "Unknown")
      )
  ) |>
  dplyr::group_by(
    season,
    session,
    zone_time_before_goal
  ) |>
  dplyr::tally() |>
  dplyr::full_join(
    tidyr::expand_grid(
      season = c(2023, 2024, 2025),
      session = c(2, 3),
      zone_time_before_goal =
        c(as.character(0:115), "Unknown") |>
        factor(
          levels = c(as.character(0:115), "Unknown")
        )
    ) |>
      dplyr::filter(!(season == 2025 & session == 3))
  ) |>
  dplyr::group_by(season, session) |>
  dplyr::arrange(zone_time_before_goal) |>
  dplyr::mutate(
    perc = tidyr::replace_na(n, 0) / sum(n, na.rm = T),
    cumulative_share = cumsum(perc)
  ) |>
  dplyr::filter(
    zone_time_before_goal != "Unknown"
    # session == 3
  ) |>
  dplyr::filter(as.numeric(zone_time_before_goal) <= 70) |>
  dplyr::mutate(
    zone_time_before_goal = as.numeric(zone_time_before_goal),
    season =
      dplyr::case_when(
        season == 2023 ~ "2023-24",
        season == 2024 ~ "2024-25",
        season == 2025 ~ "2025-26"
      )
  ) |>
  # dplyr::filter(season == 2023, session == 2) |>
  # View()
  ggplot2::ggplot(ggplot2::aes(x = zone_time_before_goal, y = cumulative_share, color = season, linetype = factor(session))) +
  # ggplot2::facet_wrap(ggplot2::vars(season)) +
  # ggplot2::geom_col(width = 1, alpha = 0.2, position = "identity") +
  ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
  ggplot2::scale_x_continuous(
    "Seconds Since Zone Entry",
    breaks =
      seq(0, 70, by = 10),
    # as.character() |>
    # factor(levels = c(as.character(0:115), "Unknown")),
    labels = as.character(0:7)
  ) +
  ggplot2::scale_y_continuous("Cumulative Percent of Shorthanded Goals Scored", labels = scales::percent) +
  ggplot2::scale_color_viridis_d(
    "Season"
  ) +
  ggplot2::scale_linetype_manual(
    "Session",
    values = c(`2` = "solid", `3` = "dashed"),
    labels = c("Regular", "Playoffs")
  ) +
  ggplot2::labs(
    title = "Cumulative Share of Shorthanded Goals by Time Since Offensive Zone Entry",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")


nhl_db_con |>
  odbc::dbGetQuery(
    "select * from eh_pbp where event_type = 'MISS' and season >= 20232024 and session = 'R'"
  ) |>
  tibble::tibble() |>
  dplyr::filter(event_description |> stringr::str_detect("Short|Failed Bank Attempt")) |>
  dplyr::group_by(season) |>
  dplyr::summarise(sum(pred_goal, na.rm = T))
  View()



pred_xg_shot_data |>
  dplyr::filter(event_type == "GOAL") |>
  dplyr::inner_join(
    time_before_goals_5v5 |>
    dplyr::filter(total_time_before_goal >= 60)
  ) |>
  dplyr::mutate(
    all_in_zone = total_time_before_goal == time_in_zone_before_goal,
    zone_time_before_goal =
      ifelse(all_in_zone, "Unknown", as.character(time_in_zone_before_goal)) |>
      factor(levels = c(as.character(0:115), "Unknown")),
    rush_secs =
      ifelse(is_rush == 0, "21+", as.character(rush_secs)) |>
      factor(levels = c(as.character(1:20), "21+"))
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      zone_time_before_goal =
        c(as.character(0:115), "Unknown") |>
        factor(levels = c(as.character(0:115), "Unknown")),
      rush_secs =
        c(as.character(1:20), "21+") |>
        factor(levels = c(as.character(1:20), "21+"))
    )
  ) |>
  dplyr::group_by(rush_secs, zone_time_before_goal) |>
  dplyr::summarise(n = sum(event_type == "GOAL", na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::mutate(perc = n / sum(n)) |>
  dplyr::filter(!(zone_time_before_goal == "Unknown" & rush_secs == "21+")) |>
  # dplyr::group_by(rush_secs) |>
  # dplyr::summarise(n = sum(n)) |>
  # ggplot2::ggplot(ggplot2::aes(x = rush_secs, y = n)) +
  # ggplot2::geom_col(color = "black", fill = "white", width = 1) +
  # print(n = 21)
  ggplot2::ggplot(ggplot2::aes(x = rush_secs, y = zone_time_before_goal, fill = perc)) +
  ggplot2::geom_tile() +
  ggplot2::scale_x_discrete("Seconds Since Zone Entry (Play-By-Play)") +
  ggplot2::scale_y_discrete(
    "Seconds Since Zone Entry (EDGE)",
    breaks =
      c(as.character(seq(10, 110, by = 10)), "Unknown") |>
      factor(levels = c(as.character(0:115), "Unknown")),
    labels = c(as.character(1:11), "Unknown")
  ) +
  ggplot2::scale_fill_viridis_c("", option = "A", breaks = c(0, 0.006125, 0.0125), labels = c("0%", "0.6%", "1.2%")) +
  ggplot2::labs(
    title = "5-on-5 Goals Scored by Seconds Since Entry Play-By-Play vs EDGE",
    # title = "5-on-5 EDGE Rush Goals Scored by Seconds Since Play-By-Play Zone Entry ",
    subtitle = "2024-25 Season",
    caption = "Data via NHL"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")




readr::read_csv("../scraper_testing/edge_animations/2023020001_ev154.csv") |>
  dplyr::filter(playerId == 0) |>
  dplyr::mutate(
    puck_in_net =
      playerId == 0 & (
        (abs(coords_x) >= 88.8 & abs(coords_x) <= (89.2 + (40/12))) &
          abs(coords_y) <= 3.2
      ),
    side = ifelse(puck_in_net, sign(coords_x), NA_integer_),
    puck_in_net = cumsum(puck_in_net) > 0
  ) |>
  tidyr::fill(side, .direction = "updown") |>
  dplyr::filter(!puck_in_net) |>
  dplyr::mutate(
    puck_in_zone =
      playerId == 0 & (
        (coords_x * side) >= 25
      ),
    in_zone_instance = cumsum(!puck_in_zone),
    in_zone_timestamp = ifelse(in_zone_instance == max(in_zone_instance) & puck_in_zone, timeStamp, NA_integer_)
    # q = ((!puck_in_zone) + 1)
  ) |>
  # View()
  dplyr::summarise(
    total_time_before_goal = range(timeStamp) |> magrittr::multiply_by(c(-1, 1)) |> sum() + 1,
    time_in_zone_before_goal =
      c(min(timeStamp * ((!puck_in_zone) + 1)), max(timeStamp)) |>
      magrittr::multiply_by(c(-1, 1)) |>
      sum() + 1
  )
  # dplyr::filter(puck_in_zone) |>
  dplyr::summarise()


































nhl_db_con |>
  odbc::dbGetQuery(
    "select
      season, game_id, event_type, event_team, pred_goal, event_player_1, event_player_2, event_player_3, home_goalie, away_goalie
    from
      eh_pbp
    where
      season >= 20212022 and
      session = 'R' and
      game_period != 5 and
      event_type in ('GOAL', 'SHOT', 'BLOCK', 'MISS') and
      (event_player_1 = 'JASON.ROBERTSON' or event_player_2 = 'JASON.ROBERTSON' or event_player_3 = 'JASON.ROBERTSON') and
      (home_goalie = 'JORDAN.BINNINGTON' or away_goalie = 'JORDAN.BINNINGTON')"
  ) |>
  tibble::tibble() |>
  dplyr::group_by(season) |>
  dplyr::summarise(
    gp = unique(game_id) |> length(),
    robertson_points = sum(event_type == "GOAL" & event_team == "DAL"),
    robertson_corsi = sum(event_team == "DAL" & event_player_1 == "JASON.ROBERTSON"),
    robertson_ixg = sum(pred_goal * (event_player_1 == "JASON.ROBERTSON"), na.rm = T),
    robertson_g = sum((event_type == "GOAL") * (event_player_1 == "JASON.ROBERTSON"), na.rm = T)
  )



nhl_db_con |>
  odbc::dbGetQuery(
    "select
      season, event_type, pred_goal, event_player_1, event_player_2, event_player_3, home_goalie, away_goalie
    from
      eh_pbp
    where
      season >= 20212022 and
      session = 'R' and
      game_period != 5 and
      event_type in ('GOAL', 'SHOT', 'BLOCK', 'MISS') and
      (event_player_1 = 'JASON.ROBERTSON' or event_player_2 = 'JASON.ROBERTSON' or event_player_3 = 'JASON.ROBERTSON') and
      (home_goalie = 'JORDAN.BINNINGTON' or away_goalie = 'JORDAN.BINNINGTON'"
  ) |>
  tibble::tibble()

