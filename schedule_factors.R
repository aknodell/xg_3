get_schedule_factors <- function(.season) {
  schedule <-
    nhlPbpScrapeR::get_raw_all_teams_json_api() |>
    nhlPbpScrapeR::extract_all_teams_from_raw_all_teams_json() |>
    dplyr::mutate(
      schedule =
        purrr::map(
          triCode,
          function(t) {
            nhlPbpScrapeR::get_raw_team_season_schedule_json_api(tm = t, season = .season) |>
              nhlPbpScrapeR::extract_team_season_schedule_from_raw_season_schedule_api()
          }
        )
    ) |>
    tidyr::unnest(schedule)

  nhl_db_con |>
    odbc::dbGetQuery(
      "select * from games where season = {.season} and session = 2" |>
        glue::glue()
    ) |>
    tibble::tibble() |>
    tidyr::pivot_longer(
      c(home_team, away_team),
      names_to = "venue",
      values_to = "team"
    ) |>
    dplyr::arrange(game_date, team) |>
    dplyr::left_join(
       schedule |>
        dplyr::filter(gameType == 2) |>
        dplyr::transmute(
          game_id = id,
          team = triCode,
          time_zone_offset =
            purrr::map_int(
              venueUTCOffset,
              function(v) {
                v |>
                  stringr::str_remove_all("[-+]") |>
                  stringr::str_c(":00") |>
                  hms::as_hms() |>
                  lubridate::hour() |>
                  as.integer()
              }
            ) *
            ifelse(stringr::str_detect(venueUTCOffset, "-"), -1, 1),
          local_start =
            purrr::map2_chr(
              startTimeUTC,
              venueTimezone,
              function(s, v) {
                s |>
                  lubridate::as_datetime() |>
                  lubridate::with_tz(v) |>
                  hms::as_hms() |>
                  as.character()
              }
            )
        )
    ) |>
    dplyr::left_join(
      schedule |>
        dplyr::filter(gameDate < "{.season %/% 10000}-11-01" |> glue::glue()) |>
        dplyr::filter(neutralSite == F, triCode == homeTeam$abbrev) |>
        dplyr::group_by(team = triCode, season_start_time_zone = venueUTCOffset) |>
        dplyr::tally() |>
        dplyr::group_by(team) |>
        dplyr::filter(n == max(n)) |>
        dplyr::ungroup() |>
        dplyr::select(-c(n)) |>
        dplyr::mutate(
          season_start_time_zone =
            purrr::map_int(
              season_start_time_zone,
              function(v) {
                v |>
                  stringr::str_remove_all("[-+]") |>
                  stringr::str_c(":00") |>
                  hms::as_hms() |>
                  lubridate::hour() |>
                  as.integer()
              }
            ) *
            ifelse(stringr::str_detect(season_start_time_zone, "-"), -1, 1)
        )
    ) |>
    dplyr::group_by(team) |>
    dplyr::transmute(
      game_id,
      team,
      neutral_site,
      is_home = as.integer(neutral_site != 1 & venue == "home_team"),
      matinee = as.integer(local_start <= "17:00:00"),
      days_since_last_game =
        tidyr::replace_na(as.integer(game_date - dplyr::lag(game_date)), 4),
      days_since_last_game =
        ifelse(days_since_last_game > 4, 4, days_since_last_game),
      travelled =
        (
          # start season at neutral site
          (is.na(dplyr::lag(venue_name)) & (neutral_site == 1)) |
            # start season on road
            (is.na(dplyr::lag(venue_name)) & (venue == "away_team")) |
            # change of venue (false for first game of season)
            !tidyr::replace_na(venue_name == dplyr::lag(venue_name), T)
        ) |>
        as.integer(),
      time_zones_changed = time_zone_offset - dplyr::lag(time_zone_offset),
      time_zones_changed =
        ifelse(
          is.na(time_zones_changed),
          time_zone_offset - season_start_time_zone,
          time_zones_changed
        )
    ) |>
    dplyr::ungroup()
}

schedule_factors <-
  tibble::tibble(
    season = c(20222023, 20232024, 20242025, 20252026)
  ) |>
  dplyr::mutate(
    schedule_fcts =
      purrr::map(season, get_schedule_factors)
  )

schedule_factors <-
  schedule_factors |>
  tidyr::unnest(schedule_fcts) |>
  dplyr::filter(days_since_last_game != 0) |>
  dplyr::group_by(season) |>
  tidyr::nest()

schedule_factors <-
  schedule_factors |>
  dplyr::ungroup()

schedule_factors |>
  tidyr::unnest(schedule_fcts) |>
  # dplyr::filter(is_home == 0, travelled == 0) |>
  dplyr::group_by(
    neutral_site,
    is_home,
    travelled
    # matinee,
    # days_since_last_game
  ) |>
  dplyr::tally() |>
  View()


nhlPbpScrapeR::get_raw_all_teams_json_api() |>
  nhlPbpScrapeR::extract_all_teams_from_raw_all_teams_json() |>
  dplyr::mutate(
    schedule =
      purrr::map(
        triCode,
        function(t) {
          nhlPbpScrapeR::get_raw_team_season_schedule_json_api(tm = t, season = 20222023) |>
            nhlPbpScrapeR::extract_team_season_schedule_from_raw_season_schedule_api()
        }
      )
  ) |>
  tidyr::unnest(schedule) |>
  dplyr::filter(gameType == 2) |>
  # colnames()
  dplyr::transmute(
    game_id = id,
    team = triCode,
    startTimeUTC,
    utc_time =
      purrr::map_chr(
        startTimeUTC,
        function(s) {
          s |>
            lubridate::as_datetime() |>
            hms::as_hms() |>
            as.character()
        }
      ),
    venueUTCOffset,
    venueTimezone,
    local_start =
      purrr::map2_chr(
        startTimeUTC,
        venueTimezone,
        function(s, v) {
          s |>
            lubridate::as_datetime() |>
            lubridate::with_tz(v) |>
            hms::as_hms() |>
            as.character()
        }
      )
  ) |>
  dplyr::group_by(local_start) |>
  dplyr::tally() |>
  dplyr::mutate(
    n = n / 2,
    cumulative = cumsum(n),
    perc = n / sum(n),
    cumulative_perc = cumsum(perc)
  ) |>
  View()


