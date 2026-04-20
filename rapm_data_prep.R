players <-
  c(
    list.files("../scraper_testing/clean_files", pattern = "pbp_202202") |>
      stringr::str_extract("\\d{10}"),
    list.files("../scraper_testing/clean_files", pattern = "pbp_202302") |>
      stringr::str_extract("\\d{10}"),
    list.files("../scraper_testing/clean_files", pattern = "pbp_202402") |>
      stringr::str_extract("\\d{10}"),
    list.files("../scraper_testing/clean_files", pattern = "pbp_202502") |>
      stringr::str_extract("\\d{10}")
  ) |>
  # head() |>
  purrr::map(
    function(f) {
      "../scraper_testing/clean_files/rosters_{f}.csv" |>
        glue::glue() |>
        readr::read_csv(
          col_select = c(api_id, position_category),
          col_types = readr::cols(
            api_id = readr::col_integer(),
            position_category = readr::col_character()
          )
        ) |>
        dplyr::filter(position_category != "G") |>
        dplyr::select(-c(position_category))
    }
  ) |>
  dplyr::bind_rows() |>
  dplyr::distinct()

players <-
  players |>
  # View()
  # tail(10) |>
  dplyr::transmute(
    event_player_1 = api_id,
    handedness =
      purrr::map_chr(
        event_player_1,
        function(id) {
          Sys.sleep(0.3)

          if (id == 8486166 | id == 8486169) {
            "L"
          } else {
            "https://api-web.nhle.com/v1/player/{id}/landing" |>
              glue::glue() |>
              httr::GET() |>
              httr::content(type = "text", encoding = "UTF-8") |>
              jsonlite::fromJSON() |>
              purrr::pluck("shootsCatches")
          }
        }
      )
  )






"2023020001" |>
  purrr::map(
    function(f) {
      roster <-
        "../scraper_testing/clean_files/rosters_{f}.csv" |>
        glue::glue() |>
        readr::read_csv(
          col_select = c(api_id, position_category),
          col_types = readr::cols(
            api_id = readr::col_integer(),
            position_category = readr::col_character()
          )
        ) |>
        dplyr::left_join(
          players,
          by = c("api_id" = "event_player_1")
        )

      pbp <-
        "../scraper_testing/clean_files/pbp_{f}.csv" |>
        glue::glue() |>
        readr::read_csv(
          col_select = c(
            game_id:game_seconds, event_type, event_length,
            tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_"),
            event_team ,home_team, away_team, home_team_def_zone,
            event_id, event_team_zone, coords_x, coords_y,
            home_skater_strength_state, shift_id
          ),
          col_types = readr::cols(
            event_type = readr::col_character(),
            event_team = readr::col_character(),
            home_team = readr::col_character(),
            away_team = readr::col_character(),
            home_team_def_zone = readr::col_character(),
            event_team_zone = readr::col_character(),
            home_skater_strength_state = readr::col_character(),
            event_team_strength = readr::col_character(),
            .default = readr::col_integer()
          )
        ) |>
        dplyr::mutate(
          fac_id = cumsum(event_type == "FAC"),
          home_score_diff =
            dplyr::lag(
              cumsum(event_type == "GOAL" & (event_team == home_team)) -
                cumsum(event_type == "GOAL" & (event_team == away_team))
            ) |>
            tidyr::replace_na(0),
          ## home net left, away net right
          period_standardized_x = coords_x * ifelse(home_team_def_zone == "left", 1, -1),
          period_standardized_y = coords_y * ifelse(home_team_def_zone == "left", 1, -1)
        )

      pbp |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::group_by(game_id, home_team, away_team, fac_id, shift_id, home_score_diff) |>
        dplyr::summarise(
          shift_length = sum(event_length),
          dplyr::across(
            c(tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_")),
            .fns = function(x) unique(x)
          )
        ) |>
        tidyr::pivot_longer(
          c(tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_")),
          values_to = "api_id"
        ) |>
        dplyr::filter(!is.na(api_id)) |>
        dplyr::left_join(roster) |>
        dplyr::group_by(fac_id, shift_id) |>
        dplyr::mutate(
          home_team_skater_type =
            "{
              sum(
                (position_category == 'F') &
                stringr::str_detect(name, 'home')
              )
            }F{
              sum(
                (position_category == 'D') &
                stringr::str_detect(name, 'home')
              )
            }D" |>
            glue::glue(),
          away_team_skater_type =
            "{
              sum(
                (position_category == 'F') &
                stringr::str_detect(name, 'away')
              )
            }F{
              sum(
                (position_category == 'D') &
                stringr::str_detect(name, 'away')
              )
            }D" |>
            glue::glue(),
          home_d_balanced =
            sum(position_category == "D" & stringr::str_detect(name, "home") & handedness == "R") >= 1 &
            sum(position_category == "D" & stringr::str_detect(name, "home") & handedness == "L") >= 1,
          away_d_balanced =
            sum(position_category == "D" & stringr::str_detect(name, "away") & handedness == "R") >= 1 &
            sum(position_category == "D" & stringr::str_detect(name, "away") & handedness == "L") >= 1,
          home_f_balanced =
            sum(position_category == "F" & stringr::str_detect(name, "home") & handedness == "R") >= 1 &
            sum(position_category == "F" & stringr::str_detect(name, "home") & handedness == "L") >= 1,
          away_f_balanced =
            sum(position_category == "F" & stringr::str_detect(name, "away") & handedness == "R") >= 1 &
            sum(position_category == "F" & stringr::str_detect(name, "away") & handedness == "L") >= 1
        ) |>
        tidyr::pivot_wider(
          id_cols = c(game_id:shift_length, home_team_skater_type:away_f_balanced),
          names_from = name,
          values_from = api_id
        )
    }
  ) |>
  dplyr::bind_rows() |>
  View()



