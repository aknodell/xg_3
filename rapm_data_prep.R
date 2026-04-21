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






"2022020003" |>
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

      tibble::tibble(

      )

      player_bio_factors <-
        pbp |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::group_by(game_id, home_team, away_team, fac_id, shift_id, home_score_diff) |>
        dplyr::summarise(
          # shift_length = sum(event_length),
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
            as.integer(
              sum(position_category == "D" & stringr::str_detect(name, "home") & handedness == "R") >= 1 &
                sum(position_category == "D" & stringr::str_detect(name, "home") & handedness == "L") >= 1
            ),
          away_d_balanced =
            as.integer(
              sum(position_category == "D" & stringr::str_detect(name, "away") & handedness == "R") >= 1 &
                sum(position_category == "D" & stringr::str_detect(name, "away") & handedness == "L") >= 1
            ),
          home_f_balanced =
            as.integer(
              sum(position_category == "F" & stringr::str_detect(name, "home") & handedness == "R") >= 1 &
                sum(position_category == "F" & stringr::str_detect(name, "home") & handedness == "L") >= 1
            ),
          away_f_balanced =
            as.integer(
              sum(position_category == "F" & stringr::str_detect(name, "away") & handedness == "R") >= 1 &
                sum(position_category == "F" & stringr::str_detect(name, "away") & handedness == "L") >= 1
            )
        ) |>
        dplyr::select(-c(name:handedness)) |>
        dplyr::distinct() |>
        print()

      players_on <-
        pbp |>
        dplyr::filter(home_skater_strength_state == "5v5") |>
        dplyr::group_by(game_id, home_team, away_team, fac_id, shift_id) |>
        dplyr::summarise(
          # shift_length = sum(event_length),
          dplyr::across(
            c(tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_")),
            .fns = function(x) unique(x)
          )
        ) |>
        tidyr::pivot_longer(
          c(tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_")),
          names_to = "on",
          values_to = "api_id"
        ) |>
        dplyr::filter(!is.na(api_id)) |>
        tidyr::pivot_longer(
          c(home_team, away_team),
          names_to = "venue",
          values_to = "team"
        ) |>
        dplyr::mutate(
          venue = venue |> stringr::str_remove("_team"),
          on = on |> stringr::str_remove("_on_\\d"),
          api_id = ifelse(venue == on, "{api_id}_off" |> glue::glue(), "{api_id}_def" |> glue::glue()),
          val = 1
        ) |>
        tidyr::pivot_wider(
          id_cols = c(game_id, team, fac_id, shift_id),
          names_from = api_id,
          values_from = val,
          values_fill = 0
        ) |>
        print()

      schedule_factors |>
        tidyr::unnest(
          schedule_fcts
        ) |>
        dplyr::inner_join(
          player_bio_factors |>
            dplyr::transmute(
              game_id, team = home_team,
              fac_id, shift_id,
              score_diff = home_score_diff,
              skater_type_off = home_team_skater_type,
              skater_type_def = away_team_skater_type,
              d_balanced_off = home_d_balanced,
              d_balanced_def = away_d_balanced,
              f_balanced_off = home_d_balanced,
              f_balanced_def = away_d_balanced
            ) |>
            dplyr::bind_rows(
              player_bio_factors |>
                dplyr::transmute(
                  game_id, team = away_team,
                  fac_id, shift_id,
                  score_diff = home_score_diff * -1,
                  skater_type_off = away_team_skater_type,
                  skater_type_def = home_team_skater_type,
                  d_balanced_off = away_d_balanced,
                  d_balanced_def = home_d_balanced,
                  f_balanced_off = away_d_balanced,
                  f_balanced_def = home_d_balanced
                )
            ),
          by = c("game_id", "team")
        ) |>
        dplyr::left_join(players_on)
    }
  ) |>
  dplyr::bind_rows() |>
  View()





test_pbp <-
  "../scraper_testing/clean_files/pbp_2025020003.csv" |>
  glue::glue() |>
  readr::read_csv(
    col_select = c(
      game_id:game_seconds, event_team_strength, event_team, event_type,
      event_length, event_player_1,
      tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_"),
      home_team, away_team, home_team_def_zone,
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
      event_detail_1 = readr::col_character(),
      event_detail_2 = readr::col_character(),
      event_detail_3 = readr::col_character(),
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


# test_pbp |>
#   dplyr::group_by(fac_id) |>
#   dplyr::summarise(
#     any(event_team_strength %in% c("PP", "SH"))
#   ) |>
#   View()


test_pbp |>
  dplyr::filter(
    event_type == "PENL" |
      event_type == "GOAL"
    # event_detail_1 != "Fighting",
    # event_detail_2 %in% c("Min", "Maj", "Ben")
  ) |>
  View()




test_fac_shadow <-
  tibble::tibble(
    game_period = 1,
    game_seconds = 0:1200
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      game_period = 2,
      game_seconds = 1200:2400
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      game_period = 3,
      game_seconds = 2400:3600
    )
  ) |>
  dplyr::bind_rows(
    if (max(test_pbp$game_seconds) > 3600) {
      tibble::tibble(
        game_period == 4,
        game_seconds = seq(3600, max(test_pbp$game_seconds))
      )
    } else {
      tibble::tibble()
    }
  ) |>
  dplyr::left_join(
    test_pbp |>
      dplyr::filter(event_type %in% c("FAC")) |>
      dplyr::transmute(
        game_seconds, game_period, fac_id, event_id, event_type,
        home_fac_zone =
          dplyr::case_when(
            period_standardized_x == -69 ~ "D",
            period_standardized_x == -20 ~ "ND",
            period_standardized_x == 0 ~ "NN",
            period_standardized_x == 20 ~ "NO",
            period_standardized_x == 69 ~ "O",
          ),
        home_fac_win = as.integer(event_team == home_team)
        # home_on_1:away_on_6
      )
  ) |>
  tidyr::fill(
    c(game_period, fac_id, home_fac_zone, home_fac_win),
    .direction = "down"
  ) |>
  dplyr::group_by(game_period) |>
  dplyr::group_by(fac_id) |>
  dplyr::mutate(
    # fac_time = min(game_seconds),
    fac_shadow = as.integer(game_seconds - min(game_seconds) <= 8)
  ) |>
  dplyr::select(-c(event_id, event_type)) |>
  dplyr::ungroup()
  View()





test_pen_shadow <-
  tibble::tibble(
    game_period = 1,
    game_seconds = 0:1200
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      game_period = 2,
      game_seconds = 1200:2400
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      game_period = 3,
      game_seconds = 2400:3600
    )
  ) |>
  dplyr::bind_rows(
    if (max(test_pbp$game_seconds) > 3600) {
      tibble::tibble(
        game_period == 4,
        game_seconds = seq(3600, max(test_pbp$game_seconds))
      )
    } else {
      tibble::tibble()
    }
  ) |>
  dplyr::left_join(
    test_pbp |>
      dplyr::filter(event_type %in% c("CHANGE", "FAC")) |>
      dplyr::select(
        game_seconds, game_period, fac_id, shift_id, event_id, event_type,
        home_skater_strength_state
      )
  ) |>
  tidyr::fill(
    c(game_period, fac_id, shift_id, home_skater_strength_state),
    .direction = "down"
  ) |>
  dplyr::left_join(
    test_pbp |>
      dplyr::filter(event_type != "PENL") |>
      dplyr::group_by(fac_id) |>
      dplyr::summarise(
        has_pp_event = any(event_team_strength %in% c("PP", "SH")) |> as.integer(),
        home_team_pp =
          any(
            (event_type %in% c("GOAL", "SHOT", "MISS", "HIT", "GIVE", "TAKE", "FAC") &
              event_team == home_team &
              event_team_strength == "PP") |
              (event_type %in% c("BLOCK") &
                 event_team == home_team &
                 event_team_strength == "SH") |
              (event_type %in% c("GOAL", "SHOT", "MISS", "HIT", "GIVE", "TAKE", "FAC") &
                 event_team == away_team &
                 event_team_strength == "SH") |
              (event_type %in% c("BLOCK") &
                 event_team == home_team &
                 event_team_strength == "PP")
          ) |>
          as.integer()
      )
  ) |>
  dplyr::group_by(game_period, game_seconds) |>
  dplyr::mutate(
    otf_change =
      tidyr::replace_na("CHANGE" %in% event_type & !"FAC" %in% event_type, F) |> as.integer()
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(game_period) |>
  dplyr::mutate(
    penalty_exp =
      tidyr::replace_na(
        (
          otf_change == 1 &
            home_skater_strength_state == "5v5" &
            has_pp_event == 1
        ),
        F
      ) |>
      as.integer()
  ) |>
  dplyr::group_by(fac_id) |>
  dplyr::mutate(
    penalty_exp = as.integer(penalty_exp == cumsum(penalty_exp) & penalty_exp != 0),
    pentalty_exp_time = ifelse(penalty_exp == 1, game_seconds, -10),
    penalty_exp_shadow =
      as.integer(
        game_seconds - max(pentalty_exp_time) <= 8 &
          game_seconds - max(pentalty_exp_time) >= 0
      )
  ) |>
  dplyr::select(
    -c(
      event_id, event_type, home_skater_strength_state, otf_change,
      penalty_exp, pentalty_exp_time
    )
  ) |>
  dplyr::ungroup()
  # dplyr::distinct()
  # View()


test_pen_shadow |>
  dplyr::full_join(test_fac_shadow) |>
  dplyr::full_join(
    test_pbp |>
      dplyr::select(
        game_period,
        game_seconds,
        fac_id,
        shift_id,
        event_id,
        event_type,
        event_team,
        home_team
      )
  ) |>
  dplyr::arrange(
    fac_id,
    shift_id,
    game_seconds
  ) |>
  tidyr::fill(
    c(
      has_pp_event,
      home_team_pp,
      penalty_exp_shadow,
      home_fac_zone,
      home_fac_win,
      fac_shadow
    ),
    .direction = "down"
  ) |>
  dplyr::mutate(event_length = game_seconds - tidyr::replace_na(dplyr::lag(game_seconds), 0)) |>
  dplyr::group_by(
    fac_id,
    shift_id,
    has_pp_event,
    home_team_pp,
    penalty_exp_shadow,
    home_fac_zone,
    home_fac_win,
    fac_shadow
  ) |>
  dplyr::summarise(
    shift_length = sum(event_length),
    home_corsi = sum(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK") & event_team == home_team, na.rm = T),
    away_corsi = sum(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK") & event_team != home_team, na.rm = T)
  ) |>
  View()


