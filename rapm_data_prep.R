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

players <-
  players |>
  # head() |>
  dplyr::mutate(
    player_name =
      purrr::map_chr(
        event_player_1,
        function(id) {
          Sys.sleep(0.3)

          json <-
            "https://api-web.nhle.com/v1/player/{id}/landing" |>
            glue::glue() |>
            httr::GET() |>
            httr::content(type = "text", encoding = "UTF-8") |>
            jsonlite::fromJSON()

          stringr::str_c(
            json |> purrr::pluck("firstName", "default"),
            json |> purrr::pluck("lastName", "default"),
            sep = " ",
            collapse = " "
          )
        }
      )
  )

get_game_rapm <-
  function(f) {
    print(f)

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
          game_id:game_seconds, event_team_strength, event_team, event_type,
          tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_"),
          home_team, away_team, home_team_def_zone,
          event_id, event_length,
          coords_x, coords_y,
          home_skater_strength_state, shift_id
        ),
        col_types = readr::cols(
          event_type = readr::col_character(),
          event_team = readr::col_character(),
          home_team = readr::col_character(),
          away_team = readr::col_character(),
          home_team_def_zone = readr::col_character(),
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
      ) |>
      dplyr::filter(fac_id != 0)

    home_tm <- pbp$home_team |> unique() |> purrr::discard(is.na)
    away_tm <- pbp$away_team |> unique() |> purrr::discard(is.na)

    corsi_events <-
      training_data |>
      dplyr::filter(
        as.integer(f) == game_id &
          event_team_strength == "EV" &
          home_skater_strength_state == "5v5"
      ) |>
      dplyr::transmute(
        game_id, game_seconds, fac_id, shift_id, event_team,
        c = 1,
        in_zone = shot_zone == "O",
        above_goal_line = in_zone & shot_y > 0,
        xg_game_weight,
        xg_shot_type,
        xg_rush_velo,
        xg_blockers_basic,
        xg_blockers_shot_type_point,
        xg_fac_win_secs,
        xg_own_reach_follow_all_secs
      )

    seconds <-
      tibble::tibble(
        game_id = as.integer(f),
        game_period = 1,
        game_seconds = 0:1200
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = as.integer(f),
          game_period = 2,
          game_seconds = 1200:2400
        )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = as.integer(f),
          game_period = 3,
          game_seconds = 2400:3600
        )
      ) |>
      dplyr::bind_rows(
        if (max(pbp$game_seconds) > 3600) {
          tibble::tibble(
            game_id = as.integer(f),
            game_period = 4,
            game_seconds = seq(3600, max(pbp$game_seconds))
          )
        } else {
          tibble::tibble()
        }
      )

    fac_shadow <-
      seconds |>
      dplyr::left_join(
        pbp |>
          dplyr::filter(event_type %in% c("FAC"), fac_id != 0) |>
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
          ),
        by = c("game_period", "game_seconds")
      ) |>
      tidyr::fill(
        c(game_period, fac_id, home_fac_zone, home_fac_win),
        .direction = "down"
      ) |>
      dplyr::group_by(game_period) |>
      dplyr::group_by(fac_id) |>
      dplyr::mutate(
        fac_shadow = as.integer(game_seconds - min(game_seconds) <= 8)
      ) |>
      dplyr::select(-c(event_id, event_type)) |>
      dplyr::ungroup()

    pen_shadow <-
      seconds |>
      dplyr::left_join(
        pbp |>
          dplyr::filter(event_type %in% c("CHANGE", "FAC"), fac_id != 0) |>
          dplyr::select(
            game_seconds, game_period, fac_id,
            event_id, event_type,
            home_skater_strength_state
          ),
        by = c("game_period", "game_seconds")
      ) |>
      tidyr::fill(
        c(game_period, fac_id,
          home_skater_strength_state),
        .direction = "down"
      ) |>
      dplyr::left_join(
        pbp |>
          dplyr::filter(event_type != "PENL", fac_id != 0) |>
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
              as.integer(),
            .groups = "drop"
          ),
        by = c("fac_id")
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
      dplyr::ungroup() |>
      dplyr::distinct()

    home_shift_shadows <-
      seconds |>
      dplyr::left_join(
        pbp |>
          dplyr::filter(fac_id != 0) |>
          dplyr::select(
            game_id, game_period, fac_id, shift_id, game_seconds, home_skater_strength_state,
            tidyselect::starts_with("home_on_"), tidyselect::starts_with("away_on_")
          ) |>
          dplyr::distinct() |>
          tidyr::pivot_longer(
            c(tidyselect::starts_with("home_on_"), tidyselect::starts_with("away_on_"))
          ) |>
          dplyr::filter(!is.na(value)) |>
          dplyr::mutate(on = 1) |>
          dplyr::mutate(
            value = "{value}_shadow_{ifelse(stringr::str_detect(name, 'home'), 'off', 'def')}" |>
              glue::glue()
          ) |>
          tidyr::pivot_wider(
            id_cols = game_id:home_skater_strength_state,
            names_from = value,
            values_from = on,
            values_fill = 0
          ),
        by = c("game_id", "game_period", "game_seconds")
      ) |>
      tidyr::fill(tidyselect::everything(), .direction = "down") |>
      tidyr::pivot_longer(
        cols = -c(game_id:home_skater_strength_state)
      ) |>
      dplyr::arrange(game_period, game_seconds, fac_id, shift_id) |>
      dplyr::group_by(name, fac_id) |>
      dplyr::mutate(
        last_time_on_ice = cummax(game_seconds * value * (home_skater_strength_state == "5v5")),
        shadow =
          (value == 0) *
          (game_seconds - last_time_on_ice > 0 & game_seconds - last_time_on_ice <= 8) *
          (home_skater_strength_state == "5v5") *
          (last_time_on_ice != 0)
      ) |>
      tidyr::pivot_wider(
        id_cols = game_id:home_skater_strength_state,
        names_from = name,
        values_from = shadow,
        values_fill = 0
      ) |>
      dplyr::ungroup()

    away_shift_shadows <-
      seconds |>
      dplyr::left_join(
        pbp |>
          dplyr::filter(fac_id != 0) |>
          dplyr::select(
            game_id, game_period, fac_id, shift_id, game_seconds, home_skater_strength_state,
            tidyselect::starts_with("home_on_"), tidyselect::starts_with("away_on_")
          ) |>
          dplyr::distinct() |>
          tidyr::pivot_longer(
            c(tidyselect::starts_with("home_on_"), tidyselect::starts_with("away_on_"))
          ) |>
          dplyr::filter(!is.na(value)) |>
          dplyr::mutate(on = 1) |>
          dplyr::mutate(
            value = "{value}_shadow_{ifelse(stringr::str_detect(name, 'away'), 'off', 'def')}" |>
              glue::glue()
          ) |>
          tidyr::pivot_wider(
            id_cols = game_id:home_skater_strength_state,
            names_from = value,
            values_from = on,
            values_fill = 0
          ),
        by = c("game_id", "game_period", "game_seconds")
      ) |>
      tidyr::fill(tidyselect::everything(), .direction = "down") |>
      tidyr::pivot_longer(
        cols = -c(game_id:home_skater_strength_state)
      ) |>
      dplyr::arrange(game_period, game_seconds, fac_id, shift_id) |>
      dplyr::group_by(name, fac_id) |>
      dplyr::mutate(
        last_time_on_ice = cummax(game_seconds * value * (home_skater_strength_state == "5v5")),
        shadow =
          (value == 0) *
          (game_seconds - last_time_on_ice > 0 & game_seconds - last_time_on_ice <= 8) *
          (home_skater_strength_state == "5v5") *
          (last_time_on_ice != 0)
      ) |>
      tidyr::pivot_wider(
        id_cols = game_id:home_skater_strength_state,
        names_from = name,
        values_from = shadow,
        values_fill = 0
      ) |>
      dplyr::ungroup()

    player_bio_factors <-
      pbp |>
      dplyr::filter(
        home_skater_strength_state == "5v5",
        # event_length != 0
        !event_type %in% c("STOP", "PENL")
      ) |>
      dplyr::group_by(game_id, home_team, away_team, fac_id, shift_id, home_score_diff) |>
      dplyr::summarise(
        dplyr::across(
          c(tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_")),
          .fns = function(x) unique(x)
        ),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        c(tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_")),
        values_to = "api_id"
      ) |>
      dplyr::filter(!is.na(api_id)) |>
      dplyr::left_join(roster, by = "api_id") |>
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
      dplyr::ungroup() |>
      dplyr::distinct()

    players_on <-
      pbp |>
      dplyr::filter(home_skater_strength_state == "5v5") |>
      dplyr::group_by(game_id, home_team, away_team, fac_id, shift_id) |>
      dplyr::summarise(
        dplyr::across(
          c(tidyr::starts_with("home_on_"), tidyr::starts_with("away_on_")),
          .fns = function(x) unique(x)
        ),
        .groups = "drop"
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
      dplyr::ungroup()

    schedule_factors |>
      tidyr::unnest(schedule_fcts) |>
      dplyr::filter(game_id == as.integer(f), team == home_tm) |>
      dplyr::transmute(
        game_id,
        # team_off = team,
        matinee,
        is_home_off = is_home,
        no_rest_off = as.integer(days_since_last_game == 1),
        # reg_rest_off = as.integer(days_since_last_game == 2),
        long_rest_off = as.integer(days_since_last_game == 3),
        rust_off = as.integer(days_since_last_game == 4),
        travelled_off = travelled,
        minus_4_tz_off = as.integer(time_zones_changed <= -4),
        minus_3_tz_off = as.integer(time_zones_changed == -3),
        minus_2_tz_off = as.integer(time_zones_changed == -2),
        minus_1_tz_off = as.integer(time_zones_changed == -1),
        plus_1_tz_off = as.integer(time_zones_changed == 1),
        plus_2_tz_off = as.integer(time_zones_changed == 2),
        plus_3_tz_off = as.integer(time_zones_changed == 3),
        plus_4_tz_off = as.integer(time_zones_changed >= 4)
        # time_zones_changed_off = time_zones_changed,
      ) |>
      dplyr::left_join(
        schedule_factors |>
          tidyr::unnest(schedule_fcts) |>
          dplyr::filter(game_id == as.integer(f), team != home_tm) |>
          dplyr::transmute(
            game_id,
            is_home_def = is_home,
            no_rest_def = as.integer(days_since_last_game == 1),
            # reg_rest_def = as.integer(days_since_last_game == 2),
            long_rest_def = as.integer(days_since_last_game == 3),
            rust_def = as.integer(days_since_last_game == 4),
            travelled_def = travelled,
            minus_4_tz_def = as.integer(time_zones_changed <= -4),
            minus_3_tz_def = as.integer(time_zones_changed == -3),
            minus_2_tz_def = as.integer(time_zones_changed == -2),
            minus_1_tz_def = as.integer(time_zones_changed == -1),
            plus_1_tz_def = as.integer(time_zones_changed == 1),
            plus_2_tz_def = as.integer(time_zones_changed == 2),
            plus_3_tz_def = as.integer(time_zones_changed == 3),
            plus_4_tz_def = as.integer(time_zones_changed >= 4)
            # time_zones_changed_def = time_zones_changed,
          ),
        by = c("game_id")
      ) |>
      dplyr::left_join(
        pen_shadow |>
          dplyr::full_join(fac_shadow, by = c("game_id", "game_period", "game_seconds", "fac_id")) |>
          dplyr::full_join(
            home_shift_shadows,
            by = c("game_id", "game_period", "game_seconds", "fac_id")
          ) |>
          dplyr::full_join(
            corsi_events |>
              dplyr::filter(event_team == home_tm),
            by = c("game_id", "game_seconds", "fac_id", "shift_id")
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
          dplyr::filter(home_skater_strength_state == "5v5") |>
          dplyr::group_by(
            game_id,
            fac_id,
            shift_id,
            long_change = as.integer(game_period %% 2 == 0),
            nn_fac_w_shadow = as.integer(home_fac_zone == "NN" & fac_shadow == 1 & home_fac_win == 1),
            nn_fac_l_shadow = as.integer(home_fac_zone == "NN" & fac_shadow == 1 & home_fac_win == 0),
            no_fac_w_shadow = as.integer(home_fac_zone == "NO" & fac_shadow == 1 & home_fac_win == 1),
            no_fac_l_shadow = as.integer(home_fac_zone == "NO" & fac_shadow == 1 & home_fac_win == 0),
            nd_fac_w_shadow = as.integer(home_fac_zone == "ND" & fac_shadow == 1 & home_fac_win == 1),
            nd_fac_l_shadow = as.integer(home_fac_zone == "ND" & fac_shadow == 1 & home_fac_win == 0),
            o_fac_w_shadow = as.integer(home_fac_zone == "O" & fac_shadow == 1 & home_fac_win == 1),
            o_fac_l_shadow = as.integer(home_fac_zone == "O" & fac_shadow == 1 & home_fac_win == 0),
            d_fac_w_shadow = as.integer(home_fac_zone == "D" & fac_shadow == 1 & home_fac_win == 1),
            d_fac_l_shadow = as.integer(home_fac_zone == "D" & fac_shadow == 1 & home_fac_win == 0),
            pp_exp_shadow = as.integer(penalty_exp_shadow == 1 & home_team_pp == 1),
            pk_exp_shadow = as.integer(penalty_exp_shadow == 1 & home_team_pp == 0),
            dplyr::across(c(tidyselect::ends_with("_off"), tidyselect::ends_with("_def")))
          ) |>
          dplyr::summarise(
            game_seconds = min(game_seconds),
            shift_length = sum(event_length),
            corsi = sum(c, na.rm = T),
            in_zone_corsi = sum(in_zone, na.rm = T),
            above_goal_line_corsi = sum(above_goal_line, na.rm = T),
            xg_game_weight = sum(xg_game_weight, na.rm = T),
            xg_shot_type = sum(xg_shot_type, na.rm = T),
            xg_rush_velo = sum(xg_rush_velo, na.rm = T),
            xg_blockers_basic = sum(xg_blockers_basic, na.rm = T),
            xg_blockers_shot_type_point = sum(xg_blockers_shot_type_point, na.rm = T),
            xg_fac_win_secs = sum(xg_fac_win_secs, na.rm = T),
            xg_own_reach_follow_all_secs = sum(xg_own_reach_follow_all_secs, na.rm = T),
            .groups = "drop"
          ) |>
          dplyr::arrange(game_seconds, fac_id, shift_id),
        by = c("game_id")
      ) |>
      dplyr::left_join(
        player_bio_factors |>
          dplyr::transmute(
            game_id,
            fac_id,
            shift_id,
            down_3 = as.integer(home_score_diff <= -3),
            down_2 = as.integer(home_score_diff == -2),
            down_1 = as.integer(home_score_diff == -1),
            up_1 = as.integer(home_score_diff == 1),
            up_2 = as.integer(home_score_diff == 2),
            up_3 = as.integer(home_score_diff >= 3),
            skater_strength_5f_off = as.integer(home_team_skater_type == "5F0D"),
            skater_strength_4f_off = as.integer(home_team_skater_type == "4F1D"),
            skater_strength_2f_off = as.integer(home_team_skater_type == "2F3D"),
            skater_strength_1f_off = as.integer(home_team_skater_type == "1F4D"),
            # skater_type_off = home_team_skater_type,
            # skater_type_def = away_team_skater_type,
            skater_strength_5f_def = as.integer(away_team_skater_type == "5F0D"),
            skater_strength_4f_def = as.integer(away_team_skater_type == "4F1D"),
            skater_strength_2f_def = as.integer(away_team_skater_type == "2F3D"),
            skater_strength_1f_def = as.integer(away_team_skater_type == "1F4D"),
            d_balanced_off = home_d_balanced,
            d_balanced_def = away_d_balanced,
            f_balanced_off = home_d_balanced,
            f_balanced_def = away_d_balanced
          ) |>
          dplyr::distinct(),
        by = c("game_id", "fac_id", "shift_id")
      ) |>
      dplyr::left_join(
        players_on |>
          dplyr::filter(team == home_tm) |>
          dplyr::select(-c(team)),
        by = c("game_id", "fac_id", "shift_id")
      ) |>
      dplyr::bind_rows(
        schedule_factors |>
          tidyr::unnest(schedule_fcts) |>
          dplyr::filter(game_id == as.integer(f), team == away_tm) |>
          dplyr::transmute(
            game_id,
            # team_off = team,
            matinee,
            is_home_off = is_home,
            no_rest_off = as.integer(days_since_last_game == 1),
            # reg_rest_off = as.integer(days_since_last_game == 2),
            long_rest_off = as.integer(days_since_last_game == 3),
            rust_off = as.integer(days_since_last_game == 4),
            travelled_off = travelled,
            minus_4_tz_off = as.integer(time_zones_changed <= -4),
            minus_3_tz_off = as.integer(time_zones_changed == -3),
            minus_2_tz_off = as.integer(time_zones_changed == -2),
            minus_1_tz_off = as.integer(time_zones_changed == -1),
            plus_1_tz_off = as.integer(time_zones_changed == 1),
            plus_2_tz_off = as.integer(time_zones_changed == 2),
            plus_3_tz_off = as.integer(time_zones_changed == 3),
            plus_4_tz_off = as.integer(time_zones_changed >= 4)
            # time_zones_changed_off = time_zones_changed,
          ) |>
          dplyr::left_join(
            schedule_factors |>
              tidyr::unnest(schedule_fcts) |>
              dplyr::filter(game_id == as.integer(f), team != away_tm) |>
              dplyr::transmute(
                game_id,
                is_home_def = is_home,
                no_rest_def = as.integer(days_since_last_game == 1),
                # reg_rest_def = as.integer(days_since_last_game == 2),
                long_rest_def = as.integer(days_since_last_game == 3),
                rust_def = as.integer(days_since_last_game == 4),
                travelled_def = travelled,
                minus_4_tz_def = as.integer(time_zones_changed <= -4),
                minus_3_tz_def = as.integer(time_zones_changed == -3),
                minus_2_tz_def = as.integer(time_zones_changed == -2),
                minus_1_tz_def = as.integer(time_zones_changed == -1),
                plus_1_tz_def = as.integer(time_zones_changed == 1),
                plus_2_tz_def = as.integer(time_zones_changed == 2),
                plus_3_tz_def = as.integer(time_zones_changed == 3),
                plus_4_tz_def = as.integer(time_zones_changed >= 4)
                # time_zones_changed_def = time_zones_changed,
              ),
            by = c("game_id")
          ) |>
          dplyr::left_join(
            pen_shadow |>
              dplyr::full_join(fac_shadow, by = c("game_id", "game_period", "game_seconds", "fac_id")) |>
              dplyr::full_join(
                away_shift_shadows,
                by = c("game_id", "game_period", "game_seconds", "fac_id")
              ) |>
              dplyr::full_join(
                corsi_events |>
                  dplyr::filter(event_team == away_tm),
                by = c("game_id", "game_seconds", "fac_id", "shift_id")
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
              dplyr::filter(home_skater_strength_state == "5v5") |>
              dplyr::group_by(
                game_id,
                fac_id,
                shift_id,
                long_change = as.integer(game_period %% 2 == 0),
                nn_fac_w_shadow = as.integer(home_fac_zone == "NN" & fac_shadow == 1 & home_fac_win == 0),
                nn_fac_l_shadow = as.integer(home_fac_zone == "NN" & fac_shadow == 1 & home_fac_win == 1),
                no_fac_w_shadow = as.integer(home_fac_zone == "ND" & fac_shadow == 1 & home_fac_win == 0),
                no_fac_l_shadow = as.integer(home_fac_zone == "ND" & fac_shadow == 1 & home_fac_win == 1),
                nd_fac_w_shadow = as.integer(home_fac_zone == "NO" & fac_shadow == 1 & home_fac_win == 0),
                nd_fac_l_shadow = as.integer(home_fac_zone == "NO" & fac_shadow == 1 & home_fac_win == 1),
                o_fac_w_shadow = as.integer(home_fac_zone == "D" & fac_shadow == 1 & home_fac_win == 0),
                o_fac_l_shadow = as.integer(home_fac_zone == "D" & fac_shadow == 1 & home_fac_win == 1),
                d_fac_w_shadow = as.integer(home_fac_zone == "O" & fac_shadow == 1 & home_fac_win == 0),
                d_fac_l_shadow = as.integer(home_fac_zone == "O" & fac_shadow == 1 & home_fac_win == 1),
                pp_exp_shadow = as.integer(penalty_exp_shadow == 1 & home_team_pp == 0),
                pk_exp_shadow = as.integer(penalty_exp_shadow == 1 & home_team_pp == 1),
                dplyr::across(c(tidyselect::ends_with("_off"), tidyselect::ends_with("_def")))
              ) |>
              dplyr::summarise(
                game_seconds = min(game_seconds),
                shift_length = sum(event_length),
                corsi = sum(c, na.rm = T),
                in_zone_corsi = sum(in_zone, na.rm = T),
                above_goal_line_corsi = sum(above_goal_line, na.rm = T),
                xg_game_weight = sum(xg_game_weight, na.rm = T),
                xg_shot_type = sum(xg_shot_type, na.rm = T),
                xg_rush_velo = sum(xg_rush_velo, na.rm = T),
                xg_blockers_basic = sum(xg_blockers_basic, na.rm = T),
                xg_blockers_shot_type_point = sum(xg_blockers_shot_type_point, na.rm = T),
                xg_fac_win_secs = sum(xg_fac_win_secs, na.rm = T),
                xg_own_reach_follow_all_secs = sum(xg_own_reach_follow_all_secs, na.rm = T),
                .groups = "drop"
              ) |>
              dplyr::arrange(game_seconds, fac_id, shift_id),
            by = c("game_id")
          ) |>
          dplyr::left_join(
            player_bio_factors |>
              dplyr::transmute(
                game_id,
                fac_id,
                shift_id,
                down_3 = as.integer(home_score_diff >= 3),
                down_2 = as.integer(home_score_diff == 2),
                down_1 = as.integer(home_score_diff == 1),
                up_1 = as.integer(home_score_diff == -1),
                up_2 = as.integer(home_score_diff == -2),
                up_3 = as.integer(home_score_diff <= -3),
                skater_strength_5f_off = as.integer(away_team_skater_type == "5F0D"),
                skater_strength_4f_off = as.integer(away_team_skater_type == "4F1D"),
                skater_strength_2f_off = as.integer(away_team_skater_type == "2F3D"),
                skater_strength_1f_off = as.integer(away_team_skater_type == "1F4D"),
                skater_strength_5f_def = as.integer(home_team_skater_type == "5F0D"),
                skater_strength_4f_def = as.integer(home_team_skater_type == "4F1D"),
                skater_strength_2f_def = as.integer(home_team_skater_type == "2F3D"),
                skater_strength_1f_def = as.integer(home_team_skater_type == "1F4D"),
                # skater_type_off = away_team_skater_type,
                # skater_type_def = home_team_skater_type,
                d_balanced_off = away_d_balanced,
                d_balanced_def = home_d_balanced,
                f_balanced_off = away_d_balanced,
                f_balanced_def = home_d_balanced
              ) |>
              dplyr::distinct(),
            by = c("game_id", "fac_id", "shift_id")
          ) |>
          dplyr::left_join(
            players_on |>
              dplyr::filter(team == away_tm) |>
              dplyr::select(-c(team)),
            by = c("game_id", "fac_id", "shift_id")
          )
      ) |>
      dplyr::filter(
        !(shift_length == 0 & corsi == 0)
      ) |>
      dplyr::mutate(
        dplyr::across(
          c(tidyselect::ends_with("shadow_off"), tidyselect::ends_with("shadow_def")),
          function(x) tidyr::replace_na(x, 0)
        ),
        shell_off =
          as.integer(
            long_change == 0 &
              (
                (game_seconds == 2400 & shift_length != 0) |
                  (game_seconds %% 1200 == 3 & !(game_seconds == 3600 & shift_length > 0))
              ) &
              (
                up_1 == 1 | up_2 == 1 | (
                  down_3 == 0 & down_2 == 0 & down_1 == 0 &
                    up_3 == 0 & up_2 == 0 & up_1 == 0
                )
              )
          ),
        shell_def =
          as.integer(
            long_change == 0 &
              (
                (game_seconds == 2400 & shift_length != 0) |
                  (game_seconds %% 1200 == 3 & !(game_seconds == 3600 & shift_length > 0))
              ) &
              (
                down_1 == 1 | down_2 == 1 | (
                  down_3 == 0 & down_2 == 0 & down_1 == 0 &
                    up_3 == 0 & up_2 == 0 & up_1 == 0
                )
              )
          ),
        shift_length = ifelse(shift_length == 0, 0.5, shift_length),
        corsi = corsi / (shift_length / 3600),
        in_zone_corsi = in_zone_corsi / (shift_length / 3600),
        above_goal_line_corsi = above_goal_line_corsi / (shift_length / 3600),
        xg_game_weight = xg_game_weight / (shift_length / 3600),
        xg_shot_type = xg_shot_type / (shift_length / 3600),
        xg_rush_velo = xg_rush_velo / (shift_length / 3600),
        xg_blockers_basic = xg_blockers_basic / (shift_length / 3600),
        xg_blockers_shot_type_point = xg_blockers_shot_type_point / (shift_length / 3600),
        xg_fac_win_secs = xg_fac_win_secs / (shift_length / 3600),
        xg_own_reach_follow_all_secs = xg_own_reach_follow_all_secs / (shift_length / 3600)
      )
  }


rapm_data_22 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202202") |>
  stringr::str_extract("\\d{10}") |>
#   sort() |>
#   # head(40) |>
#   # tail(5) |>
# "2023020001" |>
  purrr::map(
    get_game_rapm
  )
  # dplyr::bind_rows() |>
  # View()
  # colnames() |>
  # sort() |>
  # rev()


# rapm_data_22 |>
#   # head() |>
#   purrr::map(
#     function(d) {
#       d |>
#         dplyr::select(game_id, fac_id, shift_id, down_3:f_balanced_def) |>
#         dplyr::distinct() |>
#         dplyr::group_by(game_id, fac_id, shift_id) |>
#         dplyr::filter(dplyr::n() > 2)
#     }
#   ) |>
#   dplyr::bind_rows() |>
#   View()


rapm_data_22 <-
  c(
    "corsi", "in_zone_corsi", "above_goal_line_corsi", "xg_game_weight",
    "xg_shot_type", "xg_rush_velo", "xg_blockers_basic",
    "xg_blockers_shot_type_point", "xg_fac_win_secs",
    "xg_own_reach_follow_all_secs", "shift_length",
    rapm_data_22 |>
      purrr::map(
        colnames
      ) |>
      purrr::list_c() |>
      # c() |>
      unique() |>
      sort() |>
      rev() |>
      # head() |>
      purrr::discard(
        .p = function(c) {
          c %in% c(
            "game_id", "game_seconds", "fac_id", "shift_id",
            "corsi", "in_zone_corsi", "above_goal_line_corsi", "xg_game_weight",
            "xg_shot_type", "xg_rush_velo", "xg_blockers_basic",
            "xg_blockers_shot_type_point", "xg_fac_win_secs",
            "xg_own_reach_follow_all_secs", "shift_length"
          )
        }
      )
  ) |>
  # head(10) |>
# rapm_data_22 |>
#   purrr::map(
#     colnames
#   ) |>
#   purrr::list_c() |>
#   # c() |>
#   unique() |>
#   sort() |>
#   rev() |>
#   head() |>
  purrr::map(
    function(col) {
      rapm_data_22 |>
        purrr::map(
          function(d) {
            if (col %in% colnames(d)) {
              d |>
                dplyr::select(tidyselect::any_of(col))
                # as.matrix()
                # Matrix::Matrix(sparse = T)
            } else {
              tibble::tibble(
                rep(0, nrow(d))
              ) |>
                purrr::set_names(col)
                # as.marti
                # Matrix::Matrix(sparse = T)
            }
          }
        ) |>
        purrr::list_rbind() |>
        # head() |>
        as.matrix() |>
        Matrix::Matrix(sparse = T)
    }
  ) |>
  purrr::reduce(cbind2)


(rapm_data_22 |>
  head())[, -c(1:4)]


set.seed(1138)
rapm_cv_22_all <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:4)],
    y = rapm_data_22[, 1],
    weights = as.integer(rapm_data_22[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    # parallel = T,
    standardize = F
  )

set.seed(1138)
rapm_22_all <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:4)],
    y = rapm_data_22[, 1],
    weights = as.integer(rapm_data_22[, 4] * 2),
    alpha = 0,
    # parallel = T,
    standardize = F,
    lambda = rapm_cv_22_all$lambda.min
  )

rapm_22_all |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("all c rapm 22")


set.seed(1138)
rapm_cv_22_in_zone <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:4)],
    y = rapm_data_22[, 2],
    weights = as.integer(rapm_data_22[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_in_zone <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:4)],
    y = rapm_data_22[, 1],
    weights = as.integer(rapm_data_22[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_in_zone$lambda.min
  )

rapm_22_in_zone |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("in zone c rapm 22")

set.seed(1138)
rapm_cv_22_above_goal <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:4)],
    y = rapm_data_22[, 3],
    weights = as.integer(rapm_data_22[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_above_goal <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:4)],
    y = rapm_data_22[, 1],
    weights = as.integer(rapm_data_22[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_above_goal$lambda.min
  )

rapm_22_above_goal |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("above goal c rapm 22")



gc()

rapm_data_23 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202302") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
# "2023020001" |>
# c("2023020403", "2023020805") |>
  purrr::map(
    get_game_rapm
  )
  # dplyr::bind_rows()
  # dplyr::distinct()
  # View()

rapm_data_23 <-
  c(
    "corsi", "in_zone_corsi", "above_goal_line_corsi", "shift_length",
    rapm_data_23 |>
      purrr::map(
        colnames
      ) |>
      purrr::list_c() |>
      # c() |>
      unique() |>
      sort() |>
      rev() |>
      # head() |>
      purrr::discard(
        .p = function(c) {
          c %in% c(
            "game_id", "game_seconds", "fac_id", "shift_id",
            "corsi", "in_zone_corsi", "above_goal_line_corsi", "shift_length"
          )
        }
      )
  ) |>
  purrr::map(
    function(col) {
      rapm_data_23 |>
        purrr::map(
          function(d) {
            if (col %in% colnames(d)) {
              d |>
                dplyr::select(tidyselect::any_of(col))
              # as.matrix()
              # Matrix::Matrix(sparse = T)
            } else {
              tibble::tibble(
                rep(0, nrow(d))
              ) |>
                purrr::set_names(col)
              # as.marti
              # Matrix::Matrix(sparse = T)
            }
          }
        ) |>
        purrr::list_rbind() |>
        # head() |>
        as.matrix() |>
        Matrix::Matrix(sparse = T)
    }
  ) |>
  purrr::reduce(cbind2)

set.seed(1138)
rapm_cv_23_all <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:4, 20, 21)],
    y = rapm_data_23[, 1],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    # parallel = T,
    standardize = F
  )

set.seed(1138)
rapm_23_all <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:4, 20, 21)],
    y = rapm_data_23[, 1],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    # parallel = T,
    standardize = F,
    lambda = rapm_cv_23_all$lambda.min
  )

rapm_23_all |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("all c rapm 23")


set.seed(1138)
rapm_cv_23_in_zone <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:4, 20, 21)],
    y = rapm_data_23[, 2],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_in_zone <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:4, 20, 21)],
    y = rapm_data_23[, 1],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_in_zone$lambda.min
  )

rapm_23_in_zone |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("in zone c rapm 23")

set.seed(1138)
rapm_cv_23_above_goal <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:4, 20, 21)],
    y = rapm_data_23[, 3],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_above_goal <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:4, 20, 21)],
    y = rapm_data_23[, 1],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_above_goal$lambda.min
  )

rapm_23_above_goal |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("above goal c rapm 23")



gc()

rapm_data_24 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202402") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  # "2024020798" |>
  # c("2023020403", "2023020805") |>
  purrr::map(
    get_game_rapm
  )
#
# rapm_data_24 |>
#   # head() |>
#   purrr::map(
#     function(d) {
#       d |>
#         dplyr::select(game_id, fac_id, shift_id, down_3:f_balanced_def) |>
#         dplyr::distinct() |>
#         dplyr::group_by(game_id, fac_id, shift_id) |>
#         dplyr::filter(dplyr::n() > 2)
#     }
#   ) |>
#   dplyr::bind_rows() |>
#   View()

rapm_data_24 <-
  c(
    "corsi", "in_zone_corsi", "above_goal_line_corsi", "shift_length",
    rapm_data_24 |>
      purrr::map(
        colnames
      ) |>
      purrr::list_c() |>
      # c() |>
      unique() |>
      sort() |>
      rev() |>
      # head() |>
      purrr::discard(
        .p = function(c) {
          c %in% c(
            "game_id", "game_seconds", "fac_id", "shift_id",
            "corsi", "in_zone_corsi", "above_goal_line_corsi", "shift_length"
          )
        }
      )
  ) |>
  purrr::map(
    function(col) {
      rapm_data_24 |>
        purrr::map(
          function(d) {
            if (col %in% colnames(d)) {
              d |>
                dplyr::select(tidyselect::any_of(col))
              # as.matrix()
              # Matrix::Matrix(sparse = T)
            } else {
              tibble::tibble(
                rep(0, nrow(d))
              ) |>
                purrr::set_names(col)
              # as.marti
              # Matrix::Matrix(sparse = T)
            }
          }
        ) |>
        purrr::list_rbind() |>
        # head() |>
        as.matrix() |>
        Matrix::Matrix(sparse = T)
    }
  ) |>
  purrr::reduce(cbind2)

set.seed(1138)
rapm_cv_24_all <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:4)],
    y = rapm_data_24[, 1],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    # parallel = T,
    standardize = F
  )

set.seed(1138)
rapm_24_all <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:4)],
    y = rapm_data_24[, 1],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    # parallel = T,
    standardize = F,
    lambda = rapm_cv_24_all$lambda.min
  )

rapm_24_all |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("all c rapm 24")


set.seed(1138)
rapm_cv_24_in_zone <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:4)],
    y = rapm_data_24[, 2],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_in_zone <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:4)],
    y = rapm_data_24[, 1],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_in_zone$lambda.min
  )

rapm_24_in_zone |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("in zone c rapm 24")

set.seed(1138)
rapm_cv_24_above_goal <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:4)],
    y = rapm_data_24[, 3],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_above_goal <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:4)],
    y = rapm_data_24[, 1],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_above_goal$lambda.min
  )

rapm_24_above_goal |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("above goal c rapm 24")





gc()

rapm_data_25 <-
  list.files("../scraper_testing/clean_files", pattern = "pbp_202502") |>
  stringr::str_extract("\\d{10}") |>
  sort() |>
  # "2024020798" |>
# c("2025020059", "2025020223", "2025020271") |>
  purrr::map(
    get_game_rapm
  )

# rapm_data_25 |>
#   # head() |>
#   purrr::map(
#     function(d) {
#       d |>
#         dplyr::select(game_id, fac_id, shift_id, down_3:f_balanced_def) |>
#         dplyr::distinct() |>
#         dplyr::group_by(game_id, fac_id, shift_id) |>
#         dplyr::filter(dplyr::n() > 2)
#     }
#   ) |>
#   dplyr::bind_rows() |>
#   View()

rapm_data_25 <-
  c(
    "corsi", "in_zone_corsi", "above_goal_line_corsi", "shift_length",
    rapm_data_25 |>
      purrr::map(
        colnames
      ) |>
      purrr::list_c() |>
      # c() |>
      unique() |>
      sort() |>
      rev() |>
      # head() |>
      purrr::discard(
        .p = function(c) {
          c %in% c(
            "game_id", "game_seconds", "fac_id", "shift_id",
            "corsi", "in_zone_corsi", "above_goal_line_corsi", "shift_length"
          )
        }
      )
  ) |>
  purrr::map(
    function(col) {
      rapm_data_25 |>
        purrr::map(
          function(d) {
            if (col %in% colnames(d)) {
              d |>
                dplyr::select(tidyselect::any_of(col))
              # as.matrix()
              # Matrix::Matrix(sparse = T)
            } else {
              tibble::tibble(
                rep(0, nrow(d))
              ) |>
                purrr::set_names(col)
              # as.marti
              # Matrix::Matrix(sparse = T)
            }
          }
        ) |>
        purrr::list_rbind() |>
        # head() |>
        as.matrix() |>
        Matrix::Matrix(sparse = T)
    }
  ) |>
  purrr::reduce(cbind2)

set.seed(1138)
rapm_cv_25_all <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:4)],
    y = rapm_data_25[, 1],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    # parallel = T,
    standardize = F
  )

set.seed(1138)
rapm_25_all <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:4)],
    y = rapm_data_25[, 1],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    # parallel = T,
    standardize = F,
    lambda = rapm_cv_25_all$lambda.min
  )

rapm_25_all |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("all c rapm 25")


set.seed(1138)
rapm_cv_25_in_zone <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:4)],
    y = rapm_data_25[, 2],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_in_zone <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:4)],
    y = rapm_data_25[, 1],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_in_zone$lambda.min
  )

rapm_25_in_zone |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("in zone c rapm 25")

set.seed(1138)
rapm_cv_25_above_goal <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:4)],
    y = rapm_data_25[, 3],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_above_goal <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:4)],
    y = rapm_data_25[, 1],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_above_goal$lambda.min
  )

rapm_25_above_goal |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("above goal c rapm 25")
















set.seed(1138)
rapm_cv_22_game_weight <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 4],
    weights = as.integer(rapm_data_22[, 11] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_game_weight <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 4],
    weights = as.integer(rapm_data_22[, 11] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_game_weight$lambda.min
  )

rapm_22_game_weight |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg game weight rapm 22")


set.seed(1138)
rapm_cv_22_shot_type <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 5],
    weights = as.integer(rapm_data_22[, 11] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_shot_type <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 5],
    weights = as.integer(rapm_data_22[, 11] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_shot_type$lambda.min
  )

rapm_22_shot_type |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg shot type rapm 22")


set.seed(1138)
rapm_cv_22_rush_velo <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 6],
    weights = as.integer(rapm_data_22[, 11] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_rush_velo <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 6],
    weights = as.integer(rapm_data_22[, 11] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_rush_velo$lambda.min
  )

rapm_22_rush_velo |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg rush velo rapm 22")



set.seed(1138)
rapm_cv_22_blockers_basic <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 7],
    weights = as.integer(rapm_data_22[, 11] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_blockers_basic <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 7],
    weights = as.integer(rapm_data_22[, 11] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_blockers_basic$lambda.min
  )

rapm_22_blockers_basic |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers basic rapm 22")




set.seed(1138)
rapm_cv_22_blockers_shot_type_point <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 8],
    weights = as.integer(rapm_data_22[, 11] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_blockers_shot_type_point <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 8],
    weights = as.integer(rapm_data_22[, 11] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_blockers_shot_type_point$lambda.min
  )

rapm_22_blockers_shot_type_point |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers shot type point rapm 22")



set.seed(1138)
rapm_cv_22_fac_win_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 9],
    weights = as.integer(rapm_data_22[, 11] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_fac_win_secs <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 9],
    weights = as.integer(rapm_data_22[, 11] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_fac_win_secs$lambda.min
  )

rapm_22_fac_win_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg fac win secs rapm 22")



set.seed(1138)
rapm_cv_22_own_reach_follow_all_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 10],
    weights = as.integer(rapm_data_22[, 11] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_22_own_reach_follow_all_secs <-
  glmnet::glmnet(
    x = rapm_data_22[, -c(1:11)],
    y = rapm_data_22[, 10],
    weights = as.integer(rapm_data_22[, 11] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_22_own_reach_follow_all_secs$lambda.min
  )

rapm_22_own_reach_follow_all_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg own reach follow all secs rapm 22")













set.seed(1138)
rapm_cv_23_game_weight <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 5],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_game_weight <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 5],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_game_weight$lambda.min
  )

rapm_23_game_weight |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg game weight rapm 23")


set.seed(1138)
rapm_cv_23_shot_type <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 6],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_shot_type <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 6],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_shot_type$lambda.min
  )

rapm_23_shot_type |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg shot type rapm 23")


set.seed(1138)
rapm_cv_23_rush_velo <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 7],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_rush_velo <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 7],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_rush_velo$lambda.min
  )

rapm_23_rush_velo |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg rush velo rapm 23")



set.seed(1138)
rapm_cv_23_blockers_basic <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 8],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_blockers_basic <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 8],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_blockers_basic$lambda.min
  )

rapm_23_blockers_basic |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers basic rapm 23")




set.seed(1138)
rapm_cv_23_blockers_shot_type_point <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 9],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_blockers_shot_type_point <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 9],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_blockers_shot_type_point$lambda.min
  )

rapm_23_blockers_shot_type_point |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers shot type point rapm 23")



set.seed(1138)
rapm_cv_23_fac_win_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 10],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_fac_win_secs <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 10],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_fac_win_secs$lambda.min
  )

rapm_23_fac_win_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg fac win secs rapm 23")



set.seed(1138)
rapm_cv_23_own_reach_follow_all_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 11],
    weights = as.integer(rapm_data_23[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_23_own_reach_follow_all_secs <-
  glmnet::glmnet(
    x = rapm_data_23[, -c(1:11)],
    y = rapm_data_23[, 11],
    weights = as.integer(rapm_data_23[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_23_own_reach_follow_all_secs$lambda.min
  )

rapm_23_own_reach_follow_all_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg own reach follow all secs rapm 23")











set.seed(1138)
rapm_cv_24_game_weight <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 5],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_game_weight <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 5],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_game_weight$lambda.min
  )

rapm_24_game_weight |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg game weight rapm 24")


set.seed(1138)
rapm_cv_24_shot_type <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 6],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_shot_type <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 6],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_shot_type$lambda.min
  )

rapm_24_shot_type |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg shot type rapm 24")


set.seed(1138)
rapm_cv_24_rush_velo <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 7],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_rush_velo <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 7],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_rush_velo$lambda.min
  )

rapm_24_rush_velo |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg rush velo rapm 24")



set.seed(1138)
rapm_cv_24_blockers_basic <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 8],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_blockers_basic <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 8],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_blockers_basic$lambda.min
  )

rapm_24_blockers_basic |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers basic rapm 24")




set.seed(1138)
rapm_cv_24_blockers_shot_type_point <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 9],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_blockers_shot_type_point <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 9],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_blockers_shot_type_point$lambda.min
  )

rapm_24_blockers_shot_type_point |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers shot type point rapm 24")



set.seed(1138)
rapm_cv_24_fac_win_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 10],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_fac_win_secs <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 10],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_fac_win_secs$lambda.min
  )

rapm_24_fac_win_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg fac win secs rapm 24")



set.seed(1138)
rapm_cv_24_own_reach_follow_all_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 11],
    weights = as.integer(rapm_data_24[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_24_own_reach_follow_all_secs <-
  glmnet::glmnet(
    x = rapm_data_24[, -c(1:11)],
    y = rapm_data_24[, 11],
    weights = as.integer(rapm_data_24[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_24_own_reach_follow_all_secs$lambda.min
  )

rapm_24_own_reach_follow_all_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg own reach follow all secs rapm 24")














set.seed(1138)
rapm_cv_25_game_weight <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 5],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_game_weight <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 5],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_game_weight$lambda.min
  )

rapm_25_game_weight |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg game weight rapm 25")


set.seed(1138)
rapm_cv_25_shot_type <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 6],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_shot_type <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 6],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_shot_type$lambda.min
  )

rapm_25_shot_type |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg shot type rapm 25")


set.seed(1138)
rapm_cv_25_rush_velo <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 7],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_rush_velo <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 7],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_rush_velo$lambda.min
  )

rapm_25_rush_velo |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg rush velo rapm 25")



set.seed(1138)
rapm_cv_25_blockers_basic <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 8],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_blockers_basic <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 8],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_blockers_basic$lambda.min
  )

rapm_25_blockers_basic |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers basic rapm 25")




set.seed(1138)
rapm_cv_25_blockers_shot_type_point <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 9],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_blockers_shot_type_point <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 9],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_blockers_shot_type_point$lambda.min
  )

rapm_25_blockers_shot_type_point |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg blockers shot type point rapm 25")



set.seed(1138)
rapm_cv_25_fac_win_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 10],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_fac_win_secs <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 10],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_fac_win_secs$lambda.min
  )

rapm_25_fac_win_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg fac win secs rapm 25")



set.seed(1138)
rapm_cv_25_own_reach_follow_all_secs <-
  glmnet::cv.glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 11],
    weights = as.integer(rapm_data_25[, 4] * 2),
    nfolds = 10,
    alpha = 0,
    standardize = F
  )

set.seed(1138)
rapm_25_own_reach_follow_all_secs <-
  glmnet::glmnet(
    x = rapm_data_25[, -c(1:11)],
    y = rapm_data_25[, 11],
    weights = as.integer(rapm_data_25[, 4] * 2),
    alpha = 0,
    standardize = F,
    lambda = rapm_cv_25_own_reach_follow_all_secs$lambda.min
  )

rapm_25_own_reach_follow_all_secs |>
  coef() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  tibble::as_tibble() |>
  # View()
  dplyr::filter(rowname |> stringr::str_detect("\\d{7}")) |>
  dplyr::mutate(
    api_id = rowname |> stringr::str_extract("\\d{7}") |> as.integer(),
    name = rowname |> stringr::str_remove("\\d{7}_")
  ) |>
  tidyr::pivot_wider(
    id_cols = api_id,
    names_from = name,
    values_from = s0
  ) |>
  dplyr::mutate(
    net_rapm = off - def,
    net_shadow = shadow_off - shadow_def
  ) |>
  dplyr::left_join(
    players |>
      dplyr::select(api_id = event_player_1, player_name),
    by = c("api_id")
  ) |>
  View("xg own reach follow all secs rapm 25")


