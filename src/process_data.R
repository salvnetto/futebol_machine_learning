library(tidyverse)
library(FootStats)
library(here)

process_data <- function(country, league) {
  df_raw = FootStats::load_data(country, league)
  
  df <- df_raw |> 
    select(
      -opp.formation,
      -team_opp_id,
      -team_id,
      -aerials_lost,
      -sota, 
      -fld,
      -xga,
      -xag,
    ) |> 
    rename(
      "rodada" = "round",
      "competicao" = "comp",
      "temporada" = "season",
      "data" = "date",
      "dia" = "day",
      "hora" = "time",
      "formacao" = "formation",
      "resultado" = "result",
      "capitao" = "captain",
      "arbitro" = "referee",
      "gols_casa" = "gf",
      "gols_fora" = "ga",
      "publico" = "attendance",
      "chutes" = "sh",
      "chutes_a_gol" = "sot",
      "distancia_media_chute" = "dist",
      "gols_esperados" = "xg",
      "passes_completos" = "cmp",
      "passes_tentados" = "att",
      "assistencias" = "ast",
      "escanteios" = "ck",
      "cruzamentos" = "crs",
      "assistencias_esperadas" = "xa",
      "acoes_de_criacao_de_gol" = "gca",
      "posse_de_bola" = "poss",
      "acoes_de_criacao_de_chute" = "sca",
      "defesas_do_goleiro" = "saves",
      "gols_esperados_pos_chute" = "psxg",
      "duelos_aereos_vencidos" = "aerials_won",
      "bloqueios" = "blocks",
      "faltas_cometidas" = "fls",
      "desarmes" = "tkl",
      "recuperacoes" = "recov",
      "desarmes_com_sucesso" = "tklw",
      "toques_na_bola" = "touches",
      "intercepcoes" = "int",
      "passes_chaves" = "kp",
      "cruzamentos_na_pequena_area" = "crspa",
      "passes_progressivos" = "prgp",
      "invertidas" = "sw",
      "erros" = "err",
      "distancia_total_com_posse_de_bola" = "totdist",
      "distancia_progressiva_com_posse_de_bola" = "prgdist",
      "conducoes_progressiva_com_posse_de_bola" = "prgc",
      "erro_de_controle_de_bola" = "mis",
      "cartoes_vermelhos" = "crdr",
      "cartoes_amarelos" = "crdy",
      "impedimentos" = "off",
      "perda_de_posse" = "dis",
      "chutoes" = "clr",
      "conducoes_terco_final" = "carries_att_3rd",
      "passes_terco_final" = "pass_3rd",
      "desarmes_no_ataque" = "tkl_att_3rd",
      "desarmes_no_meio_de_campo" = "tkl_mid_3rd",
      "desarmes_na_defesa" = "tkl_def_3rd",
      "toque_no_ataque" = "touches_att_3rd",
      "toque_no_meio_de_campo" = "touches_mid_3rd",
      "toque_na_defesa" = "touches_def_3rd",
      "toque_na_pequena_area_do_ataque" = "touches_att_pen",
      "toque_na_pequena_area_da_defesa" = "touches_def_pen"
    ) |> 
    group_by(venue) |> 
    group_split(.keep = TRUE) |> 
    set_names(c("Away", "Home")) |> 
    map(~ {
      if (.x$venue[1] == "Home") {
        rename(.x,
               home_team = team_name,
               away_team = opponent)
      } else {
        rename(.x,
               home_team = opponent,
               away_team = team_name)
      }
    })
    
  
  keys <- c("competicao", "temporada", "rodada", "data", "dia", "hora", "arbitro", "home_team", "away_team")
  
  database <- df$Home |> 
    select(-venue) |> 
    left_join(
      df$Away |> 
        select(-gols_casa, -gols_fora, -publico, -resultado, -venue),
      by = keys,
      suffix = c(".casa", ".fora")
    ) %>%
    select(all_of(keys), sort(setdiff(colnames(.), keys))) |> 
    rename(
      "time_casa" = "home_team",
      "time_fora" = "away_team",
    )
  
    
  
  saveRDS(database, here("data", paste0("database_", league,".rds")))
}

train_test_processing <- function(league, window) {
  df <- readRDS(here("data", paste0("database_", league, ".rds")))
  
  df_processed <- df |>
    mutate(
      'gols_xg.casa' = gols_casa - gols_esperados.casa,
      'gols_xg.fora' = gols_fora - gols_esperados.fora,
      
      'gols_xg_contra.casa' = gols_casa - gols_esperados.fora,
      'gols_xg_contra.fora' = gols_fora - gols_esperados.casa,
      
      'taxa_chute_a_gol.casa' = if_else(chutes.casa == 0, 0, chutes_a_gol.casa / chutes.casa),
      'taxa_chute_a_gol.fora' = if_else(chutes.fora == 0, 0, chutes_a_gol.fora / chutes.fora),
      
      'gols_por_chute.casa' = if_else(chutes.casa == 0, 0, gols_casa / chutes.casa),
      'gols_por_chute.fora' = if_else(chutes.fora == 0, 0, gols_fora / chutes.fora),
      
      'gols_por_chute_a_gol.casa' = if_else(chutes_a_gol.casa == 0, 0, gols_casa / chutes_a_gol.casa),
      'gols_por_chute_a_gol.fora' = if_else(chutes_a_gol.fora == 0, 0, gols_fora / chutes_a_gol.fora),
    ) |> 
    arrange(data) |> 
    mutate(match_id = row_number())
  
  # First rolling stats (home/away separately)
  home_stats <- df_processed |> 
    select(match_id, temporada, rodada, publico, data, hora, time_casa, gols_feitos = gols_casa, gols_sofridos = gols_fora, dplyr::contains(".casa")) |> 
    group_by(time_casa, temporada) |> 
    mutate(
      across(
        .cols = where(is.numeric) & !c(match_id, rodada),
        .fns = ~ zoo::rollmean(lag(.x), k = window, fill = NA, align = "right"),
        .names = "{.col}_mm{window}.casa"
      )
    ) |> 
    ungroup() |> 
    select(match_id, dplyr::contains("_mm"))
  
  away_stats <- df_processed |> 
    select(match_id, temporada, rodada, data, publico, hora, time_fora, gols_feitos = gols_fora, gols_sofridos = gols_casa, dplyr::contains(".fora")) |> 
    group_by(time_fora, temporada) |> 
    mutate(
      across(
        .cols = where(is.numeric) & !c(match_id, rodada),
        .fns = ~ zoo::rollmean(lag(.x), k = window, fill = NA, align = "right"),
        .names = "{.col}_mm{window}.fora"
      )
    ) |> 
    ungroup() |> 
    select(match_id, dplyr::contains("_mm"))
  
  team_stats <- df_processed |> 
    left_join(home_stats, by = "match_id") |> 
    left_join(away_stats, by = "match_id")
  
  # Second rolling stats (team-level regardless of local)
  home_stats <- team_stats |> 
    select(match_id, resultado, temporada, data, rodada, time = time_casa, gols_feitos = gols_casa, gols_sofridos = gols_fora, contains(".casa")) |> 
    mutate(local = "casa") |> 
    rename_with(~ str_remove(.x, "\\.casa$"), .cols = dplyr::contains(".casa")) |> 
    mutate(
      pontos = case_when(
        resultado == "W" ~ 3,
        resultado == "D" ~ 1,
        resultado == "L" ~ 0,
      )
    ) |> 
    select(-resultado)
  
  away_stats <- team_stats |> 
    select(match_id, resultado, temporada, data, rodada, time = time_fora, gols_feitos = gols_fora, gols_sofridos = gols_casa, contains(".fora")) |> 
    mutate(local = "fora") |> 
    rename_with(~ str_remove(.x, "\\.fora$"), .cols = dplyr::contains(".fora")) |> 
    mutate(
      pontos = case_when(
        resultado == "W" ~ 3,
        resultado == "D" ~ 1,
        resultado == "L" ~ 0,
      )
    ) |> 
    select(-resultado)
  
  
  team_stats2 <- bind_rows(home_stats, away_stats) |> 
    arrange(temporada, data, rodada) |> 
    group_by(time, temporada) |> 
    select(-contains("_mm")) |> 
    mutate(
      across(
        .cols = where(is.numeric) & !c(match_id, rodada),
        .fns = ~ zoo::rollmean(lag(.x), k = window, fill = NA, align = "right"),
        .names = "{.col}_mm{window}.time"
      ),
      pontos_ate_partida.time = cumsum(pontos)
    ) |> 
    ungroup() |> 
    select(match_id, time, contains(".time")) 
  
  # FINAL
  df_processed_final <- team_stats |> 
    left_join(team_stats2, by = c("match_id", "time_casa" = "time"), suffix = c("_casa", "_remover")) |> 
    left_join(team_stats2, by = c("match_id", "time_fora" = "time"), suffix = c("_fora", "_remover")) |> 
    select(-contains("_remover"))
  
  data <- df_processed_final |> 
    select(
      match_id,
      temporada,
      rodada,
      data,
      dia,
      hora,
      arbitro,
      time_casa,
      gols_casa,
      gols_fora,
      time_fora,
      resultado,
      contains("_mm")
    )
  
  train <- data |> 
    filter(
      temporada != "2025",
      !(temporada %in% c("2024", "2024-2025") & rodada %in% 30:38)
    )
  
  test <- data |> 
    filter(
      temporada != "2025",
      temporada %in% c("2024", "2024-2025") & rodada %in% 30:38
    )
  
  
  
  train_test <- list(
    train = train,
    test = test
  )
  
  # RETURN
  saveRDS(train_test, here("data", "train_test", paste0(league, ".rds")))
  
}




### ============================================================================
leagues <- list(
  'england' = 'premier_league',
  'brazil' = 'brasileirao_a',
  'germany' = 'bundesliga',
  'spain' = 'laliga',
  'italy' = 'serie_a'
)

imap(leagues, ~ process_data(.y, .x))
imap(leagues, ~ train_test_processing(.x, window = 3))