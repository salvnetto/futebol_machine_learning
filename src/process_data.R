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
      "formacao" = "formation"
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


leagues <- list(
  'england' = 'premier_league',
  'brazil' = 'brasileirao_a',
  'germany' = 'bundesliga',
  'spain' = 'laliga',
  'italy' = 'serie_a'
)

imap(leagues, ~ process_data(.y, .x))

