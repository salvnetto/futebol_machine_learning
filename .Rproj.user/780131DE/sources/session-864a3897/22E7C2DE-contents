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
      -aerials_lost
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
    
  
  keys <- c("comp", "season", "round", "date", "day", "time", "referee", "home_team", "away_team")
  
  database <- df$Home |> 
    select(-venue) |> 
    left_join(
      df$Away |> 
        select(-gf, -ga, -attendance, -result, -venue),
      by = keys,
      suffix = c(".home", ".away")
    ) %>%
    select(all_of(keys), sort(setdiff(colnames(.), keys)))
  
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

