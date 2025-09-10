# GERA ESTRUTURA CAMPEONATO
generate_fixtures <- function() {
  num_teams <- 20
  teams <- 1:num_teams
  num_rounds <- (num_teams - 1) * 2
  schedule <- data.frame(round = integer(), team_name = integer(), opponent = integer())
  
  # First half of season
  rotating <- teams[-1]
  for (round in 1:(num_teams - 1)) {
    matches <- data.frame(round = round, team_name = teams[1], opponent = rotating[1])
    others <- rotating[-1]
    home_teams <- others[1:(length(others)/2)]
    away_teams <- rev(others)[1:(length(others)/2)]
    matches <- rbind(matches, data.frame(round = round, team_name = home_teams, opponent = away_teams))
    schedule <- rbind(schedule, matches)
    rotating <- c(rotating[-1], rotating[1])
  }
  
  # Second half (reverse fixtures)
  second_half <- schedule
  second_half$round <- second_half$round + (num_teams - 1)
  second_half <- data.frame(round = second_half$round,
                            team_name = second_half$team_name,
                            opponent = second_half$opponent)
  
  full_schedule <- rbind(schedule, second_half)
  full_schedule <- full_schedule[order(full_schedule$round), ]
  
  full_schedule <- full_schedule |> 
    mutate(
      split = ifelse(round >= 34, "test", "train"),
      comp = "gerado",
      season = 2025,
      date = "00/00/2025"
    )
  
  return(full_schedule)
}


# SIMULA GOLS
simulate_data <- function(full_schedule, model, params) {
  if (model == "double_poisson") {df = double_poisson(full_schedule, model, params)}
  if (model == "bivariated_poisson") {df = bivariated_poisson(full_schedule, model, params)}
  if (model == "dynamic_poisson") {df = dynamic_poisson(full_schedule, model, params)}
  
  # data para o stan
  data <- list(
    # train
    nteams = 20,
    ngames = nrow(df),
    team1 = df$team_name,
    team2 = df$opponent,
    y1 = df$gf,
    y2 = df$ga,
    # test
    ngames_test = nrow(df),
    team1_test = df$team_name,
    team2_test = df$opponent,
    # dynamic
    nrounds = length(unique(df$round)),
    round_t = df$round,
    round_t_test = df$round
  )
  
  return(list(
    data = data,
    params = params
  ))
}




# =========
# double_poisson
double_poisson <- function(full_schedule, model, params) {
  beta_0 = params$beta_0
  home = params$home
  sigma_att = params$sigma_att
  sigma_def = params$sigma_def
  
  params_rnd <- tibble(
    team_name = 1:20,
    att = rnorm(20, 0, sigma_att),
    def = rnorm(20, 0, sigma_def)
  )
  
  schedule_params <- full_schedule |> 
    left_join(params_rnd |> rename(att_x = att, def_x = def), by = "team_name") |> 
    left_join(params_rnd |> rename(att_y = att, def_y = def), by = c("opponent" = "team_name"))
  
  df <- schedule_params |> 
    mutate(
      lambda_gf = exp(beta_0 + att_x - def_y + home),
      lambda_ga = exp(beta_0 + att_y - def_x),
      gf = rpois(n(), lambda = lambda_gf),
      ga = rpois(n(), lambda = lambda_ga),
      result = case_when(
        gf > ga ~ "W",
        ga > gf ~ "L",
        gf == ga ~ "D",
        TRUE ~ NA
      )
    )
  
  return(df)
} 


# bivariated_poisson_diag_inflated
bivariated_poisson <- function(full_schedule, model, params) {
  beta_0 = params$beta_0
  home = params$home
  sigma_att = params$sigma_att
  sigma_def = params$sigma_def
  rho = params$rho
  
  params_rnd <- tibble(
    team_name = 1:20,
    att = rnorm(20, 0, sigma_att),
    def = rnorm(20, 0, sigma_def)
  )
  
  schedule_params <- full_schedule |> 
    left_join(params_rnd |> rename(att_x = att, def_x = def), by = "team_name") |> 
    left_join(params_rnd |> rename(att_y = att, def_y = def), by = c("opponent" = "team_name"))
  
  df <- schedule_params |> 
    mutate(
      # lambdas (inclui o + exp(rho) conforme o código original)
      lambda_gf = exp(beta_0 + att_x - def_y + home) + exp(rho),
      lambda_ga = exp(beta_0 + att_y - def_x) + exp(rho),
      
      # resultados "não contaminados" gerados por Poisson
      gf = rpois(n(), lambda = lambda_gf),
      ga = rpois(n(), lambda = lambda_ga),
      
      # resultado W/L/D
      result = case_when(
        gf > ga ~ "W",
        gf < ga ~ "L",
        gf == ga ~ "D",
        TRUE ~ NA_character_
      )
    )
  
  return(df)
} 

# dynamic_poisson
dynamic_poisson <- function(full_schedule, model, params) {
  beta_0 <- params$beta_0
  home <- params$home
  sigma_att <- params$sigma_att
  sigma_def <- params$sigma_def
  
  # derive teams and rounds from schedule
  teams <- sort(unique(c(full_schedule$team_name, full_schedule$opponent)))
  nrounds <- max(full_schedule$round)
  
  # simulate random-walk attack & defense per team across rounds
  params_rnd <- map_dfr(teams, function(team) {
    # initial values
    att0 <- rnorm(1, 0, sigma_att)
    def0 <- rnorm(1, 0, sigma_def)
    
    if (nrounds > 1) {
      att_step <- rnorm(nrounds - 1, 0, sigma_att)
      def_step <- rnorm(nrounds - 1, 0, sigma_def)
      att_vec <- att0 + c(0, cumsum(att_step))
      def_vec <- def0 + c(0, cumsum(def_step))
    } else {
      att_vec <- att0
      def_vec <- def0
    }
    
    tibble(
      round = seq_len(nrounds),
      team_name = team,
      att = att_vec,
      def = def_vec
    )
  })
  
  # join team-round params to schedule (home team = team_name)
  schedule_params <- full_schedule %>%
    left_join(params_rnd %>% rename(att_x = att, def_x = def),
              by = c("round", "team_name")) %>%
    left_join(params_rnd %>% rename(att_y = att, def_y = def),
              by = c("round" = "round", "opponent" = "team_name"))
  
  # simulate goals
  df <- schedule_params %>%
    mutate(
      lambda_gf = exp(beta_0 + att_x - def_y + home),   # goals for (home)
      lambda_ga = exp(beta_0 + att_y - def_x),          # goals against (away)
      gf = rpois(n(), lambda = lambda_gf),
      ga = rpois(n(), lambda = lambda_ga),
      result = case_when(
        gf > ga ~ "W",
        ga > gf ~ "L",
        gf == ga ~ "D",
        TRUE ~ NA_character_
      )
    )
  
  return(df)
}