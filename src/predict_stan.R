library(tidyverse)
library(cmdstanr)
library(here)
library(posterior)


predicted_results <- function(test_data, draws) {
  test_data$home_win <- NA
  test_data$draw <- NA
  test_data$home_lost <- NA
  test_data$lambda_home <- NA
  test_data$lambda_away <- NA
  
  
  for (i in 1:nrow(test_data)) {
    y1 <- posterior::draws_of(draws$y1_pred)[, i]  # Posterior samples for X (home), length 10000
    y2 <- posterior::draws_of(draws$y2_pred)[, i]  # Posterior samples for Y (away), length 10000
    n_preds <- length(y1)                         # Number of samples
    
    test_data$home_win[i] <- sum(y1 > y2) / n_preds   # P(X > Y): Probability of home win
    test_data$draw[i] <- sum(y1 == y2) / n_preds      # P(X == Y): Probability of draw
    test_data$home_lost[i] <- sum(y1 < y2) / n_preds  # P(X < Y): Probability of home loss
    
    test_data$lambda_home[i] <- mean(y1)  # Expected goals (home)
    test_data$lambda_away[i] <- mean(y2)  # Expected goals (away)
    
  }
  
  return(test_data)
}

fit_stan_predictions <- function(league, model) {
  data <- readRDS(here("data", "train_test", paste0(league, ".rds")))
      
  # ONLY ONE SEASON            
  train <- data$train |> 
    filter(temporada %in% c("2024", "2024-2025"))
  teams <- unique(c(train$time_casa, train$time_fora))
  train <- train |> 
    mutate(
      time_casa_id = match(time_casa, teams),
      time_fora_id = match(time_fora, teams)
    )
  test <- data$test |> 
    filter(temporada %in% c("2024", "2024-2025")) |> 
    mutate(
      time_casa_id = match(time_casa, teams),
      time_fora_id = match(time_fora, teams)
    )
  
  # PARAMS
  iter = 2500
  warmup = 2500
  chains = 4
  cores = 8
  
  # DATA
  data_stan <- list(
    # train
    nteams = length(unique(train$time_casa)),
    ngames = nrow(train),
    team1 = train$time_casa_id,
    team2 = train$time_fora_id,
    y1 = train$gols_casa,
    y2 = train$gols_fora,
    # test
    ngames_test = nrow(test),
    team1_test = test$time_casa_id,
    team2_test = test$time_fora_id,
    # dynamic
    nrounds = length(unique(train$rodada)) + length(unique(test$rodada)),
    round_t = train$rodada,
    round_t_test = test$rodada
  )
  
  # MODEL AND FIT
  model_path <- cmdstan_model(here("models_stan", model, paste0(model, ".stan")))
  
  fit <- model_path$sample(
    data = data_stan,
    iter_sampling = iter,
    iter_warmup = warmup,
    chains = chains,
    parallel_chains = cores,
    
  )
  
  fit$save_object(file = here("data", "fits_stan", model, paste0(model, "_", league, ".rds")))
  
  draws <- posterior::as_draws_rvars(fit$draws())
  preds <- predicted_results(test, draws) |> 
    select(-contains("_mm")) |> 
    mutate(
      pred_result = case_when(
        home_win > draw & home_win > home_lost ~ "W",
        home_lost > draw & home_lost > home_win ~ "L",
        TRUE ~ "D"
      ),
      pred_success = ifelse(resultado == pred_result, 1, 0)
    )
  
  saveRDS(preds, here("results", "stan", paste0(model, "_", league, ".rds")))
}


### ============================================================================
leagues <- list(
  'double_poisson' = 'premier_league',
  'double_poisson' = 'brasileirao_a',
  'double_poisson' = 'bundesliga',
  'double_poisson' = 'laliga',
  'double_poisson' = 'serie_a',

  'bivariated_poisson' = 'premier_league',
  'bivariated_poisson' = 'brasileirao_a',
  'bivariated_poisson' = 'bundesliga',
  'bivariated_poisson' = 'laliga',
  'bivariated_poisson' = 'serie_a',
  
  'dynamic_poisson' = 'premier_league',
  'dynamic_poisson' = 'brasileirao_a',
  'dynamic_poisson' = 'bundesliga',
  'dynamic_poisson' = 'laliga',
  'dynamic_poisson' = 'serie_a'
)

imap(leagues, ~ fit_stan_predictions(.x, .y))
