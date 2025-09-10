library(latex2exp)

# STAN
fit_stan <- function(data, model) {
  model_stan = cmdstan_model(paste0("models_stan/", model, "/", model, ".stan"))
  
  fit = model_stan$sample(
    data = data$data,
    iter_sampling = 2500,
    iter_warmup = 2500,
    chains = 1,
    parallel_chains = 1,
  )
  
  summary <- fit$summary()
  
  # retorne ambos
  list(
    posterior = summary
  )
}





# PLOT
make_plot <- function(folder, M, params) {
  true_params <- bind_rows(params) |> 
    pivot_longer(names_to = "variable", values_to = "true_value", cols = everything()) |> 
    expand_grid(replica = seq_len(M))
  
  resultados <- readRDS(paste0("data/geracao_dados_stan/", folder, "/result.rds"))
  
  df_summary <- map_dfr(
    resultados,
    ~ .x$posterior,
    .id = "replica"
  ) |>
    mutate(replica = as.integer(replica)) |> 
    inner_join(true_params, by = c("replica", "variable")) |> 
    mutate(
      bias = (mean - true_value),
      rel_bias = bias/true_value
    )
  
  param_labels <- c(
    beta_0 = TeX("$\\beta_{int}$"),
    home = TeX("$\\beta_{casa}$"),
    sigma_att = TeX("$\\sigma_{atq}$"),
    sigma_def = TeX("$\\sigma_{def}$"),
    rho = TeX("$\\rho$")
  )
  plot <- ggplot(df_summary, aes(x = variable, y = bias)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
    labs(x= "Parâmetros", y= "Viés") +
    scale_x_discrete(labels = param_labels)
  
  plot1 <- ggplot(df_summary, aes(x = variable, y = rel_bias)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
    labs(x= "Parâmetros", y= "Viés relativo") +
    scale_x_discrete(labels = param_labels)
  
  #return(list(plot, plot1))
  return(plot)
}
