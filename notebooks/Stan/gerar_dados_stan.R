library(tidyverse)
library(cmdstanr)
library(posterior)
library(here)
library(snowfall)

source(here("src", "sim_data.R"))
source(here("src", "fit_sim_data.R"))

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfLibrary(tidyverse)
sfLibrary(cmdstanr)
sfLibrary(posterior) 


# Parametros
M <- 200
folder <- model <- "double_poisson" #bivariated_poisson #dynamic_poisson #double_poisson
params <- list(
  sigma_att = 0.05,
  sigma_def = 0.05,
  home = .35,
  beta_0 = -.015,
  ## bivariated
  rho = -.5
)


dir.create(file.path("data", "geracao_dados_stan",folder, "dataset"), recursive = TRUE, showWarnings = FALSE)

full_schedule <- generate_fixtures()

sfExport(
  "simulate_data", "fit_stan", "generate_fixtures", "make_plot", 
  "double_poisson", "folder", "bivariated_poisson", "dynamic_poisson",
  "params", "model", "full_schedule"
)

# SIMULA DADOS
sfLapply(1:M, function(i){
  data <- simulate_data(full_schedule, model, params)
  saveRDS(data, paste0("data/geracao_dados_stan/", folder, "/dataset/data", i, ".rds"))
  return(NULL)
})


# FITA OS DADOS

resultados <- sfLapply(1:M, function(i){
  data_gen <- readRDS(paste0("data/geracao_dados_stan/", folder, "/dataset/data", i, ".rds"))
  result <- fit_stan(data_gen, model)
  return(result)
})

saveRDS(resultados, paste0("data/geracao_dados_stan/", folder, "/result.rds"))
sfStop()

# GRAFICO FINAL
plot <- make_plot(folder, M, params)
ggsave(paste0("results/Stan/", folder, "/plot_bias.png"), plot = plot[[1]], width = 6, height = 4, dpi = 300)
ggsave(paste0("results/Stan/", folder, "/plot_rel_bias.png"), plot = plot[[2]], width = 6, height = 4, dpi = 300)


# plot1 <- make_plot("double_poisson", M, params) + ggtitle("Poisson Independente")
# plot2 <- make_plot("bivariated_poisson", M, params) + ggtitle("Poisson Bivariada")
# plot3 <- make_plot("dynamic_poisson", M, params) + ggtitle("Poisson Dinâmica")
# combined_plot <- grid.arrange(plot1, plot2, plot3, ncol = 1)
# ggsave("resultados_viés.png", combined_plot, width = 6, height = 5)
