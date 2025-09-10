data {
  //train
  int<lower=1> nrounds;
  int<lower=1> nteams;
  int<lower=1> ngames;
  array[ngames] int<lower=1, upper=nrounds> round_t;
  array[ngames] int<lower=1, upper=nteams> team1;
  array[ngames] int<lower=1, upper=nteams> team2;
  array[ngames] int<lower=0> y1;
  array[ngames] int<lower=0> y2;

  //test
  int<lower=1> ngames_test;
  array[ngames_test] int<lower=1, upper=nrounds> round_t_test;
  array[ngames_test] int<lower=1, upper=nteams> team1_test;
  array[ngames_test] int<lower=1, upper=nteams> team2_test;
}

parameters {
  matrix[nrounds, nteams] att_raw;
  matrix[nrounds, nteams] def_raw;
  real<lower=0> sigma_att; // Standard deviation of att parameter
  real<lower=0> sigma_def; // Standard deviation of def parameter
  real home;
  real beta_0; 
}

transformed parameters {
  matrix[nrounds, nteams] att;
  matrix[nrounds, nteams] def;
  vector[ngames] theta1;
  vector[ngames] theta2;

  for (t in 1:nrounds){
    att[t] = att_raw[t] - mean(att_raw[t]);
    def[t] = def_raw[t] - mean(def_raw[t]);
  }

  for (k in 1:ngames) {
    theta1[k] = beta_0 + att[round_t[k], team1[k]] - def[round_t[k], team2[k]] + home;
    theta2[k] = beta_0 + att[round_t[k], team2[k]] - def[round_t[k], team1[k]];
  }
}

model{
  // Priors
  sigma_att ~ cauchy(0, 2.5);
  sigma_def ~ cauchy(0, 2.5);
  home ~ normal(0, 10);
  beta_0 ~ normal(0, 10);

  target += normal_lpdf(att_raw[1] | 0, sigma_att);
  target += normal_lpdf(def_raw[1] | 0, sigma_def);


  for (t in 2:nrounds) {
      target += normal_lpdf(att_raw[t] | att_raw[t-1], sigma_att);
      target += normal_lpdf(def_raw[t] | def_raw[t-1], sigma_def);
  }

  // Likelihood
  for (k in 1:ngames) {
    target += poisson_log_lpmf(y1[k] | theta1[k]);
    target += poisson_log_lpmf(y2[k] | theta2[k]);
  }
}

generated quantities {
  vector[ngames] log_lik;
  vector[ngames_test] theta1_new;
  vector[ngames_test] theta2_new;
  array[ngames_test] int y1_pred;
  array[ngames_test] int y2_pred;

  // Log-likelihood for training data
  for (k in 1:ngames) {
    log_lik[k] = poisson_log_lpmf(y1[k] | theta1[k]) +
                 poisson_log_lpmf(y2[k] | theta2[k]);
  }

  // Predictive distributions for test data
  for (k in 1:ngames_test) {
    theta1_new[k] = beta_0 + att[round_t_test[k], team1_test[k]] - def[round_t_test[k], team2_test[k]] + home;
    theta2_new[k] = beta_0 + att[round_t_test[k], team2_test[k]] - def[round_t_test[k], team1_test[k]];
  }

  y1_pred = poisson_log_rng(theta1_new);
  y2_pred = poisson_log_rng(theta2_new);
}
