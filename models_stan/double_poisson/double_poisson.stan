data {
  int<lower=1> nteams;
  int<lower=1> ngames;
  array[ngames] int team1; // Index of home team
  array[ngames] int team2; // Index of away team 
  array[ngames] int<lower=0> y1; // Goals home team
  array[ngames] int<lower=0> y2; // Goals away team
  
  int<lower=1> ngames_test;
  array[ngames_test] int team1_test; 
  array[ngames_test] int team2_test;
}

parameters {
  vector[nteams] att; 
  vector[nteams] def;
  real beta_0;
  real home;
  real<lower=0> sigma_att; // Standard deviation of att parameter
  real<lower=0> sigma_def; // Standard deviation of def parameter
}

transformed parameters {
  vector[ngames] theta1;
  vector[ngames] theta2;

  theta1 = (beta_0 + att[team1] - def[team2] + home);
  theta2 = (beta_0 + att[team2] - def[team1]);
}


model {
  // Priors distributions for these parameters
  sigma_att ~ cauchy(0, 2.5);
  sigma_def ~ cauchy(0, 2.5);
  att ~ normal(0, sigma_att);
  def ~ normal(0, sigma_def);
  home ~ normal(0, 10);
  beta_0 ~ normal(0, 10);

  // Likelihood
  for (k in 1:ngames) {
    target += poisson_log_lpmf(y1[k] | theta1[k]);
    target += poisson_log_lpmf(y2[k] | theta2[k]);
  }
}

generated quantities {
  vector[ngames] log_lik; 
  vector[ngames_test] theta1_pred;
  vector[ngames_test] theta2_pred;
  array[ngames_test] int y1_pred; 
  array[ngames_test] int y2_pred; 
  
  for (k in 1:ngames) {
    log_lik[k] = poisson_log_lpmf(y1[k] | theta1[k]) +
                 poisson_log_lpmf(y2[k] | theta2[k]);
  }

  theta1_pred = exp(beta_0 + att[team1_test] - def[team2_test] + home);
  theta2_pred = exp(beta_0 + att[team2_test] - def[team1_test]);

  y1_pred = poisson_rng(theta1_pred); 
  y2_pred = poisson_rng(theta2_pred); 
}

