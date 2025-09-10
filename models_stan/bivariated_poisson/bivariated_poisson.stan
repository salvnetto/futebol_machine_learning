data{
  //train
  int<lower=1> nteams; 
  int<lower=1> ngames;
  array[ngames] int team1; 
  array[ngames] int team2; 
  array[ngames] int<lower=0> y1;
  array[ngames] int<lower=0> y2;
  //test
  int<lower=1> ngames_test;
  array[ngames_test] int team1_test;
  array[ngames_test] int team2_test; 
}

parameters{
  vector[nteams] att; 
  vector[nteams] def; 
  real home; 
  real rho; // Covariance parameter - Parameter controlling the dependence between home and away team goals
  real beta_0; 
  real<lower=0> sigma_att; // Standard deviation of att parameter
  real<lower=0> sigma_def; // Standard deviation of def parameter
}

transformed parameters{
  vector[ngames] theta1; 
  vector[ngames] theta2; 
  vector[ngames] theta3; // Covariance parameter

  for (n in 1:ngames){
    theta1[n] = exp(beta_0 + att[team1[n]] - def[team2[n]] + home); 
    theta2[n] = exp(beta_0 + att[team2[n]] - def[team1[n]]); 
    theta3[n] = exp(rho); 
  }
}

model{
  // Priors distributions
  sigma_att ~ cauchy(0, 2.5);
  sigma_def ~ cauchy(0, 2.5);
  att ~ normal(0, sigma_att);
  def ~ normal(0, sigma_def);
  beta_0 ~ normal(0, 10);
  home ~ normal(0, 10);
  rho ~ normal(0, 10);

// likelihood
  for (n in 1:ngames){
    target+=poisson_lpmf(y1[n]| theta1[n] + theta3[n]);
    target+=poisson_lpmf(y2[n]| theta2[n] + theta3[n]);
  }
}

generated quantities {
  vector[ngames] log_lik; 
  vector[ngames_test] theta1_pred;
  vector[ngames_test] theta2_pred;
  vector[ngames_test] theta3_pred; 
  array[ngames_test] int y1_pred;
  array[ngames_test] int y2_pred;

  // Calculate log-likelihood for each observation
  for (k in 1:ngames) {
    log_lik[k] = poisson_lpmf(y1[k] | theta1[k] + theta3[k]) +
                 poisson_lpmf(y2[k] | theta2[k] + theta3[k]);
  }

  for (n in 1:ngames_test){
    theta1_pred[n] = exp(beta_0 + att[team1_test[n]] - def[team2_test[n]] + home);
    theta2_pred[n] = exp(beta_0 + att[team2_test[n]] - def[team1_test[n]]);
    theta3_pred[n] = exp(rho);

    y1_pred[n] = poisson_rng(theta1_pred[n] + theta3_pred[n]);
    y2_pred[n] = poisson_rng(theta2_pred[n] + theta3_pred[n]);
  }
}
