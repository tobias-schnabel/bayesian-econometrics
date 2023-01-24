data {

  int <lower = 0> N; // Defining the number of defects in the test dataset
  // response
  int <lower = 0, upper = 1> y [N];
  // number of columns in the design matrix X
  int <lower = 0> K;
  // design matrix X
  // does not include an intercept
  matrix [N, K] X;
   // Defining the number of probabilistic predictions required from the model
  //vector <lower = 0> [K] default_pred;
  //keep responses
  int use_log_lik;
  int use_y_rep;
}
parameters {
  // The (unobserved) model parameters that we want to recover
  real alpha;
  vector[K] beta;
}
transformed parameters {
  vector[N] eta;
  eta = alpha + X * beta;
}
model {

  // multiple logistic regression model 
  y ~ bernoulli_logit(eta);
  
  // Prior models for the unobserved parameters
  // alpha ~ normal(0, 1);
  // beta ~ normal(1, 1);

}

generated quantities {
  // simulate data from the posterior
  vector[N * use_y_rep] y_rep;
  // log-likelihood posterior
  vector[N * use_log_lik] log_lik;
  for (i in 1:num_elements(y_rep)) {
    y_rep[i] = bernoulli_rng(inv_logit(eta[i]));
  }
  for (i in 1:num_elements(log_lik)) {
    log_lik[i] = bernoulli_logit_lpmf(y[i] | eta[i]);
  }
}
