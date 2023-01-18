data {

  int <lower = 0> N; // Defining the number of defects in the test dataset
  // response
  real y[N];
  // number of columns in the design matrix X
  int <lower = 0> K;
  // design matrix X
  // does not include an intercept
  matrix [N, K] X;
   // Defining the number of probabilistic predictions required from the model
  vector <lower = 0> [K] default_pred;
  
}

parameters {
  
  // The (unobserved) model parameters that we want to recover
  real alpha;
  real beta;
  
}

model {

  // multiple logistic regression model 
  y ~ bernoulli_logit(alpha + beta * log(X));
  
  // Prior models for the unobserved parameters
  alpha ~ normal(0, 1);
  beta ~ normal(1, 1);

}

generated quantities {
  
  // Using the fitted model for  prediction.
  // estimate posterior predictive distributions for corresponding X
  vector [K] postpred_pr;
  
  for (k in 1:K) {
    
    postpred_pr[k] = inv_logit(alpha + beta * log(default_pred[k]));
    
  }
  
}