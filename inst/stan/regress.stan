
data{
  int<lower=1> n;
  int<lower=1> p;
  int<lower=0, upper=1> approach;
  vector[n] y;
  matrix[n, p] x;
  real mu_beta;
  real<lower=0> sigma_beta;
  real<lower=0> a_sigma;
  real<lower=0> b_sigma;
}

parameters{
  real<lower=0> sigma;
  vector[p] beta;
}

transformed parameters{
  real sigma2 = sigma^2;
}

model{
  y ~ normal(x*beta, sigma);

  if(approach==1){
    beta ~ normal(mu_beta, sigma_beta);
    sigma ~ gamma(a_sigma, b_sigma);
  }
}











