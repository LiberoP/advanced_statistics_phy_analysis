data {
int<lower=1> N;
vector[N] x;
array[N] int<lower=0, upper=1> y;
}

parameters {
real b0;
}

model {
b0 ~ normal(0, 3);
y ~ bernoulli_logit(b0);
}

generated quantities {
array[N] int y_rep;
for (i in 1:N)
y_rep[i] = bernoulli_logit_rng(b0);

vector[N] log_lik;
for (i in 1:N)
log_lik[i] = bernoulli_logit_lpmf(y[i] | b0);
}