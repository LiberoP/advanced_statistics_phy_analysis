library(ggplot2) # even used?
data <- scan("C:/Users/Libero/uni/1B/adv_stats/assignment_1/dataset.txt", what=integer())
typeof(data)
#[1] "integer"
length(data)
#[1] 500

hist(data, breaks=20) # sqrt(500) ca. 22
#a plot...

s <- sum(data)
cat(s)
#2670
n <- length(data)
cat(n)
#500

# therefore choice of gamma prior parameters will have negligible impact; so:
alpha=2
beta=1
alpha_prime <- s + alpha 
beta_prime <- beta + n
cat(alpha_prime, beta_prime)
#2672 501

curve(dgamma(x, shape = alpha_prime, rate = beta_prime), from = 0, to = 10, main="Posterior", xlab="Lambda parameter", ylab="Probability", col = "red", lwd = 2)
# prettier?

lambda_map=(alpha_prime-1)/beta_prime
posterior_mean=alpha_prime/beta_prime
posterior_std=sqrt(alpha_prime / beta_prime **2)
skewness=2.0/sqrt(alpha_prime)
cat(lambda_map, posterior_mean, posterior_std, skewness)
#5.331337 5.333333 0.1031764 0.03869116
# skewness << 1

x_vals <- 0:max(data)  
hist(data, prob = TRUE, breaks = seq(min(data)-0.5, max(data)+0.5, by = 1), main = "Poisson model: prediction vs data", xlab = "Counts", ylab="Frequency", ylim=c(0,0.2))
points(x_vals, dpois(x_vals, lambda=lambda_map), col = "red", type = 'h', lwd=3)
# prettier?

p_s <- beta_prime/(1+beta_prime)
x_bar= mean(data)
cat(x_bar)
#5.34
plot(0, type = "n", xlim = range(x_vals), ylim = c(0, 0.2), 
     xlab = "x", ylab = "Probability", main = "Prediction for new event")
points(x_vals, dnbinom(x_vals, size=alpha_prime, prob=p_s), col = "red", type = 'h', lwd=3)
abline(v = 2*x_bar, col = "blue", lty = 4, lwd=2)
# prettier?

cdf_2_x_bar = pnbinom(2*x_bar, size=alpha_prime, prob=p_s)
prob_excess= 1- cdf_2_x_bar
cat(prob_excess)
#0.02092887

###########################################################
###########################################################
alpha_1=2
beta_1=2
alpha_2=2
beta_2=1




N_samples <- 100000
# sample from priors
r_mc  <- rgamma(N_samples, shape=alpha_2, rate=beta_2)
pi_mc <- rbeta(N_samples,  alpha_1, beta_1)
# log-likelihood at each sample
log_likelihood <- sapply(1:N_samples, function(i) # apply function element-by-element, from 1 to N_samples
  sum(dnbinom(data, size=r_mc[i], prob=pi_mc[i], log=TRUE))
)
# norm constant = E[likelihood] under prior
diff_log_likelihood <- log_likelihood - max(log_likelihood)  # for stability
norm_const <- mean(exp(diff_log_likelihood)) * exp(max(log_likelihood))
cat(norm_const)
#fix this?? (norm for 3.1)




# Define the log of the unnormalised posterior (for numerical stability):
log_unnorm_posterior <- function(r, pi) {
  # Binomial coefficient part:
  log_bin <- sum(lchoose(data + r - 1, data))
  log_rest <- (alpha_1 - 1 + n * r) * log(pi) + 
              (beta_1 - 1 + s) * log(1 - pi) + 
              (alpha_2 - 1) * log(r) - beta_2 * r
  return(log_bin + log_rest)
}
# Create a grid of r and pi values
r_vals <- seq(0.01, 20, length.out = 100)   
pi_vals <- seq(0.01, 0.99, length.out = 100)
grid <- expand.grid(r = r_vals, pi = pi_vals)
grid$log_z <- mapply(log_unnorm_posterior, grid$r, grid$pi)
# Plot using image() (color map) or contour()
# Reshape to matrix for image/contour
z_mat <- matrix(grid$log_z, nrow = length(r_vals), ncol = length(pi_vals))
image(r_vals, pi_vals, z_mat, xlab = "r", ylab = "pi", 
      main = "Unnnormalised log posterior")
contour(r_vals, pi_vals, z_mat, add = TRUE, col = "white", levels = seq(-5000, 0, by=50))  # white contours
# prettier?
# Alternatively, a perspective plot:
persp(r_vals, pi_vals, z_mat, theta = 30, phi = 20, 
      xlab = "r", ylab = "pi", zlab = "log post")
# prettier?

neg_log_unn_posterior <- function(params) -log_unnorm_posterior(params[1], params[2])
result <- optim(par=c(7.5,0.5), fn=neg_log_unn_posterior, method="L-BFGS-B", hessian=TRUE, lower=c(0.01,0.01), upper=c(100,0.99))
# only method for box costraints multi-dim functions; in pi=0 or 1 log not defined
result
#$par
#[1] 6.6155459 0.5533274
#
#$value
#[1] 1238.389
#
#$counts
#function gradient 
#      14       14 
#
#$convergence
#[1] 0
#
#$message
#[1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#
#$hessian
#           [,1]       [,2]
#[1,]   35.00149  -903.6251
#[2,] -903.62510 24194.5119
r_map <- result$par[1]
pi_map <- result$par[2]
covariance_matrix <- solve(result$hessian)
delta_r <- sqrt(covariance_matrix[1,1])
delta_pi <- sqrt(covariance_matrix[2,2])
cat(delta_r)
#0.8935039
cat(delta_pi)
#0.03398453

log_lik_map <- sum(dnbinom(data, size=r_map, prob=pi_map, log=TRUE))
log_prior_map <- dgamma(r_map, alpha_2, beta_2, log=TRUE) + 
                 dbeta(pi_map, alpha_1, beta_1, log=TRUE)

log_evidence <- log_lik_map + log_prior_map + log(2*base::pi) + 0.5*log(det(covariance_matrix))
cat(log_evidence)
#-1239.919
# very small, but expected for big dataset
# check if true??

log_evidence_pois <- -sum(lgamma(data+1)) + 
                      alpha*log(beta) - lgamma(alpha) + 
                      lgamma(alpha_prime) - alpha_prime*log(beta_prime)
cat(log_evidence_pois)
#-1278.505
bf=exp(log_evidence-log_evidence_pois)
cat(bf)
#5.7222e+16
# suprising, but ok bc large dataset: (see Claude chat)

hist(data, prob=TRUE, breaks=seq(min(data)-0.5, max(data)+0.5, by=1),
     main="NegBin model: prediction vs data", xlab="Counts", ylim=c(0,0.2))
points(x_vals, dnbinom(x_vals, size=r_map, prob=pi_map), 
       col="red", type='h', lwd=3)
