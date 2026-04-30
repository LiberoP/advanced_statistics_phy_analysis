library(fields)

# import dataset
data <- scan("C:/Users/Libero/uni/1B/adv_stats/assignment_1/dataset.txt", what=integer())

# explore it
typeof(data)
#[1] "integer"
length(data)
#[1] 500
x_mean <- mean(data)
cat(x_mean)
#5.34
x_std <- sd(data)
cat(x_std)
#3.019708

s <- sum(data)
cat(s)
#2670
n <- length(data)
cat(n)
#500

# data histogram
data_histo <- hist(data, breaks=20, main="Data", xlab= expression(x[i]), ylab="Occurences") # sqrt(500) = 22,...
ymax <- max(data_histo$counts)
abline(v = x_mean, col = "blue", lty = 4, lwd=2)
abline(v = x_mean + c(-1, 1) * x_std, col = "light blue", lty = 2, lwd = 2)


###########################################################
#                   POISSON MODEL
###########################################################

# pick parameters (ininfluent)
alpha=2
beta=1

# change of variables
alpha_prime <- s + alpha 
beta_prime <- beta + n
cat(alpha_prime, beta_prime)
#2672 501

# estimates from posterior
lambda_map=(alpha_prime-1)/beta_prime
posterior_mean=alpha_prime/beta_prime
posterior_std=sqrt(alpha_prime / beta_prime **2)
skewness=2.0/sqrt(alpha_prime)
cat(lambda_map, posterior_mean, posterior_std, skewness)
#5.331337 5.333333 0.1031764 0.03869116

# log posterior plot
curve(dgamma(x, shape = alpha_prime, rate = beta_prime), 
      from = 2, to = 11, log = "y", axes = FALSE,
      main = "Log-posterior probability distribution \n (Poisson model)", 
      xlab = expression(lambda), ylab = expression(log(p(lambda*"|"*D))),, col = "dark blue", lwd = 2)
axis(1, at = seq(2, 11, by = 0.1), labels = FALSE, tcl = -0.2)
axis(1, at = seq(2, 11, by = 0.5), labels = TRUE)
usr <- par("usr")
y_lo <- 10^usr[3]
y_hi <- 10^usr[4]
axis(2, at = c(y_lo, y_hi), labels = format(log(c(y_lo, y_hi)), digits = 2, scientific = FALSE))
abline(v = posterior_mean, col = "blue", lty = 4, lwd = 2)
abline(v = posterior_mean + c(-1, 1) * posterior_std, col = "light blue", lty = 2, lwd = 2)
box()

# plug-in likelihood vs data plot (Poisson model)
x_vals <- 0:max(data)  
hist(data, prob = TRUE, breaks = seq(min(data)-0.5, max(data)+0.5, by = 1),
     main = "Model vs data \n (Poisson model)", xlab = expression(x[i]), ylab="Frequency", ylim=c(0,0.2))
points(x_vals, dpois(x_vals, lambda=lambda_map), col = "red", type = 'h', lwd=3)
legend("topright", legend = c("Data", "Plug-in likelihood"), 
       col = c("gray", "red"), pch = c(15, NA), lty = c(NA, 1), 
       lwd = c(NA, 3), pt.cex = 0.8, cex = 0.8, bty = "n")
box()

p_s <- beta_prime/(1+beta_prime)

# posterior prediction plot
plot(0, type = "n", xlim = range(x_vals), ylim = c(0, 0.2), 
     xlab = expression(x[new]), ylab = expression(p(x[new]*"|"*D)), main = "Posterior prediction for new event")
points(x_vals, dnbinom(x_vals, size=alpha_prime, prob=p_s), col = "dark blue", type = 'h', lwd=3)
abline(v = 2*x_mean, col = "red", lty = 4, lwd=2)

# p(x>2*x_mean) calculation
cdf_2_x_mean = pnbinom(2*x_mean, size=alpha_prime, prob=p_s)
prob_excess= 1- cdf_2_x_mean
cat(prob_excess)
#0.02092887

###########################################################
#                   NEGBIN MODEL
###########################################################

# pick parameters
alpha_1=2
beta_1=2
alpha_2=2
beta_2=1

# log of the unnormalised posterior - function definition
log_unnorm_posterior <- function(r, pi) {
  log_bin <- sum(lchoose(data + r - 1, data)) # binomial coefficient part
  log_rest <- (alpha_1 - 1 + n * r) * log(pi) + 
              (beta_1 - 1 + s) * log(1 - pi) + 
              (alpha_2 - 1) * log(r) - beta_2 * r
  return(log_bin + log_rest)
}

# 2-var joint posterior plot
# grid of r and pi values
r_vals <- seq(0.01, 20, length.out = 100)   
pi_vals <- seq(0.01, 0.99, length.out = 100)
grid <- expand.grid(r = r_vals, pi = pi_vals)
grid$log_z <- mapply(log_unnorm_posterior, grid$r, grid$pi)
# reshape to matrix for image/contour
z_mat <- matrix(grid$log_z, nrow = length(r_vals), ncol = length(pi_vals))
image.plot(r_vals, pi_vals, z_mat, xlab = "r", ylab = expression(pi), 
           main = "Unnormalised log posterior value",
           cex.main = 1., cex.lab = 1., cex.axis = 1.)
contour(r_vals, pi_vals, z_mat, add = TRUE, col = "white", # white contours
        levels = seq(-5000, 0, by = 50), drawlabels = FALSE)


# minimisation of negative log-likelihood
neg_log_unn_posterior <- function(params) -log_unnorm_posterior(params[1], params[2])
result <- optim(par=c(7.5,0.5), fn=neg_log_unn_posterior, method="L-BFGS-B", hessian=TRUE,
                lower=c(0.01,0.01), upper=c(100,0.99))
# because only method for box costraints multi-dim functions; in pi=0 or 1 log not defined
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

# inspect result of minimisation
r_map <- result$par[1]
pi_map <- result$par[2]
covariance_matrix <- solve(result$hessian)
delta_r <- sqrt(covariance_matrix[1,1])
delta_pi <- sqrt(covariance_matrix[2,2])
cat(delta_r)
#0.8935039
cat(delta_pi)
#0.03398453

# log evidence of NegBin model - calculation
log_lik_map <- sum(dnbinom(data, size=r_map, prob=pi_map, log=TRUE))
log_prior_map <- dgamma(r_map, alpha_2, beta_2, log=TRUE) + 
                 dbeta(pi_map, alpha_1, beta_1, log=TRUE)

log_evidence <- log_lik_map + log_prior_map + log(2*base::pi) + 0.5*log(det(covariance_matrix))
cat(log_evidence)
#-1239.919

# log evidence of Poisson model - calculation
log_evidence_pois <- -sum(lgamma(data+1)) + 
                      alpha*log(beta) - lgamma(alpha) + 
                      lgamma(alpha_prime) - alpha_prime*log(beta_prime)
cat(log_evidence_pois)
#-1278.505
bf=exp(log_evidence-log_evidence_pois)
cat(bf)
#5.7222e+16

# plug-in likelihood vs data plot (NegBin model)
hist(data, prob = TRUE, breaks = seq(min(data)-0.5, max(data)+0.5, by = 1),
     main = "Model vs data \n (NegBin model)", xlab = expression(x[i]), ylab="Frequency", ylim=c(0,0.2))
points(x_vals, dnbinom(x_vals, size=r_map, prob=pi_map), 
       col="red", type='h', lwd=3)
legend("topright", legend = c("Data", "Plug-in likelihood"), 
col = c("gray", "red"), pch = c(15, NA), lty = c(NA, 1), 
lwd = c(NA, 3), pt.cex = 0.8, cex = 0.8, bty = "n")
box()