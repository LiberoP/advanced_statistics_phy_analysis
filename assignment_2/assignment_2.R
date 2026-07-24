library(ggplot2)
library(zoo) # for rollmean

# import dataset
full_data <- read.csv(
  "C:/Users/Libero/uni/1B/adv_stats/assignment_2/assignment2_changepoint_counts.csv",
  header = TRUE
)
data <- full_data$x
# explore it
typeof(data)
#[1] "integer"
s_N <- sum(data)
cat(s_N)
#486
N <- length(data)
cat(N)
#120
x_mean <- mean(data)
cat(x_mean)
#4.05
x_std <- sd(data)
cat(x_std)
#1.965458

# barplot of data + statistics
df_bar <- data.frame(i = seq_along(data), x = data)
df_bar$rollmean <- rollmean(data, k = 10, fill = NA, align = "right")

ggplot(df_bar, aes(x = i, y = x)) +
  geom_col(aes(fill = "Observed counts"), width = 0.8) +
  geom_hline(
    aes(yintercept = x_mean, color = "Sample mean", linetype = "Sample mean"),
    linewidth = 0.8
  ) +
  geom_hline(
    aes(
      yintercept = x_mean + x_std,
      color = "\u00b11 SD",
      linetype = "\u00b11 SD"
    ),
    linewidth = 0.8
  ) +
  geom_hline(
    aes(
      yintercept = x_mean - x_std,
      color = "\u00b11 SD",
      linetype = "\u00b11 SD"
    ),
    linewidth = 0.8
  ) +
  geom_line(
    aes(
      y = rollmean,
      color = "Rolling mean (10)",
      linetype = "Rolling mean (10)"
    ),
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  scale_fill_manual(name = NULL, values = c("Observed counts" = "grey40")) +
  scale_color_manual(
    name = NULL,
    breaks = c(
      "Observed counts",
      "Sample mean",
      "\u00b11 SD",
      "Rolling mean (10)"
    ),
    values = c(
      "Observed counts" = "grey40",
      "Sample mean" = "black",
      "\u00b11 SD" = "grey80",
      "Rolling mean (10)" = "red"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    breaks = c(
      "Observed counts",
      "Sample mean",
      "\u00b11 SD",
      "Rolling mean (10)"
    ),
    values = c(
      "Observed counts" = "solid",
      "Sample mean" = "dashed",
      "\u00b11 SD" = "dotted",
      "Rolling mean (10)" = "solid"
    )
  ) +
  labs(
    title = "Observed event counts per time interval",
    x = "Time interval i",
    y = "Number of events"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )


###########################################################
#                   GIBBS SAMPLER
###########################################################

# pick prior parameters
alpha <- 1
beta <- 0.1

gibbs_sampler <- function(n_steps, counts, start, alpha, beta, m_min, m_max) {
  lambda_1 <- numeric(n_steps)
  lambda_2 <- numeric(n_steps)
  m <- numeric(n_steps)
  lambda_1[1] <- start[1]
  lambda_2[1] <- start[2]
  m[1] <- start[3]

  N <- length(counts)
  s_N <- sum(counts) # constant

  # Allowed m values
  m_vals <- m_min:m_max
  log_probs <- numeric(length(m_vals)) # to avoid recomputing full pmf

  for (t in 2:n_steps) {
    m_curr <- m[t - 1]
    s_m <- sum(counts[1:m_curr]) # sum of first m_curr observations

    # Gibbs step 1: lambda_1
    alpha1 <- s_m + alpha
    beta1 <- m_curr + beta
    lambda_1[t] <- rgamma(1, shape = alpha1, rate = beta1)

    # Gibbs step 2: lambda_2
    alpha2 <- s_N - s_m + alpha
    beta2 <- N - m_curr + beta
    lambda_2[t] <- rgamma(1, shape = alpha2, rate = beta2)

    # Gibbs step 3: m
    # Precompute S_m for all candidate m
    S_cand <- cumsum(counts)[m_vals] # cumulative sum up to each candidate
    log_probs <- S_cand *
      (log(lambda_1[t]) - log(lambda_2[t])) +
      m_vals * (lambda_2[t] - lambda_1[t])
    probs <- exp(log_probs - max(log_probs)) # stabilise: by subtracting a constant get one exponent = 0, to avoid division by 0; we normalize after anyways
    probs <- probs / sum(probs) # normalization
    m[t] <- sample(m_vals, size = 1, prob = probs)
  }

  cbind(lambda1 = lambda_1, lambda2 = lambda_2, m = m)
}

N_tot <- 1000000

system.time({
  chains <- list(
    gibbs_sampler(N_tot, data, c(1, 1, 20), alpha, beta, 5, N - 5), # arbitrary starting point
    gibbs_sampler(N_tot, data, c(10, 0.5, 50), alpha, beta, 5, N - 5),
    gibbs_sampler(N_tot, data, c(0.5, 5, 100), alpha, beta, 5, N - 5)
  )
})
# user  system elapsed
#    user  system elapsed
# 130.22    0.89  141.78

# Combine the three chains into a single data frame for plotting - keep only every 1000 iterations for faster plotting
# 1. For each chain i, turn its matrix into a data frame with iteration number, parameters, and a chain label
chain_df <- do.call(
  rbind,
  lapply(seq_along(chains), function(i) {
    full_n <- nrow(chains[[i]])
    keep_idx <- seq(1, full_n, by = 1000) # indices to keep
    mat <- chains[[i]][keep_idx, , drop = FALSE] # extract those rows
    data.frame(
      iteration = keep_idx,
      lambda1 = mat[, "lambda1"],
      lambda2 = mat[, "lambda2"],
      m = mat[, "m"],
      chain = factor(i)
    )
  })
)

burn_in <- 10000 # for now, only used to show vertical line

# Reshape to long format for faceting
chain_long <- with(
  chain_df,
  data.frame(
    iteration = rep(iteration, 3),
    value = c(lambda1, lambda2, m),
    parameter = rep(c("lambda[1]", "lambda[2]", "m"), each = nrow(chain_df)),
    chain = factor(rep(chain, 3), labels = paste("Chain", 1:3))
  )
)

ggplot(chain_long, aes(x = iteration, y = value)) +
  geom_line(color = "black", linewidth = 0.3) +
  geom_vline(
    aes(xintercept = burn_in, color = "j=10000", linetype = "j=10000"),
    linewidth = 0.5,
    show.legend = TRUE
  ) +
  facet_grid(
    rows = vars(parameter),
    cols = vars(chain),
    scales = "free_y",
    switch = "y",
    labeller = labeller(parameter = label_parsed, chain = label_value)
  ) +
  scale_color_manual(name = NULL, values = c("j=10000" = "red")) +
  scale_linetype_manual(name = NULL, values = c("j=10000" = "dashed")) +
  labs(x = "Iteration j", y = "Parameter value") +
  ggtitle("Trace Plots of MCMC Chains") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    axis.line = element_line(color = "grey60"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# =================================================
# =================================================
# PARAMETER ESTIMATION
# =================================================
# =================================================

lag_max <- 1000
# Concatenated chain (used later for summaries and acf plots)
chain <- do.call(rbind, lapply(chains, function(ch) ch[-(1:burn_in), ]))
N_c <- nrow(chain) # total post‑burn‑in draws (optional)
cat(N_c)
# 2970000

# ------------------------------------------------------
# 1.  Parameter lambda1
# ------------------------------------------------------
ESS_lambda1 <- 0
for (i in seq_along(chains)) {
  #
  ch_burned <- chains[[i]][-(1:burn_in), "lambda1"]
  acf_obj <- acf(ch_burned, plot = FALSE, lag.max = lag_max)
  rho <- acf_obj$acf[-1]
  pairs <- rho[seq(1, length(rho) - 1, by = 2)] +
    rho[seq(2, length(rho), by = 2)]
  pos <- which(pairs < 0)[1]
  if (is.na(pos)) {
    pos <- length(pairs) + 1
  }
  tau_int <- 1 + 2 * sum(pairs[1:(pos - 1)])
  ESS_lambda1 <- ESS_lambda1 + length(ch_burned) / (2 * tau_int)
}
cat("lambda1: ESS =", round(ESS_lambda1, 1), "\n")
# 1166494

# ------------------------------------------------------
# 2.  Parameter lambda2
# ------------------------------------------------------
ESS_lambda2 <- 0
for (i in seq_along(chains)) {
  ch_burned <- chains[[i]][-(1:burn_in), "lambda2"]
  acf_obj <- acf(ch_burned, plot = FALSE, lag.max = lag_max)
  rho <- acf_obj$acf[-1]
  pairs <- rho[seq(1, length(rho) - 1, by = 2)] +
    rho[seq(2, length(rho), by = 2)]
  pos <- which(pairs < 0)[1]
  if (is.na(pos)) {
    pos <- length(pairs) + 1
  }
  tau_int <- 1 + 2 * sum(pairs[1:(pos - 1)])
  ESS_lambda2 <- ESS_lambda2 + length(ch_burned) / (2 * tau_int)
}
cat("lambda2: ESS =", round(ESS_lambda2, 1), "\n")
# lambda2: ESS = 1256340
# ------------------------------------------------------
# 3.  Parameter m
# ------------------------------------------------------
ESS_m <- 0
for (i in seq_along(chains)) {
  ch_burned <- chains[[i]][-(1:burn_in), "m"]
  acf_obj <- acf(ch_burned, plot = FALSE, lag.max = lag_max)
  rho <- acf_obj$acf[-1]
  pairs <- rho[seq(1, length(rho) - 1, by = 2)] +
    rho[seq(2, length(rho), by = 2)]
  pos <- which(pairs < 0)[1]
  if (is.na(pos)) {
    pos <- length(pairs) + 1
  }
  tau_int <- 1 + 2 * sum(pairs[1:(pos - 1)])
  ESS_m <- ESS_m + length(ch_burned) / (2 * tau_int)
}
cat("m: ESS =", round(ESS_m, 1), "\n")
# m: ESS = 1077852

# ACF plot with faceting (same style as trace plots)
lag_max_acf <- 100
acf_list <- list()
for (i in seq_along(chains)) {
  ch <- chains[[i]][-(1:burn_in), ]
  for (par in c("lambda1", "lambda2", "m")) {
    acf_obj <- acf(ch[, par], lag.max = lag_max_acf, plot = FALSE)
    df_acf <- data.frame(
      lag = acf_obj$lag[, 1, 1],
      acf = acf_obj$acf[, 1, 1],
      chain = paste("Chain", i),
      parameter = switch(
        par,
        lambda1 = "lambda[1]",
        lambda2 = "lambda[2]",
        m = "m"
      )
    )
    acf_list[[length(acf_list) + 1]] <- df_acf
  }
}
acf_long <- do.call(rbind, acf_list)
acf_long$parameter <- factor(
  acf_long$parameter,
  levels = c("lambda[1]", "lambda[2]", "m")
)

ggplot(acf_long, aes(x = lag, y = acf)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.3
  ) +
  geom_line(colour = "black", linewidth = 0.3) +
  facet_grid(
    rows = vars(parameter),
    cols = vars(chain),
    scales = "free_y",
    switch = "y",
    labeller = labeller(parameter = label_parsed)
  ) +
  labs(x = "Lag", y = "Autocorrelation") +
  ggtitle("Autocorrelation Functions (post-burn-in)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(colour = "grey80", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(face = "bold"),
    axis.line = element_line(colour = "grey60"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# ======================================================
# Posterior summaries for lambda1, lambda2, m
# ======================================================

# Helper function to compute MAP for continuous parameters

map_kde <- function(x) {
  dens <- density(x[seq(1, length(x))]) # compute a kernel density estimate (default: Gaussian basis,
  # one curve per point, bandwith given by 1.06 * estimate(sigma) * sample_size)...
  dens$x[which.max(dens$y)]
}
# --- lambda1 ---
mean_l1 <- mean(chain[, "lambda1"])
med_l1 <- median(chain[, "lambda1"])
sd_l1 <- sd(chain[, "lambda1"])
map_l1 <- map_kde(chain[, "lambda1"])
ci_l1 <- quantile(chain[, "lambda1"], probs = c(0.025, 0.975))
# --- lambda2 ---
mean_l2 <- mean(chain[, "lambda2"])
med_l2 <- median(chain[, "lambda2"])
sd_l2 <- sd(chain[, "lambda2"])
map_l2 <- map_kde(chain[, "lambda2"])
ci_l2 <- quantile(chain[, "lambda2"], probs = c(0.025, 0.975))
# --- m (discrete: MAP is the most frequent value) ---
mean_m <- mean(chain[, "m"])
med_m <- median(chain[, "m"])
sd_m <- sd(chain[, "m"])
# MAP for discrete m: value with highest frequency
m_counts <- table(chain[, "m"])
map_m <- as.numeric(names(m_counts)[which.max(m_counts)])
ci_m <- quantile(chain[, "m"], probs = c(0.025, 0.975))
# Print results
cat(sprintf(
  "lambda1: mean=%.3f, median=%.3f, sd=%.3f, MAP=%.3f, 95%% CI=[%.3f, %.3f]\n",
  mean_l1,
  med_l1,
  sd_l1,
  map_l1,
  ci_l1[1],
  ci_l1[2]
))
# lambda1: mean=3.207, median=3.202, sd=0.234, MAP=3.190, 95% CI=[2.763, 3.681]
cat(sprintf(
  "lambda2: mean=%.3f, median=%.3f, sd=%.3f, MAP=%.3f, 95%% CI=[%.3f, %.3f]\n",
  mean_l2,
  med_l2,
  sd_l2,
  map_l2,
  ci_l2[1],
  ci_l2[2]
))
# lambda2: mean=5.111, median=5.101, sd=0.323, MAP=3.190, 95% CI=[4.504, 5.773]
cat(sprintf(
  "m:      mean=%.1f, median=%.0f, sd=%.1f, MAP=%.0f, 95%% CI=[%.0f, %.0f]\n",
  mean_m,
  med_m,
  sd_m,
  map_m,
  ci_m[1],
  ci_m[2]
))
# m:      mean=66.2, median=65, sd=5.1, MAP=65, 95% CI=[59, 77]

# ======================================================
# Probability that lambda2 > lambda1
# ======================================================
prob_l2_gt_l1 <- mean(chain[, "lambda2"] > chain[, "lambda1"])
cat(cat("P(lambda2 > lambda1) = ", prob_l2_gt_l1))
# P(lambda2 > lambda1) =  0.999996

# ======================================================
# Probability that x_new > 2 x_bar
# ======================================================
x_bar <- x_mean # already computed
prob_new_gt_2xbar <- mean(ppois(
  2 * x_bar,
  lambda = chain[, "lambda2"],
  lower.tail = FALSE
)) #
cat(sprintf("P(x_new > 2 x_bar | D) = %.4f\n", prob_new_gt_2xbar))
#P(x_new > 2 x_bar | D) = 0.0776
# plot Posterior predictive distribution
x_new_sim <- rpois(length(chain[, "lambda2"]), lambda = chain[, "lambda2"])
pred_df <- data.frame(x_new = x_new_sim)
ggplot(pred_df, aes(x = x_new)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1,
    boundary = 0.5,
    fill = "grey40",
    color = "black",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = x_bar,
    color = "red",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 2 * x_bar,
    color = "blue",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = x_bar + 0.5,
    y = Inf,
    label = "bar(x)",
    color = "red",
    hjust = 0,
    vjust = 1.5,
    parse = TRUE
  ) +
  annotate(
    "text",
    x = 2 * x_bar + 0.5,
    y = Inf,
    label = "2*bar(x)",
    color = "blue",
    hjust = 0,
    vjust = 1.5,
    parse = TRUE
  ) +
  labs(
    title = "Posterior predictive distribution",
    x = expression(x[new]),
    y = "Density"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.line = element_line(color = "grey60")
  )

# ======================================================
# BAYES FACTOR
# ======================================================
m_min <- 5
m_max <- N - 5
# Compute Bayes factor BF10 = p(D | M1) / p(D | M0) using conjugate Gamma-Poisson marginals
# Already available: data, N, s_N, alpha, beta, m_min, m_max
# total sum of counts
m_vals <- m_min:m_max # allowed change points (5 to N-5)
S_m <- cumsum(data)[m_vals] # cumulative sums up to each candidate m
# Log of the factor coming from M0's marginal (without the 1/prod(x!) factor, which cancels)
log_term0 <- alpha *
  log(beta) -
  lgamma(alpha) +
  (s_N + alpha) * log(N + beta) -
  lgamma(s_N + alpha)
# Log of the summand inside the sum over m (each term = product of two Gamma-Poisson marginals)
log_terms <- lgamma(S_m + alpha) -
  (S_m + alpha) * log(m_vals + beta) +
  lgamma(s_N - S_m + alpha) -
  (s_N - S_m + alpha) * log(N - m_vals + beta)
# Stable log-sum-exp across m
max_log <- max(log_terms)
log_sum <- log(sum(exp(log_terms - max_log))) + max_log - log(length(m_vals))
log_BF10 <- log_term0 + log_sum
cat(sprintf("log(BF10) = %.2f\n", log_BF10))
# log(BF10) = 8.29
cat(sprintf("BF10 = %.2e\n", exp(log_BF10)))
# BF10 = 4.00e+03
