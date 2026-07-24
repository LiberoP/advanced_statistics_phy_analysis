# for correct compilation of stan files (make.exe)
Sys.setenv(PATH = paste("C:/rtools44/usr/bin", Sys.getenv("PATH"), sep = ";"))

library(ggplot2)
library(cmdstanr)
library(posterior)
library(loo) # for model comparison

# import dataset
full_data <- read.csv(
  "C:/Users/Libero/uni/1B/adv_stats/assignment_3/logistic_data.csv",
  header = TRUE
)
x_data <- full_data$x
y_data <- full_data$y
# explore it
typeof(x_data)
# [1] "double"
typeof(y_data)
# [1] "integer"
cat(min(x_data), max(x_data))
#-2.35841 2.443902
cat(min(y_data), max(y_data))
N <- length(x_data)
cat(N)
# 120
x_mean <- mean(x_data)
cat(x_mean)
# 0.08112576
x_std <- sd(x_data)
cat(x_std)
# 0.9821174
y_mean <- mean(y_data)
cat(y_mean)
# 0.45
y_std <- sd(y_data)
cat(y_std)
# 0.4995797

# standardize x variable
x_stdd <- (x_data - x_mean) / x_std
stan_data_sdd <- list(N = N, x = x_stdd, y = y_data)

# plot raw data
df_plot <- data.frame(x = x_data, y = y_data) # dataframe

n_bins <- 15
df_plot$bin <- cut(df_plot$x, breaks = n_bins) # bin x in n_bins
binned <- aggregate(y ~ bin, data = df_plot, FUN = mean)
bin_midpoints <- tapply(df_plot$x, df_plot$bin, mean) # x coordinate as mean of x's in each bin
binned$x_mid <- as.numeric(bin_midpoints)
colnames(binned)[2] <- "prop"
# x-axis breaks
major_breaks <- c(round(min(x_data), 2), -2, -1, 0, 1, 2, round(max(x_data), 2))
major_breaks <- sort(unique(major_breaks))
minor_breaks <- seq(floor(min(x_data)), ceiling(max(x_data)), by = 0.2)
# Fine y positions for grid lines
y_grid_pos <- seq(0, 1, by = 0.2)
ggplot() +
  # Manual horizontal grid lines
  geom_hline(yintercept = y_grid_pos, color = "grey80", linewidth = 0.3) +
  # Data points
  geom_point(
    data = df_plot,
    aes(x = x, y = y, color = "Observed data"),
    size = 1.5,
    alpha = 0.5
  ) +
  geom_point(
    data = binned,
    aes(x = x_mid, y = prop, color = "Proportion of y=1 points (binned x)"),
    size = 1.5,
    shape = 18
  ) +
  scale_y_continuous(
    name = expression(y[i]),
    breaks = c(0, 1),
    limits = c(-0.05, 1.05),
    sec.axis = sec_axis(
      ~.,
      name = "Proportion (binned)",
      breaks = y_grid_pos,
      labels = scales::label_number(accuracy = 0.1)
    )
  ) +
  scale_x_continuous(breaks = major_breaks, minor_breaks = minor_breaks) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Observed data" = "black",
      "Proportion of y=1 points (binned x)" = "red"
    )
  ) +
  labs(
    x = expression(x[i])
  ) +
  theme_bw(base_size = 12, base_family = "serif") +
  theme(
    # Keep x grid lines, remove y grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.2),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.title = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 10, color = "black", family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, family = "serif")
  )

# function to compile and fit both stan models
build_fit_stan_model <- function(stan_file_path, my_data) {
  my_mod <- cmdstan_model(stan_file_path)
  cat("MODEL COMPILED SUCCESSFULLY\n")
  my_fit <- my_mod$sample(
    data = my_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    seed = 222 # for reproducibility
  )
  return(list(mod = my_mod, fit = my_fit))
}

res <- build_fit_stan_model(
  "C:/Users/Libero/uni/1B/adv_stats/assignment_3/bernoulli_model.stan",
  stan_data_sdd
) # chain takes about 2 seconds
mod <- res$mod
fit <- res$fit

fit$summary(c("b0", "b1"))
# # A tibble: 2 × 10
#   variable   mean median    sd   mad     q5    q95  rhat ess_bulk ess_tail
#   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
# 1 b0       -0.260 -0.260 0.207 0.209 -0.594 0.0807  1.00    6203.    5162.
# 2 b1        1.01   1.01  0.244 0.246  0.625 1.42    1.00    6367.    5282.

fit$cmdstan_diagnose()
# Checking sampler transitions treedepth.
# Treedepth satisfactory for all transitions.

# Checking sampler transitions for divergences.
# No divergent transitions found.

# Checking E-BFMI - sampler transitions HMC potential energy.
# E-BFMI satisfactory.

# Rank-normalized split effective sample size satisfactory for all parameters.

# Rank-normalized split R-hat values satisfactory for all parameters.

# Processing complete, no problems detected.

# function to plot chain samples on par1-par2 plane (main model only)
plot_2_correlations <- function(my_draws_df, params_list) {
  cm <- cor(as.matrix(my_draws_df[, params_list])) # correlation matrix
  p <- ggplot(
    my_draws_df,
    aes(x = .data[[params_list[1]]], y = .data[[params_list[2]]])
  ) +
    geom_point(alpha = 0.15, size = 0.8, color = "black") +
    coord_fixed(ratio = 1) + # to keep correct proportions (visual clue)
    labs(
      x = expression(T[0]),
      y = expression(T[1])
    ) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.line = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10, color = "black")
    )
  return(list(cor = cm, plot = p))
}

draws_df <- as_draws_df(fit$draws()) # draws from posterior: dataframe
lapply(draws_df[, c("b0", "b1")], quantile, probs = c(0.025, 0.975)) # quantiles for 95% CI
# $b0
#       2.5%      97.5%
# -0.6535741  0.1452110

# $b1
#     2.5%    97.5%
# 0.543916 1.504841

cor_plot <- plot_2_correlations(draws_df, c("b0", "b1"))
print(cor_plot$plot)
cor_plot$cor # correlation matrix: abs value in 0,1 (corr = covariance / product of variances)
#             b0          b1
# b0  1.00000000 -0.09151987
# b1 -0.09151987  1.00000000

# function to  plot posterior: mean, 95% CI, comparison to data
plot_posterior <- function(
  my_draws_df,
  my_x_stdd,
  my_y_data,
  my_x_grid = NULL,
  n_grid = 100,
  n_bins = 15
) {
  if (is.null(my_x_grid)) {
    # custom grid optional
    x_grid <- seq(min(my_x_stdd), max(my_x_stdd), length.out = n_grid)
  } else {
    x_grid <- my_x_grid
  }

  # matrix: rows correspond to (beta0, beta1) samples, columns to the x grid
  S <- nrow(my_draws_df)
  pi_mat <- matrix(NA, nrow = S, ncol = length(x_grid))

  # uncertainty on predicted outcome - not used
  # y_rep <- rbinom(S, size = 1, prob = pi_mat[, i])

  # to consider only b0 if model is the "intercept-only"
  if (!"b1" %in% colnames(my_draws_df)) {
    b1_vals <- rep(0, S)
  } else {
    b1_vals <- my_draws_df$b1
  }

  # porbability of success value for each matrix element
  # plogis is the inverse logit: plogis(a) = 1/(1+exp(-a))
  for (i in seq_along(x_grid)) {
    pi_mat[, i] <- plogis(my_draws_df$b0 + b1_vals * x_grid[i])
  }

  # ... get its mean and quantiles; apply to columns ("2")
  pi_mean <- apply(pi_mat, 2, mean)
  pi_lo <- apply(pi_mat, 2, quantile, 0.025)
  pi_hi <- apply(pi_mat, 2, quantile, 0.975)

  # Bin observed data as in the beginning
  data_binned <- data.frame(x = my_x_stdd, y = my_y_data)
  data_binned$bin <- cut(data_binned$x, breaks = n_bins)
  binned_props <- aggregate(y ~ bin, data = data_binned, FUN = mean)
  binned_mids <- aggregate(x ~ bin, data = data_binned, FUN = mean)
  binned_df <- data.frame(x_mid = binned_mids$x, prop = binned_props$y)

  plot_df <- data.frame(x_stdd = x_grid, mean = pi_mean, lo = pi_lo, hi = pi_hi)

  # Define x axis limits based on observed data
  x_min_obs <- min(my_x_stdd)
  x_max_obs <- max(my_x_stdd)

  # Fine grid positions (every 0.2) for x and y (but restrict grid to observed range)
  x_grid_pos <- seq(floor(x_min_obs), ceiling(x_max_obs), by = 0.2)
  x_grid_pos <- x_grid_pos[x_grid_pos >= x_min_obs & x_grid_pos <= x_max_obs]
  y_grid_pos <- seq(0, 1, by = 0.2)

  # Integer breaks for x-axis labels (only those inside the observed range)
  integer_breaks <- seq(floor(x_min_obs), ceiling(x_max_obs), by = 1)
  integer_breaks <- integer_breaks[
    integer_breaks >= x_min_obs & integer_breaks <= x_max_obs
  ]

  p <- ggplot() +
    # Manual grid lines
    geom_hline(yintercept = y_grid_pos, color = "grey85", linewidth = 0.3) +
    geom_vline(xintercept = x_grid_pos, color = "grey85", linewidth = 0.3) +
    # Posterior credible ribbon
    geom_ribbon(
      data = plot_df,
      aes(x = x_stdd, ymin = lo, ymax = hi, fill = "95% Credible interval"),
      alpha = 0.3
    ) +
    # Posterior mean line
    geom_line(
      data = plot_df,
      aes(x = x_stdd, y = mean, color = "Posterior mean"),
      linewidth = 1
    ) +
    # Binned observed proportions
    geom_point(
      data = binned_df,
      aes(x = x_mid, y = prop, shape = "Observed proportion (binned)"),
      size = 2,
      color = "black"
    ) +
    # Scales
    scale_y_continuous(
      breaks = y_grid_pos,
      limits = c(-0.02, 1.02),
      name = expression(P(y == 1 ~ "|" ~ D, x))
    ) +
    scale_x_continuous(
      limits = c(x_min_obs, x_max_obs),
      breaks = integer_breaks,
      minor_breaks = x_grid_pos,
      name = "x (standardized)",
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("95% Credible interval" = "red")) +
    scale_color_manual(values = c("Posterior mean" = "red")) +
    scale_shape_manual(values = c("Observed proportion (binned)" = 16)) +
    guides(
      shape = guide_legend(order = 1),
      color = guide_legend(order = 2),
      fill = guide_legend(order = 3, override.aes = list(alpha = 0.3))
    ) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.line = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10, color = "black"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 10, family = "serif"),
      legend.margin = margin(t = 0, b = 0, unit = "pt"),
      legend.spacing.x = unit(5, "pt")
    )

  return(p)
}

plot_posterior(draws_df, x_stdd, y_data)

# function to plot trace plot(s), sampels histogram(s), ACF(s)
plot_various_graphs <- function(my_fit, params_list) {
  draws_array <- as.array(my_fit$draws())
  n_iters <- dim(draws_array)[1]
  n_chains <- dim(draws_array)[2]

  df_list <- list()
  idx <- 1 # index for chains
  for (p in params_list) {
    # b0, and maybe b1
    for (chain in 1:n_chains) {
      vals <- draws_array[, chain, p]
      df_list[[idx]] <- data.frame(
        Parameter = p,
        Chain = factor(chain),
        Iteration = seq_along(vals),
        Value = as.numeric(vals),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  df_long <- do.call(rbind, df_list) # all chains

  # rename parameters as "beta"s
  df_long$Parameter <- factor(
    df_long$Parameter,
    levels = params_list,
    labels = paste0("beta[", sub("b", "", params_list), "]")
  )

  # trace plots part
  trace_plot <- ggplot(df_long, aes(x = Iteration, y = Value, color = Chain)) +
    geom_line(alpha = 0.7) +
    facet_wrap(
      ~Parameter,
      scales = "free_y",
      ncol = 1,
      labeller = label_parsed
    ) +
    labs(x = "Iteration", y = "Parameter value") +
    scale_color_manual(
      values = c(
        "1" = "#E69F00",
        "2" = "#56B4E9",
        "3" = "#009E73",
        "4" = "#CC79A7"
      )
    ) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.line = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12, family = "serif"),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )

  # occurences histogram part
  hist_plot <- ggplot(df_long, aes(x = Value, fill = Chain, color = Chain)) +
    # relative frequency: each bar = count in bin / total count across all chains
    geom_histogram(
      aes(y = after_stat(count / sum(count))),
      alpha = 0.4,
      bins = 30,
      position = "identity"
    ) +
    # aesthetics only
    facet_wrap(~Parameter, scales = "free", ncol = 1, labeller = label_parsed) +
    labs(x = "Parameter value", y = "Relative frequency") +
    scale_color_manual(
      values = c(
        "1" = "#E69F00",
        "2" = "#56B4E9",
        "3" = "#009E73",
        "4" = "#CC79A7"
      )
    ) + # colorblind-friendly palette
    scale_fill_manual(
      values = c(
        "1" = "#E69F00",
        "2" = "#56B4E9",
        "3" = "#009E73",
        "4" = "#CC79A7"
      )
    ) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.line = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12, family = "serif"),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )

  # ACFs part
  acf_list <- list()
  for (param in params_list) {
    param_draws <- as.vector(my_fit$draws(param))
    acf_obj <- acf(param_draws, plot = FALSE)
    acf_list[[param]] <- data.frame(
      Lag = as.numeric(acf_obj$lag),
      ACF = as.numeric(acf_obj$acf),
      Parameter = param
    )
  }
  acf_df <- do.call(rbind, acf_list)
  acf_df$Parameter <- factor(
    acf_df$Parameter,
    levels = params_list,
    labels = paste0("beta[", sub("b", "", params_list), "]")
  )

  acf_plot <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    geom_segment(aes(xend = Lag, yend = 0), color = "black", linewidth = 0.7) +
    facet_wrap(~Parameter, ncol = 1, labeller = label_parsed) +
    labs(x = "Lag", y = "ACF") +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      axis.line = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12, family = "serif"),
      legend.position = "none",
      legend.text = element_text(size = 9, family = "serif")
    )
  return(list(acf = acf_plot, trace = trace_plot, density = hist_plot))
}
graphs <- plot_various_graphs(fit, c("b0", "b1"))
print(graphs$trace)
print(graphs$density)
print(graphs$acf)

# POINT 4
# uncertainty on pi_new is the red interval before, just for a fixed x
x_new_original <- 0 # fixed value
x_new_std <- (x_new_original - x_mean) / x_std # standardize it
# posterior draws of pi_new
pi_new_draws <- plogis(draws_df$b0 + draws_df$b1 * x_new_std) # pi(x=0  )
# ie for all chain samples, and fixed x_new, plot posterior's pi
# posterior mean and 95% credible interval
pi_mean <- mean(pi_new_draws)
pi_ci <- quantile(pi_new_draws, c(0.025, 0.975))
cat(pi_mean, pi_ci)
# 0.4156503 0.3174578 0.5160177
central_point <- (pi_ci[1] + pi_ci[2]) / 2
cat(central_point)
# 0.4195013
# approximately equal to mean; so, can use central interval:
delta_half <- (-pi_ci[1] + pi_ci[2]) / 2
cat(delta_half)
# 0.09792069

# note: posterior predictive probability P(y_new=1 | x_new, D) = pi_mean
# histogram of pi_new draws
par(family = "serif")
hist(
  pi_new_draws,
  breaks = 30,
  probability = TRUE,
  xlab = expression(pi[new]),
  col = "grey50",
  main = "",
  border = "white",
  yaxt = "n",
  xaxt = "n"
)
axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
axis(2, las = 1)
lines(density(pi_new_draws), col = "red", lwd = 2)
legend(
  "topright",
  legend = c("Histogram", "KDE estimate"),
  fill = c("grey50", NA),
  border = c("black", NA),
  col = c(NA, "red"),
  lty = c(NA, 1),
  lwd = c(NA, 2),
  bty = "n",
  cex = 0.9
)

# INTERCEPT ONLY MODEL
res <- build_fit_stan_model(
  "C:/Users/Libero/uni/1B/adv_stats/assignment_3/intercept_only_model.stan",
  stan_data_sdd
)
i_mod <- res$mod
i_fit <- res$fit
i_fit$summary(c("b0"))
# # A tibble: 1 × 10
#   variable   mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
#   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
# 1 b0       -0.202 -0.200 0.183 0.179 -0.505 0.100  1.00    3011.    3961.
i_fit$cmdstan_diagnose()
# (...)
# (everything ok)
i_draws_df <- as_draws_df(i_fit$draws()) # draws from posterior
plot_posterior(i_draws_df, x_stdd, y_data)
i_graphs <- plot_various_graphs(i_fit, c("b0"))
print(i_graphs$trace)
print(i_graphs$density)
print(i_graphs$acf)

# MODEL COMPARISON
# extract log-likelihood matrix (draws x N)
log_lik <- fit$draws("log_lik", format = "matrix")
# compute LOO object for both models
loo <- loo(log_lik)
i_log_lik <- i_fit$draws("log_lik", format = "matrix")
i_loo <- loo(i_log_lik)
comp <- loo_compare(loo, i_loo) # compare the two models based on ELPD statistics
print(loo)
print(i_loo)
# (...)
# diagnostics ok: in particular k < 0.7
print(comp) # difference in ELPD and standard error
#       elpd_diff se_diff
# model1  0.0       0.0
# model2 -9.9       4.2

# note: if elpd_diff > 2*se_diff, difference is considered significant; here 9.9 > 2* 4.2
