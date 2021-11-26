# replicate smoking study
library(cmdstanr)
library(data.table)

load("synth/smoking.rda")

# stan_file <- "synth_penalized.stan"
stan_file <- "synth_horseshoe_b_tau_x.stan"
mod_synth <- cmdstan_model(stan_file)

smoking <- data.table(smoking)
smoking_y <- dcast(smoking, year ~ state, value.var = "cigsale")
target <- "California"
target_index <- which(names(smoking_y) == target)
other_index <- which(names(smoking_y) %in% names(smoking_y)[c(-1, -4)])

which(smoking_y$year == 1988)

X_pred <- cbind(predictors_target[-1], predictors_control[-1])

X_pred[3, ] <- log(X_pred[3, ] / (1 - X_pred[3, ]))

stan_data <- list(
  T = nrow(smoking_y),
  J = ncol(smoking_y) - 1,
  L = 8,
  P = nrow(X_pred),
  X = X_pred,
  Y = transpose(smoking_y[ , c(..target_index, ..other_index)]),
  trt_times = nrow(smoking_y) - which(smoking_y$year == 1988)
)

fit_synth <- mod_synth$sample(data = stan_data,
                              seed = 123123,
                              iter_warmup = 500,
                              iter_sampling = 500,
                              init = 0.1,
                              chains = 4,
                              parallel_chains = 4,
                              max_treedepth = 13,
                              adapt_delta = 0.8
)
