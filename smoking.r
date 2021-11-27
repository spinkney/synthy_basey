# replicate smoking study
library(cmdstanr)
library(data.table)

load("synth/smoking.rda")

# stan_file <- "synth_penalized.stan"
stan_file <- "synth_horseshoe_b_tau_x.stan"
mod_synth <- cmdstan_model(stan_file)

smoking <- data.table(smoking)
smoking_y <- dcast(smoking, year ~ state, value.var = "cigsale")
summaries <- smoking %>%
  group_by(state) %>%
  summarize(retprice = mean(retprice, na.rm = TRUE),
            lnincome = mean(lnincome, na.rm = TRUE),
            age15to24 = mean(age15to24, na.rm = TRUE),
            beer = mean(beer, na.rm = TRUE))

sm1975 <- data.frame(smoking[year == 1975, "cigsale"],
                     smoking[year == 1975, "state"])
sm1980 <- data.frame(smoking[year == 1980, "cigsale"],
                     smoking[year == 1980, "state"])
sm1988 <- data.frame(smoking[year == 1988, "cigsale"],
                     smoking[year == 1988, "state"])
sm1975 <- rename(sm1975, cigsale1975 = cigsale)
sm1980 <- rename(sm1980, cigsale1980 = cigsale)
sm1988 <- rename(sm1988, cigsale1988 = cigsale)

summaries <- inner_join(summaries, sm1975, by = "state")
summaries <- inner_join(summaries, sm1980, by = "state")
summaries <- inner_join(summaries, sm1988, by = "state")

predictors_target <- t(as.matrix(summaries[summaries$state == "California", 2:8]))
predictors_control <- t(as.matrix(summaries[summaries$state != "California", 2:8]))
X_pred <- cbind(predictors_target, predictors_control)

target <- "California"
target_index <- which(names(smoking_y) == target)
other_index <- which(names(smoking_y) %in% names(smoking_y)[c(-1, -4)])

which(smoking_y$year == 1988)


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
