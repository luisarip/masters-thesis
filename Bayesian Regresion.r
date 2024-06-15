### Bayesian Regresion
### Language: R
### Author: Luisa Ripoll Alberola

### Import libraries

library(brms)
library(rstan)
library(rstanarm)
library(rstantools)
library(caret)
library(tidyr)
library(dplyr)
library(cmdstanr)

### Read data

dataset6 <- read.csv("dataset6.csv")
head(dataset6)

### Check data types

dataset6$Users <- as.integer(dataset6$Users)
dataset6$New_users <- as.integer(dataset6$New_users)
dataset6$Avg_session <- as.integer(dataset6$Avg_session)
dataset6$Sessions <- as.integer(dataset6$Sessions)
# dataset6$Section <- as.factor(dataset6$Section)
dataset6$Year <- as.integer(dataset6$Year)
dataset6$Month <- as.factor(dataset6$Month)
# dataset6$Month <- as.integer(dataset6$Month)
# Convert columns 10 to 44 into integers
dataset6[, 10:44] <- lapply(dataset6[, 10:44], as.integer)
dataset6$Topics <- as.factor(dataset6$Topics)
dataset6$author_quartiles <- as.factor(dataset6$author_quartiles)
dataset6$Author_gender <- as.factor(dataset6$Author_gender)
head(dataset6)

### Converting NA levels into "Other"

# First, we convert them to char
dataset6$Section <- as.character(dataset6$Section)
dataset6$Topics <- as.character(dataset6$Topics)
dataset6$Author_gender <- as.character(dataset6$Author_gender)

# Modifications
dataset6$Section[is.na(dataset6$Section)] <- "Other"
dataset6$Topics[is.na(dataset6$Topics)] <- "Other"
dataset6$Author_gender[is.na(dataset6$Author_gender)] <- "Other"

any(is.na(dataset6))

# Converting back to factor data type
dataset6$Section <- as.factor(dataset6$Section)
dataset6$Topics <- as.factor(dataset6$Topics)
dataset6$Author_gender <- as.factor(dataset6$Author_gender)

### Checking for NA values
sum(is.na(dataset6))

### Applying dummy variables

dummy_data <- dummyVars(~ ., data = dataset6)
dataset7 <- predict(dummy_data, newdata = dataset6)
colnames(dataset7)
# Saving dataset with dummy vars
write.csv(dataset7, "dataset7.csv")

### Train and test partitions

# set.seed(123)  # for reproducibility
# dataset7 <- as.data.frame(dataset7)
# Define predictors
# predictors <- setdiff(names(dataset7), "Users")

# Split data into training and testing sets
# train_index <- createDataPartition(dataset7$Users, p = 0.8, list = FALSE)
# train_data <- dataset7[train_index, ]
# test_data <- dataset7[-train_index, ]

### 1st model: Poisson regression using stan_glm()

# Defining feature1
feature1 <- dataset6[, names(dataset6) %in% c("Users", "Pages_session", "Sessions", 
                     "New_users", "Avg_session", "positive_title", "trust_subtitle",
                      "P_rebound", "negative_title")]

# Simulate prior distribution
users_prior <- stan_glm(Users ~ ., data = feature1, 
                                 family = poisson,
                                 prior_intercept = normal(4, 2^2),
                                 prior = normal(0, 4^2, autoscale = TRUE),
                                 chains = 4, iter = 5000*2, seed = 102, 
                                 prior_PD = TRUE)

prior_summary(users_prior)

model11 <- update(users_prior, prior_PD = FALSE)

# Plotting the results: trace, ACF...
mcmc_trace(model11)
mcmc_dens_overlay(model11)
mcmc_acf(model11)

# Posterior predictive checks
par(mfrow = c(1,2))
set.seed(1)
pp_check(model11, plotfun = "hist", nreps = 5) + 
  xlab("Users") +
  xlim(0,200)
pp_check(model11) + 
  xlab("Users") +
  xlim(0, 200)

### 2nd model: Negative binomial model using stan_glm()

# Checking mean and variance are not equal
feature1 %>% 
  summarize(mean = mean(Users), var = var(Users))

model12 <- stan_glm(
  Users ~ ., 
  data = feature1, family = neg_binomial_2,
  prior_intercept = normal(2, 4, autoscale = TRUE),
  prior = normal(0, 4^2, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2,  seed = 102, 
  prior_PD = TRUE)

model12 <- update(model12, prior_PD = FALSE)

set.seed(1)
pp_check(model12, plotfun = "hist", nreps = 5) + 
  xlab("Users") +
  xlim(0,200)
pp_check(model12) + 
  xlab("Users") +
  xlim(0, 200)

### Checking possible priors for the dispersion parameter

# Set the shape and rate parameters
shape <- 2285
rate <- 637

# Define the range for the x-axis
x <- seq(0, shape/rate + 5*sqrt(shape/(rate^2)), length.out = 1000)

# Compute the density of the Gamma distribution
y <- dgamma(x, shape = shape, rate = rate)

# Plot the Gamma distribution
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = expression(phi %~% Gamma(2285, 637)),
     xlab = expression(phi), ylab = "Density")
grid()

# Adding more variability:
shape <- 13
rate <- 3
x <- seq(0, shape/rate + 5*sqrt(shape/(rate^2)), length.out = 1000)
y <- dgamma(x, shape = shape, rate = rate)

plot(x, y, type = "l", col = "blue", lwd = 2,
     main = expression(phi %~% Gamma(13,3)),
     xlab = expression(phi), ylab = "Density")
grid()

### 3rd model: Negative binomial model using brms package

library("cmdstanr")
# cmdstanr::check_cmdstan_toolchain(fix = TRUE)
install_cmdstan()

# Taking the 2nd selection of variables. 
# With the 1st selection of variables, it doesn't converge.
feature1 <- dataset6[, names(dataset6) %in% c("Users", "Sessions", 
                     "author_quartiles", "positive_subtitle", "negative_title", 
                     "negative_subtitle", "Pages_session", "positive_title",
                     "trust_subtitle")]
dummy_data_2 <- dummyVars(~ ., data = feature1)
feature1 <- predict(dummy_data_2, newdata = feature1)
feature1 <- as.data.frame(feature1)

priors <- c(
  set_prior("normal(4, 4)", class = "Intercept"),
  set_prior("normal(0, 16)", class = "b"),
  set_prior("gamma(13,3)", class = "shape")
)

init_fun <- function() {
  list(
    Intercept = 2.8,
    b = rnorm(n = ncol(feature1) - 1, mean = 0, sd = 2.5)
  )
}

# Use cmdstanr as the backend
model13 <- brm(
  formula = Users ~ .,
  data = feature1,
  family = negbinomial(),
  prior = priors,
  chains = 4,
  iter = 5000,
  warmup = 750,
  cores = parallel::detectCores(),
  threads = threading(2, grainsize = 500),
  backend = "cmdstanr",
  silent = 0,
  # init = init_fun,
  # control = list(adapt_delta = 0.99)  # Increase adapt_delta
)

summary(model13)

mcmc_acf(model13)

# Posterior predictive checks
set.seed(1)
pp_check(model13, plotfun = "hist", nreps = 50) + 
  xlab("Users") +
  xlim(0,200)
pp_check(model13) + 
  xlab("Users") +
  xlim(0, 200)

### 4th model: the dispersion parameter not constant.
### Warning: this method achieved poor results: unreliable convergence, low coefficients, NA predictions.  

library("cmdstanr")
install_cmdstan()

feature1 <- dataset6[, names(dataset6) %in% c("Users", "Sessions", 
                     "author_quartiles", "positive_subtitle",  
                     "negative_subtitle", "Pages_session", "positive_title",
                     "trust_subtitle")]

dummy_data_2 <- dummyVars(~ ., data = feature1)
feature1 <- predict(dummy_data_2, newdata = feature1)
feature1 <- as.data.frame(feature1)

priors <- c(
  set_prior("normal(4, 4)", class = "Intercept"),
  set_prior("normal(0, 16)", class = "b"),
  set_prior("normal(0, 4)", class = "b", dpar = "shape"),
  set_prior("normal(0, 4)", class = "Intercept", dpar = "shape")
)
  
model14 <- brm(bf(Users ~., shape ~ Users),
  data = as.data.frame(feature1),
  family = negbinomial(),
  prior = priors, chains = 4, iter = 2000,   
  cores = parallel::detectCores(),
  threads = threading(2, grainsize = 500),
  backend = "cmdstanr",
  # sample_prior = "only",
  silent = 0)

summary(model14)

pp_check(model14, ndraws = 50) + 
  xlab("Users") +
  xlim(0, 200)