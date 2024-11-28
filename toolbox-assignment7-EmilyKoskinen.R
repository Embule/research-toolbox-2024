## Q1
# Install color library
# install.packages("wesanderson")
library(wesanderson)

# The Power law of Learning curve
power <- function(t, mu) {
  1 - (t + 1) ^ -mu
}

mu = c(0.9, 0.8, 0.7, 0.4, 0.3, 0.2)
n_col <- 1

plot(1, 1, type = "n")
for (i in mu) {
  curve(
    power(x, i),
    add = (i != mu[1]),
    0,
    5,
    col = wes_palette("AsteroidCity2")[n_col],
    ylim = c(0, 1),
    main = 'Power of Law learning curves',
    xlab = 'Trials',
    ylab = 'Performance'
  )
  n_col <- n_col + 1
}
legend("topleft", legend = paste("mu =", mu), col = wes_palette("AsteroidCity2"), lwd = 2)

# The exponential learning curve 
exponential <- function(t, mu) {
  1 - exp(-mu * t)
}
mu <-  c(.05, .2 , .4, .6, .8, 1.0)
n_col <- 1

plot(1, 1, type = "n")
for (i in mu) {
  curve(
    exponential(x, i),
    add = (i != mu[1]),
    0,
    5,
    col = wes_palette("AsteroidCity2")[n_col],
    ylim = c(0, 1),
    main = 'Exponential learning curves',
    xlab = 'Trials',
    ylab = 'Performance'
  )

  n_col <- n_col + 1
}
legend("topleft", legend = paste("mu =", mu), col = wes_palette("AsteroidCity2"), lwd = 2)


## Q2


## Q3
c <- 5
mu <- 2
murre <- function(t) {
  (1 - exp(-mu * t))^c
}

curve(
  murre(x),
  from = 0,
  to = 5,
  add = TRUE,
  col = wes_palette("Moonrise3"),
  lwd = 2,
  main = "Murre function",
  ylab = "Performance", xlab = "Trials"
)


## Q4
set.seed(111)
murre <- function(t, mu, c) {
  (1 - exp(-mu * t))^c
}

n <- 30
c <- 5
mu <- 0.25

noise <- rnorm(n, 0, 0.05)
trials <- seq(0, 30, length.out = 30)
simulations <- murre(trials, mu, c) + noise

plot(
  trials,
  simulations,
  col = wes_palette("GrandBudapest2")[3],
  ylim = c(0, 1),
  xlab = "Trials",
  ylab = "Performance",
  main = "Murre data",
  pch = 16
)


## Q5

# Subject 1
set.seed(111)
murre <- function(t, mu, c) {
  (1 - exp(-mu * t))^c
}

n <- 6 # number of trials
c <- 18 # number of words
mu <- 0.85 # rate of learning

noise <- rnorm(n, 0, 0.05)
trials <- seq(1,6)
simulations <- murre(trials, mu, c) + noise

plot(
  trials,
  simulations,
  col = wes_palette("GrandBudapest2")[1],
  ylim = c(0, 1),
  xlab = "Trials",
  ylab = "Performance",
  main = "Simulated data for subject 1",
  pch = 16
)

# Subject 2
set.seed(111)
murre <- function(t, mu, c) {
  (1 - exp(-mu * t))^c
}

n <- 6 # number of trials
c <- 18 # number of words
mu <- 0.6 # rate of learning

noise <- rnorm(n, 0, 0.02)
trials <- seq(1,6)
simulations <- murre(trials, mu, c) + noise

plot(
  trials,
  simulations,
  col = wes_palette("GrandBudapest2")[4],
  xlab = "Trials",
  ylab = "Performance",
  main = "Simulated data for subject 2",
  pch = 16
)

## Q6

# Plotting (log)likelihood of one binomial observation 
lnL_binom = function(p, x, n) {
  log(factorial(n) / (factorial(x) * (factorial(n - x)))) + log(p ^ x) +
  log((1 - p) ^ (n - x))
}
curve(lnL_binom(x, x = 10, n = 100),
      0.05,
      0.2,
      ylab = "log-likelihood",
      main = "LogLikelihood for one binomial observation, x = 10")


## Q7 
N = 1000
set.seed(123)
x <- runif(N)
w <- runif(N)
sigma <- rnorm(N, 0, 0.05)
y <- 3 - 0.2 * x + 0.5 * w  + sigma
fit <- lm(y ~ x + w)

plot(x, y)
abline(fit, col = "red")

plot(x, w)
abline(fit, col = "red")

summary(fit)

LL <- function(beta0, beta1, beta2, mu, sigma) {
  residual = y - beta0 - beta1 * x + beta2 * w
  likelihoods = suppressWarnings(dnorm(residual, 0, sigma))
  - sum(log(likelihoods)) 
}

# install.packages('bbmle')
# library(bbmle)
fit2 <- mle2(LL, start = list(
  beta0 = 4,
  beta1 = 1,
  beta2 = 2,
  sigma = .5
))

summary(fit2)


## Q8
data <- read.csv("language-dataset.csv")
data

# get data for each subject
subject1 <- subset(data, data$Subject == 257350)
subject2 <- subset(data, data$Subject == 257380)


# Subject 1 
N = nrow(subject1)
P <- subject1$accuracy
T <- seq(1:6)
mu = 5
# P = 1 - exp(-mu * T) + rnorm(N, 0, .1)
plot(T, P, main = "Subject 1 exponential")

# Fit the data with exponential function
LL_exp1 <- function(mu, sigma) {
  residual = P - (1 - exp(-mu * T))
  likelihood = suppressWarnings(dnorm(residual, 0, sigma))
  - sum(log(likelihood)) # log likelihood of all observations
}

fit_exp1 <- mle2(LL_exp1, start = list(mu = 2, sigma = 1))
summary(fit_exp1)

# Plot the exponential curve with the estimated (MLE) mu
f = function(T, mu)
  1 - exp(-mu * T)
curve(f(x, mu = fit_exp1@coef[[1]]),
      0,
      6,
      add = TRUE,
      col = 'red')


# S-curve law of practice in R
mu = 5
d = .5
P = subject1$accuracy # logistic function
plot(T, P, main = "Subject 1 logistic")

# Fit the data using maximum likelihood
LL_logistic1 <- function(mu, d, sigma) {
  residual = P - 1 / (1 + exp(-mu * (T - d)))
  likelihood = suppressWarnings(dnorm(residual , 0, sigma))
  - sum(log(likelihood))
}

fit_logistic1 <- mle2(LL_logistic1, start = list(mu = 2, d = 1, sigma = 1))
summary(fit_logistic1)

# Plot the curve with the estimated (MLE) mu and estimated (MLE) d
f = function(T, mu, d)
  1 / (1 + exp(-mu * (T - d)))
curve(
  f(x, mu = fit_logistic1@coef[[1]], d = fit_logistic1@coef[[2]]),
  0,
  6,
  add = TRUE,
  col = 'green'
)

# Subject 2 ############
N = nrow(subject2)
mu = 5
P = subject2$accuracy
plot(T, P, main = "Subject 2 exponential")

# Fit the data with exponential function
LL_exp2 <- function(mu, sigma) {
  residual = P - (1 - exp(-mu * T))
  likelihood = suppressWarnings(dnorm(residual, 0, sigma))
  - sum(log(likelihood)) # log likelihood of all observations
}

fit_exp2 <- mle2(LL_exp2, start = list(mu = 2, sigma = 1))
summary(fit_exp2)

# Plot the exponential curve with the estimated (MLE) mu
f = function(T, mu)
  1 - exp(-mu * T)
curve(f(x, mu = fit_exp2@coef[[1]]),
      0,
      6,
      add = TRUE,
      col = 'red')


# S-curve law of practice in R
mu = 5
d = .5
plot(T, P, main = "Subject 2 logistic")

# Fit the data using maximum likelihood
LL_logistic2 <- function(mu, d, sigma) {
  residual = P - 1 / (1 + exp(-mu * (T - d)))
  likelihood = suppressWarnings(dnorm(residual , 0, sigma))
  - sum(log(likelihood))
}

fit_logistic2 <- mle2(LL_logistic2, start = list(mu = 2, d = 1, sigma = 1))
summary(fit_logistic2)

# Plot the curve with the estimated (MLE) mu and estimated (MLE) d
f = function(T, mu, d)
  1 / (1 + exp(-mu * (T - d)))
curve(
  f(x, mu = fit_logistic2@coef[[1]], d = fit_logistic2@coef[[2]]),
  0,
  6,
  add = TRUE,
  col = 'green'
)

# model comparisons
AIC_exp1 = -2 * as.numeric(logLik(fit_exp1)) + 2 * 2 # 1.616097
AIC_logistic1 = -2 * as.numeric(logLik(fit_logistic1)) + 2 * 3 # -13.55511
AIC_exp2 = -2 * as.numeric(logLik(fit_exp2)) + 2 * 2 # -11.41547
AIC_logistic2 = -2 * as.numeric(logLik(fit_logistic2)) + 2 * 3 # -20.47122











