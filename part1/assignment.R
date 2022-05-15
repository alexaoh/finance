## ----setup, include=FALSE--------------------------
rm(list = ls())
knitr::opts_chunk$set(echo = F, warning = F, fig.width = 10, comment = "#>", size = "scriptsize")
setwd("/home/ajo/gitRepos/finance/part1")
library(ggplot2)
library(tidyverse)
library(reshape2)


## --------------------------------------------------
set.seed(061999)
s0 <- 12
T1 <- 1
mu <- 0.03
sigma <- 0.17
n <-  250
t <- seq(0, T1, length.out = n+1) # n steps means n+1 points in this vector. 
h <- t[2] - t[1] # h = T1/n gives the same result. 
M1 <- 10
M2 <- 100
M3 <- 1000
M4 <- 10000
M5 <- 100000


## --------------------------------------------------
# Price path stochastic process. 
price.path <- function(s0, t, mu, sigma, version = "onestep"){
  if (version == "onestep"){
    Wt <- rnorm(n = length(t), sd = sqrt(t[2]-t[1])) # Draw length(t) normally distributed 
    #variables with mean 0 and variance h. 
    W2 <- cumsum(Wt) # Step size is constant, grid is uniform. We calculate the cumulative 
    #sum of the standard normal draw to simulate the Wiener process, which is N(0,t) at 
    #time t, i.e. the variance increases when the process is run further and further away 
    #from t = 0. 
    return(s0*exp(mu*t)*exp(sigma*W2-sigma^2/2*t)) # Return the one step solution to the SDE. 
  } else if (version == "EM") {
    n <- length(t)-1 # t is not strictly needed for this version,
    #but we simply extract n from t to make the function parameters as simple as possible. 
    #We subtract 1 because length of t is n+1. 
    Z <- rnorm(n) # Draw normally distributed variables. 
    S.values <- rep(NA, length.out = n+1) # Define vector for saving entire price path. 
    S.values[1] <- s0 # First value in price path is s0. 
    h <- t[2]-t[1] # Step length. 
    for (j in 2:(n+1)){ # Loop for EM scheme. 
      S.values[j] <- S.values[j-1] + mu*S.values[j-1]*h + sigma*S.values[j-1]*sqrt(h)*Z[j-1]
    }
    return(S.values) # Return the entire price path. 
  } else{
    stop("Please input a valid version name, either `EM` or `onestep`.")
  }
}
# Improvement: Could implement antithetic sampling in order to reduce the variance of the MC estimator. 
# This can be improved in the entire assignment. 


## --------------------------------------------------
# Generate the price paths for all the given number of paths. 
version <- "onestep" # Choose the version of the function we want to use. 

# For each of the M_i number of paths, we calculate M_i paths using the price.path function. 
M1.paths <- matrix(rep(NA, length.out = M1*(n+1)), nrow = M1)
for (i in 1:M1){
   M1.paths[i,] <- price.path(s0 = s0, t = t, mu = mu, sigma = sigma, version = version)
}

M2.paths <- matrix(rep(NA, length.out = M2*(n+1)), nrow = M2)
for (i in 1:M2){
  M2.paths[i,] <- price.path(s0, t, mu, sigma, version)
}

M3.paths <- matrix(rep(NA, length.out = M3*(n+1)), nrow = M3)
for (i in 1:M3){
  M3.paths[i,] <- price.path(s0, t, mu, sigma, version)
}

M4.paths <- matrix(rep(NA, length.out = M4*(n+1)), nrow = M4)
for (i in 1:M4){
  M4.paths[i,] <- price.path(s0, t, mu, sigma, version)
}

M5.paths <- matrix(rep(NA, length.out = M5*(n+1)), nrow = M5)
for (i in 1:M5){
  M5.paths[i,] <- price.path(s0, t, mu, sigma, version)
}


## --------------------------------------------------
mean.value <- s0*exp(mu*t)
matplot(t(M1.paths), type = "l", main = paste0(M1," paths"), xlab = "t in years", ylab = "S", xaxt = "n", ylim = c(8, 20))
lines(mean.value) # Drift / "mean value".
axis(1, at=seq(0,n, by = 10), labels = seq(0,1,length.out = n/10+1))

matplot(t(M2.paths), type = "l", main = paste0(M2," paths"), xlab = "t in years", ylab = "S", xaxt = "n", ylim = c(8, 20))
#lines(apply(M2.paths, 2, mean), cex = 2)
lines(mean.value) # Drift / "mean value".
axis(1, at=seq(0,n, by = 10), labels = seq(0,1,length.out = n/10+1))

matplot(t(M3.paths), type = "l", main = paste0(M3," paths"), xlab = "t in years", ylab = "S", xaxt = "n", ylim = c(8, 20))
lines(mean.value) # Drift / "mean value".
axis(1, at=seq(0,n, by = 10), labels = seq(0,1,length.out = n/10+1))


## --------------------------------------------------
# Calculate the Monte-Carlo estimator of $\hat{S}_T$ for each of the values of $M_i$.
hat_ST <- function(M){
  mean(M[,dim(M)[[2]]]) # Calculate mean over last value of all M_i paths. 
}
# This simply calculates the average over the last value of all the different paths. 

CI_ST <- function(M){
  s <- sd(M[,dim(M)[[2]]]) # Calculate sample standard deviation over last value of all M_i paths. 
  m <- hat_ST(M) # Calculate mean.
  ste <- qnorm(0.975)*s/sqrt(dim(M)[[1]])
  return(list(l = m - ste, u = m + ste))
}


## --------------------------------------------------
M1.hat <- hat_ST(M1.paths) 
CI1 <- CI_ST(M1.paths)
M2.hat <- hat_ST(M2.paths) 
CI2 <- CI_ST(M2.paths)
M3.hat <- hat_ST(M3.paths) 
CI3 <- CI_ST(M3.paths)
M4.hat <- hat_ST(M4.paths) 
CI4 <- CI_ST(M4.paths)
M5.hat <- hat_ST(M5.paths) 
CI5 <- CI_ST(M5.paths)

col1 <- rbind(M1.hat, CI1$l, CI1$u)
col2 <- rbind(M2.hat, CI2$l, CI2$u)
col3 <- rbind(M3.hat, CI3$l, CI3$u)
col4 <- rbind(M4.hat, CI4$l, CI4$u)
col5 <- rbind(M5.hat, CI5$l, CI5$u)


## --------------------------------------------------
hats <- cbind(col1, col2, col3, col4, col5)
colnames(hats) <- c("M1 = 10", "M2 = 100", "M3 = 1000", "M4 = 10000", "M5 = 100000") 
rownames(hats) <- c("Est.", "Lower CI", "Upper CI")
knitr::kable(hats, caption = "Monte Carlo Estimation for S_T, varying M")


## --------------------------------------------------
mean.value[length(mean.value)] # Drift / "mean value".
# This can also be calculated simply by s0*exp(mu*T1).


## --------------------------------------------------
M.star <- 1000
n1 <- 12
n2 <- 24
n3 <- 250
n4 <- 1000


## --------------------------------------------------
# Find new paths. 
n1.paths <- matrix(rep(NA, length.out = M.star*(n1+1)), nrow = M.star)
t1 <- seq(0, T1, length.out = n1+1) # n steps means n+1 points in this vector. 
for (i in 1:M.star){
  n1.paths[i,] <- price.path(s0, t1, mu, sigma, version)
}

n2.paths <- matrix(rep(NA, length.out = M.star*(n2+1)), nrow = M.star)
t2 <- seq(0, T1, length.out = n2+1) # n steps means n+1 points in this vector.
for (i in 1:M.star){
  n2.paths[i,] <- price.path(s0, t2, mu, sigma, version )
}

n3.paths <- matrix(rep(NA, length.out = M.star*(n3+1)), nrow = M.star)
t3 <- seq(0, T1, length.out = n3+1) # n steps means n+1 points in this vector. 
for (i in 1:M.star){
  n3.paths[i,] <- price.path(s0, t3, mu, sigma, version)
}

n4.paths <- matrix(rep(NA, length.out = M.star*(n4+1)), nrow = M.star)
t4 <- seq(0, T1, length.out = n4+1) # n steps means n+1 points in this vector. 
for (i in 1:M.star){
  n4.paths[i,] <- price.path(s0, t4, mu, sigma, version)
}


## ---- eval = F, echo = F---------------------------
## # Burde nok helst unngå den transponerte, heller definere matrisen omvendt.
## matplot(t(n1.paths), type = "l")
## matplot(t(n2.paths), type = "l")
## matplot(t(n3.paths), type = "l")
## matplot(t(n4.paths), type = "l")


## --------------------------------------------------
n1.hat <- hat_ST(n1.paths) 
CI1 <- CI_ST(n1.paths)
n2.hat <- hat_ST(n2.paths) 
CI2 <- CI_ST(n2.paths)
n3.hat <- hat_ST(n3.paths) 
CI3 <- CI_ST(n3.paths)
n4.hat <- hat_ST(n4.paths) 
CI4 <- CI_ST(n4.paths)

col1 <- rbind(n1.hat, CI1$l, CI1$u)
col2 <- rbind(n2.hat, CI2$l, CI2$u)
col3 <- rbind(n3.hat, CI3$l, CI3$u)
col4 <- rbind(n4.hat, CI4$l, CI4$u)

hats <- cbind(col1, col2, col3, col4)
colnames(hats) <- c("n1 = 12", "n2 = 24", "n3 = 250", "n4 = 1000")
rownames(hats) <- c("Est.", "Lower CI", "Upper CI")
knitr::kable(hats, caption = "Monte Carlo Estimation for S_T, varying n")


## --------------------------------------------------
set.seed(061999)
s0 <- 24
T1 <- 1.5
r <- 0.02
sigma <- 0.22
K <- 23.5


## --------------------------------------------------
# BSM pricing formula for European call option, 
# to calculate the fair price at t = 0 in closed form.
BSM <- function(s0, T1, r, sigma, K){
  d1 <- (log(s0/K)+(r+sigma^2/2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  s0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2)  
}


## --------------------------------------------------
(BSM.price <- BSM(s0, T1, r, sigma, K))


## --------------------------------------------------
# Monte Carlo estimator for Euler-Maruyama. 
# This is for a Call option still (almost general) (slide 28)
MC <- function(M, n, r, s0, mu, sigma, T1, payoff.profile, set.S, ...){
  h <- T1/n # Calculate step size for use in Euler-Maruyama. 
  C.values <- rep(NA, length.out = M) # Initiate vector for saving payoffs per path. 
  for (i in 1:M){ # Initiate loop for M path.
    Z <- rnorm(n) # Generate n random variables standard normally. 
    S.values <- rep(NA, length.out = n+1) # Initiate vector for saving price path. 
    S.values[1] <- s0 # Initiate starting price at time t = 0. 
    for (j in 2:(n+1)){ # Loop for Euler-Maruyama - recursively estimate price path. 
      S.values[j] <- S.values[j-1] + mu*S.values[j-1]*h + sigma*S.values[j-1]*sqrt(h)*Z[j-1]
    }
    S <- set.S(S.values) # Set expected price used in payoff-profile. 
    # Generality is kept using a helper function set.S.
    C.values[i] <- payoff.profile(S, K) # Calculate payoff for the path and save in C.values.
  }
  return(list(MC = exp(-r*T1)*mean(C.values), path.values = exp(-r*T1)*C.values)) # Return discounted average payoff. 
  # This is the MC estimator. We also return the discounted C.values, which will be used to calculate the CIs in problem C. 
}


## --------------------------------------------------
# Definition of helper functions to feed into MC estimator code above. 
Call.profile <- function(S, K){
  max(0, S-K)
}

# Feed the function into the MC estimator in order to calculate estimate for Call option. 
Call.set.S <- function(S.values){
  S.values[length(S.values)] # The Call option uses the last value in the list. 
  # This is standard for European Call option. 
}


## --------------------------------------------------
M.list <- c(10, 100, 1000, 10000, 100000)
n.list <- c(10, 100, 1000)
# Simulate paths with the MC estimator and Euler-Maruyama. 
combinations <- matrix(rep(NA, length.out = length(M.list)*length(n.list)), nrow = length(M.list))

for (i in 1:length(M.list)){
  for (j in 1:length(n.list)){
    h <- T1/n.list[j]
    combinations[i, j] <- MC(M = M.list[i], n = n.list[j], r = r, s0 = s0, mu = r, 
                  sigma = sigma, T1 = T1, payoff.profile = Call.profile, set.S = Call.set.S, K = K)$MC
  }
}


## --------------------------------------------------
# Calculations of relative error and absolute error.
rel.errors <- (BSM.price - combinations)/BSM.price
abs.errors <- abs(BSM.price - combinations)


## --------------------------------------------------
# Tabulate the results.
df.rel <- data.frame(rel.errors)
rownames(df.rel) <- c("M = 10", "M = 100", "M = 1000", "M = 10000", "M = 100000")
colnames(df.rel) <- c("n = 10", "n = 100", "n = 1000")

df.abs <- data.frame(abs.errors)
rownames(df.abs) <- c("M = 10", "M = 100", "M = 1000", "M = 10000", "M = 100000")
colnames(df.abs) <- c("n = 10", "n = 100", "n = 1000")

knitr::kable(df.rel, caption = "Relative Errors")
knitr::kable(df.abs, caption = "Absolute Errors")


## --------------------------------------------------
set.seed(061999)
s0 <- 20
T1 <- 1
r <- 0.02
sigma <- 0.24
K <- 20
n <- 252
M <- 10000

# This function is fed into the MC estimator instead. 
Asian.set.S <- function(S.values){ 
  # We extract every 5th value and calculate their arithmetic mean.
  mean(S.values[seq(5, length(S.values), 5)])
}

# Feed the payoff profile into the MC estimator. 
# This is the same as the Call.profile above, so not needed!
Asian.profile <- function(S, K){
  max(0, S-K)
}

# Still assuming that mu = r.
(Asian.price <- MC(M = M, n = n, r = r, s0 = s0, mu = r, 
   sigma = sigma, T1 = T1, Asian.profile, Asian.set.S, K = K)$MC)


## --------------------------------------------------
set.seed(061999)
s0 <- 22
T1 <- 2
r <- 0.02
sigma <- 0.29
K <- 21
n <- 1000
M1 <- 1000
M2 <- 10000
M3 <- 100000

# This function can be fed into the MC estimator instead. 
lookback.set.S <- function(S.values){ 
  # We return the maximum value among the S.values. 
  max(S.values)
}

# Feed the payoff profile into the MC estimator. 
# This is the same as the Call.profile above, so not needed!
lookback.profile <- function(S, K){
  max(0, S-K)
}

# We still assume that mu = r.


## --------------------------------------------------
# We calculate CIs of this option price for the different values of M.
M1.results <- MC(M = M1, n = n, r = r, s0 = s0, mu = r, 
   sigma = sigma, T1 = T1, lookback.profile, lookback.set.S, K = K)
M1.path.values <- M1.results$path.values 
M1.MC <- M1.results$MC

M2.results <- MC(M = M2, n = n, r = r, s0 = s0, mu = r, 
   sigma = sigma, T1 = T1, lookback.profile, lookback.set.S, K = K)
M2.path.values <- M2.results$path.values 
M2.MC <- M2.results$MC

M3.results <- MC(M = M3, n = n, r = r, s0 = s0, mu = r, 
   sigma = sigma, T1 = T1, lookback.profile, lookback.set.S, K = K)
M3.path.values <- M3.results$path.values 
M3.MC <- M3.results$MC


## --------------------------------------------------
# Now we can calculate the 95% CI's. This is done using the same formula as explained earlier. 
lookback.CI <- function(M.path.values){
  z.alpha <- qnorm(0.975) # Quantile. 
  M <- length(M.path.values) # Number of paths. 
  M.MC <- mean(M.path.values) # Point estimator
  sd1 <- sd(M.path.values) # Standard error.
  ste <- z.alpha*sd1/sqrt(M) # +- value. 
  list(l = M.MC - ste, u = M.MC + ste) # Return CI. 
}

CI1 <- lookback.CI(M1.path.values)
CI2 <- lookback.CI(M2.path.values)
CI3 <- lookback.CI(M3.path.values)

col1 <- rbind(M1.MC, CI1$l, CI1$u)
col2 <- rbind(M2.MC, CI2$l, CI2$u)
col3 <- rbind(M3.MC, CI3$l, CI3$u)

hats <- cbind(col1, col2, col3)
colnames(hats) <- c("M1 = 1000", "M2 = 10000", "M3 = 100000") 
rownames(hats) <- c("Est.", "Lower CI", "Upper CI")
knitr::kable(hats, caption = "Monte Carlo Estimation for Lookback Option, Varying M")
