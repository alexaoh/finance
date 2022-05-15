# Executable from Problem C.
# Alexander J Ohrt - Financial Statistics.

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
