# Executable from Problem B.
# Alexander J Ohrt - Financial Statistics.

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
