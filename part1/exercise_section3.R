# BSM pricing formula for European call option, 
# to calculate the fair price at t = 0 in closed form.
BSM <- function(s0, K, T1, r, sigma){
  d1 <- (log(s0/K)+(r+sigma^2/2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  s0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2)  
}

# Exercises on slide 45

# 1 - BSM formula pricing.
para <- list(s0 = 20, K = 18, T1 = 1, r = 0.05, sigma = 0.2)
# Call price
BSM(s0 = para$s0, K = para$K, T1 = para$T1, r = para$r, sigma = para$sigma)
# Put price
BSM(s0 = para$s0, K = para$K, T1 = para$T1, r = para$r, sigma = para$sigma)-
  para$s0+exp(-para$r*para$T1)*para$K

para <- list(s0 = 32.5, K = 30, T1 = 0.75, r = 0.01, sigma = 0.23)
# Call price
BSM(s0 = para$s0, K = para$K, T1 = para$T1, r = para$r, sigma = para$sigma)
# Put price
BSM(s0 = para$s0, K = para$K, T1 = para$T1, r = para$r, sigma = para$sigma)-
  para$s0+exp(-para$r*para$T1)*para$K

para <- list(s0 = 79, K = 20, T1 = 1.5, r = 0.02, sigma = 0.43)
# Call price
BSM(s0 = para$s0, K = para$K, T1 = para$T1, r = para$r, sigma = para$sigma)
# Put price
BSM(s0 = para$s0, K = para$K, T1 = para$T1, r = para$r, sigma = para$sigma)-
  para$s0+exp(-para$r*para$T1)*para$K

# THE FIRST COINCIDES WITH THE RESULTS FROM THE SLIDE, BUT THE TWO SECOND ONES DO NOT!
# I BELIEVE SOMETHING MIGHT BE WRONG WITH THE NUMBERS ON THE SLIDE. 

# 2 - EM method implementation.
s0 <- 10
T1 <- 2
mu <- 0.03
sigma <- 0.19
M <- 100
n <- c(24, 504, 2500)
h <- T1/n

make.paths <- function(n, h){
  C.values <- matrix(rep(NA, length.out = M*(n+1)), nrow = M)
  for (i in 1:M){
    Z <- rnorm(n)
    S.values <- rep(NA, length.out = n+1)
    S.values[1] <- s0
    for (j in 2:(n+1)){
      S.values[j] <- S.values[j-1] + mu*S.values[j-1]*h + sigma*S.values[j-1]*sqrt(h)*Z[j-1]
    }
    C.values[i,] <- S.values
  }
  C.values
}

paths1 <- make.paths(n[1], h[1]) 
matplot(t(paths1), type = "l", main = paste0(n[1]," steps"), xlab = "Discretization Points")

paths2 <- make.paths(n[2], h[2]) 
matplot(t(paths2), type = "l", main = paste0(n[2]," steps"), xlab = "Discretization Points")

paths3 <- make.paths(n[3], h[3]) 
matplot(t(paths3), type = "l", main = paste0(n[3]," steps"), xlab = "Discretization Points")

# 3 - Repeat 2 and calculate the MC estimator for S_T
s0 <- 10
T1 <- 2
mu <- 0.03
sigma <- 0.19
M <- 10000
n <- c(24, 504, 2500, 1000)
h <- T1/n

n <- n[4]
h <- h[4]

make.paths <- function(){
  C.values <- rep(NA, length.out = M)
  for (i in 1:M){
    Z <- rnorm(n)
    S.values <- rep(NA, length.out = n+1)
    S.values[1] <- s0
    for (j in 2:(n+1)){
      S.values[j] <- S.values[j-1] + mu*S.values[j-1]*h + sigma*S.values[j-1]*sqrt(h)*Z[j-1]
    }
    S <- S.values[length(S.values)] # Choose last value for European call option price calculation.
    C.values[i] <- S
  }
  mean(C.values) # Return discounted average payoff
}

make.paths() # The result in the slides is 10.62, which is very close to mine!
