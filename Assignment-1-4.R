##----------------------------------------------------------------------------##
##                 FIE450 - Assignment 1-4                                    ##
##----------------------------------------------------------------------------##

rm(list=ls())

## Parameters ------------------------------------------------------------------

sigma <- 0.18                             # Volatility of the underlying
b <- 850                                  # Barrier
S0 <- K <- 878.01                         # Underlying price & strike
T <- 1.25                                 # Maturity
dt <- 0.25                                # Time step
n <- 10000                                # Paths
Z <- c(1, 0.99, 0.98, 0.97, 0.96, 0.95)   # Bond prices
r <- Z[-length(Z)]/Z[-1]                  # Discount rates
r <- diag(r, 5)                           # Identity matrix of discount rates
rf <- (1/Z[length(Z)])^(1/T)-1            # Annual discount rate


## Task 4.1 --------------------------------------------------------------------

simulate.paths.fast <- function(S0, r, sigma, dt, T, n) {
  t <- seq(dt, T, by = dt) 
  m <- length(t) 
  e <- matrix(exp((-0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(n * m)), m, n) 
  S <- e
  S <- r %*% S                    # Multiplying rates with matching time periods 
  S <- apply(S, 2, cumprod)
  S <- S * S0
  S <- rbind(S0, S) 
  return(S)
}

S <- simulate.paths.fast(S0, r, sigma, dt, T, n)

payoffs <- function(S, K, b, rf) {
  I <- apply(S<b, 2, any) 
  X <- I*pmax((S[nrow(S), ] - K), 0)
  X0 <- X/(1 + rf)^T
  return(X0)
}

X0 <- payoffs(S, K, b, rf)
V <- mean(X0)


## Task 4.2 --------------------------------------------------------------------

SE <- sd(X0)/sqrt(length(X0)) 

alpha <- 0.999
z <- -qnorm((1 - alpha)/2)
ci <- c(V - z * SE, V + z * SE)


## Task 4.3 --------------------------------------------------------------------

simulate.paths.fast.as <- function(S0, r, sigma, dt, T, n) {
  t <- seq(dt, T, by = dt)
  m <- length(t)
  z <- rnorm(n * m)
  z.as <- -z
  Z <- matrix(c(z, z.as), m, n * 2)
  e <- exp((-0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
  S <- e
  S <- r %*% S
  S <- apply(S, 2, cumprod)
  S <- S0 * S
  S <- rbind(S0, S)
  return(S)
}

n <- n/2

S.as <- simulate.paths.fast.as(S0, r, sigma, dt, T, n)
X0.as <- payoffs(S.as, K, b, rf)
V.as <- mean(X0.as)


## Task 4.4 --------------------------------------------------------------------

X.pairs <- (X0.as[1:n] + X0.as[(n+1):(2 * n)])/2
SE.as <- sd(X.pairs)/sqrt(n)

ci.as <- c(V.as - z * SE.as, V.as + z * SE.as)


## Task 4.5 --------------------------------------------------------------------

rho <- cor(X0.as[1:n], X0.as[(n + 1):(2 * n)])

# Since rho is negative, it means that the variance is reduced due to the 
# antithetic sampling. Hence, it improves the accuracy of the estimator.

