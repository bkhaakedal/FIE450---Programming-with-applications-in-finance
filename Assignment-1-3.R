##----------------------------------------------------------------------------##
##                 FIE450 - Assignment 1-3                                    ##
##----------------------------------------------------------------------------##

rm(list=ls())

## Task 3.1 --------------------------------------------------------------------

P.market.bid <- 16.25      # Put bid price
P.market.ask <- 17.50      # Put ask price

S0 <- 878.01               # Underlying price
K  <- 880.00               # Strike price
T  <- as.numeric(as.Date("2021-02-19") - as.Date("2021-01-26")) / 365 # Maturity
rf <- 0.01                 # Risk-free

# Function that computes the price of an European put option
put <- function(S0, sigma, K, rf, T) {
  d1 <- (log(S0/K) + (rf + sigma^2/2) * T)/sigma/sqrt(T)
  d2 <- d1 - sigma * sqrt(T)
  P <- exp(-rf * T) * K * pnorm(-d2) - S0 * pnorm(-d1) # Modified for put option
  return(P)
}

# Function that computes the squared error between the model and market price.
obj.fun <- function(sigma, P.market, S0, K, rf, T) {
  P.model <- put(S0, sigma, K, rf, T)
  eps <- (P.model - P.market)^2
  return(eps)
}

# Minimizing using bid price 
res <- nlm(obj.fun, p = 0.2, P.market = P.market.bid, S0, K, rf, T)

sigma.bid <- res$estimate


## Task 3.2 --------------------------------------------------------------------

# Minimizing using ask price
res <- nlm(obj.fun, p = 0.2, P.market = P.market.ask, S0, K, rf, T)

sigma.ask <- res$estimate

