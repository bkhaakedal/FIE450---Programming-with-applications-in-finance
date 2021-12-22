##----------------------------------------------------------------------------##
##                 FIE450 - Assignment 1-2                                    ##
##----------------------------------------------------------------------------##

rm(list=ls())

## Parameters ------------------------------------------------------------------

sigma <- 0.10  # Volatility p.a.
S0 <- 100      # Starting price
r <- 0.05      # Expected rate of return p.a.
T <- 5         # Time window
n <- 1000      # Number of paths
dt <- 1/250    # Sampling frequency 


## Task 2.1 --------------------------------------------------------------------

## Simulates a set of price paths and returns a matrix
simulate.paths.fast <- function(S0, r, sigma, dt, T, n) {
  t <- seq(dt, T, by = dt)                           
  m <- length(t)  
  e <- matrix(exp((r - 0.5 * sigma^2) * dt + 
                    sigma * sqrt(dt) * rnorm(n * m)), m, n) 
  S <- apply(e, 2, cumprod) 
  S <- S * S0 
  S <- rbind(S0, S) 
  return(S)
}

S1 <- simulate.paths.fast(S0, r, sigma, dt, T, n) # Matrix all paths
R1 <- diff(S1)/S1[-nrow(S1),]                     # Matrix of returns %
returns <- apply(R1, 2, mean) * 250               # Annualized expected returns

se.a <- sd(returns)/sqrt(length(returns))         # Standard Error Monte-Carlo


## Task 2.2 --------------------------------------------------------------------

se.b <- sd(R1[,1])/sqrt(nrow(R1)) * 250


## Task 2.3 --------------------------------------------------------------------

S2 <- simulate.paths.fast(S0, r, sigma, dt = 1/12, T, n) 
R2 <- diff(S2)/S2[-nrow(S2),] 
returns2 <- apply(R2, 2, mean) * 12 

se.c <- sd(returns2)/sqrt(length(returns2)) 


## Task 2.4 --------------------------------------------------------------------

se.d <- sd(R2[,1])/sqrt(nrow(R2)) * 12


## Task 2.5 --------------------------------------------------------------------

# Increasing frequency, but keeping timespan constant, does not improve results. 


## Task 2.6 --------------------------------------------------------------------

# We prefer monthly, because it requires less computational power (faster). 

