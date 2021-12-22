##----------------------------------------------------------------------------##
##                       FIE450 - Assignment 2-4                              ##
##----------------------------------------------------------------------------##

rm(list=ls())
load("FIE450-Assignment-2.RData")

df[, -1] <- df[,-1]/100         # From percentages to decimals
mkt[ , -1] <- mkt[ , -1]/100

# Assumption:
# To ensure that there are 60 past return observations, we start at return
# t = 61. Therefore we extract 61 observations each iteration in the loop.
# When evaluating performance over 12 months, we extract the previous 12,
# BEFORE the 61st observation.
# I.e: In the first iteration, we have extracted observation 1-61. Then we
# extract row 49 to 60 to evaluate the cumulative return of the past 12 months. 
# The first portfolio return is therefore based on returns in row 61.


## Task 4.1 --------------------------------------------------------------------

r <- c()  # Return series

# For loop that compute the 120 portfolio returns from t = 61 to t = 180.
for (i in 1:120) {
  df.t <- df[i:(60+i), -1]
  df.t <- df.t[, colSums(is.na(df.t)) == 0]
  mom <- df.t[(nrow(df.t)-12):(nrow(df.t)-1), ]  # Extract past 12 months
  cum.return <- cumsum(mom)                      # Compute cumulative return
  cum.return <- cum.return[nrow(cum.return), ]   # Select last cumulative
  x <- as.vector(t(cum.return))                  # Cumulative return vector
  return <- as.vector(t(df.t[61, ]))             # Select return at t
  
  sort.x <- sort(x, index.return = TRUE)         # Sort x, save original index
  n <- round(length(x) * 0.2)                    # n = stocks to buy and sell
  r.low <- sort.x$ix[(1:n)]                      # Indicies of low cum.returns
  r.high <- sort.x$ix[(length(x)-n+1):length(x)] # Indicies of high cum.returns
  return.low <- return[r.low]                    # Lowest returns at t = 60+i
  return.high <- return[r.high]                  # Highest returns at t = 60+i
  
  # Assuming zero-cost momentum strategy (sum total portfolio weights = 0)
  w <- (1 / (2*n))                               
  p.return <- w * (sum(return.high) - sum(return.low))
  
  r <- c(r, p.return)                            # Return series of strategy
}


## Task 4.2 --------------------------------------------------------------------

mu <- mean(r) * 12


## Task 4.3 --------------------------------------------------------------------

sr <- mu / (sd(r) * sqrt(12))

