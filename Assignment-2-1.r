##----------------------------------------------------------------------------##
##                       FIE450 - Assignment 2-1                              ##
##----------------------------------------------------------------------------##

rm(list=ls())
load("FIE450-Assignment-2.RData")

df[, -1] <- df[,-1]/100         # From percentages to decimals
mkt[ , -1] <- mkt[ , -1]/100

# Assumption:
# To ensure that there are 60 past return observations, we start at return
# t = 61. This ensures that row 60+i does not contain NAs. That is, we only use 
# return observations from t=61 to t=180, to end up with 120 portfolio returns. 


## Task 1.1 --------------------------------------------------------------------

r <- c()  # Return series

# For loop that compute the 120 portfolio returns from t = 61 to t = 180.
for (i in 1:120) {
  df.t <- df[i:(60+i), -1]                     # Extract 61 rows, remove date
  df.t <- df.t[, colSums(is.na(df.t)) == 0]    # Select columns without NA's
  returns <- as.vector(t(df.t[61, ]))          # Extract the last returns
  omega <- 1 / (length(returns))               # Compute 1/n weight
  weighted.returns <- omega * returns         
  portfolio.return <- sum(weighted.returns)
  r <- c(r, portfolio.return)                  # Return series of this strategy             
}


## Task 1.2 --------------------------------------------------------------------

mu <- mean(r) * 12


## Task 1.3 --------------------------------------------------------------------

R <- mu / (sd(r) * sqrt(12))


## Task 1.4 --------------------------------------------------------------------

V <- prod(1+r) - 1

