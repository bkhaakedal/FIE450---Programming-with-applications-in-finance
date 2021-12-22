##----------------------------------------------------------------------------##
##                       FIE450 - Assignment 2-2                              ##
##----------------------------------------------------------------------------##

rm(list=ls())
load("FIE450-Assignment-2.RData")

df[, -1] <- df[,-1]/100         # From percentages to decimals
mkt[ , -1] <- mkt[ , -1]/100

require(quadprog)

# Assumption:
# To ensure that there are 60 past return observations, we start at return
# t = 61. This ensures that row 60+i does not contain NAs. That is, we only use 
# return observations from t=61 to t=180, to end up with 120 portfolio returns. 


## Task 2.1 --------------------------------------------------------------------

r <- c()  # Return series

# Backtesting a minimum variance strategy. Rolling window = 60. 
for (i in 1:120) {
  df.t <- df[i:(60+i), -1]
  df.t <- df.t[, colSums(is.na(df.t)) == 0]
  mkt.t <- mkt[i:(59+i), ]                      # Extract 60 rows
  
  # Estimate coefficients using the Single Index Model
  # Regress past 60 observations of each stock, with past 60 market observations
  reg <- apply(df.t[-nrow(df.t), ], 2, function(v) {         
    res <- lm(v ~ mkt.t$MKTRF3)                 # Regresses stock_i with market
    c(coefficients(res), var(residuals(res)))   # Extract alpha, beta, var.eps 
  })
  
  alpha <- 0        # Assuming efficient markets, alpha = 0
  beta <- reg[2, ]
  var.eps <- reg[3, ]
  
  mu.index <- mean(mkt.t$MKTRF3)
  var.index <- var(mkt.t$MKTRF3)
  mu <- beta * mu.index
  Sigma <- var.index * (as.matrix(beta) %*% beta)
  diag(Sigma) <- diag(Sigma) + var.eps
  
  # Solve quadratic programming problem to determine optimal portfolio. 
  d <- rep(0, length(mu))               # No linear term, set d = 0  
  A <- as.matrix(rep(1, length(mu)))   
  b0 <- c(1)                            # Sum weights = 100 %
  res <- solve.QP(Dmat = Sigma, 
                  dvec = d, 
                  Amat = A, 
                  bvec = b0, 
                  meq = 1)              # Only one strict equality constraint
  omega <- res$solution
  
  returns <- df.t[nrow(df.t), ]         # Return vector for iteration i
  r <- c(r, sum(omega * returns))       # Return series of this strategy
}


## Task 2.2 --------------------------------------------------------------------

mu <- mean(r) * 12


## Task 2.3 --------------------------------------------------------------------

sr <- mu / (sd(r) * sqrt(12))

