##----------------------------------------------------------------------------##
##                       FIE450 - Assignment 2-3                              ##
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


## Task 3.1 --------------------------------------------------------------------

r <- c()  # Return series
w <- c()  # Weight series

# Backtesting a minimum variance strategy with weight bounds = [0, 0.10] 
for (i in 1:120) {
  df.t <- df[i:(60+i), -1]
  df.t <- df.t[, colSums(is.na(df.t)) == 0]
  mkt.t <- mkt[i:(59+i), ]  
  
  # Estimate coefficients using the Single Index Model
  # Regress past 60 observations of each stock, with past 60 market observations
  reg <- apply(df.t[-nrow(df.t), ], 2, function(v) {
    res <- lm(v ~ mkt.t$MKTRF3)
    c(coefficients(res), var(residuals(res))) 
  })
  
  alpha <- 0
  beta <- reg[2, ]
  var.eps <- reg[3, ]
  
  mu.index <- mean(mkt.t$MKTRF3)
  var.index <- var(mkt.t$MKTRF3)
  mu <- beta * mu.index
  Sigma <- var.index * (as.matrix(beta) %*% beta)
  diag(Sigma) <- diag(Sigma) + var.eps
  
  # Solve quadratic programming problem to determine optimal portfolio. 
  d <- rep(0, length(mu))                  # No linear term, set d = 0
  A <- t(rbind(1,                          # Initialize constraint 1.
               diag(1, length(mu)),        # Initialize constraint 2.
               diag(-1, length(mu))))      # Initialize constraint 3.
  b0 <- c(1,                               # 1. Sum weights 100 %
          rep(0, length(mu)),              # 2. Lower bound (0.00) 
          rep(-0.1, length(mu)))           # 3. Upper bound (0.10)
  
  res <- solve.QP(Dmat = Sigma, 
                  dvec = d, 
                  Amat = A, 
                  bvec = b0, 
                  meq = 1)                 # Only one strict equality constraint
  omega <- res$solution
  
  w <- rbind(w, c(min(omega), max(omega))) # Weight series of this strategy
  
  returns <- df.t[nrow(df.t), ]            # Return vector for iteration i
  r <- c(r, sum(omega * returns))          # Return series of this strategy
}


## Task 3.2 --------------------------------------------------------------------

mu <- mean(r) * 12


## Task 3.3 --------------------------------------------------------------------

sr <- mu / (sd(r) * sqrt(12))


## Task 3.4 --------------------------------------------------------------------

w <- c(min(w[,1]), max(w[,2]))

# The maximum weight over the entire time period is 0.10. 
# The minimum weight is -4.382431e-16 (approximately zero). This is 
# because of rounding errors caused by the optimizer (solve.QP).

