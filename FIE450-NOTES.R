##----------------------------------------------------------------------------##
##                                                                            ##
##           FIE450 - PROGRAMMING WITH APPLICATIONS IN FINANCE                ##
##                                                                            ##
##----------------------------------------------------------------------------##

#	2 Stock index characteristics 
#	2.1 Reading a text file into R 
#	2.2 Accessing data content 
#	2.3 Data processing 
#	2.4 Expected return and volatility 
#	2.5 Exercises 

#	3 Volatility 
#	3.1 EWMA model 
#	3.1.1 Maximum likelihood approach 
#	3.1.2 Functions in R 
#	3.1.3 Optimization in R 
#	3.1.4 Searching for the optimal EWMA parameter 
#	3.2 GARCH model 
#	3.3 Volatility forecasts 
#	3.4 Option-Implied Volatility 
#	3.5 Exercises 

#	4 Monte-Carlo simulation 
#	4.1 Fundamental Theorem of Asset Pricing
#	4.2 Principals of Monte-Carlo simulation 
#	4.3 A model for the index price
#	4.4 Monte-Carlo Error 
#	4.5 Variance reduction 
#	4.6 Interpreting the results 
#	4.7 Exercises

#	5 Data processing
#	5.1 Obtaining the data 
#	5.2 Data cleansing 
#	5.3 Rolling observations forward in time 
#	5.4 One stock price observation per month 
#	5.5 Return computation 
#	5.6 Market capitalization and weights 
#	5.7 Market returns 
#	5.8 From long to wide-format 
#	5.9 Risk-free interest rate 
#	5.10 Exercises 

#	6 Mean-Variance Portfolios
#	6.1 Optimization problem 
#	6.2 Expected returns and covariances 
#	6.3 Solving for the optimal portfolio 
#	6.4 Single Index Model 
#	6.4.1 Estimating the Single Index model
#	6.4.2 Expected returns and risk in the single index model
#	6.4.3 Solving for the optimal portfolio 
#	6.5 Capital allocation line 
#	6.6 Exercises 



##----------------------------------------------------------------------------##
##                 2 - Stock index characteristics                            ##
##----------------------------------------------------------------------------##

rm(list=ls()) # clear environment
dev.off()     # clear plots
cat("\014")   # clear console
setwd("~/NHH/Master/2. Semester/FIE450 - Programming with Applications in Finance/Filer")


# Mini case 1: Stock index characteristics
# One of your clients would like to invest in the OBX Total Return Index (OBX). 
# He heard about diversification and knows that this is the only “free lunch” 
# in finance. But he wonders what return he can expect by investing in the 
# index. He is also worried about the risk he would be exposed to. 
# Thus, he asks for your advice. Can you help him?


## 2.1 Reading a text file -----------------------------------------------------

obx <- read.csv("OBX.csv", sep = ",")
head(obx)
dim(obx)

## 2.2 Accessing data content --------------------------------------------------

## dataframe: df[row, column]
obx[1, 1]
obx[1, 2]

# All these are equivalent to obx[1, 2]:
obx[1, "Last"]
obx$Last[1]
obx[["Last"]][1]

# only row 1
obx[1, ]

# only column 2
obx[ , 2]
obx[ , "Last"]

# create a vector - list of something
c(1, 2, 3) 
1:3
1:50

# select spesific rows/columns
obx[1:50, ]
obx$Last[c(1, 2, 3)]
obx$Last[1:50]

# Columns and rows
nrow(obx)
ncol(obx)

# Select the three last rows: 
obx[(nrow(obx) - 2):nrow(obx), ]
tail(obx, 3)


## 2.3 Data processing ---------------------------------------------------------

# Column headers
names(obx)
names(obx) <- c("Date", "Last", "High", "Low", "Turnover")
names(obx)
head(obx)

## Date columns
obx[1, 1]     # its not date format
obx[, 1] <- as.Date(obx[, 1], format = "%d.%m.%y")

# "%d.%m.%y == dd.mm.yy
# you maybe need to change this for other datasets (f.eks dd-mm-yyyy osv)
?strftime

# check column format/type: 
class(obx[, 1])


# Order example
v <- c(50, 30, 40, 10)
i <- order(v)
i
v[i]

# order the data set
obx <- obx[order(obx$Date), ]

# selecting columns
obx <- obx[, c("Date", "Last")]

# plot
plot(obx$Date, obx$Last, type = "l", xlab = "", ylab = "OBX")

## Compute returns - log return
obx$r <- c(NA, diff(log(obx$Last)))
head(obx)

# How the diff works in a vector
diff(c(1, 2, 4, 8, 100))


# Save file
save(obx, file = "OBX.RData")


rm(obx, i, v)
load("OBX.RData")
head(obx)


## 2.4 Expected return and volatility ------------------------------------------

# mean and standard deviation -> DAILY expected return and volatility

mean(obx$r)     # return NA since we have 1 or more NAs
sd(obx$r)       # 


# Ignore NAs
mean(obx$r, na.rm = TRUE)
sd(obx$r, na.rm = TRUE)


# trading days in a year = 250
# annual returns and volatility:

mu <- mean(obx$r, na.rm = TRUE) * 250
sigma <- sd(obx$r, na.rm = TRUE) * sqrt(250)

# Sharpe ratio: 
mu/sigma

# Standard error - SE = sd / sqrt(n)
r <- na.omit(obx$r)
n <- length(r)
SE <- sd(r)/sqrt(n) * 250
SE


# pnorm, qnorm, dnorm
# info: http://seankross.com/notes/dpqr/
# qnorm - (inverse) normal distribution

# if confidence probability, p = 0.99
p <- 0.99
z <- -qnorm((1 - p) / 2)

# confidence interval is:
c(mu - z * SE, mu + z * SE)

# alternative way to calculate confidence interval: 
qnorm(0.5 - p/2, mu, SE) # lower 
qnorm(0.5 + p/2, mu, SE) # upper


## 2.5 Exercises ---------------------------------------------------------------

rm(list=ls()) 
dev.off()     
cat("\014")   

## 1. Repeat the excercise in this section with return observations 
##    starting in 2010. What is the 99.9% confidence interval?

# Create dataset with data from 2010
obx2010 <- read.csv("OBX.csv", sep = ",")                     # read csv
obx2010 <- obx2010[ , 1:2]                                    # remove columns
names(obx2010) <- c("Date", "Last")                           # edit column names
obx2010[ , 1] <- as.Date(obx2010[ , 1], format = "%d.%m.%y")  # date format
obx2010 <- obx2010[order(obx2010$Date), ]                     # order by date
obx2010 <- with(obx2010, obx2010[(Date < "2010-01-01"), ])    # filter by 2010

tail(obx2010)


# Calculate log-returns, annual E[return] & vol 
obx2010$r <- c(NA, diff(log(obx2010$Last)))         # new colum r = log returns
head(obx2010)

mu <- mean(obx2010$r, na.rm = TRUE) * 250           # annual expected return
sigma <- sd(obx2010$r, na.rm = TRUE) * sqrt(250)    # annual volatility

# Calculate SE, and conficence interval
r <- na.omit(obx2010$r)                             # r = all non-NA returns
n <- length(r)                                      # n = # re
SE <- sd(r) * sqrt(250)/sqrt(n)
p <- 0.99
z <- -qnorm((1 - p) / 2)
ci <- c(mu - z * SE, mu + z * SE)
ci

##------------------------------------------------------------------------------
## 2. Download the complete history of monthly stock prices in USD for IBM 
##    (Ticker: IBM) from finance.yahoo.com. 9 Plot the complete time-series of 
##    stock prices. Compute simple and log returns. Based on both series 
##    compute an estimate for the expected return. Which one is larger? Why? 
##    Further compute the 1% quantiles.

rm(list=ls())
dev.off()
cat("\014")


# Process dataset
ibm <- read.csv("IBM.csv", sep = ",")
ibm <- ibm[ , c(1, 5)]
ibm[ , 1] <- as.Date(ibm[ , 1], format = "%Y-%m-%d")    # Y = 2018 | y = 18

# Plot time series
plot(ibm$Date, ibm$Close, type = "l", xlab = "", ylab = "IBM")

# Compute simple and log returns
ibm$log_r <- c(NA, diff(log(ibm$Close)))
ibm$simple_r <- c(NA, ibm$Close[-1] / ibm$Close[-length(ibm$Close)] - 1)
head(ibm)

# ibm$simple_r <- c(NA, diff(ibm$Close))  <- this is wrong. dont know why

# Expected return - annually 

mu_log <- mean(ibm$log_r, na.rm = TRUE) * 12
mu_simple <- mean(ibm$simple_r, na.rm = TRUE) * 12

# Expected simple return > Expected log return (0.078 > 0.049)
# Because, the smallest possible simple return  is -100%, 
# that is minus infinity for the log return.

# 1 % Quantiles

p <- 0.99
z <- -qnorm((1 - p) / 2)

# log return
r <- na.omit(ibm$log_r)
n <- length(r)
SE_log <- sd(r) * sqrt(12)/sqrt(n)

# simple return
r <- na.omit(ibm$simple_r)
n <- length(r)
SE_simple <- sd(r) * sqrt(12)/sqrt(n)

# Confidence intervals
interval_log <- c(mu_log - z * SE_log, mu_log + z * SE_log)
interval_simple <- c(mu_simple - z * SE_simple, mu_simple + z * SE_simple)


# checking results

mu_log
interval_log
# 0.0489 is within [0.0257, 0.0720] -> mean estimate seems reasonably correct

mu_simple
interval_simple
# 0.0777 is within [0.0545, 0.100] -> mean estimate seems reasonably correct


##------------------------------------------------------------------------------
## 3. Download the complete history of daily and monthly stock prices in USD 
##    for Microsoft (Ticker: MSFT) from finance.yahoo.com. For both sampling 
##    frequencies compute the annualized means, standard deviations, variances,
##    and 99% confidence intervals. Compare the results.

rm(list=ls())
dev.off()
cat("\014")

# DAILY dataset
msft_d <- read.csv("MSFT_daily.csv", sep = ",")
msft_d <- msft_d[ , c(1, 5)]
msft_d[ , 1] <- as.Date(msft_d[ , 1], format = "%Y-%m-%d")    
head(msft_d)

msft_d$r <- c(NA, diff(log(msft_d$Close)))          # Returns - log
mu_d <- mean(msft_d$r, na.rm = TRUE) * 250          # Mean - expected return
sigma_d <- sd(msft_d$r, na.rm = TRUE) * sqrt(250)   # Standard deviation - vol
var_d <- sigma_d^2                                  # Variance

# Standard Error, SE = sd / sqrt(n)
r <- na.omit(msft_d$r)
n <- length(r)
SE_d <- sd(r) * sqrt(250)/sqrt(n)



# MONTHLY dataset
msft_m <- read.csv("MSFT_monthly.csv", sep = ",")
msft_m <- msft_m[ , c(1, 5)]
msft_m[ , 1] <- as.Date(msft_m[ , 1], format = "%Y-%m-%d")
head(msft_m)

msft_m$r <- c(NA, diff(log(msft_m$Close)))          # Returns - log
mu_m <- mean(msft_m$r, na.rm = TRUE) * 12           # Mean - expected return
sigma_m <- sd(msft_m$r, na.rm = TRUE) * sqrt(12)    # Standard deviation - vol
var_m <- sigma_m^2                                  # Variance

# Standard Error, SE = sd / sqrt(n)
r <- na.omit(msft_m$r)
n <- length(r)
SE_m <- sd(r) * sqrt(12)/sqrt(n)


# 99 % confidence interval
p <- 0.99
z <- -qnorm((1 - p) / 2)

# Confidence intervals is:
ci_d <- c(mu_d - z * SE_d, mu_d + z * SE_d)
ci_m <- c(mu_m - z * SE_m, mu_m + z * SE_m)


# Daily
mu_d
ci_d
# # 0.22 is within [0.21, 0.23] -> mean estimate seems correct

# Monthly
mu_m
ci_m
# # 0.22 is within [0.18, 0.26] -> mean estimate seems correct

# Testing results in a table
results <- matrix(c(mu_d, mu_m, sigma_d, sigma_m, var_d, var_m, 
                    SE_d, SE_m), ncol = 2, byrow = TRUE)
colnames(results) <- c("Daily", "Monthly")
rownames(results) <- c("Mean", "SD", "Var", "SE")
results <- as.table(results)
results


##----------------------------------------------------------------------------##
##                          3 - Volatility                                    ##
##----------------------------------------------------------------------------##

rm(list=ls()) # clear environment
dev.off()     # clear plots
cat("\014")   # clear console
setwd("~/NHH/Master/2. Semester/FIE450 - Programming with Applications in Finance/Filer")


# Volatility: simple,EWMA,GARCH: https://www.investopedia.com/articles/07/ewma.asp

# Dataset from chapter 2 -------------------------------------------------------
load("OBX.RData")
mu <- mean(obx$r, na.rm = TRUE) * 250
sigma <- sd(obx$r, na.rm = TRUE) * sqrt(250)
sharpe <- mu/sigma
r <- na.omit(obx$r)
n <- length(r)
SE <- sd(r) * sqrt(250)/sqrt(n)

# Mini case 2: Volatility
# Your client is unsatisfied with your work. He let you know that he has an MBA 
# in finance and knows how to compute means and standard deviations. 
# That is not what he is paying you for. In the conversation with him he 
# mentions terms such as “volatility regimes”, “volatility cluster” and the like. 
# Feeling embarrassed you go back to you desk and start thinking about what your 
# client really meant. How can you improve your work so that you don’t loose 
# him as a client?

# Plot returns: 
plot(obx$Date, obx$r, type = "l", xlab = "", ylab = "returns")


## 3.1 - EWMA model (Exponentially Weighted Moving Average) --------------------
# https://www.youtube.com/watch?v=ffDLG7Vt6JE

lambda <- 0.94
lambda.vec <- lambda^(0:(n-1))
r.vec <- r[length(r):1]   # reverse vector, so that last obs get more weight
sigma2 <- (1 - lambda) * sum(lambda.vec * r.vec^2)   # equation 12
sigma <- sqrt(sigma2)

# The predicted annualized vol for day t is: 
sigma * sqrt(250)

# The original vol was way higher: 
sd(r.vec) * sqrt(250)

# ALTERNATIVE way to calculate sigma (with function "rev"): 
lambda <- 0.94
sigma2 <- sum((1 - lambda) * lambda^(0:(length(r) - 1)) * rev(r)^2)
sigma <- sqrt(sigma2)
sigma * sqrt(250)
# rev reverse the whole vector (1,2,3,4) -> (4,3,2,1)


# FOR loop ---------------------------------------------------------------------

v <- 1:5
for (i in v) {
  cat(i, "\n")
}
##------------------------------------------------------------------------------

# Computing the historical EWMA volatilities using for loop:

sigma2 <- c()
for (i in 1:length(r)) {
  sigma2 <- c(sigma2, sum((1 - lambda) * lambda^(0:(i - 1)) * rev(r[1:i])^2))
}
sigma <- sqrt(sigma2)

plot(sigma * sqrt(250), xlab = "", ylab = "EWMA", type = "l")



## 3.1.1. Maximum Likelihood approach ------------------------------------------
## 3.1.2 Functions in R --------------------------------------------------------

# Define a function that divide two numbers: 
div <- function(a, b) {
  result <- a/b
  return(result)
}

div(4,2)
div(2,4)
div(b = 2, a = 4)

div2 <- function(a, b = 2) {
  a/b
}

div2(4)
div2(4, 3)


#  Allow one function to pass on argument settings to another nested function
plot.normal <- function(x1, x2, mu, sigma) {
  x <- seq(x1, x2, by = 0.01)
  y <- dnorm(x, mu, sigma)
  plot(x, y, type = "l", xlab = "", ylab = "")
}

plot.normal(-3, 3, 0, 1.5)


## Can also instead type "..." as an argument: 

plot.normal <- function(x1, x2, ...) {
  x <- seq(x1, x2, by = 0.01)
  y <- dnorm(x, ...)
  plot(x, y, type = "l", xlab = "", ylab = "")
}

plot.normal(-3, 3, 1, 1.5)

## 3.1.3 Optimization in R -----------------------------------------------------

x <- seq(-1, 5, by = 0.1)
y <- -(x -2)^2
plot(x, y, type = "l")

# We see that x = 2 maximizes this function. 
# But we want the answer numerically:

f <- function(x) {
  y <- (x - 2)^2 
  return(y)
}

# nlm is a optimization function. 
# It minimizes only, so we remove the negative sign from f above ("y <- ...")
res <- nlm(f, 0)
res
res$minimum       # Value of the estimated minimum of f.
res$estimate      # Point at which the minimum value of f is obtained.

## 3.1.4 Searching for the optimal EWMA parameter ------------------------------

# See page 33 in lecture notes:

## Computes the EWMA variance
##
## r: Vector of return observations
## lambda: EWMA parameter
## Returns the EWMA variance
ewma.var <- function(r, lambda) {
  sigma2 <- sum((1 - lambda)*lambda^(0:(length(r) - 1))*rev(r)^2)
  return(sigma2)
}

## Computes historical EWMA variances
##
## r: Vector of return observations
## lambda: EWMA parameter
## Returns a variance vector
hist.ewma.var <- function(r, lambda) {
  sigma2 <- c()
  for (i in 1:length(r)) {
    sigma2 <- c(sigma2, ewma.var(r[1:i], lambda))
  }
  return(sigma2)
}

## Log-likelihood function for EWMA volatility model.
##
## lambda: EWMA parameter
## r: Return vector
## Returns the negative of the log likelihood.
ewma.ll.fun <- function(lambda, r) {
  sigma2 <- hist.ewma.var(r, lambda)
  sigma2 <- sigma2[-length(sigma2)]
  r <- r[-1]
  log.ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-log.ll)
}

## LOG LIKELIHOOD: 
#  We want to maximize this function, i.e. we want to search for the parameters 
# that increase the likelihood of observing exactly the given returns.
#  The only unknown is the variance.

res <- nlminb(0.5, ewma.ll.fun, lower=1e-6, upper=1 - 1e-6, r = r)
res

res$par            # The optimal λ that maximizes the log-likelihood
res$objective      # Value of the objective function.
res$convergence    # Indicates the optimization routine has converged
res$iterations     # Number of iterations needed until converged

 

# What is the EWMA volatility forecast given λ = 0.92?

sqrt(ewma.var(r, lambda = 0.94) * 250)
sqrt(ewma.var(r, lambda = res$par) * 250)


## 3.2 GARCH model -------------------------------------------------------------
# https://www.youtube.com/watch?v=inoBpq1UEn4

## Computes the GARCH variance
##
## r: Vector of return observations
## omega: GARCH parameter
## alpha: GARCH parameter
## beta: GARCH parameter
## Returns a vector of GARCH variances
garch.var <- function(r, omega, alpha, beta) {
  sigma2 <- r[1]^2 
  for (i in 2:length(r)) {
    sigma2 <- c(sigma2, omega + alpha*r[i]^2 + beta*sigma2[i - 1])
  }
  return(sigma2)
}



## Log-likelihood function for GARCH volatility model.
##
## par: Vector of GARCH parameters
## r: Vector of return observations
## Returns the negative of the log likelihood.
garch.ll.fun <- function(par, r) {
  omega <- par[1]
  alpha <- par[2]
  beta <- par[3]
  sigma2 <- garch.var(r, omega, alpha, beta)
  r <- r[-1]
  sigma2 <- sigma2[-length(sigma2)]
  ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-ll)
}


res <- nlminb(c(0.001, 0.3, 0.3), garch.ll.fun, lower = 1e-06,
              upper = 1 - 1e-06, r = r)
res


# The long-term variance VL =  ω/γ = omega / gamma is: 

omega <- res$par[1]
alpha <- res$par[2]
beta <- res$par[3]
gamma <- 1 - alpha - beta
VL <- omega/gamma
VL
sqrt(VL * 250)

# This is nearly the same as the unconditional standard deviation 
# that we have estimated earlier.


## 3.3 Volatility forecasts ----------------------------------------------------

# One year forecasts

# EWMA model
ewma.sigma2 <- ewma.var(r, lambda = 0.94)

sqrt(ewma.sigma2 * 250)     # Annualized volatility


# GARCH model

# We need to compute predictions for all 250 days

m <- 249
garch.sigma2 <- garch.var(r, omega, alpha, beta)
garch.sigma2.t <- garch.sigma2[length(garch.sigma2)]
garch.sigma2.vec <- VL + (alpha + beta)^(0:m) * (garch.sigma2.t - VL)
garch.sigma.vec <- sqrt(garch.sigma2.vec)

plot(garch.sigma.vec * sqrt(250), xlab = "Days", ylab = "GARCH Volatility",
     type = "l")


sqrt(sum(garch.sigma2.vec))

# How do you interpret both results?
# Why is the GARCH volatility higher than the EWMA volatility?

# 3.4 Option-Implied Volatility ------------------------------------------------

# Ticker    Type  Strike  Maturity    Buy   Sell
# OBX8B770  Call  770     2018-02-16  7.50  8.75

C.market <- mean(c(7.5, 8.75))

# The parameters:
S0 <- 766.12   # OBX index
K  <- 770      # Strike price
T  <- as.numeric(as.Date("2018-02-16") - as.Date("2018-01-24")) / 365  # Maturity
rf <- 0.008    # Risk-free interest rate -> 80 bp



# Function that computes the price of a European call option

call <- function(S0, sigma, K, rf, T) {
  d1 <- (log(S0/K) + (rf + sigma^2/2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  C <- S0 * pnorm(d1) - exp(-rf * T) * K * pnorm(d2)
  return(C)
}


# Objective function (minimize function page 42)

obj.fun <- function(sigma, C.market, S0, K, rf, T) {
  C.model <- call(S0, sigma, K, rf, T)
  eps <- (C.model - C.market)^2
  return(eps)
}

# Optimizing   (p = starting value)

res <- nlm(obj.fun, p = 0.2, C.market = C.market, S0 = S0, K = K, 
           rf = rf, T = T)

res$estimate

# Sample standard deviation = 0.236
# EWMA vol = 0.098
# GARCH vol = 0.207
# Option implied vol = 0.127

# For short-term investments (< 3 months) -> use option-implied og EWMA
# For long-term investments (> 3 months) -> use GARCH

## Sample standard deviation - rather poor estimate --> prediction one year out
## EWMA, GARCH -> predictions for one year in the future
## Option implied --> Prediction one month out (because of the maturity of 
##                                              the option we chose)
## EWMA mener de siste observasjonene er viktigst, så kan brukes på < 3 mnd

## Option implied vol = tell us what investors thinks about the market now!
## Whenever possible, use implied volatilities
## But dont use options for more than a year into the future. 
## more than a year into the future -> use EWMA/GARCH/Simple.


## 3.5 Exercises ---------------------------------------------------------------

rm(list=ls()) # clear environment
dev.off()     # clear plots
cat("\014")   # clear console
setwd("~/NHH/Master/2. Semester/FIE450 - Programming with Applications in Finance/Filer")


##------------------------------------------------------------------------------
# 1. Download the complete history of monthly stock prices in USD for Google 
#   (Ticker: GOOG) from finance.yahoo.com. Estimate an EWMA model to forecast 
#    the 1-month volatility.

# Read and process dataset
goog <- read.csv("GOOG.csv", header = TRUE, sep = ",", dec = ".")
goog <- goog[ , c("Date", "Close")]                 
goog[ , 1] <- as.Date(goog[ , 1], format = "%Y-%m-%d")    
goog$r <- c(NA, diff(log(goog$Close)))

# Stats about GOOG
mu <- mean(goog$r, na.rm = TRUE) * 12
sigma <- sd(goog$r, na.rm = TRUE) * sqrt(12)
r <- na.omit(goog$r)
n <- length(r)
se <- sd(r) * sqrt(12)/sqrt(n)

# EWMA modelling

## Using recommended lambda 0.94
lambda <- 0.94
sigma2 <- sum((1 - lambda) * lambda^(0:(length(r) - 1)) * rev(r)^2)
sigma <- sqrt(sigma2)

sigma * sqrt(12)       # The predicted annual vol for today + 1-month
sd(r.vec) * sqrt(12)   # The orignial vol (sd)


## Estimating own lambda - log-likelihood

## Computes the EWMA variance
##
## r: Vector of return observations
## lambda: EWMA parameter
## Returns the EWMA variance
ewma.var <- function(r, lambda) {
  sigma2 <- sum((1 - lambda)*lambda^(0:(length(r) - 1))*rev(r)^2)
  return(sigma2)
}

## Computes historical EWMA variances
##
## r: Vector of return observations
## lambda: EWMA parameter
## Returns a variance vector
hist.ewma.var <- function(r, lambda) {
  sigma2 <- c()
  for (i in 1:length(r)) {
    sigma2 <- c(sigma2, ewma.var(r[1:i], lambda))
  }
  return(sigma2)
}

## Log-likelihood function for EWMA volatility model.
##
## lambda: EWMA parameter
## r: Return vector
## Returns the negative of the log likelihood.
ewma.ll.fun <- function(lambda, r) {
  sigma2 <- hist.ewma.var(r, lambda)
  sigma2 <- sigma2[-length(sigma2)]
  r <- r[-1]
  log.ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-log.ll)
}

res <- nlminb(0.5, ewma.ll.fun, lower=1e-6, upper=1 - 1e-6, r = r)

res$par            # The optimal λ that maximizes the log-likelihood

# EWMA volatility forecasts:
sqrt(ewma.var(r, lambda = 0.94) * 12)
sqrt(ewma.var(r, lambda = res$par) * 12)

#### EWMA FORECAST ONE YEAR (??) PAGE 40 IN PDF:
ewma.sigma2 <- ewma.var(r, lambda = 0.94)
sqrt(ewma.sigma2 * 12)


##------------------------------------------------------------------------------
# 2. Download the complete history of monthly stock prices in USD for 
#    Google (Ticker: GOOG) from finance.yahoo.com. 
#    Estimate a GARCH(1,1) model to forecast the 1-month volatility.

## Computes the GARCH variance
##
## r: Vector of return observations
## omega: GARCH parameter
## alpha: GARCH parameter
## beta: GARCH parameter
## Returns a vector of GARCH variances
garch.var <- function(r, omega, alpha, beta) {
  sigma2 <- r[1]^2 
  for (i in 2:length(r)) {
    sigma2 <- c(sigma2, omega + alpha*r[i]^2 + beta*sigma2[i - 1])
  }
  return(sigma2)
}

## Log-likelihood function for GARCH volatility model.
##
## par: Vector of GARCH parameters
## r: Vector of return observations
## Returns the negative of the log likelihood.
garch.ll.fun <- function(par, r) {
  omega <- par[1]
  alpha <- par[2]
  beta <- par[3]
  sigma2 <- garch.var(r, omega, alpha, beta)
  r <- r[-1]
  sigma2 <- sigma2[-length(sigma2)]
  ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-ll)
}

res <- nlminb(c(0.001, 0.3, 0.3), garch.ll.fun, lower = 1e-06,
              upper = 1 - 1e-06, r = r)

omega <- res$par[1]
alpha <- res$par[2]
beta <- res$par[3]
gamma <- 1 - alpha - beta
VL <- omega/gamma
VL
sqrt(VL * 12)


## GARCH FORECAST FOR NEXT 12 MONTHS??? PAGE 40 IN PDF:
m <- 11
garch.sigma2 <- garch.var(r, omega, alpha, beta)
garch.sigma2.t <- garch.sigma2[length(garch.sigma2)]
garch.sigma2.vec <- VL + (alpha + beta)^(0:m) * (garch.sigma2.t - VL)
garch.sigma.vec <- sqrt(garch.sigma2.vec)




##------------------------------------------------------------------------------
# 3. Go to finance.yahoo.com and search for call options on Google. 
#    Choose the shortest maturity and compute the implied volatilities of a 
#    range of options with varying strike prices. Assume a risk-free 
#    interest rate of 50 bp. Can you replicate the implied volatilities
#    that are given on the same page?


## TABELLENE ER UFULLSTENDIGE, FÅR IKKE OPP BID/ASK





##----------------------------------------------------------------------------##
##                 4 - Monte-Carlo simulation                                 ##
##----------------------------------------------------------------------------##

rm(list=ls()) # clear environment
dev.off()     # clear plots
cat("\014")   # clear console
setwd("~/NHH/Master/2. Semester/FIE450 - Programming with Applications in Finance/Filer")

# Mini case 3 Exotic options ---------------------------------------------------
# You are an expert witness in a court case where a bank was accused by an 
# investor of having incorrectly quoted the price of a security. The investor 
# sued the bank claiming that the selling price (i.e. bid price) of this product
# was too low and thus unfair. The security is a knock-out warrant call or 
# down-and-out call option, respectively, on the EURO STOXX 50. Certificates 
# have became very popular among investors in the recent past because it allows 
# the trading of more sophisticated products compared with regular options. 
# Barrier options are also less expensive than standard options. You have been 
#asked to determine the market price of this security and evaluate whether the 
#selling price was fair. You were provided with the following information:

# WKN              PR1LWN
# Name             Knock-out warrant call
# Type             Down-and-out call option
# Underlying       EUROSTOXX 50
# Strike price     3220
# Barrier level    3220
# Selling date     January 27, 2017
# Selling price    0.84
# Maturity         April 27, 2017
# Ratio            1:100


# 4.1 Fundamental Theorem of Asset Pricing -------------------------------------


# 4.2 Principals of Monte-Carlo simulation -------------------------------------
# https://www.youtube.com/watch?v=7ESK5SaP-bc&t=398s

n <- 100000
roll <- sample(1:6, n, replace = TRUE)
length(roll)
head(roll)
table(roll)
mean(roll)

# 4.3 A model for the index price ----------------------------------------------


# Equation 45, page 47: 
# Our process grows with the risk-free rate. 
# The model is valid in a #risk-neutral" world - RISK NEUTRAL VALUATION
# In a risk-neutral world, investors do not increase the rate of return beyond 
# the risk-free rate to be compensated for risk. 
# Of course, we are not risk-neutral, we are risk-averse. We simply pretend to 
# be so because it makes valuation much easier.

# In a risk-neutral world: 
# The expected return on any investment is the risk-free rate.
# The discountrate used for the expected payoff is the risk-free rate


## ** MONTE-CARLO SIMULATION ** ##

# we are going to simulate the underlying - the euro stoxx index EUROSTOXX50
# underlysing should follow a mathematical motion 
# use monte carlo to compute the expected value of the expected risky payoff

sigma <- 0.16
S0    <- 3303
T     <- 0.25
b     <- K <- 3220
rf    <- 0.002
f     <- 0.01
n     <- 5000

# There are 250 trading days in a year. 
# We want to simulate the index twice a day. Therefore: 

dt <- 1/250 * 0.5

# We only check twice a day wheter the stock index has fallen below the barrier.



## Simulates *one* path of a stock index.
##
## S0: Today's index price
## rf: risk-free interest rate
## sigma: volatility
## dt: time step
## T: time horizon
## Returns the path of index prices as a vector.
simulate.path <- function(S0, rf, sigma, dt, T) {
  S <- S0
  t <- seq(dt, T, by=dt)
  for (i in 1:length(t)) {
    Si <- S[length(S)]*exp((rf - 0.5*sigma^2)*dt +
                             sigma*sqrt(dt)*rnorm(1))
    S <- c(S, Si)
  }
  return(S)
}

# Lets look at ONE scenario:
S <- simulate.path(S0, rf, sigma, dt, T)
plot(S, type = "l")
abline(h = b, col = "red")


# We need many more scenarios to determine the expected value of the option


## Simulates a set of price paths.
##
## S0: today's index price
## rf: risk-free interest rate
## sigma: volatility
## dt: time step
## T: time horizon
## n: number of scenarios
## Returns the simulated paths as a matrix
simulate.paths <- function(S0, rf, sigma, dt, T, n) {
  S <- c()
  for (i in 1:n) {
    S <- cbind(S, simulate.path(S0, rf, sigma, dt, T))
  }
  return(S)
}



# This function uses the function simulate.path which returns a vector of stock index
# prices. We then create a matrix object. A matrix is very similar to a data frame. The major
# difference is that all its elements must be of the same type (for example, numerics). We use
# the command cbind to append columns to another column or, more generally, a matrix.

v1 <- c(1, 2, 3)
v2 <- c(4, 5, 6)
cbind(v1, v2)

m <- c()
v1 <- c(1, 2, 3)
cbind(m, v1)

# FYI: the function rbind is to append rows to each other. 

S <- simulate.paths(S0, rf, sigma, dt, T, n = 5000)


# R is slow because of the for loop in our function. Lets improve it: 

# The way you do this is to draw the random variables all at once and then 
# process them more efficiently.

t <- seq(dt, T, by = dt)
m <- length(t)
e <- exp((rf - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(m))


# cumprod = compute a cumulative product in R
# cumsum = compute a cumulative sum in R

S <- c(S0, S0 * cumprod(e))


# Trying another way, doing all at once
t <- seq(dt, T, by = dt)
m <- length(t)
e <- matrix(exp((rf - 0.5 * sigma^2) * dt + sigma * sqrt(dt) *
                  rnorm(n * m)), m, n)



# How to use the function matrix: 

matrix(1:6, 2, 3)
matrix(1:6, 2, 3, byrow = TRUE)
matrix(c(1, 2), 2, 3)



# Back to our problem:

S <- S0 * apply(e, 2, cumprod)

# The first argument of the apply function is typically a matrix. 
# The second argument which is set to 2 means to apply the function given as 
# the third argument *columnwise*. 
# If we want to apply something *rowwise* we set the second argument to 1.

m <- matrix(1:6, 2, 3)
m
apply(m, 2, sum)
apply(m, 1, sum)


# Lets put everything together: 

simulate.paths.fast <- function(S0, rf, sigma, dt, T, n) {
  t <- seq(dt, T, by = dt)
  m <- length(t)
  e <- matrix(exp((rf - 0.5 * sigma^2) * dt + sigma * sqrt(dt) *
                    rnorm(n * m)), m, n)
  S <- apply(e, 2, cumprod)
  S <- S * S0
  S <- rbind(S0, S)
  return(S)
}


set.seed(1)
system.time(S1 <- simulate.paths(S0, rf, sigma, dt, T, n = 5000))

set.seed(1)
system.time(S2 <- simulate.paths.fast(S0, rf, sigma, dt, T, n = 5000))

##  The CPU time that has elapsed for calling simulate.paths is more than
##  100 quicker by calling simulate.paths.fast.


# Next we want to verify whether the set of paths of both functions indeed yield
# the same option price

## Computes the option payoff for each scenario.
##
## S: Matrix of stock index prices with (m + 1) rows and n columns.
## K: Strike price
## b: Boundary level
## rf: risk-free interest rate
## f: Scaling factor for the payoff
## Returns a vector of discounted option payoffs.
payoffs <- function(S, K, b, rf, f) {
  I <- apply(S > b, 2, all)
  X <- I * pmax((S[nrow(S), ] - K), 0) * f
  X0 <- exp(-rf * T) * X
  return(X0)
}




# Explanation: 
x <- c(100, 110, 90, 120)
y <- 100
x > y

all(x > y)


# TRUE = 1, FALSE = 0
1 != 1
1 == 1
TRUE == 0
TRUE == 1
FALSE == 0
FALSE == 1


# Using our two sets of scenarios we compute the value of the option. 
# We approximate for the expectation using the mean of the discounted payoffs.

X0 <- payoffs(S1, K, b, rf, f)
mean(X0)

X0 <- payoffs(S2, K, b, rf, f)
mean(X0)

# As you see we get exactly the same results no matter whether we use the 
# inefficient or the efficient function. This confirms that both functions do 
# exactly the same.



# ANYWAYS: is 0.9633 a fair price?
# Recall the investor sold the security for 0.84

## 4.4 Monte-Carlo Error -------------------------------------------------------

# So far we have an estimate of the option price.
# We also need to tell how reliable it is


for (i in 1:10) {
  S <- simulate.paths.fast(S0, rf, sigma, dt, T, n = 5000)
  X0 <- payoffs(S, K, b, rf, f)
  C <- mean(X0)
  cat(i, ":", C, "\n")
}


# Monte-carlo error (standard error - SE)
SE <- sd(X0)/sqrt(length(X0))
SE

# Confidence interval, alpha = 0.99
alpha <- 0.99
z <- -qnorm((1 - alpha)/2)
c(mean(X0) - z * SE, mean(X0) + z * SE)


## 4.5 Variance reduction ------------------------------------------------------

# Method: antithetic sampling / antithetic variates

## Simulates 2*n antithetic pairs of price paths.
##
## S0: today's price
## rf: risk-free interest rate
## sigma: volatility
## dt: time step
## T: time horizon
## n: number of paired paths
## Returns the simulated paths as a matrix
simulate.paths.fast.as <- function(S0, rf, sigma, dt, T, n) {
  t <- seq(dt, T, by = dt)
  m <- length(t)
  z <- rnorm(n*m)
  z.as <- -z
  Z <- matrix(c(z, z.as), m, n*2)
  e <- exp((rf - 0.5 * sigma^2) * dt + sigma * sqrt(dt)*Z)
  S <- apply(e, 2, cumprod)
  S <- S * S0
  S <- rbind(S0, S)
  return(S)
}

# We first draw n*m standard normal random variables. 
# We then compute their antithetic counterparts. Based on the total set of 
# random variables we compute the price paths. The following code simulates 
# paths based on antithetic sampling and plots one specific antithetic pair. 

n <- 2500
S.as <- simulate.paths.fast.as(S0, rf, sigma, dt, T, n = n)
plot(S.as[, 1], type = "l", col = "blue", xlab = "", ylab = "Antithetic pair",
     ylim = range(S.as[, c(1, n + 1)]))
lines(S.as[, n + 1], type = "l", col = "red")


# We now compute the discounted option payoffs and the estimated option value. 
# Finally we determine the SE and the confidence interval.

X0.as <- payoffs(S.as, K, b, rf, f)
mean(X0.as)


X.pairs <- (X0.as[1:n] + X0.as[(n + 1):(2 * n)])/2
SE <- sd(X.pairs)/sqrt(n)
SE


alpha <- 0.99
z <- -qnorm((1 - alpha)/2)
c(mean(X0.as) - z * SE, mean(X0.as) + z * SE)


# The question is under what condition is an antithetic variate estimator better
# than an ordinary Monte Carlo estimator?

cor(X0.as[1:n], X0.as[(n + 1):(2 * n)])



## 4.6 Interpreting the results ------------------------------------------------

# The selling price for the investor was outside the confidence interval.
# We would conclude that the selling price was significantly lower than the 
# fair market price.

# However, keep in mind that the bank always buys (investor sells) a security at
# a price that is lower and sells at a price higher than the fair market price. 
# This is the bid-ask spread from which the bank profits.

# Assume for the moment that the fair price (optimistically) was at the lower 
# bound of the confidence interval, that is at 0.92, then the spread the bank 
# earns is 

abs(0.84/0.92 - 1) * 2

# abs( selling price / lower bound CI - 1) * 2
#0.1739 is a reasonable spread for a security like this.

# Now assume (pessimistically) that the fair value was at the upper bound of
# the confidence interval, that is at 1.03, then the spread would be

abs(0.84/1.03 - 1) * 2

#This would be probably too much. However, spreads beyond 10% are not unlikely. 


#To get a more realistic estimate we need to rely on a finer time grid. 
# Let’s monitor the underlying ten times a day, for example.


S <- simulate.paths.fast.as(S0, rf, sigma, dt = 1/250 * 0.1, T, n = 2500)
X0 <- payoffs(S, K, b, rf, f)
X.pairs <- (X0[1:n] + X0[(n + 1):(2 * n)])/2
SE <- sd(X.pairs)/sqrt(n)
c(mean(X0) - z * SE, mean(X0) + z * SE)

#As you see the selling price is now very close to the lower bound of the 
# confidence interval and thus the fair market price. The reason is that a finer
# time grid makes it more likely that the underlying falls below the boundary 
# which in turn makes the call option less valuable. We close this case by 
# concluding that the price was fair.



## 4.7 Exercises ---------------------------------------------------------------

##------------------------------------------------------------------------------
# 1. Use Monte Carlo simulation with 10000 scenarios to price an at-the-money 
#    European call option with expiration in 2.5 years. The underlying trades at
#    85 and has a volatility of 35% p.a. The risk-free interest rate is 80 bp. 
#    Verify the obtained estimate using the Black-Scholes option pricing formula.



##------------------------------------------------------------------------------
# 2. Use Monte Carlo simulation with 5000 pairs of antithetic variates to price 
#    an at-the-money European put option with maturity in 0.5 years. 
#    The underlying trades at 50 and has a volatility of 15% p.a. The risk-free 
#    interest rate is 50 bp. Verify the obtained estimate using the Black-Scholes 
#    option pricing formula for put options.



##----------------------------------------------------------------------------##
##                 5 - Data processing                                        ##
##----------------------------------------------------------------------------##

rm(list=ls()) # clear environment
dev.off()     # clear plots
cat("\014")   # clear console
setwd("~/NHH/Master/2. Semester/FIE450 - Programming with Applications in Finance/Filer")

## Mini case 4 Data processing -------------------------------------------------
# You are an intern in an investment bank. Your boss asks you to download stock 
# price data of all stocks traded on the Oslo Stock Exchange (OSE). Your job is 
# to clean and process the data so as to make it ready to be used for a trading 
# strategy.


## 5.1 Obtaining the data ------------------------------------------------------

stocks1 <- read.csv("Stocks-1980-2000.csv", header = FALSE, sep = ";", dec = ",")
stocks2 <- read.csv("Stocks-2000-2010.csv", header = FALSE, sep = ";", dec = ",")
stocks3 <- read.csv("Stocks-2010-2014.csv", header = FALSE, sep = ";", dec = ",")
stocks4 <- read.csv("Stocks-2014-2018.csv", header = FALSE, sep = ";", dec = ",")


dim(stocks1)
dim(stocks2)
dim(stocks3)
dim(stocks4)

# We want to bind the data frames by rows (have to have the same number of rows)
stocks <- rbind(stocks1, stocks2, stocks3, stocks4)

dim(stocks)

rm(stocks1, stocks2, stocks3, stocks4)

## 5.2 Data cleansing ----------------------------------------------------------

head(stocks)
tail(stocks)
summary(stocks)


stocks$V8 <- NULL      # Drop the entire column
colnames(stocks) <- c("Date", "SecurityId", "SecurityName", "SecurityType",
                      "Last", "AdjLast", "SharesIssued")

head(stocks)


## FACTORS ---------------------------------------------------------------------

stocks$SecurityName[1]

# The column "SecutiryName" is a factor (before, not anymore)

ratings <- c("BBB", "A", "BB", "BB", "BBB", "AAA", "AA", "AA")
ratings

ratings <- factor(ratings)
ratings

ratings <- factor(c("BBB", "A", "BB", "BB", "BBB", "AAA", "AA", "AA"))
ratings

ratings <- factor(ratings, levels = c("AAA", "AA", "A", "BBB",
                                      "BB", "B", "CCC", "CC", "C"))
ratings


as.numeric(ratings) # a factor can always be converted to a numeric number. 
# in regressions, factors is interpreted as dummy variable.
levels(ratings)


factor(ratings, labels = c("a", "b", "c", "d", "e"))

summary(ratings)

##------------------------------------------------------------------------------

# Filterings -------------------------------------------------------------------

# Types of shares:
summary(stocks$SecurityType)
stocks$SecurityType <- factor(stocks$SecurityType)
summary(stocks$SecurityType) # We want the types ordinary shares, easily traded

stocks <- stocks[stocks$SecurityType == "Ordinary Shares", ]
stocks$SecurityType <- NULL


# Remove NA's
summary(stocks$AdjLast) # We have many NA's
summary(stocks$Last)

stocks <- stocks[!is.na(stocks$AdjLast), ]
stocks <- stocks[!is.na(stocks$Last), ]

# Remove pennystocks (higly risky stocks, high transaction costs, osv)
stocks <- stocks[stocks$Last > 10, ]
stocks <- stocks[stocks$SharesIssued > 0, ]
nrow(stocks)

stocks$Date <- as.Date(stocks$Date, format = "%m/%d/%Y")

head(stocks)

# na.omit() checks every row, if a column has one NA, it deletes the entire row.
# na.omit(stocks)


## 5.3 Rolling observations forward in time ------------------------------------

# We only want prices from late in each month.
# Create a vector with only last months:
months <- seq(as.Date("1980-01-01"), as.Date("2018-02-01"), by = "1 month")
months <- months - 1


# dates
stocks$Date.aux <- cut(stocks$Date, months, right = TRUE)
head(stocks)


## CUT -----------------------------

b <- c(0, 5, 10, 15)             # intervals
v <- c(1, 7, 11, 0, 5, 10, 15)   # values
c <- cut(v, b, right = TRUE)     # right closed = true
c
as.numeric(c)


## ---------------------------------

head(stocks$Date)
head(stocks$Date.aux) # want it to 1980-01-30, not 1979-12-31

stocks$Date[1]
stocks$Date.aux[1]

i <- as.numeric(stocks$Date.aux)

head(i)
head(months[i+1])


stocks$Date.aux <- months[i + 1]   # use this line

stocks$Date[1:5]
stocks$Date.aux[1:5]


## 5.4 One stock price observation per month -----------------------------------

# We want only one observation of each stock price per month
num <- aggregate(stocks$SecurityId, list(stocks$Date.aux, stocks$SecurityId),
                 length)


## LIST & AGGREGATE ------------------------------------------------------------

list(c(1, 2, 3), c(100, 200), 7)

list(c("A", "B"), c(4, 5, 6))

list(letters = c("A", "B"), numbers = c(3, 5, 6))


df <- data.frame(id = c("a", "a", "a", "b", "b"), val = 1:5)
df

aggregate(df$val, list(df$id), sum)
aggregate(df$val, list(df$id), length)
aggregate(df$id, list(df$id), length)

## -----------------------------------------------------------------------------


# We want only one observation of each stock price per month
num <- aggregate(stocks$SecurityId, list(stocks$Date.aux, stocks$SecurityId), length)

# Is there really just one observation per firm and month?
head(num[num$x > 1, ])

# We have some cases where observations are more than 1. 
# An example: 
stocks[stocks$Date.aux == "1992-06-30" & stocks$SecurityId == 6002, ]


# Solution: we will take the most recent observation in a given month:
head(stocks)

stocks <- stocks[order(stocks$SecurityId, stocks$Date), ]
head(stocks)

stocks$row <- 1:nrow(stocks)
head(stocks)

rows <- aggregate(stocks$row, list(stocks$Date.aux, stocks$SecurityId), max)
head(rows)
stocks[stocks$Date.aux == "1992-06-30" & stocks$SecurityId == 6002, ]


stocks <- stocks[rows$x, ]
stocks[stocks$Date.aux == "1992-06-30" & stocks$SecurityId == 6002, ]


stocks$row <- NULL

head(stocks)


# Only prices that occurs 5 days before end-of-month

stocks$delta.t <- as.numeric(stocks$Date.aux - stocks$Date)
summary(stocks$delta.t) # one row with data as far as 30 days.

stocks <- stocks[stocks$delta.t <= 5, ] # 5 days before
stocks$delta.t <- NULL

# We only need the end-of-month date
stocks$Date <- stocks$Date.aux
stocks$Date.aux <- NULL


## 5.5 Return computation ------------------------------------------------------

# R = simple return
# r = log return

# SIMPLE RETURNS ---------------------------------------------------------------
p <- c(100, 120, 110, 100, 90)

p[-1]
p[-length(p)]

p[-1] / p[-length(p)] - 1  

## -----------------------------------------------------------------------------


# Make sure the data frame is sorted before computing returns

stocks <- stocks[order(stocks$SecurityId, stocks$Date), ]

stocks$R <- unlist(tapply(stocks$AdjLast, stocks$SecurityId,
                          function(v) c(v[-1]/v[-length(v)] - 1, NA)))
#                                Rt+1 = Pt+1 / Pt           - 1

# NA as last argument = forwardlooking return
# NA as first argument = backwardlooking return


## TAPPLY ----------------------------------------------------------------------

df <- data.frame(id = c("a", "a", "a", "b", "b"), 
                 val = c(100, 200, 120, 140, 160))
df

fn <- function(v) {
  c(diff(v), NA)
}

fn2 <- function(v) {
  c(NA, diff(v))
}

fn(c(100, 200, 150))
fn2(c(100, 200, 150))



tapply(df$val, list(df$id), fn)
tapply(df$val, list(df$id), function(v) c(NA, diff(v)))

unlist(tapply(df$val, list(df$id), fn))  ## UNLIST the list, make a vector

df$d <- unlist(tapply(df$val, list(df$id), fn))
df

## -----------------------------------------------------------------------------


# We need to check if there could be cases when a stock stops trading for some 
# period and then trades again. This would distort our return computation
# because the underlying price observations may be a long way from each other

stocks <- stocks[order(stocks$SecurityId, stocks$Date), ]

stocks$delta.t <- unlist(tapply(stocks$Date, list(stocks$SecurityId),
                                function(v) c(as.numeric(diff(v)), NA)))

head(stocks)

summary(stocks$delta.t) 
summary(stocks$R)

# Apparently there are cases where stocks trade just once which result in an NA.
# We also observe trade that are apart from each other for many days.
# We will only consider returns bases on price observation within a month, 
# i.e. 31 calendar days.

stocks <- stocks[!is.na(stocks$delta.t), ]
stocks <- stocks[stocks$delta.t <= 31, ]
stocks$delta.t <- NULL
nrow(stocks)


## 5.6 Market capitalization and weights ---------------------------------------

# We first add a column MarketCap that shows the market capitalization in MNOK:
stocks$MarketCap <- stocks$Last * stocks$SharesIssued / 1e+06

# Why use last, and not adjlast? Because sharesissued is adjusting.


# We compute the total market capitalization each month and plot the time series
res <- aggregate(stocks$MarketCap, list(stocks$Date), sum)

names(res) <- c("Date", "TotalMarketCap")
plot(res$Date, res$TotalMarketCap,
     type="l", xlab="", ylab="Total Market Capitalization [mln]")


# Merge total market cap with original dataframe: (add mktcap as column to stocks)
stocks <- merge(stocks, res, by = "Date")

# If the date columns have different names: by.x = "Date", by.y = "Dato"

nrow(stocks)
head(stocks)

stocks$Weight <- stocks$MarketCap/stocks$TotalMarketCap
summary(stocks$Weight)


## 5.7 Market returns ----------------------------------------------------------

# ew = equally weighted market return
# vw = value weighted market return

market.ew <- aggregate(stocks$R, list(stocks$Date), mean)
names(market.ew) <- c("Date", "RM.ew")
head(market.ew)


stocks$h <- stocks$R * stocks$Weight

market.vw <- aggregate(stocks$h, list(stocks$Date), sum)
names(market.vw) <- c("Date", "RM.vw")
head(market.vw)


RM.ew <- cumprod(1 + market.ew$RM.ew)   # cumprod, because simple return (cumsum if logreturn)
RM.vw <- cumprod(1 + market.vw$RM.vw)

plot(market.ew$Date, RM.ew, type = "l", xlab = "", ylab = "Market index",
     col = "red")
lines(market.vw$Date, RM.vw, type = "l", col = "blue")
legend("topleft", c("Equally-weighed", "Value-weighed"), lwd = 2,
       col = c("red", "blue"))

# The red line outperform the blue line. 
# Equal weighted 

save(stocks, market.ew, market.vw, file = "Stocks.RData")


## 5.8 From long to wide-format ------------------------------------------------

stocks.wide <- reshape(stocks[, c("Date", "SecurityId", "R")],
                       v.names = "R", idvar = "Date", timevar = "SecurityId", 
                       direction = "wide")
stocks.wide <- stocks.wide[order(stocks.wide$Date), ]
stocks.wide[1:10, 1:4]


## RESHAPE ---------------------------------------------------------------------

df <- data.frame(id = c("a", "a", "b"), 
                 t = c(1, 2, 2), 
                 val = c(100, 20, 40), 
                 f = c(10, 32, 32))
df

df2 <- reshape(df, v.names = "val", idvar = "t", timevar = "id", 
               direction = "wide")
df2

# v.names = which column shall be transformed to wide
# idvar = the key in the wide format
# timevar = want the v.names of each X to vary over time

reshape(df2, direction = "long")

## -----------------------------------------------------------------------------


market.cap.wide <- 
  reshape(stocks[, c("Date", "SecurityId", "MarketCap")],
          v.names = "MarketCap", 
          idvar = "Date", 
          timevar = "SecurityId",
          direction = "wide")

market.cap.wide <- market.cap.wide[order(market.cap.wide$Date), ]

market.cap.wide[1:4, 1:4]


save(stocks.wide, market.cap.wide, file = "Stocks-Wide.RData")


## 5.9 Risk-free interest rate -------------------------------------------------


# Data from 1980 - 1985
df1 <- read.csv("MMR-1980-1985.csv", skip = 12)
head(df1)

df1 <- df1[, c(1, 2)]
names(df1) <- c("Date", "rf")

months <- seq(as.Date("1959-05-01"), as.Date("1986-12-01"), by = "1 month")
months <- months - 1

df1$Date <- months
df1$rf <- df1$rf/100
df1 <- df1[df1$Date >= "1980-01-01", ]

head(df1)

# Data from 1986 - 2013
df2 <- read.csv("Nibor-1986-2013.csv", skip = 16)
df2 <- df2[, c(1, 5)]
names(df2) <- c("Date", "rf")
head(df2)

months <- seq(as.Date("1986-01-01"), as.Date("2013-12-01"), by = "1 month")
months <- months - 1

df2$Date <- months
df2$rf <- df2$rf/100

head(df2)

# Data from 2013 - 2018
df3 <- read.csv("Nibor-2014-2018.csv", skip = 0)
df3 <- df3[, c(1, 2)]
names(df3) <- c("Date", "rf")
head(df3)

df3$Date <- as.Date(df3$Date, format = "%d.%m.%y")

df3 <- df3[order(df3$Date), ]

df3 <- df3[df3$Date >= "2013-03-01" & df3$Date <= "2018-01-31", ]

months <- seq(as.Date("2013-03-01"), as.Date("2018-02-01"), by = "1 month")

df3$Date2 <- cut(df3$Date, months)
head(df3)

df3 <- aggregate(df3$rf, list(df3$Date2), head, n = 1) # head, n = 1 -> first observation for every month

names(df3) <- c("Date", "rf")
head(df3)

df3$Date <- as.Date(df3$Date) - 1

df3$rf <- df3$rf/100

head(df3)


plot(df1$Date, df1$rf, 
     xlab = "", 
     ylab = "", 
     xlim = range(df1$Date, df2$Date, df3$Date), 
     ylim = range(df1$rf, df2$rf, df3$rf),
     type = "l", col = "black")
lines(df2$Date, df2$rf, col = "blue")
lines(df3$Date, df3$rf, col = "red")


# Combine all three

range(df1$Date)
range(df2$Date)
range(df3$Date)


rf <- rbind(df1[df1$Date < "1985-12-31", ],
            df2, 
            df3[df3$Date > "2013-11-30", ])

rf$rf <- rf$rf/12                   
# we scale down to monthly (since we have monthly stock returns)
# monthly stockreturns -> monthly risk-free (its annual here)


save(rf, file = "Riskfree-Rate.RData")




## 5.10 Exercises --------------------------------------------------------------

# 1. Compute for each month the number of return observations in the data frame 
#    stocks that we used during the lecture. Create a plot showing the 
#    time-series of observations each month.




##------------------------------------------------------------------------------
# 2. Download the complete daily stock price information for the following stock
#    as given by their tickers from finance.yahoo.com: IBM, AAPL, XOM, KO and GS. 
#    Merge all samples, compute daily simple returns, and transform the data 
#    frame into wide-format. Compute the annualized mean and volatility. 
#    Determine also the covariance and the correlation using the functions cov 
#    and cor





##------------------------------------------------------------------------------
# 3. Obtain the 3-month Nibor rates from Norges Bank and Oslo Stock Exchange and
#    extend this series with the 3-month Norwegian Treasury Bills in the primary 
#    market to construct a monthly time-series of risk-free interest rates.




##----------------------------------------------------------------------------##
##                 6 - Mean-Variance Portfolios                               ##
##----------------------------------------------------------------------------##

rm(list=ls()) # clear environment
dev.off()     # clear plots
cat("\014")   # clear console
setwd("~/NHH/Master/2. Semester/FIE450 - Programming with Applications in Finance/Filer")



# Mini case 5 - Mean-Variance Portfolios ---------------------------------------
# You work in an asset management company. A rich Norwegian client calls you. 
# He tells you that he would like to invest 50 million NOK in Norwegian stocks 
# only and asks for your advice in finding an optimal portfolio that delivers a 
# target return of 5% p.a. You immediately tell him that it is more advantageous
# to diversify and invest in other asset classes too, preferably in 
# international ones. However, your client refuses, probably because he’s a real
# patriot. You promise him to think about the problem and to call him back once
# you have found an optimal solution.


## 6.1 Optimization problem ----------------------------------------------------

## 6.2 Expected returns and covariances ----------------------------------------

load("Stocks-Wide.RData")   ## Stock returns
load("Stocks.RData")        ## Market return
load("Riskfree-Rate.RData") ## Risk-free interest rate


# Merge stock return, risk-free rate, market return
df <- merge(rf, stocks.wide, by = "Date")
df <- merge(market.vw, df, by = "Date")
df[1:4, 1:7]


# Deduct rf from stock returns to get excess returns: 

rf <- df[, c("Date", "rf")]
RM <- df[, c("Date", "RM.vw")]
R <- df[, -c(2, 3)]             # df without RM.vw and rf column


dim(rf)             
dim(RM)            
dim(R)               # all these have 449 rows, IMPORTANT!!
range(rf$Date)       # and same date range 
range(RM$Date)
range(R$Date)


# [ , -1] => deselct the first column (Date column), and subtract rf from every other column

R[, -1] <- R[, -1] - rf$rf
RM[, -1] <- RM[, -1] - rf$rf



## Principle of replication in R -----------------------------------------------
m <- matrix(1:4, 2, 2)
m
m - c(0.1, 0.2)

## -----------------------------------------------------------------------------

save(R, RM, file = "Excess-Returns.RData")

R[1:4, 1:5]
head(RM)


# Today is 31.12.2017. 
# Only stocks traded today and last month. 
# Only stocks that have been traded at least 75 % of the time during last 60 months.

dim(R)

not.NA <- !is.na(R[R$Date == "2017-11-30", ])
sum(not.NA)     # 111 not na's

R <- R[, not.NA]
dim(R)           # only 111 stocks now


R <- tail(R, n = 60)    # only 60 last rows (months)
RM <- tail(RM, n = 60)


# With apply we determine the proportion of non-missing observations.
liq <- apply(R, 2, function(v) sum(!is.na(v))/length(v))
summary(liq)
R <- R[, liq >= 0.75]


# PARAMETERS:  R[ , -1] not select datecolumn. na.rm=T, remove NAs
mu <- apply(R[, -1], 2, mean, na.rm = TRUE) * 12            # returns
Rho <- cor(R[, -1], use = "pairwise.complete.obs")          # correlations  
Sigma <- cov(R[, -1], use = "pairwise.complete.obs") * 12   # covariances


summary(mu) # not want to invest in negative returns. 

# pairwise.complete.obs -> when comparing a pair of two stocks. 
# If stock 1 has 60 observations, and stock2 has 50, it uses 50 to comoute corr.

summary(Rho[lower.tri(Rho)]) # lower.tri => to get the correct summary (not double count)


sum(mu < 0)        # How many stocks have E[r] < 0?

mu[mu < 0] <- 0    # these 6 stocks have an expected return of 0 now

# This is for practical reasons. Solving with negative E[r] fucks up the optimal solution


## 6.3 Solving for the optimal portfolio ---------------------------------------

# the function solve.QP can be found in package "quadprog"

require(quadprog)

A <- t(rbind(1, mu))
dim(A)

mu.star <- 0.05

d <- rep(0, length(mu))   # we dont have a linear term, so we just set all to 0
length(d)                 # is 53, great. 

b0 <- c(1, mu.star)

solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)

# cannot invert covariance matrix

require(Matrix)
Sigma2 <- nearPD(Sigma)$mat
Sigma2 <- as.matrix(Sigma2)

res <- solve.QP(Dmat = Sigma2, dvec = d, Amat = A, bvec = b0, meq = 2)


# What are the weights for the optimal solution?
omega <- res$solution
hist(omega)


# Is the desired return 0.05?
t(omega) %*% as.matrix(mu)        # t omega = transposed omega * mus (eq. 59)        

# The portfolio standard deviation
sqrt(t(omega) %*% Sigma2 %*% as.matrix(omega))     # eq. 60

# Results: 
# ANNUALIZED Return 0.05  (we scaled them earlier)
# ANNUALIZED Vol 0.0001 -> basicly risk-free??? Something wrong? page 87

# Problem: historical returns does not reflect future returns. 
# Some estimates here are probably very poorly. 
# In practice (as here), our inputs into markowitz model sucks, therefore our output sucks.
# The more constraints you have on markowitz, the more sensible results you get. 


# Contraint = all weights must be positive: NO SHORTS

A <- t(rbind(1, mu, diag(1, length(mu))))
mu.star <- 0.05
b0 <- c(1, mu.star, rep(0, length(mu)))
res <- solve.QP(Dmat = Sigma2, dvec = d, Amat = A, bvec = b0, meq = 2)

omega <- res$solution

sum(omega)   # 100 %


t(omega) %*% as.matrix(mu)                        # return = 0.05

sqrt(t(omega) %*% Sigma2 %*% as.matrix(omega))    # sd = 0.09

summary(omega)

# The more constraints you put onto Markowitz, the more sensible results. 
# But the inputs are shit (the expected returns), so the model sucks. 



## 6.4 Single Index Model ------------------------------------------------------

## 6.4.1 Estimating the Single Index model -------------------------------------

# Equation 68: 

dim(R)
R[1:5,1:7] 
dim(RM)
RM[1:5,]
# regress every column (not date column) with market.


reg <- apply(R[, -1], 2, function(v) {
  res <- lm(v ~ RM$RM.vw)
  c(coefficients(res), var(residuals(res)))
})
rownames(reg) <- c("alpha", "beta", "var.eps")


dim(reg)
reg[, 1:5]





## REGRESSION ------------------------------------------------------------------

x <- c(4, 2, 6, 7, 3, 4)
y <- c(100, 200, 140, 160, 170, 190)
lmres <- lm(y ~ x)
lmres

plot(x, y)
abline(lmres)

summary(lmres)
coefficients(lmres)
residuals(lmres)

summary(lmres)$coefficients

summary(lmres)$r.squared

summary(lmres)$adj.r.squared


## -----------------------------------------------------------------------------


# 6.4.2 Expected returns and risk in the single index model --------------------

# alpha <- reg[1, ] * 12    # han vil ha aplha = 0 (assuming)
beta <- reg[2, ]
var.eps <- reg[3, ] * 12
mu.index <- mean(RM$RM.vw) * 12
var.index <- var(RM$RM.vw) * 12
mu <- beta * mu.index
Sigma <- var.index * (as.matrix(beta) %*% beta)   # eq. 75
diag(Sigma) <- diag(Sigma) + var.eps              # eq. 75 legger til var feilledd i diagonalen

Sigma[1:4, 1:4]



##6.4.3 Solving for the optimal portfolio --------------------------------------

A <- t(rbind(1, mu))
mu.star <- 0.05
d <- rep(0, length(mu))
b0 <- c(1, mu.star)
res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)

omega <- res$solution
t(omega) %*% as.matrix(mu)                          # mu_p
sqrt(t(omega) %*% Sigma %*% as.matrix(omega))       # sigma_p
summary(omega)                                      # no weight is negative!


# Only positive weights: 
A <- t(rbind(1, mu, diag(1, length(mu))))    # adjusting
mu.star <- 0.05
b0 <- c(1, mu.star, rep(0, length(mu)))      # adjusting
res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)

omega <- res$solution
t(omega) %*% as.matrix(mu)
sqrt(t(omega) %*% Sigma %*% as.matrix(omega))
summary(omega)


# FRONTIER: --------------------------------------------------------------------
mu.p.vec <- seq(0, 0.2, length = 100)
sigma.p.vec <- c()
for (i in 1:length(mu.p.vec)) {
  mu.star <- mu.p.vec[i]
  b0 <- c(1, mu.star, rep(0, length(mu)))
  res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0,
                  meq = 2)
  omega <- res$solution
  sigma.p.vec <- c(sigma.p.vec, sqrt(t(omega) %*% Sigma %*% as.matrix(omega)))
}

plot(sigma.p.vec, mu.p.vec, type = "l", xlim = c(0, max(sigma.p.vec)),
     xlab = "Volatility", ylab = "Expected return")


# We see that the optimal portfolio that delivers a target return of 5% is 
# indeed efficient. That is, there is no other portfolio that has a lower risk 
# and still yields 5%


## I TIMEN GJORDE HAN DETTE: --------------------------------------------------
mu.p.vec <- seq(0, 0.2, length = 100)
sigma.p.vec <- c()
for (i in 1:length(mu.p.vec)) {
  mu.star <- mu.p.vec[i]
  b0 <- c(1, mu.star, rep(0, length(mu)))
  A <- t(rbind(1, mu, diag(1, length(mu))))
  d <- rep(0, length(mu))
  res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 2)
  omega <- res$solution
  sigma.p.vec <- c(sigma.p.vec, sqrt(t(omega) %*% Sigma %*% as.matrix(omega)))
}

plot(sigma.p.vec, mu.p.vec, type = "l", xlim = c(0, max(sigma.p.vec)),
     xlab = "Volatility", ylab = "Expected return")

## la til alle variablene på nytt?
## gir samme resultat som det over. 


### Weights and stocks
R[1:3, 1:4]
omega[1]     # is the weight for security 6288



## 6.5 Capital allocation line -------------------------------------------------

mu.star <- 0.05
A <- t(rbind(mu, diag(1, length(mu))))   # add this line!!
b0 <- c(mu.star, rep(0, length(mu)))
d <- rep(0, length(mu))
res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 1)


w <- res$solution      # weight, not omega 
omega <- w/sum(w)      # weights of the tangecy portoflioo
omega

y <- t(omega) %*% as.matrix(mu)     # E[r] of tangency portoflio
x <- sqrt(t(omega) %*% Sigma %*% as.matrix(omega)) # sd/vol of tangency portfolio

points(x, y, pch = 4, lwd = 4, col = "red")
abline(0, y/x, lwd = 2, col = "red", lty = 2)  # CAL line
# since we have excess returns we start at 0 (first argument)

points(0, 0, pch = 4, lwd = 4, col = "blue") # risk free
abline(h = 0.05, lty = 2) # the client 5 % return


0.05/y             # how much to invest in risky tangency portoflio
x2 <- 0.05/y * x   # then we get an vol p.a. 

points(x2, 0.05, col = "darkgreen", lwd = 4)  # this is where the investors invest.


round(omega, 2)
omega * 0.05/c(y) * 5e+07   # how much to invest in tancency potfolio (50 mill)
# in norske kroner terms

y/x     # sharpe ratio (ex-ante) (for ex-post, need to do a backtesting)

# ex-ante or ex-post sharpe ratio



## 6.6 Exercises ---------------------------------------------------------------

# 1. Use the estimates of the single index model and find the optimal portfolio
#    that has a volatility of 8% p.a. assuming you cannot short stocks.




##------------------------------------------------------------------------------
# 2. Write a function that computes the unconstrained frontier, i.e. the 
#    expected returns and the corresponding volatilities. The function shall be 
#    defined as follows:

## Computes the frontier.
##
## mu: vector of expected returns of length n
## Sigma: covariance matrix (n x n)
## mu.p.vec: vector of desired portfolio returns of length m
## Returns a vector of corresponding portfolio variances of length m
frontier <- function(mu, Sigma, mu.p.vec) {
  
  
}



##------------------------------------------------------------------------------
# 3. Use the estimates based a single-index model and do an unconstrained 
#    portfolio optimization to find the tangency portfolio. Backtest this 
#    strategy using a rolling window of exactly 60 return observations. That is,
#    start at the earliest date possible where you have at least 60 return 
#    observations. Find the tangency portfolio. Invest in this tangency 
#    portfolio for one month. Find a new tangency portfolio and invest in the 
#    new one. Plot the return series this strategy generates. 
#    What is the Sharpe ratio?




