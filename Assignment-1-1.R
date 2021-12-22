##----------------------------------------------------------------------------##
##                 FIE450 - Assignment 1-1                                    ##
##----------------------------------------------------------------------------##

rm(list=ls())

## Data processing -------------------------------------------------------------

eqnr <- read.csv("Data-EQNR-OL.csv", sep = "|", dec = ",")  
eqnr[, 1] <- as.Date(eqnr[, 1], format = "%m/%d/%y")
eqnr <- eqnr[, c("Date", "Adj.Close")]
eqnr <- eqnr[order(eqnr$Date), ]

# Compute column with log returns
eqnr$r <- c(NA, diff(log(eqnr$Adj.Close)))


## Task 1.1 --------------------------------------------------------------------

# We have weekly data -> assuming 52 trading weeks in a year

# Annual expected log return
mu1 <- mean(eqnr$r, na.rm = TRUE) * 52

# 99 % confidence interval
r1 <- na.omit(eqnr$r)
n1 <- length(r1)
SE1 <- sd(r1)/sqrt(n1) * 52

p <- 0.99
z <- -qnorm((1 - p) / 2)

ci1 <- c(mu1 - z * SE1, mu1 + z * SE1)


## Task 1.2 --------------------------------------------------------------------

# Dates from 01-02-2016
eqnr2 <- eqnr[eqnr$Date >= "2016-02-01", ]

# Annual expected log return
mu2 <- mean(eqnr2$r, na.rm = TRUE) * 52


# 99 % confidence interval
r2 <- na.omit(eqnr2$r)
n2 <- length(r2)
SE2 <- sd(r2)/sqrt(n2) * 52

ci2 <- c(mu2 - z * SE2, mu2 + z * SE2)


## Task 1.3 --------------------------------------------------------------------

# Estimate (a) is more precise, because the confidence interval is narrower.


## Task 1.4 --------------------------------------------------------------------

f <- SE2/SE1

# Estimate (a) is approximately twice as precise as estimate (b).

