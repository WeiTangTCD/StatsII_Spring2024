#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
# generate 1,000 Cauchy random variables
set.seed(123)
data<-rcauchy(1000, location = 0, scale = 1)
# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))
print(D)
# get the length of data 
n <- length(data)
# set the parameters
temp_sum=0
pi=3.14159
e=2.71828
# do a for loop according to the formula to calculate the p-value
for (k in 1:n){
  exponent<- -1*((((2*k-1)**2)*((pi)**2))/(8*(D**2)))
  temp_sum=temp_sum+ (e**exponent)
}
# get p-value
p<-(((2*pi)**0.5)/D)*temp_sum
# do a KS test by R
ks_result <- ks.test(data, "pnorm", alternative = "two.sided", exact = FALSE)
ks_result
# Compare the p-value in two results
print(ks_result$p.value)
print(p)


#####################
# Problem 2
#####################

set.seed (123)
# Variable x: Generate 200 random numbers from a uniform distribution
data <- data.frame(x = runif(200, 1, 10))
# Variable y: Generate values based on a linear relationship
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Compute the negative log-likelihood for OLS
linear.lik <- function(beta, y, X) {
  # Compute the residuals by subtracting the predicted values from the actual values
  resid <- y - X %*% beta
  # Compute the sum of squared residuals
  lik <- sum(resid^2)
  # Return the result
  return(lik)
}
# build model by OLS regression using BFGS algorithm
ols_bfgs <- optim(fn=linear.lik,par=c(1,1),hessian=TRUE,y=data$y,X=cbind(1,data$x),method="BFGS")

cat("Coefficients using BFGS algorithm:\n")
cat("Intercept:", ols_bfgs$par[1], "\n")
cat("Slope:", ols_bfgs$par[2], "\n")
cat("\n")

# build linear model
ols_lm <- lm(y ~ x, data = data)
# Check the result
ols_lm
ols_bfgs

#show the result of linear model
cat("Coefficients using linear model:\n")
cat(summary(ols_lm)$coefficients[, 1], "\n")

#Compare the coefficients
cat("\nComparison in different ways of regression\n")
cat("difference in intercept:", ols_bfgs$par[1] - coef(ols_lm)[1], "\n")
cat("difference in slope:", ols_bfgs$par[2] - coef(ols_lm)[2], "\n")

