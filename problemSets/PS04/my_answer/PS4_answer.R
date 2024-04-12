#PS4

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0)  
    for (package in package.list) 
      detach(package, character.only=TRUE)
}
detachAllPackages()

pkgTest <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"), pkgTest)

# load the data set
data(child)
View(child)
# use the Surv() to build the survival model
child_surv <- with(child, Surv(enter, exit, event))
# run Cox proportional hazard model
cox <- coxph(child_surv ~ sex + m.age, data = child)
# get the summary
summary(cox)
# perform a test to assess the quality of the model
drop1(cox, test = "Chisq")
# display the Cox model using stargazer
stargazer(cox, type = "text")

# Interpretation of coefficients:
# Holding m.age constant, relative to male infants, female infants have a decrease of 0.082 in the expected log hazard.
# Holding sex constant, for each one-year increase in maternal age, there is an increase of 0.34 in the expected log hazard.

# Exponentiated parameter estimates to obtain hazard ratios
exp(-0.082)
exp(0.008)
# Explanation: 
# The hazard ratio for female infants compared to male infants is 0.92, indicating that female infants are 0.92 times as likely to die relative to male infants.
# The hazard ratio for female infants compared to male infants is 1.008, indicating that  for each additional year of maternal age, the relative hazard increases by 1.008 times.
