####################
# PS3 Wei Tang
####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics","package:nnet", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base","package:AER","package:pscl")
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

lapply(c("nnet", "MASS","pscl","AER"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#####################
# Problem 1
#####################

# load and read data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# create a new column called GDP_change, including categories:"increase","decrease","no_change".
# GDP_change is depending on GDPWdiff.
for (i in 1:length(gdp_data$GDPWdiff)) {
  if (gdp_data$GDPWdiff[i] > 0) {
    gdp_data$GDP_change[i] <- "increase"
  } else if (gdp_data$GDPWdiff[i] < 0) {
    gdp_data$GDP_change[i] <- "decrease"
  } else {
    gdp_data$GDP_change[i] <- "no_change"
  }
}

# factorize the response variables and explanatory variables, put their levels and labels in the same order.
gdp_data$GDP_change <- factor(gdp_data$GDP_change, levels = c("decrease","no_change","increase"),labels = c("decrease","no_change","increase"))
gdp_data$OIL <- factor(gdp_data$OIL, levels = c(0,1),labels = c("otherwise","Exceed 50%"))
gdp_data$REG <- factor(gdp_data$REG, levels = c(0,1),labels = c("Non-Democracy","Democracy"))

# unordered model
# relevel GDP_change and set "no_change" as the reference category.
gdp_data$GDP_change <- relevel(gdp_data$GDP_change,ref = "no_change")
# run a unordered model 
unordered_mod <- multinom(GDP_change ~ REG + OIL, data = gdp_data)
# get the summary of the model to get coefficients
summary(unordered_mod)
# exponent e by coefficients
exp(coef(unordered_mod))

# ordered model
# run an ordered model
ordered_mod <- polr(GDP_change ~ REG + OIL, data = gdp_data)
# get the summary of the model to get coefficients
summary(ordered_mod)
# exponent e by coefficients
exp(coef(ordered_mod))
# get the confidence interval of the coefficients
exp(cbind(OR=coef(ordered_mod),confint(ordered_mod)))


#####################
# Problem 2
#####################

# load and read data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")
# run Poisson regression and build model
mod.ps <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family = poisson)
# check the summary of the model
summary(mod.ps)
# do dispersion test
dispersiontest(mod.ps)
# build a zero-inflation model
mod.zip <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, dist = "poisson")  # 拟合零膨胀泊松模型
# check the summary of the model
summary(mod.zip)
# withdraw coefficients
cfs <- coef(mod.zip)
# predict the d mean number of visits
pre_data <- data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1)
# get the estimate from original Poisson regression model
exp(predict(mod.ps, newdata = pre_data))
# get the estimate from zero-inflation model

exp(predict(mod.zip, newdata = pre_data))
