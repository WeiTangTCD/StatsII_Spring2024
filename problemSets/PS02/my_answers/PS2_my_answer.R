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

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
# check the data frame
dfm <- climateSupport
head(dfm)
# Question1
# clean the data, transform the dummy variables
dummy_country_variables <- model.matrix(~ countries -1, data = dfm)
dummy_sanctions_variables <- model.matrix(~ sanctions -1, data = dfm)
dfm$choice<-ifelse(dfm$choice=="Supported",1,0)
dfm <- cbind(dfm, dummy_country_variables)
dfm <- cbind(dfm, dummy_sanctions_variables)
# delete some useless columns
dfm <- dfm[, -which(names(dfm) == "countries")]
dfm <- dfm[, -which(names(dfm) == "sanctions")]
dfm <- dfm[, -which(names(dfm) == "sanctionsNone")]
dfm <- dfm[, -which(names(dfm) == "countries20 of 192")]
# Replace the special symbols and whitespaces of the variable names
colnames(dfm) <- gsub(" ", "_", colnames(dfm))
colnames(dfm) <- gsub("%", "percent", colnames(dfm))
View(dfm)
# train the additive model without interactive terms.
additive_model <- glm(choice ~ `countries80_of_192` + `countries160_of_192` +  `sanctions5percent` + `sanctions15percent` + `sanctions20percent`,
                      data = dfm,
                      family = binomial(link = "logit"))
# check the summary of the model
summary(additive_model)
#Question2
# (a)
# (b)
# create a new_data with 80 of 192 countries participating with no sanctions
new_data <-data.frame(countries80_of_192=1, countries160_of_192=0,  `sanctions5percent` =0, `sanctions15percent`=0, `sanctions20percent`=0)
# predict individual support probability
predicted_probability <- predict(additive_model,newdata = new_data,type = "response")
# get the result of probability
print(predicted_probability)

# (c)
# train the additive model with interactive terms.
model_with_interaction <- glm(choice ~ `countries80_of_192` + `countries160_of_192` +  `sanctions5percent` + `sanctions15percent` + `sanctions20percent`+`countries80_of_192`:`sanctions5percent`+`countries80_of_192`:`sanctions15percent`+`countries80_of_192`:`sanctions20percent`+`countries160_of_192`:`sanctions5percent`+`countries160_of_192`:`sanctions15percent`+`countries160_of_192`:`sanctions20percent`, 
                              data = dfm,
                              family = "binomial")

# do a likelihood test between the 2 models
likelihood_ratio_test <- anova(additive_model, model_with_interaction, test = "LRT")
print(likelihood_ratio_test)

# another way to check the necessity of interactive terms:
# check the summary to see the p-value of the corresponding coefficients
summary(model_with_interaction)
summary(additive_model)
new_data <-data.frame(countries80_of_192=1, countries160_of_192=0,  `sanctions5percent` =0, `sanctions15percent`=0, `sanctions20percent`=0,)
