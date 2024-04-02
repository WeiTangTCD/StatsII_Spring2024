install.packages("CBPS")
#install.packages("stargazer")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("plyr")
install.packages("plm")
#install.packages("readxl")
#install.packages("MatchIt")


# load packages into R Library

library("CBPS")
library("stargazer")
library("tidyverse")
library("dplyr")
library("plyr")
library("plm")
library("readxl")
library("ggplot2")

### Set the working directory

#NOTE: You must save this R Script and all replication data/materials in a folder titled "Data and Replication" on your computer desktop
wd<-getwd()
setwd(wd)

#load the data for the covariate balancing propensity score (CBPS) analysis
dataset <- read_excel("Dataset1.xlsx")
  

# replace empty polyarchy scores
dataset <- dataset %>% dplyr::mutate(v2x_polyarchy = replace_na(v2x_polyarchy, 0.5))

#reduce dataset to just the relevant variables, set as data frame, and finally set as a panel dataframe
df <- subset(dataset, select = c("year_count", "pam_caseid", "TotalBattleFieldDeathsBestEst", "CompositeIndexNatlMilitCapabil", "SecurityInvolveBinaryRateMinimum", "InvolvementBinaryRate", "InvolvementBinaryTotal", "InvolvementLevelTotal", "InvolveLevelRateByBinaryRate", "v2x_polyarchy", "agg_implem_score", "e_gdppc", "PresenceOfWartimeMediationOrPKO", "YearlyPersonnelAvg", "SigMajorOrMinorConfl_1stYrSurv"))
df <- as.data.frame(df)
df <- pdata.frame(df, index = c("pam_caseid", "year_count"), drop.index = FALSE)
#check the type of the variables
typeof(df$InvolveLevelRateByBinaryRate)



# Conduct CBPS analysis for H1a, H2a, H3a
# Note H1a: (Dimension I): The higher the proportion of peace agreement provisions that missions are mandated to enforce, the higher the rate of peace agreement implementation
# Note H2a: (Dimension II): The more security-oriented provisions that missions are mandated to enforce, the higher the rate of peace agreement implementation
# Note H3a: (Dimension III): The more directly involved missions are mandated to be in the implementation of peace agreement provisions, the higher the rate of peace agreement implementation

# set matching and generate models with array of control variables for H1a

fit1 <- CBPS::CBPS(InvolvementBinaryRate ~ TotalBattleFieldDeathsBestEst + v2x_polyarchy + CompositeIndexNatlMilitCapabil, data = df, ATE = TRUE, method = "exact", standardize = TRUE)
balance(fit)

model1 <- plm(diff(agg_implem_score) ~  InvolvementBinaryRate, data = df, weights = fit$weights)
model2<- plm(diff(agg_implem_score) ~SecurityInvolveBinaryRateMinimum, data = df, weights = fit2$weights)
model3 <- plm(diff(agg_implem_score) ~InvolveLevelRateByBinaryRate, data = df, weights = fit2$weights)
stargazer(model1, model2, model3, align=TRUE, out = "TableMain.html")

model1 <- plm(diff(agg_implem_score) ~  InvolvementBinaryRate, data = df, weights = fit$weights)
model1a <- plm(diff(agg_implem_score) ~ InvolvementBinaryRate + e_gdppc, data = df, weights = fit$weights)
model1b <- plm(diff(agg_implem_score) ~ InvolvementBinaryRate + e_gdppc + YearlyPersonnelAvg, data = df, weights = fit$weights)

#output for CBPS for H1a: Table E1
stargazer(model1, model1a, model1b, align=TRUE, out = "TableH1a.html")


# set matching and generate models with array of control variables for H2a
fit2 <- CBPS::CBPS(SecurityInvolveBinaryRateMinimum ~ TotalBattleFieldDeathsBestEst + v2x_polyarchy + CompositeIndexNatlMilitCapabil, data = df, ATE = TRUE, method = "exact", standardize = TRUE)
balance(fit2)

model2 <- plm(diff(agg_implem_score) ~SecurityInvolveBinaryRateMinimum, data = df, weights = fit2$weights)
model2a <- plm(diff(agg_implem_score) ~ SecurityInvolveBinaryRateMinimum + e_gdppc, data = df, weights = fit2$weights)
model2b <- plm(diff(agg_implem_score) ~ SecurityInvolveBinaryRateMinimum + e_gdppc + YearlyPersonnelAvg, data = df, weights = fit2$weights)

#output for CBPS for H2a: Table E2

stargazer(model2, model2a, model2b, align=TRUE, out = "TableH2a.html")


fit3 <- CBPS::CBPS(InvolveLevelRateByBinaryRate ~ TotalBattleFieldDeathsBestEst + v2x_polyarchy + CompositeIndexNatlMilitCapabil, data = df, ATE = TRUE, method = "exact", standardize = TRUE)
balance(fit2)

# set matching and generate models with array of control variables for H3a

model3 <- plm(diff(agg_implem_score) ~InvolveLevelRateByBinaryRate, data = df, weights = fit2$weights)
model3a <- plm(diff(agg_implem_score) ~ InvolveLevelRateByBinaryRate + e_gdppc, data = df, weights = fit2$weights)
model3b <- plm(diff(agg_implem_score) ~ InvolveLevelRateByBinaryRate + e_gdppc + YearlyPersonnelAvg, data = df, weights = fit2$weights)


#output for CBPS for H3a: Table E3
stargazer(model3, model3a, model3b, align=TRUE, out = "TableH3a.html")

###############################################################
model1$coefficients
model2$coefficients
model3$coefficients
unique(x3)
mod<-plm(diff(agg_implem_score) ~e_gdppc, data = df, weights = fit2$weights)
summary(mod)

