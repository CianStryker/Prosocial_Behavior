############################################################
###### Table A.6: Correlation between destruction and ######
######            government support in eastern Osh   ######
############################################################

rm(list=ls())

# load required libraries
library(readstata13)
library(stargazer)

# read data
data <- read.dta13("./kyrgyzstan.dta")

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1

data_uzbek_east <- data[which(data$ethnicity=="Uzbek" & data$east_of_akbuura==1),]

data_uzbek_east$pol_say <- as.numeric(data_uzbek_east$pol_say)-1
data_uzbek_east$pol_say_scale <- scale(data_uzbek_east$pol_say)

# recode "political say" variable to dummy
data_uzbek_east$pol_say <- as.numeric(data_uzbek_east$pol_say)-1

# run regression
model1 <- lm(pol_say_scale ~ affected, data= data_uzbek_east)

#summary(model1)
stargazer(model1,
          covariate.labels = c("Destruction"),
          dep.var.labels = c("People like me have no say in what the government does"),
          star.char = c("*", "**", "***"),
          title = "Table A.6: Correlation between destruction and government support in eastern Osh",
          star.cutoffs = c(0.05, 0.01, 0.001))




