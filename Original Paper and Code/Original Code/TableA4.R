###################################################
##### Table A.4: Regression of destruction on #####
#####            individual-level covariates  #####
###################################################

rm(list=ls())

# load required libraries
library(readstata13)
library(stargazer)

# read data
data <- read.dta13("./kyrgyzstan.dta")

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1

# subset data set according to ethnic groups
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]

# recode variables
data_uzbek$female <- as.integer(data_uzbek$sex)-1
data_uzbek$education_primary <- ifelse(data_uzbek$education=="No formal education" | data_uzbek$education=="Incomplete primary school" | data_uzbek$education=="Complete primary school", 1,0)
data_uzbek$education_secondary <- ifelse(data_uzbek$education=="Religious school / Madrasa" | data_uzbek$education=="Incomplete secondary" | data_uzbek$education=="Complete secondary", 1,0)
data_uzbek$education_tertiary <- ifelse(data_uzbek$education=="Some university, no degree" | data_uzbek$education=="University-level education, with degree", 1,0)
data_uzbek$marital_married <- ifelse(data_uzbek$marital_status=="Married",1,0)
data_uzbek$marital_single <- ifelse(data_uzbek$marital_status=="Single",1,0)
data_uzbek$marital_other <- ifelse(data_uzbek$marital_status=="Divorced/separated" | data_uzbek$marital_status=="Widowed",1,0)
data_uzbek$job_employee <- ifelse(data_uzbek$occupation=="Full time employee (30 hours a week or more)" | data_uzbek$occupation=="Part time employee (less than 30 hours a week)", 1,0)
data_uzbek$job_self_employed <- ifelse(data_uzbek$occupation=="Self-employed / business", 1,0)
data_uzbek$job_retired <- ifelse(data_uzbek$occupation=="Retired",1,0)
data_uzbek$job_housewife <- ifelse(data_uzbek$occupation=="Housewife",1,0)
data_uzbek$job_student <- ifelse(data_uzbek$occupation=="Student",1,0)
data_uzbek$job_unemployed <- ifelse(data_uzbek$occupation=="Unemployed",1,0)
data_uzbek$job_other <- ifelse(data_uzbek$occupation=="Other",1,0)
data_uzbek$lived_abroad <- ifelse(data_uzbek$livedabroad=="Yes", 1,0)
data_uzbek$owns_accommodation <- ifelse(data_uzbek$owns_house_apartment=="Yes",1,0)
data_uzbek$lives_apartment <- ifelse(data_uzbek$housing=="Apartment",1,0)
data_uzbek$income <- data_uzbek$income/70

# run regression model
m1 <- lm(affected ~ female + age + children + income + livedabroad + migrants + household_size +
           lives_apartment + owns_accommodation + education_primary + education_secondary +
           marital_married + marital_single + job_employee + job_self_employed + job_retired +
           job_housewife + job_student + job_unemployed, data = data_uzbek)

# print Table A.4
summary(m1)

stargazer(m1, 
          dep.var.labels = c("Affected"),
          star.char = c("*", "**", "***"),
          title = " Regression of destruction on individual-level covariates",
          star.cutoffs = c(0.05, 0.01, 0.001))

