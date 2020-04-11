##################################################################################
###### Table A.9: First stage regression of destruction on barrack distance ######
##################################################################################

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

### First stage
data_uzbek$distance <- 1-data_uzbek$apc_min_distance
dataAgg <- aggregate(data_uzbek[,c("affected", "distance")], 
                     list(data_uzbek$id_psu),
                     mean)

# run regressions
first_stage_ind <- lm(affected ~ distance, data=data_uzbek)
#summary(first_stage_ind)
first_stage_psu <- lm(affected ~ distance, data=dataAgg)
#summary(first_stage_psu)

# table output
stargazer(first_stage_ind, first_stage_psu, covariate.labels = c("Distance to closest barack"),
          star.char = c("*", "**", "***"),
          title = "Table A.9: First stage regression of destruction on barrack distance",
          star.cutoffs = c(0.05, 0.01, 0.001))




