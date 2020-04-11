##############################################################################
### Table A.8: Regression of prosocial behavior on destruction controlling ###
###            for confounders (Manski bound imputation for migrants)      ###
##############################################################################

rm(list=ls())

# load libraries
library(readstata13)
library(stargazer)

# read data
data <- read.dta13("./kyrgyzstan.dta")

### Some data cleaning
data$social_cap_retro <- data$leadership
data$movedhouse <- ifelse(data$movedhouse=="Always lived in this house",0, ifelse(data$movedhouse=="Moved house ",1,99))
data$migrateoutyear_after2010 <- ifelse(data$migrateoutyear>2010,1,0)

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1
data$pd_in <- as.integer(data$pd_in) - 1
data$pd_out <- as.integer(data$pd_out) - 1

# rename variable 
data$social_cap_retro <- data$leadership

# subset data set according to ethnic groups
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]

# scale variables
data_uzbek$pd_in_scale <- scale(data_uzbek$pd_in)
data_uzbek$dg_in_scale <- scale(data_uzbek$dg_in)
data_uzbek$pd_out_scale <- scale(data_uzbek$pd_out)
data_uzbek$dg_out_scale <- scale(data_uzbek$dg_out)

# When did you live abroad last time?
data_uzbek_affected <- data_uzbek[which(data_uzbek$affected==1),]

##############
### Manski ###
##############
data_uzbek$pd_in_manski  <- scale(ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==1, 1, ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==0, 0, data_uzbek$pd_in)))
data_uzbek$dg_in_manski  <- scale(ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==1, 100, ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==0, 0, data_uzbek$dg_in)))
data_uzbek$pd_out_manski  <- scale(ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==1, 1, ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==0, 0, data_uzbek$pd_out)))
data_uzbek$dg_out_manski  <- scale(ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==1, 100, ifelse(data_uzbek$movedhouse==1 & data_uzbek$affected==0, 0, data_uzbek$dg_out)))
data_uzbek$cooperation_index_manski <- rowSums(cbind(data_uzbek$pd_in_manski, data_uzbek$dg_in_manski, data_uzbek$pd_out_manski, data_uzbek$dg_out_manski), na.rm=T)/4

model1 <- lm(pd_in_manski ~ affected + economy_index + state_index  + social_cap_retro, data=data_uzbek)
model2 <- lm(dg_in_manski ~ affected + economy_index + state_index  + social_cap_retro, data=data_uzbek)
model3 <- lm(pd_out_manski ~ affected + economy_index + state_index + social_cap_retro, data=data_uzbek)
model4 <- lm(dg_out_manski ~ affected + economy_index + state_index + social_cap_retro, data=data_uzbek)
model5 <- lm(cooperation_index_manski ~ affected + economy_index + state_index + social_cap_retro, data=data_uzbek)

stargazer(model1, model2, model3, model4, model5, 
          covariate.labels = c("Destruction", "Wealth index", "State capacity index", "Community policing index"),
          dep.var.labels = c("Cooperation in Prisoner's Dilemma", "Investment in Dictator Game", "Cooperation in Prisoner's Dilemma", "Investment in Dictator Game" , "Cooperation-Index"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001))
          


