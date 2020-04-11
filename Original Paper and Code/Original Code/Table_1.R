##################################################################
##### Table 1: Effect of Destruction on Prosocial Behavior   #####
#####         (controlling for confounders and mobilization) #####
##################################################################

rm(list=ls())

# load required libraries
library(readstata13)
library(stargazer)


# read data
data <- read.dta13("./kyrgyzstan.dta")

##### Cleaning
# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1
data$pd_in <- as.integer(data$pd_in)
data$pd_out <- as.integer(data$pd_out)


# rename variable 
data$social_cap_retro <- data$leadership

# subset data set according to ethnic groups
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]

# scale variables
data_uzbek$pd_in_scale <- scale(data_uzbek$pd_in)
data_uzbek$dg_in_scale <- scale(data_uzbek$dg_in)
data_uzbek$pd_out_scale <- scale(data_uzbek$pd_out)
data_uzbek$dg_out_scale <- scale(data_uzbek$dg_out)
data_uzbek$cooperation_index <- rowSums(cbind(data_uzbek$pd_in_scale, 
                                              data_uzbek$dg_in_scale, 
                                              data_uzbek$pd_out_scale, 
                                              data_uzbek$dg_out_scale), na.rm=T)/4


##### Table
# run linear models 
model1 <- lm(pd_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index + aj_vote_share, data=data_uzbek)
model2 <- lm(dg_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index + aj_vote_share, data=data_uzbek)
model3 <- lm(pd_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index + aj_vote_share, data=data_uzbek)
model4 <- lm(dg_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index + aj_vote_share, data=data_uzbek)
model5 <- lm(cooperation_index ~ affected + economy_index + state_index + social_cap_retro + access_index + aj_vote_share, data=data_uzbek)

# print summaries
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

# table output
stargazer(model1, model2, model3, model4, model5, 
          covariate.labels = c("Destruction", "Wealth index", "State capacity index", "Community policing index", "Accessibility index", "AJ %"),
          dep.var.labels = c("Cooperation in Prisoner's Dilemma", "Investment in Dictator Game", "Cooperation in Prisoner's Dilemma", "Investment in Dictator Game" , "Cooperation-Index"),
          star.char = c("*", "**", "***"),
          title = "Table 1: Effect of Destruction on Prosocial Behavior (controlling for confounders and mobilization)",
          star.cutoffs = c(0.05, 0.01, 0.001))


