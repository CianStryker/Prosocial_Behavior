##################################################################
##### Table A.5: Correlation between experimental measures #######
#####            and self-reported behavior                #######
##################################################################

rm(list=ls())

# load required libraries
library(readstata13)
library(stargazer)
library(stringr)

# read data
data <- read.dta13("./kyrgyzstan.dta")

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1

##### Data Cleaning #####
# subset data set according to ethnic groups
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]

# scale variables
data_uzbek$pd_out_scale <- scale(as.integer(data_uzbek$pd_out))
data_uzbek$dg_out_scale <- scale(as.integer(data_uzbek$dg_out))

# generate outgroup cooperation index
data_uzbek$out_group_cooperation_index <- rowSums(cbind(data_uzbek$pd_out_scale, data_uzbek$dg_out_scale), na.rm=T)/2
data_uzbek$comm_lang_use <- as.numeric(data_uzbek$comm_lang_use)

# OLS models
model <- lm(comm_lang_use ~ out_group_cooperation_index, data=data_uzbek)
summary(model)

# output
print ("Table: A.5: Correlation between experimental measures and
self-reported behavior")
stargazer(model,
          covariate.labels = c("Outgroup pro-sociality index"),
          dep.var.labels = c("Use of others' language"),
          star.char = c("*", "**", "***"),
          title = "Table: A.5: Correlation between experimental measures and self-reported behavior",
          star.cutoffs = c(0.05, 0.01, 0.001))


