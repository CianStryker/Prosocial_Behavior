##############################################################
###### Table A.12: Decisions and expectations in the PD ###### 
######             played with the ingroup              ######
##############################################################

rm(list=ls())

# load required libraries
library(readstata13)
library(stargazer)


# read data
data <- read.dta13("./kyrgyzstan.dta")

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1
data$pd_in <- as.integer(data$pd_in)
data$pd_out <- as.integer(data$pd_out)

# subset data set according to ethnic groups
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]

# scale variables
data_uzbek$pd_in_scale <- scale(data_uzbek$pd_in)
data_uzbek$dg_in_scale <- scale(data_uzbek$dg_in)
data_uzbek$pd_out_scale <- scale(data_uzbek$pd_out)
data_uzbek$dg_out_scale <- scale(data_uzbek$dg_out)
data_uzbek$cooperation_index <- rowSums(cbind(data_uzbek$pd_in_scale, data_uzbek$dg_in_scale, data_uzbek$pd_out_scale, data_uzbek$dg_out_scale), na.rm=T)/4

# Table parts 
tableA <- table(data_uzbek$pd_in[data_uzbek$affected == 0], data_uzbek$pd_in_guess[data_uzbek$affected == 0])
tableA <- tableA/sum(tableA)
tableA <- tableA[order(rownames(tableA), decreasing = T), order(colnames(tableA), decreasing = T)]

tableB <- table(data_uzbek$pd_in[data_uzbek$affected == 1], data_uzbek$pd_in_guess[data_uzbek$affected == 1]) 
tableB <- tableB/sum(tableB)
tableB <- tableB[order(rownames(tableB), decreasing = T), order(colnames(tableB), decreasing = T)]

# entire table
tableA12 <- cbind(tableA, tableB)
tableA12
