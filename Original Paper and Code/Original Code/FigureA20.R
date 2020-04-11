###################################################################
###### Figure A.20: Effect of Riot Destruction on Prosocial #######
######              Behavior (first game decision only)     #######
###################################################################

rm(list=ls())

# load required libraries
library(AER)
library(ape)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(igraph)
library(kableExtra)
library(MatchIt)
library(plyr)
library(psych)
library(raster)
library(rgdal)
library(readstata13)
library(ri)
library(spdep) # conflict with older version of dplyr if error message is shown update dplyr to 0.8.0
library(tseries)
library(tidyverse)

# read data
data <- read.dta13("./kyrgyzstan.dta")

##### Cleaning
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
data_uzbek$cooperation_index <- rowSums(cbind(data_uzbek$pd_in_scale, 
                                              data_uzbek$dg_in_scale, 
                                              data_uzbek$pd_out_scale, 
                                              data_uzbek$dg_out_scale), na.rm=T)/4


# generate scale first round prisoners variable 
data_uzbek$pd_1_scale <- scale(as.numeric(data_uzbek$pd_1))


#### Figure
# calculate linear models
model1 <- lm(pd_1_scale ~ affected, data_uzbek)
model2 <- lm(pd_1_scale ~ affected, data_uzbek[data_uzbek$pd1_ingroupmatch == 1,])
model3 <- lm(pd_1_scale ~ affected, data_uzbek[data_uzbek$pd1_ingroupmatch == 0,])

# extract coefficients
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "First PD Decision All")[2,]
model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "First PD Decision Ingroup")[2,]
model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "First PD Decision Outgroup")[2,]

# rowbind all models
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame))

# generate variable
allModelFrame$Variable <- c(3,2,1)

allModelFrame$Variable <- factor(allModelFrame$Variable, labels=c("First PD Decision Outgroup", "First PD Decision Ingroup", "First PD Decision All"))

pd = position_dodge(0.5)

myColors <- c("#000000", "#000000", "#000000")

interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

figureA20 <- ggplot(allModelFrame, aes(colour = as.factor(Variable))) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/4, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  coord_flip(ylim = c(-0.45,0.2)) + theme_bw() + 
  theme(legend.position="none") + 
  #ggtitle("Cooperation among Uzbeks") +  
  ylab("")  + xlab("") +     
  scale_color_manual(values=myColors) +   
  theme(text = element_text(size=24, family="Times")) +
  theme(plot.title = element_text(hjust = 0.5))

# plot output
figureA20