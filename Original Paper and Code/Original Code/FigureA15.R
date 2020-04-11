#########################################################################
##### Figure A.15: Interaction effect of Riot Destruction Historic  #####
#####              Wealth on Prosocial Behavior (OLS)               #####
#########################################################################

rm(list=ls())

# load required libraries
library(ggplot2)
library(readstata13)

##### Cleaning
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
data_uzbek$cooperation_index <- rowSums(cbind(data_uzbek$pd_in_scale, 
                                              data_uzbek$dg_in_scale, 
                                              data_uzbek$pd_out_scale, 
                                              data_uzbek$dg_out_scale), na.rm=T)/4

data_uzbek$income <- scale(data_uzbek$income)
data_uzbek$affected <- scale(data_uzbek$affected)
data_uzbek$status_neigh <- scale(as.numeric(data_uzbek$status_neigh))

##### Figure
#Uzbek
model1 <- lm(pd_in_scale ~ affected*status_neigh + affected + status_neigh, data=data_uzbek)
model2 <- lm(dg_in_scale ~ affected*status_neigh + affected + status_neigh, data=data_uzbek)
model3 <- lm(pd_out_scale ~ affected*status_neigh + affected + status_neigh, data=data_uzbek)
model4 <- lm(dg_out_scale ~ affected*status_neigh + affected + status_neigh, data=data_uzbek)
model5 <- lm(cooperation_index ~ affected*status_neigh + affected + status_neigh, data=data_uzbek)

model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Prisoner's Dilemma ingroup")[4,]
model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Dictator Game ingroup")[4,]
model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Prisoner's Dilemma outgroup")[4,]
model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "Dictator Game outgroup")[4,]
model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Index")[4,]

# set confidence interval parameters
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame, model5Frame))
allModelFrame$Variable <- c(1,2,3,4, 5)
allModelFrame$Variable <- factor(allModelFrame$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame$Variable) <- gsub("  ", "\n", levels(allModelFrame$Variable))

# set colors
myColors <- c("#000000", "#000000", "#000000", "#000000", "#000000")

# Plot
figureA.15 <- ggplot(allModelFrame, aes(colour = as.factor(Variable))) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/4, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  coord_flip(ylim = c(-0.2,0.2)) + theme_bw() + 
  theme(legend.position="none") + 
  ylab("")  + xlab("") +     
  scale_color_manual(values=myColors) +   
  theme(text = element_text(size=24, family="Times")) +
  theme(plot.title = element_text(hjust = 0.5))

# plot output
figureA.15

