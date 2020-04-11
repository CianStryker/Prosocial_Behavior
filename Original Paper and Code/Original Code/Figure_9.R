#################################################################
##### Figure 9: Effect of Riot Destruction on Losses (OLS) ######
#################################################################


rm(list=ls())

# load required libraries
library(ggplot2)
library(readstata13)

# read data
data <- read.dta13("./kyrgyzstan.dta")

##### Cleaning 
# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1

# subset data set according to ethnic groups
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]

##### Figure
# run regressions of loss indecse on riot for Uzbek sample
model1 <- lm(losses_1 ~ affected, data=data_uzbek)
model2 <- lm(losses_2 ~ affected, data=data_uzbek)
model3 <- lm(losses_3 ~ affected, data=data_uzbek)
model4 <- lm(losses_4 ~ affected, data=data_uzbek)
model5 <- lm(losses_5 ~ affected, data=data_uzbek)

# extract coefficients and standard errors
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Car")[2,]
model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "TV")[2,]
model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "House")[2,]
model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "Money")[2,]
model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Business")[2,]

# rowbind data (coefficients and standard errors) from regression models 
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame, model5Frame))
allModelFrame$Variable <- c(1,2,3,4, 5)
allModelFrame$Variable <- factor(allModelFrame$Variable, labels=c("Car", "TV", "House", "Money", "Business"))
levels(allModelFrame$Variable) <- gsub("  ", "\n", levels(allModelFrame$Variable))

# set multipliers for confidence intervals
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# set colors for graph
myColors <- c("#000000", "#000000", "#000000", "#000000", "#000000")

# Plot
figure7 <- ggplot(allModelFrame, aes(colour = as.factor(Variable))) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/4, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  coord_flip(ylim = c(-0.1,0.3)) + theme_bw() + 
  theme(legend.position="none") + 
  #ggtitle("Cooperation among Uzbeks") +  
  ylab("")  + xlab("") +     
  scale_color_manual(values=myColors) +   
  theme(text = element_text(size=24, family="Times")) +
  theme(plot.title = element_text(hjust = 0.5))

# plot output
figure7
