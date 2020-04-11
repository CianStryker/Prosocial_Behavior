##############################################################
###### Figure A.14: Effect of Individual Victimization #######
######              on Prosocial Behavior (IV)         #######
##############################################################

rm(list=ls())

# load required libraries
library(AER)
library(data.table)
library(ggplot2)
library(gridExtra)
library(readstata13)

# read data
data <- read.dta13("./kyrgyzstan.dta")

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1
data$pd_in <- as.integer(data$pd_in)
data$pd_out <- as.integer(data$pd_out)

##### Data Cleaning #####
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

# calculate distance
data_uzbek$distance <- 1-data_uzbek$apc_min_distance

# aggregate data
dataAgg <- aggregate(data_uzbek[,c("lossindex", "distance")], 
                     list(data_uzbek$id_psu),
                     mean)

# run first stage regressions
first_stage_ind <- lm(lossindex ~ distance, data=data_uzbek)
first_stage_psu <- lm(lossindex ~ distance, data=dataAgg)


### IV models ###
model12 <- ivreg(pd_in_scale ~ lossindex | apc_min_distance, data = data_uzbek)
model22 <- ivreg(dg_in_scale ~ lossindex | apc_min_distance , data = data_uzbek)
model32 <- ivreg(pd_out_scale ~ lossindex  | apc_min_distance , data = data_uzbek)
model42 <- ivreg(dg_out_scale ~ lossindex  | apc_min_distance , data = data_uzbek)
model52 <- ivreg(cooperation_index ~ lossindex  | apc_min_distance , data = data_uzbek)


model12Frame <- data.frame(Variable = rownames(summary(model12)$coef),
                           Coefficient = summary(model12)$coef[, 1],
                           SE = summary(model12)$coef[, 2],
                           modelName = "PD ingroup")[2,]
model22Frame <- data.frame(Variable = rownames(summary(model22)$coef),
                           Coefficient = summary(model22)$coef[, 1],
                           SE = summary(model22)$coef[, 2],
                           modelName = "DG ingroup")[2,]
model32Frame <- data.frame(Variable = rownames(summary(model32)$coef),
                           Coefficient = summary(model32)$coef[, 1],
                           SE = summary(model32)$coef[, 2],
                           modelName = "PD outgroup")[2,]
model42Frame <- data.frame(Variable = rownames(summary(model42)$coef),
                           Coefficient = summary(model42)$coef[, 1],
                           SE = summary(model42)$coef[, 2],
                           modelName = "DG outgroup")[2,]
model52Frame <- data.frame(Variable = rownames(summary(model52)$coef),
                           Coefficient = summary(model52)$coef[, 1],
                           SE = summary(model52)$coef[, 2],
                           modelName = "Index")[2,]



allModelFrame2 <- data.frame(rbind(model12Frame, model22Frame, model32Frame, model42Frame, model52Frame))
allModelFrame2$Variable <- c(1,2,3,4,5)
allModelFrame2$Variable <- factor(allModelFrame2$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame2$Variable) <- gsub("  ", "\n", levels(allModelFrame2$Variable))
allModelFrame2$matrix_style <- "2SLS"


pd = position_dodge(0.5)
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

figureA.14 <- ggplot(allModelFrame2, aes(shape=matrix_style)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = pd) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval2,
                     ymax = Coefficient + SE*interval2),
                 lwd = 1/4, position = pd) + 
  geom_point(aes(x = Variable, y = Coefficient, shape = matrix_style),
             position = pd,fill = "WHITE", size = 3) + 
  coord_flip(ylim = c(-2.25,0.2)) + theme_bw() + 
  theme(legend.position="bottom") + 
  scale_shape_manual(values = c(23, 24, 25), name ="") +
  ylab("")  + xlab("") +     
  theme(text = element_text(size=18, family="Times"))

# plot output
figureA.14
