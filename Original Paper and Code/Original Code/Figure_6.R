############################################################################
##### Figure 6: Effect of Riot Destruction on Prosocial Behavior (IV) ######
############################################################################

rm(list=ls())

# load required libraries
library(AER)
library(ggplot2)
library(readstata13)
library(spdep) # conflict with older version of dplyr if error message is shown update dplyr to 0.8.0
library(tseries)

# read data
load("~/Replication_Project/Data/kyrgyzstan.RData")
data <- table

##### Cleaning 
# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1
data$pd_in <- as.integer(data$pd_in)
data$pd_out <- as.integer(data$pd_out)

# generate new variables
data$distance <- data$apc_min_distance

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

##### Figure
# First stage #
data_uzbek$distance <- 1-data_uzbek$apc_min_distance
dataAgg <- aggregate(data_uzbek[,c("affected", "distance")], 
                     list(data_uzbek$id_psu),
                     mean)

# run first stage regressions for individual and aggregate data
first_stage_ind <- lm(affected ~ distance, data=data_uzbek)
first_stage_psu <- lm(affected ~ distance, data=dataAgg)

# IV models
# run iv regressions
model11 <- lm(pd_in_scale ~ distance , data=data_uzbek)
model12 <- ivreg(pd_in_scale ~ affected | apc_min_distance, data = data_uzbek)
model21 <- lm(dg_in_scale ~ distance , data=data_uzbek)
model22 <- ivreg(dg_in_scale ~ affected | apc_min_distance , data = data_uzbek)
model31 <- lm(pd_out_scale ~ distance , data=data_uzbek)
model32 <- ivreg(pd_out_scale ~ affected  | apc_min_distance , data = data_uzbek)
model41 <- lm(dg_out_scale ~ distance , data=data_uzbek)
model42 <- ivreg(dg_out_scale ~ affected  | apc_min_distance , data = data_uzbek)
model51 <- lm(cooperation_index ~ distance , data=data_uzbek)
model52 <- ivreg(cooperation_index ~ affected  | apc_min_distance , data = data_uzbek)

# aggregate data
dataAgg <- aggregate(data_uzbek[,c("apc_min_distance", "distance", "pd_in_scale", "dg_in_scale", 
                                   "pd_out_scale", "dg_out_scale", "cooperation_index", "affected", 
                                   "economy_index", "state_index", "social_cap_retro")], 
                     list(data_uzbek$id_psu),
                     mean)
names(dataAgg)[1] <- "psu"

dataAgg <- dataAgg[!is.na(dataAgg$social_cap_retro),] 

# load and edit the travel time matrix
ttmat <- read.matrix("Data/z.travel_time.csv", header = T, sep = ";", skip = 0)
row.names(ttmat) <- ttmat[,1]
ttmat <- ttmat[,2:ncol(ttmat)]
ttmat <- ttmat[row.names(ttmat) %in% dataAgg$psu, colnames(ttmat) %in% dataAgg$psu]
ttmat_sort <- ttmat[order(as.numeric(row.names(ttmat))),]
ttmat_sort <- ttmat_sort[,order(as.numeric(colnames(ttmat_sort)))]
ttlistw <- mat2listw(ttmat_sort)

# formulas
f1 <- pd_in_scale ~ distance 
f2 <- dg_in_scale ~ distance 
f3 <- pd_out_scale ~ distance 
f4 <- dg_out_scale ~ distance 
f5 <- cooperation_index ~ distance 

#basic OLS models
model1 <- lm(pd_in_scale ~ distance , data=dataAgg)
model2 <- lm(dg_in_scale ~ distance , data=dataAgg)
model3 <- lm(pd_out_scale ~ distance , data=dataAgg)
model4 <- lm(dg_out_scale ~ distance , data=dataAgg)
model5 <- lm(cooperation_index ~ distance , data=dataAgg)

# spatial models
dataAgg <- dataAgg[order(dataAgg$psu),]
model13 <- errorsarlm(f1, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model23 <- errorsarlm(f2, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model33 <- errorsarlm(f3, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model43 <- errorsarlm(f4, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model53 <- errorsarlm(f5, data=dataAgg, ttlistw, tol.solve=1.0e-30)

# extract coefficients and standard errors
model11Frame <- data.frame(Variable = rownames(summary(model11)$coef),
                           Coefficient = summary(model11)$coef[, 1],
                           SE = summary(model11)$coef[, 2],
                           modelName = "PD ingroup")[2,]
model21Frame <- data.frame(Variable = rownames(summary(model21)$coef),
                           Coefficient = summary(model21)$coef[, 1],
                           SE = summary(model21)$coef[, 2],
                           modelName = "DG ingroup")[2,]
model31Frame <- data.frame(Variable = rownames(summary(model31)$coef),
                           Coefficient = summary(model31)$coef[, 1],
                           SE = summary(model31)$coef[, 2],
                           modelName = "PD outgroup")[2,]
model41Frame <- data.frame(Variable = rownames(summary(model41)$coef),
                           Coefficient = summary(model41)$coef[, 1],
                           SE = summary(model41)$coef[, 2],
                           modelName = "DG outgroup")[2,]
model51Frame <- data.frame(Variable = rownames(summary(model51)$coef),
                           Coefficient = summary(model51)$coef[, 1],
                           SE = summary(model51)$coef[, 2],
                           modelName = "Index")[2,]
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
model13Frame <- data.frame(Variable = "affected",
                           Coefficient = model13$coefficients[2],
                           SE = model13$rest.se[2],
                           modelName = "Prisoner's Dilemma ingroup")
model23Frame <- data.frame(Variable = "affected",
                           Coefficient = model23$coefficients[2],
                           SE = model23$rest.se[2],
                           modelName = "Dictator Game ingroup")
model33Frame <- data.frame(Variable = "affected",
                           Coefficient = model33$coefficients[2],
                           SE = model33$rest.se[2],
                           modelName = "Prisoner's Dilemma outgroup")
model43Frame <- data.frame(Variable = "affected",
                           Coefficient = model43$coefficients[2],
                           SE = model43$rest.se[2],
                           modelName = "Dictator Game outgroup")
model53Frame <- data.frame(Variable = "affected",
                           Coefficient = model53$coefficients[2],
                           SE = model53$rest.se[2],
                           modelName = "Index")

# bind all models to dataframes
# Instruments
allModelFrame1 <- data.frame(rbind(model11Frame, model21Frame, model31Frame, model41Frame, model51Frame))
allModelFrame1$Variable <- c(1,2,3,4,5)
allModelFrame1$Variable <- factor(allModelFrame1$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame1$Variable) <- gsub("  ", "\n", levels(allModelFrame1$Variable))

# 2SLS
allModelFrame2 <- data.frame(rbind(model12Frame, model22Frame, model32Frame, model42Frame, model52Frame))
allModelFrame2$Variable <- c(1,2,3,4,5)
allModelFrame2$Variable <- factor(allModelFrame2$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame2$Variable) <- gsub("  ", "\n", levels(allModelFrame2$Variable))

# Instrument (SAM)
allModelFrame3 <- data.frame(rbind(model13Frame, model23Frame, model33Frame, model43Frame, model53Frame))
allModelFrame3$Variable <- c(1,2,3,4,5)
allModelFrame3$Variable <- factor(allModelFrame3$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame3$Variable) <- gsub("  ", "\n", levels(allModelFrame3$Variable))

# rowbind all models 
allModelFram <- rbind(allModelFrame1, allModelFrame2, allModelFrame3)
allModelFram$matrix_style <- rep(c("Instrument", "2SLS", "Instrument (SAM)"),each=5)

# set multipliers for confidence intervals
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# set up dodge
pd = position_dodge(0.5)

# build plot
figure6 <- ggplot(allModelFram, aes(shape=matrix_style)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = pd) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval2,
                     ymax = Coefficient + SE*interval2),
                 lwd = 1/4, position = pd) + 
  geom_point(aes(x = Variable, y = Coefficient, shape = matrix_style),
             position = pd,fill = "WHITE", size = 3) + 
  coord_flip(ylim = c(-0.95,0.22)) + theme_bw() + 
  theme(legend.position="bottom") + 
  scale_shape_manual(values = c(23, 24, 25), name ="") +
  ylab("")  + xlab("") +     
  theme(text = element_text(size=18, family="Times"))

# plot output
figure6
