###########################################################
###### Figure A.9: Effect of Riot Destruction on     ######
###### Prosocial Behavior (autocorrelation adjusted) ######
###########################################################

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

#Some cleaning
data_uzbek$neigh_leader <- data_uzbek$leadership

# aggregate on psu level
dataAgg <- aggregate(data_uzbek[,c("pd_in_scale", "dg_in_scale", "pd_out_scale", "dg_out_scale", "cooperation_index", "affected", "economy_index", "state_index", "neigh_leader", "access_index")], 
                     list(data_uzbek$id_psu),
                     mean)
names(dataAgg)[1] <- "psu"

## Throw out missings, spatial model cannot deal with them
dataAgg <- dataAgg[!is.na(dataAgg$neigh_leader),] 


### Estimate with travel matrix
ttmat <- read.matrix("z.travel_time.csv", header = T, sep = ";", skip = 0)
row.names(ttmat) <- ttmat[,1]
ttmat <- ttmat[,2:ncol(ttmat)]
ttmat <- ttmat[row.names(ttmat) %in% dataAgg$psu, colnames(ttmat) %in% dataAgg$psu]
ttmat_sort <- ttmat[order(as.numeric(row.names(ttmat))),]
ttmat_sort <- ttmat_sort[,order(as.numeric(colnames(ttmat_sort)))]

# matrix to weighted list object
ttlistw <- mat2listw(ttmat_sort)


##### Figure
# formulas
f1 <- pd_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f2 <- dg_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f3 <- pd_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f4 <- dg_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f5 <- cooperation_index ~ affected + economy_index + state_index  + neigh_leader + access_index

#basic OLS models
model1 <- lm(pd_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model2 <- lm(dg_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model3 <- lm(pd_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model4 <- lm(dg_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model5 <- lm(cooperation_index ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)

# spatial models
dataAgg <- dataAgg[order(dataAgg$psu),]

model1 <- errorsarlm(f1, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model2 <- errorsarlm(f2, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model3 <- errorsarlm(f3, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model4 <- errorsarlm(f4, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model5 <- errorsarlm(f5, data=dataAgg, ttlistw, tol.solve=1.0e-30)


model1Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model1$coefficients[2],
                                 SE = model1$rest.se[2],
                                 modelName = "Prisoner's Dilemma ingroup")
model2Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model2$coefficients[2],
                                 SE = model2$rest.se[2],
                                 modelName = "Dictator Game ingroup")
model3Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model3$coefficients[2],
                                 SE = model3$rest.se[2],
                                 modelName = "Prisoner's Dilemma outgroup")
model4Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model4$coefficients[2],
                                 SE = model4$rest.se[2],
                                 modelName = "Dictator Game outgroup")
model5Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model5$coefficients[2],
                                 SE = model5$rest.se[2],
                                 modelName = "Index")

interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

allModelFrame_travel <- data.frame(rbind(model1Frame_travel,  model2Frame_travel,  model3Frame_travel,  model4Frame_travel,  model5Frame_travel))
allModelFrame_travel$Variable <- c(1,2,3,4, 5)
allModelFrame_travel$Variable <- factor(allModelFrame_travel$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame_travel$Variable) <- gsub("  ", "\n", levels(allModelFrame_travel$Variable))



#### Estimate with adjacency matrix
shape <- shapefile("sampling_area_psus_mollweide.shp")
shape <- shape[shape$id_psu %in% dataAgg$psu,]
nb <- poly2nb(shape)
names(nb) <- shape$id_psu

#weighted list
lw <- nb2listw(nb, zero.policy=TRUE, style="B")

# formulas
f1 <- pd_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f2 <- dg_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f3 <- pd_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f4 <- dg_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f5 <- cooperation_index ~ affected + economy_index + state_index  + neigh_leader + access_index

#basic OLS models
model1 <- lm(pd_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model2 <- lm(dg_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model3 <- lm(pd_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model4 <- lm(dg_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model5 <- lm(cooperation_index ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)

# spatial models
dataAgg <- dataAgg[order(dataAgg$psu),]

# spatial models
model1 <- errorsarlm(f1, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model2 <- errorsarlm(f2, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model3 <- errorsarlm(f3, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model4 <- errorsarlm(f4, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model5 <- errorsarlm(f5, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)


model1Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model1$coefficients[2],
                                    SE = model1$rest.se[2],
                                    modelName = "Prisoner's Dilemma ingroup")
model2Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model2$coefficients[2],
                                    SE = model2$rest.se[2],
                                    modelName = "Dictator Game ingroup")
model3Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model3$coefficients[2],
                                    SE = model3$rest.se[2],
                                    modelName = "Prisoner's Dilemma outgroup")
model4Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model4$coefficients[2],
                                    SE = model4$rest.se[2],
                                    modelName = "Dictator Game outgroup")
model5Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model5$coefficients[2],
                                    SE = model5$rest.se[2],
                                    modelName = "Index")

interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

allModelFrame_adjacency <- data.frame(rbind(model1Frame_adjacency, model2Frame_adjacency, model3Frame_adjacency, model4Frame_adjacency, model5Frame_adjacency))
allModelFrame_adjacency$Variable <- c(1,2,3,4, 5)
allModelFrame_adjacency$Variable <- factor(allModelFrame_adjacency$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame_adjacency$Variable) <- gsub("  ", "\n", levels(allModelFrame_adjacency$Variable))


#### Estimate with geodesic matrix
matrix <- read.csv("z.distance_matrix.csv")
distance_matrix <- as.matrix(matrix)

# matrix to weighted list object
distance_matrix_list <- mat2listw(distance_matrix)

# formulas 
f1 <- pd_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f2 <- dg_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f3 <- pd_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f4 <- dg_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index
f5 <- cooperation_index ~ affected + economy_index + state_index  + neigh_leader + access_index

#basic OLS models
model1 <- lm(pd_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model2 <- lm(dg_in_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model3 <- lm(pd_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model4 <- lm(dg_out_scale ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)
model5 <- lm(cooperation_index ~ affected + economy_index + state_index + neigh_leader + access_index, data=dataAgg)

# spatial models
dataAgg <- dataAgg[order(dataAgg$psu),]

model1 <- errorsarlm(f1, data=dataAgg, distance_matrix_list, tol.solve=1.0e-30)
model2 <- errorsarlm(f2, data=dataAgg, distance_matrix_list, tol.solve=1.0e-30)
model3 <- errorsarlm(f3, data=dataAgg, distance_matrix_list, tol.solve=1.0e-30)
model4 <- errorsarlm(f4, data=dataAgg, distance_matrix_list, tol.solve=1.0e-30)
model5 <- errorsarlm(f5, data=dataAgg, distance_matrix_list, tol.solve=1.0e-30)

model1Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model1$coefficients[2],
                                   SE = model1$rest.se[2],
                                   modelName = "Prisoner's Dilemma ingroup")
model2Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model2$coefficients[2],
                                   SE = model2$rest.se[2],
                                   modelName = "Dictator Game ingroup")
model3Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model3$coefficients[2],
                                   SE = model3$rest.se[2],
                                   modelName = "Prisoner's Dilemma outgroup")
model4Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model4$coefficients[2],
                                   SE = model4$rest.se[2],
                                   modelName = "Dictator Game outgroup")
model5Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model5$coefficients[2],
                                   SE = model5$rest.se[2],
                                   modelName = "Index")

interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

allModelFrame_geodesic <- data.frame(rbind(model1Frame_geodesic, model2Frame_geodesic, model3Frame_geodesic, model4Frame_geodesic, model5Frame_geodesic))
allModelFrame_geodesic$Variable <- c(1,2,3,4,5)
allModelFrame_geodesic$Variable <- factor(allModelFrame_geodesic$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame_geodesic$Variable) <- gsub("  ", "\n", levels(allModelFrame_geodesic$Variable))



### Combine to plot
allModelFram <- rbind(allModelFrame_travel, allModelFrame_adjacency, allModelFrame_geodesic)
allModelFram$matrix_style <- rep(c("Travel", "Adjacency", "Geodesic"),each=5)

pd = position_dodge(0.5)

figureA.9 <- ggplot(allModelFram, aes(shape=matrix_style)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = pd) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval2,
                     ymax = Coefficient + SE*interval2),
                 lwd = 1/4, position = pd) + 
  geom_point(aes(x = Variable, y = Coefficient, shape = matrix_style),
             position = pd,fill = "WHITE", size = 3) + 
  coord_flip(ylim = c(-0.80,0.2)) + theme_bw() + 
  theme(legend.position="bottom") + 
  scale_shape_manual(values = c(23, 24, 25), name ="") + 
  ylab("")  + xlab("") +     
  theme(text = element_text(size=18, family="Times"))

# plot output
figureA.9
