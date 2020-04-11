###################################################################
###### Figure A.17: Effect of Riot Destruction on Prosocial #######
######              Behavior (Kyrgyz sample)                #######
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
library(plyr)
library(psych)
library(raster)
library(rgdal)
library(readstata13)
library(ri)
library(spdep) # conflict with older version of dplyr if error message is shown update dplyr to 0.8.0
library(stringr)
library(tseries)
library(tidyverse)

# read data
data <- read.dta13("./kyrgyzstan.dta")

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1
data$pd_in <- as.integer(data$pd_in)
data$pd_out <- as.integer(data$pd_out)

##### Data Cleaning #####
# subset data set according to ethnic groups
#data kyrgyz
data_kyrgyz <- data[which(data$ethnicity=="Kyrgyz"),]

data_kyrgyz$pd_in_scale <- scale(data_kyrgyz$pd_in)
data_kyrgyz$dg_in_scale <- scale(data_kyrgyz$dg_in)
data_kyrgyz$pd_out_scale <- scale(data_kyrgyz$pd_out)
data_kyrgyz$dg_out_scale <- scale(data_kyrgyz$dg_out)
data_kyrgyz$cooperation_index <- rowSums(cbind(data_kyrgyz$pd_in_scale, data_kyrgyz$dg_in_scale, data_kyrgyz$pd_out_scale, data_kyrgyz$dg_out_scale), na.rm=T)/4

#Some cleaning
data_kyrgyz$social_cap_retro <- data_kyrgyz$leadership

# aggregate on psu level
dataAgg <- aggregate(data_kyrgyz[,c("pd_in_scale", "dg_in_scale", "pd_out_scale", "dg_out_scale", "cooperation_index", "access_index", "affected", "economy_index", "state_index", "social_cap_retro")],
                     list(data_kyrgyz$id_psu),
                     mean)
names(dataAgg)[1] <- "psu"

## Throw out missings, spatial model cannot deal with them
dataAgg <- dataAgg[!is.na(dataAgg$social_cap_retro),]



####### Estimate with travel matrix
ttmat <- read.matrix("z.travel_time.csv", header = T, sep = ";", skip = 0)
row.names(ttmat) <- ttmat[,1]
ttmat <- ttmat[,2:ncol(ttmat)]
ttmat <- ttmat[row.names(ttmat) %in% dataAgg$psu, colnames(ttmat) %in% dataAgg$psu]
ttmat_sort <- ttmat[order(as.numeric(row.names(ttmat))),]
ttmat_sort <- ttmat_sort[,order(as.numeric(colnames(ttmat_sort)))]

# matrix to weighted list object
ttlistw <- mat2listw(ttmat_sort)

# Estimate spatial lag models
lw <- ttlistw
model1 <- lagsarlm(pd_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index , data=dataAgg, lw, zero.policy=TRUE)
model2 <- lagsarlm(dg_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model3 <- lagsarlm(pd_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model4 <- lagsarlm(dg_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model5 <- lagsarlm(cooperation_index ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)

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
allModelFrame_travel$Variable <- factor(allModelFrame_travel$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality  index"))
levels(allModelFrame_travel$Variable) <- gsub("  ", "\n", levels(allModelFrame_travel$Variable))




####### Estimate with adjacency matrix
shape <- shapefile("sampling_area_psus_mollweide.shp")
shape <- shape[shape$id_psu %in% dataAgg$psu,]
nb <- poly2nb(shape)
names(nb) <- shape$id_psu

#weighted list
lw <- nb2listw(nb, zero.policy=TRUE )

# Estimate spatial lag models
lw <- lw
model1 <- lagsarlm(pd_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model2 <- lagsarlm(dg_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model3 <- lagsarlm(pd_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model4 <- lagsarlm(dg_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model5 <- lagsarlm(cooperation_index ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)

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
allModelFrame_adjacency$Variable <- factor(allModelFrame_adjacency$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality  index"))
levels(allModelFrame_adjacency$Variable) <- gsub("  ", "\n", levels(allModelFrame_adjacency$Variable))



####### Estimate with geodesic matrix
matrix <- read.csv("z.distance_matrix_kyrgyz.csv")
distance_matrix <- as.matrix(matrix)

# matrix to weighted list object
distance_matrix_list <- mat2listw(distance_matrix)


# Estimate spatial lag models
lw <- distance_matrix_list
model1 <- lagsarlm(pd_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model2 <- lagsarlm(dg_in_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model3 <- lagsarlm(pd_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model4 <- lagsarlm(dg_out_scale ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)
model5 <- lagsarlm(cooperation_index ~ affected + economy_index + state_index + social_cap_retro + access_index, data=dataAgg, lw, zero.policy=TRUE)

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
allModelFrame_geodesic$Variable <- factor(allModelFrame_geodesic$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality  index"))
levels(allModelFrame_geodesic$Variable) <- gsub("  ", "\n", levels(allModelFrame_geodesic$Variable))



####### Combine to plot
allModelFram <- rbind(allModelFrame_travel, allModelFrame_adjacency, allModelFrame_geodesic)
allModelFram$matrix_style <- rep(c("Travel", "Adjacency", "Geodesic"),each=5)
allModelFram <- allModelFram[order(allModelFram$modelName),]
allModelFram <- allModelFram[13:15,]
pd = position_dodge(0.5)

figureA17 <- ggplot(allModelFram, aes(shape=matrix_style)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = pd) +
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval2,
                     ymax = Coefficient + SE*interval2),
                 lwd = 1/4, position = pd) +
  geom_point(aes(x = Variable, y = Coefficient, shape = matrix_style),
             position = pd,fill = "WHITE", size = 3) +
  coord_flip(ylim = c(-0.8,0.2)) + theme_bw() +
  theme(legend.position="bottom") +
  scale_shape_manual(values = c(23, 24, 25), name ="") +
  #ggtitle("Cooperation among kyrgyzs") +
  ylab("")  + xlab("") +
  theme(text = element_text(size=18, family="Times"))

# plot output
figureA17