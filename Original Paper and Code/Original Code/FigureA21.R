############################################################
####### Figure A21: Effect of riot on pre-registered #######
#######             mechanism outcomes               #######
############################################################

rm(list=ls())

# load required libraries
library(AER)
library(ape)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
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

# rename variable
data$social_cap_retro <- data$leadership

# data cleaning
data$leadership <- ifelse(data$leadership=="Not powerful at all", 1,
                          ifelse(data$leadership=="Not powerful", 2,
                                 ifelse(data$leadership=="Neither powerful, nor not powerful", 3,
                                        ifelse(data$leadership=="Powerful", 4,
                                               ifelse(data$leadership=="Very powerful", 5, 99)))))
data$coethnic_hero <- ifelse(data$heroes_1==1 & data$ethnicity=="Uzbek", 1,
                             ifelse(data$heroes_3==1 & data$ethnicity=="Kyrgyz", 1, 0))
data$risk <- scale(data$risk) 
data$comm_relations <- scale(as.numeric(data$comm_relations))

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

# build other indeces
data_uzbek$ethnicization_index <- rowSums(cbind(scale(data_uzbek$identification_2), 
                                                scale(data_uzbek$coethnic_hero)), na.rm=F)/2
data_uzbek$interdependence_index <- rowSums(cbind(scale(100-data_uzbek$remittances_percentage), 
                                                  as.numeric(data_uzbek$coethnic_employer), na.rm=F))/2
data_uzbek$expectation_index <- rowSums(cbind(scale(as.numeric(data_uzbek$comm_relations)), 
                                              scale(as.numeric(data_uzbek$comm_lang_use))), na.rm=F)/2


#mean imputation
data_uzbek$ethnicization_index[is.na(data_uzbek$ethnicization_index)] = mean(data_uzbek$ethnicization_index, na.rm=TRUE)
data_uzbek$comm_relations[is.na(data_uzbek$comm_relations)] = mean(data_uzbek$comm_relations, na.rm=TRUE)
data_uzbek$expectation_index[is.na(data_uzbek$expectation_index)] = mean(data_uzbek$expectation_index, na.rm=TRUE)


# aggregate on psu level
dataAgg <- aggregate(data_uzbek[,c("affected", "economy_index", "state_index", "leadership", "access_index", 
                                   "risk", "ethnicization_index", "expectation_index", "interdependence_index")], 
                     list(data_uzbek$id_psu),
                     mean)
names(dataAgg)[1] <- "psu"

dataAgg <- dataAgg[!is.na(dataAgg$ethnicization_index),] 
dataAgg <- dataAgg[!is.na(dataAgg$expectation_index),] 


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
f1 <- risk ~ affected + economy_index + state_index + leadership + access_index
f2 <- ethnicization_index ~ affected + economy_index + state_index + leadership + access_index
f3 <- expectation_index ~ affected + economy_index + state_index + leadership + access_index
f4 <- interdependence_index ~ affected + economy_index + state_index + leadership + access_index


#basic OLS models
model1 <- lm(risk ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model2 <- lm(ethnicization_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model3 <- lm(expectation_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model4 <- lm(interdependence_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)


# spatial models
dataAgg <- dataAgg[order(dataAgg$psu),]


model1 <- errorsarlm(f1, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model2 <- errorsarlm(f2, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model3 <- errorsarlm(f3, data=dataAgg, ttlistw, tol.solve=1.0e-30)
model4 <- errorsarlm(f4, data=dataAgg, ttlistw, tol.solve=1.0e-30)


model1Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model1$coefficients[2],
                                 SE = model1$rest.se[2],
                                 modelName = "Risk preferences")
model2Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model2$coefficients[2],
                                 SE = model2$rest.se[2],
                                 modelName = "Ethnicization")
model3Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model3$coefficients[2],
                                 SE = model3$rest.se[2],
                                 modelName = "Future conflict")
model4Frame_travel <- data.frame(Variable = "affected",
                                 Coefficient = model4$coefficients[2],
                                 SE = model4$rest.se[2],
                                 modelName = "Economic interdependence")

interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

allModelFrame_travel <- data.frame(rbind(model1Frame_travel,  model2Frame_travel,  model3Frame_travel,  model4Frame_travel))
allModelFrame_travel$Variable <- c(1,2,3,4)
allModelFrame_travel$Variable <- factor(allModelFrame_travel$Variable, labels=c("Risk  preferences", "Ethnicization", "Future  conflict", "Economic  interdependence"))
levels(allModelFrame_travel$Variable) <- gsub("  ", "\n", levels(allModelFrame_travel$Variable))


### Estimate with adjacency matrix
shape <- shapefile("sampling_area_psus_mollweide.shp")
shape <- shape[shape$id_psu %in% dataAgg$psu,]
nb <- poly2nb(shape)
names(nb) <- shape$id_psu

#weighted list
lw <- nb2listw(nb, zero.policy=TRUE )


# formulas
f1 <- risk ~ affected + economy_index + state_index + leadership + access_index
f2 <- ethnicization_index ~ affected + economy_index + state_index + leadership + access_index
f3 <- expectation_index ~ affected + economy_index + state_index + leadership + access_index
f4 <- interdependence_index ~ affected + economy_index + state_index + leadership + access_index


#basic OLS models
model1 <- lm(risk ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model2 <- lm(ethnicization_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model3 <- lm(expectation_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model4 <- lm(interdependence_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)

# spatial models
dataAgg <- dataAgg[order(dataAgg$psu),]

# spatial models
model1 <- errorsarlm(f1, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model2 <- errorsarlm(f2, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model3 <- errorsarlm(f3, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model4 <- errorsarlm(f4, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)


model1Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model1$coefficients[2],
                                    SE = model1$rest.se[2],
                                    modelName = "Risk preferences")
model2Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model2$coefficients[2],
                                    SE = model2$rest.se[2],
                                    modelName = "Ethnicization")
model3Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model3$coefficients[2],
                                    SE = model3$rest.se[2],
                                    modelName = "Future conflict")
model4Frame_adjacency <- data.frame(Variable = "affected",
                                    Coefficient = model4$coefficients[2],
                                    SE = model4$rest.se[2],
                                    modelName = "Economic interdependence")

interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

allModelFrame_adjacency <- data.frame(rbind(model1Frame_adjacency, model2Frame_adjacency, model3Frame_adjacency, model4Frame_adjacency))
allModelFrame_adjacency$Variable <- c(1,2,3,4)
allModelFrame_adjacency$Variable <- factor(allModelFrame_travel$Variable, labels=c("Risk  preferences", "Ethnicization", "Future  conflict", "Economic  interdependence"))
levels(allModelFrame_adjacency$Variable) <- gsub("  ", "\n", levels(allModelFrame_adjacency$Variable))



#### Estimate with geodesic matrix

### load km distance matrix (calculated with fields package)
matrix <- read.csv("z.distance_matrix.csv")
distance_matrix <- as.matrix(matrix)

# matrix to weighted list object
distance_matrix_list <- mat2listw(distance_matrix)

# formulas
f1 <- risk ~ affected + economy_index + state_index + leadership + access_index
f2 <- ethnicization_index ~ affected + economy_index + state_index + leadership + access_index
f3 <- expectation_index ~ affected + economy_index + state_index + leadership + access_index
f4 <- interdependence_index ~ affected + economy_index + state_index + leadership + access_index


#basic OLS models
model1 <- lm(risk ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model2 <- lm(ethnicization_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model3 <- lm(expectation_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)
model4 <- lm(interdependence_index ~ affected + economy_index + state_index + leadership + access_index, data=dataAgg)


# spatial models
dataAgg <- dataAgg[order(dataAgg$psu),]

# spatial models
model1 <- errorsarlm(f1, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model2 <- errorsarlm(f2, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model3 <- errorsarlm(f3, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)
model4 <- errorsarlm(f4, data=dataAgg, lw, tol.solve=1.0e-30, zero.policy = T)


model1Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model1$coefficients[2],
                                   SE = model1$rest.se[2],
                                   modelName = "Risk preferences")
model2Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model2$coefficients[2],
                                   SE = model2$rest.se[2],
                                   modelName = "Ethnicization")
model3Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model3$coefficients[2],
                                   SE = model3$rest.se[2],
                                   modelName = "Future conflict")
model4Frame_geodesic <- data.frame(Variable = "affected",
                                   Coefficient = model4$coefficients[2],
                                   SE = model4$rest.se[2],
                                   modelName = "Economic interdependence")

interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

allModelFrame_geodesic <- data.frame(rbind(model1Frame_geodesic, model2Frame_geodesic, model3Frame_geodesic, model4Frame_geodesic))
allModelFrame_geodesic$Variable <- c(1,2,3,4)
allModelFrame_geodesic$Variable <- factor(allModelFrame_travel$Variable, labels=c("Risk  preferences", "Ethnicization", "Future  conflict", "Economic  interdependence"))
levels(allModelFrame_geodesic$Variable) <- gsub("  ", "\n", levels(allModelFrame_geodesic$Variable))


### Combine to plot
allModelFram <- rbind(allModelFrame_travel, allModelFrame_adjacency, allModelFrame_geodesic)
allModelFram$matrix_style <- rep(c("Travel", "Adjacency", "Geodesic"),each=4)

pd = position_dodge(0.5)

figureA21 <- ggplot(allModelFram, aes(shape=matrix_style)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = pd) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval2,
                     ymax = Coefficient + SE*interval2),
                 lwd = 1/4, position = pd) + 
  geom_point(aes(x = Variable, y = Coefficient, shape = matrix_style),
             position = pd,fill = "WHITE", size = 3) + 
  coord_flip(ylim = c(-0.8,0.8)) + theme_bw() + 
  theme(legend.position="bottom") + 
  scale_shape_manual(values = c(23, 24, 25), name ="") +
  #ggtitle("Cooperation among Uzbeks") +  
  ylab("Z-Score")  + xlab("") +     
  theme(text = element_text(size=18, family="Times"))

# plot output
figureA21