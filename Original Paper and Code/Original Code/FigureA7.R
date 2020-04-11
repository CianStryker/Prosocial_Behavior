#########################################################################
###### Figure A.7: Spatial autocorrelation: Geodesic distance (km) ######
#########################################################################

rm(list=ls())

# load required libraries
library(ape)
library(data.table)
library(dplyr)
library(ggplot2)
library(igraph)
library(kableExtra)
library(plyr)
library(raster)
library(rgdal)
library(readstata13)
library(spdep) # conflict with older version of dplyr if error message is shown update dplyr to 0.8.0
library(stargazer)
library(stringr)
library(tseries)


# read data
data <- read.dta13("./kyrgyzstan.dta")

##### Cleaning
# recode variables
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

#Aggregate data on PSU
dataAgg <- aggregate(data_uzbek[,c("cooperation_index")], 
                     list(data_uzbek$id_psu),
                     mean, na.rm=T)
names(dataAgg) <- c("psu", "cooperation_index")

##### Figure

### load km distance matrix (calculated with fields package)
matrix <- read.csv("z.distance_matrix.csv")
distance_matrix <- as.matrix(matrix)

### Get adjacency matrix
shape <- shapefile("sampling_area_psus_mollweide.shp")
shape <- shape[shape$id_psu %in% dataAgg$psu,]
nb <- poly2nb(shape)
names(nb) <- shape$id_psu
lw <- nb2listw(nb, zero.policy=TRUE,  style = "B" )
adjacency_matrix <- listw2mat(lw) # Note, a correlogram makes no sense for this binary measure
colnames(adjacency_matrix) <- seq(1:196)
rownames(adjacency_matrix) <- seq(1:196)

# calculate autocorrelations
autocorr <- function(w,x,dist){
  aa <- ceiling(max(w)/dist)
  dists <- seq(0,aa*dist,dist)
  cors <- NULL
  for(i in 1:aa){
    w1 <- ifelse(w > dists[i] & w <= dists[i+1], 1, 0) 
    w2 <- w1
    for(j in 1:dim(w1)[1]){
      nu <- sum(w1[j,])
      if(nu>0){
        w2[j,] <- w1[j,]/nu
      }  
    }
    lag <- w2 %*% x
    cors <- c(cors,cor(x,lag))
  }
  return(cors)
}


### Distance matrix
matrix_chosen <- distance_matrix

# at 0.15 km
dist_chosen = 0.15
ac1 <- autocorr(w=matrix_chosen, x=dataAgg$cooperation_index, dist = dist_chosen)

# MC analysis
it <- 1000
mc <- matrix(NA,nrow=it,ncol=length(ac1))
for(i in 1:it){
  dataAgg$rand <- sample(dataAgg$cooperation_index, length(dataAgg$cooperation_index),replace=F)
  mc[i,] <- autocorr(w=matrix_chosen, x=dataAgg$rand, dist = dist_chosen)
}


ac1 <- data.frame(cbind(ac1,seq(dist_chosen,dist_chosen*length(ac1),dist_chosen)))
ac1 <- cbind(ac1,t(apply(mc,2,quantile, probs = c(0.025,0.975))))
names(ac1) <- c("ac","dist","lci","uci")

figureA.7 <- ggplot(ac1, aes(dist, ac)) +
  geom_point(colour = "black", size = 3) +
  geom_line(colour = "red") +
  scale_x_continuous('Kilometer',limits=c(0,dist_chosen*length(ac1[,1]))) + 
  scale_y_continuous('Autocorrelation',limits=c(-0.95,0.95)) +
  theme_bw() + 
  geom_hline(yintercept=0) +   
  geom_smooth(aes(ymin = lci, ymax = uci), stat="identity",fill="grey",colour="black") +
  theme(text = element_text(size=18, family="Times"))


# plot output
figureA.7
