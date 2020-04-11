#############################################
###### Figure 8: Realized estimates #########
#############################################

# Setup
rm(list=ls())
options(max.print=10000)

library(foreign)
library(plyr)
library(readstata13)
library(AER)
library(data.table)
library(ggplot2)
library (dplyr)

data <- read.dta13("./kyrgyzstan.dta")


data_uzbek <- data[which(data$ethnicity=="Uzbek"),]
data_uzbek$pd_in_scale <- scale(data_uzbek$pd_in)
data_uzbek$dg_in_scale <- scale(data_uzbek$dg_in)
data_uzbek$pd_out_scale <- scale(data_uzbek$pd_out)
data_uzbek$dg_out_scale <- scale(data_uzbek$dg_out)
data_uzbek$cooperation_index <- rowSums(cbind(data_uzbek$pd_in_scale, data_uzbek$dg_in_scale, data_uzbek$pd_out_scale, data_uzbek$dg_out_scale), na.rm=T)/4
data_uzbek$distance <- data_uzbek$apc_min_distance


################################
###### IV estimates as reference
################################

#Combined sample
ols <- lm(cooperation_index ~ affected, data = data_uzbek)
iv <- ivreg(cooperation_index ~ affected | distance , data = data_uzbek)
instrument <- lm(cooperation_index ~ distance , data = data_uzbek)

#################################
#### Insheet distance matrix ####
#################################

# Insheet distances from any given PSU to 9500 randomly chosen APC locations 
DistMat <- as.data.frame(read.csv("distances_ri.csv", header = TRUE, sep =","))[-1]

# Remove "X" from name
colnames(DistMat) <- c(na.omit(as.numeric(unlist(strsplit(as.matrix(colnames(DistMat)), "X"))))) 

# Obtain PSU names from survey data as character to match to DistMat
psus <- c(as.character(unique(data_uzbek$id_psu)))

# Reduce distance matrix to entries for PSUs where we sampled Uzbek respondents
sample_psus <- names(DistMat)[(names(DistMat) %in% psus)]
DistMat <- DistMat[, sample_psus]

# Add information whether East of Ak-Buura river
eoa <- as.data.frame(read.csv("ri_east_of_akbuura.csv", header = TRUE))[2]
DistMat$eoa <-eoa

# Create matrices that only contain values for either east or west
DistMat_east <- subset(DistMat, eoa==1)
DistMat_west <- subset(DistMat, eoa==0)
DistMat = subset(DistMat, select = -c(eoa) )
DistMat_west = subset(DistMat_west, select = -c(eoa) )
DistMat_east = subset(DistMat_east, select = -c(eoa) )

# Fill up the vectors so that both have a length of 5,000
set.seed(1000)
DistMat_east <- rbind(DistMat_east, DistMat_east[sample(169), ])
set.seed(1000)
DistMat_west <- rbind(DistMat_west, DistMat_west[sample(331),])


############
#### RI ####
############

# shuffle distance matrices 
set.seed(1000)
DistMat_east <- DistMat_east[sample(nrow(DistMat_east)),]
set.seed(1000)
DistMat_west <- DistMat_west[sample(nrow(DistMat_west)),]

# random sampling with replacement, 10,000 draws from values east and west of the river 
n <- 10000
set.seed(1000)
rand_vals <- sample(c(1:5000), n, replace = TRUE)

# store coefficient in vector with elements equal to draws
estim <- matrix(0, nrow=n, ncol=1)

# loop through all values 
j <- 0
for (i in rand_vals){
  j <- j + 1
  # take row from both distance matrices (east and west), transpose, find minimum distance, add column for matching 
  dist_i_west <- as.data.frame(t(DistMat_west[i,]))
  dist_i_east <- as.data.frame(t(DistMat_east[i,]))
  dist_i <- cbind(dist_i_west, dist_i_east)
  # Choose APC location in east or west that is closest to a given interview location
  dist_i <- as.data.frame(apply(dist_i, 1, FUN=min))
  dist_i <- setDT(dist_i, keep.rownames = TRUE)
  colnames(dist_i)=c("id_psu", "drivedist")
  
  # merge with data frame (match m:1)
  combinedData <- join(data_uzbek, dist_i, by='id_psu', type='left', match='all')
  
  # Estimate reduced form regression coefficient
  ivest_i <- lm(cooperation_index ~ drivedist, data=combinedData)  

  # Save coef in matrix
  estim[j,1] <- estim[j,1] + coef(ivest_i)[2] 
}

hist(estim)

# Plot distribution of coefficients, calculate confidence interval and test

# Create density plot
estim <- estim*-1  #convert to negative scale "closeness to barracks"
data_combined <- as.data.frame(estim)
mean(estim)
figure8 <- ggplot(data_combined, aes(x=V1)) +  geom_density(bw = 0.02) +
  theme_bw() + 
  ylab("Density")  + xlab("Estimate") +     
  theme(text = element_text(size=18, family="Times")) +
  scale_x_continuous(limits = c(-0.2,0.2)) + 
  geom_vline(xintercept=instrument$coefficients[2]*-1, linetype=2, color = "grey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
# plot output
figure8


#Significance

# reference value (-0.1109755 )
instrument$coefficients[2]*-1

# 90 percent percentile two-sided, 95% percentile one-sided (statistically sign. at 5% level)
print(c(quantile(estim, .05),quantile(estim, .95))) 

# P-value/ number of estimates smaller than observed coefficient 
percentile <- function(x,perc) ecdf(x)(perc)
p <- (percentile(estim,-0.1109755))
p



