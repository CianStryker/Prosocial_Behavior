################################################################
#### Figure A.16: Correlations between prosociality and    #####
#                  instrument conditional on expectations  #####
################################################################

# The figure shows the correlation between the index for prosociality
# towards the ingroup (on the y-axis) and the instrument (distance to the nearest APC,
# on the x-axis), conditional on respondents' expectations about future intergroup relations

rm(list=ls())

# load required libraries
library(readstata13)
library(ggplot2)

##### Cleaning
# read data
data <- read.dta13("./kyrgyzstan.dta")
data_uzbek <- data[data$ethnicity!="Kyrgyz",]

# scale ingroup-games variables
data_uzbek$pd_in_scale <- scale(as.integer(data_uzbek$pd_in))
data_uzbek$dg_in_scale <- scale(data_uzbek$dg_in)
data_uzbek$cooperation_index_in <- rowSums(cbind(data_uzbek$pd_in_scale, data_uzbek$dg_in_scale), na.rm=T)/2

# clean variable
data_uzbek$comm_relations <- as.factor(data_uzbek$comm_relations)
data_uzbek$comm_relations1 <- as.integer(data_uzbek$comm_relations) # necessary for geom_smooth

##### Figure
# plot figure
figureA16 <- ggplot(data = data_uzbek[!is.na(data_uzbek$comm_relations),], 
       aes(y = cooperation_index_in, x = apc_min_distance, color = comm_relations)) +
  geom_point() + 
  geom_smooth(data = data_uzbek[data_uzbek$comm_relations1 == 2,],
              aes(y = cooperation_index_in, x = apc_min_distance),
              method = "lm", se = F) +
  geom_smooth(data = data_uzbek[data_uzbek$comm_relations1 == 3,],
              aes(y = cooperation_index_in, x = apc_min_distance),
              method = "lm", se = F) +
  geom_smooth(data = data_uzbek[data_uzbek$comm_relations1 == 1,],
              aes(y = cooperation_index_in, x = apc_min_distance),
              method = "lm", se = F) +
  ylab("Prosociality towards ingroup") +
  xlab("Distance to APC") + 
  guides(colour=guide_legend(title="Expectation of future \n intergroup relations:")) +
  scale_colour_grey()
figureA16
