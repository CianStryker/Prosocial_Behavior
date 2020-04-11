###################################################################################
####### Table A.10: Regression of the instrument on pre-treatment covariates ######
###################################################################################

rm(list=ls())

# load required libraries
library(readstata13)
library(stargazer)


# read data
data <- read.dta13("./kyrgyzstan.dta")

# recode variables
data$affected <- as.integer(data$affected)
data$affected <- data$affected - 1
data$pd_in <- as.integer(data$pd_in)
data$pd_out <- as.integer(data$pd_out)
data$father_edu <- as.integer(data$father_edu)

# generate new variables
data$distance <- data$apc_min_distance
data$wealth_psu_historic <- ifelse(data$neigh_status=="Very badly off", 1, ifelse(data$neigh_status=="Badly off", 2, ifelse(data$neigh_status=="Neither well off, nor badly off", 3, ifelse(data$neigh_status=="Well off", 4, ifelse(data$neigh_status=="Very well off", 5,99)))))
data$wealth_psu_historic <- as.double(data$wealth_psu_historic)
data$police_station_historic <- data$neigh_places_3
data$female <- as.integer(data$sex) - 1

# subset data set according to ethnic groups
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]
data_uzbek$distance <-data_uzbek$apc_min_distance

# aggregate data 
dataAgg <- aggregate(data_uzbek[,c("distance", "wealth_psu_historic", "nl09", "distance_healthfacilities", "police_station_historic", "leadership", "far", "ap_ratio_roads", "female", "age", "father_edu", "east_of_akbuura")], 
                     list(data_uzbek$id_psu),
                     mean)

# run regression of distance on pre-treatment covariates
selection_model <- lm(distance ~ nl09 + wealth_psu_historic + distance_healthfacilities + 
                        police_station_historic + leadership + far + ap_ratio_roads +
                        female + age + father_edu + east_of_akbuura, data = dataAgg)
summary(selection_model)

# table output
stargazer(selection_model, covariate.labels = c("Nighttime lights", "Historic wealth",
                                                "Hospital distance", "Police station",
                                                "Leadership", "Floor area ratio", "Street width",
                                                "Female",
                                                "Age", "Father's education",
                                                "East of main river"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001))


