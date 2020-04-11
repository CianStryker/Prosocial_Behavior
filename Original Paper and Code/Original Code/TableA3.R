###################################
##### Table A.3: Measurement ######
###################################

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
data$sex <- as.integer(data$sex)
data$father_edu <- as.integer(data$father_edu)


### clean vars
data$pd_in <- (data$pd_in - 1) * 100
data$pd_out <- (data$pd_out - 1) * 100 
data$wealth_psu_historic <- ifelse(data$neigh_status=="Very badly off", 1, 
                                   ifelse(data$neigh_status=="Badly off", 2, 
                                          ifelse(data$neigh_status=="Neither well off, nor badly off", 3, 
                                                 ifelse(data$neigh_status=="Well off", 4, 
                                                        ifelse(data$neigh_status=="Very well off", 5,99)))))
data$wealth_psu_historic <- as.double(data$wealth_psu_historic)
data$police_station_historic <- data$neigh_places_3
data$police_station_historic <- data$police_station_historic*100
data$far <- (1 - data$far) * 100

# generate new variables for table
data$sample <- 1
data$treat_kyrgyz <- ifelse(data$ethnicity=="Kyrgyz" & data$affected==1,1,0)
data$control_kyrgyz <- ifelse(data$ethnicity=="Kyrgyz" & data$affected==0,1,0)
data$treat_uzbek <- ifelse(data$ethnicity=="Uzbek" & data$affected==1,1,0)
data$control_uzbek <- ifelse(data$ethnicity=="Uzbek" & data$affected==0,1,0)

# subset data set for descriptives
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]



### Select vars
treats <- c("sample", "treat_uzbek", "control_uzbek")
vars <- c("pd_in", "pd_out", "dg_in", "dg_out", "nl09", "wealth_psu_historic", "police_station_historic",
          "distance_healthfacilities", "leadership", "far", "ap_ratio_roads")

vars %in% colnames(data)

# helper function
get_stats <- function(var, treat, data) {
  c(sum(!is.na(data[, var][data[, treat] == 1])), mean(data[, var][data[, treat] ==1 ], na.rm = T))
}

# loop
btable <- lapply(treats, function(y) t(sapply(vars, function(x) {
  get_stats(x, y, data = data_uzbek)
})))

# to data, add n, colnames
btable <- data.frame(do.call(cbind, btable))

# rename rownames
rownames(btable) <- c("PD ingroup", "PD outgroup", "DG ingroup (Soms)", "DG outgroup (Soms)",
                      "Nighttime lights (0-61)", "Historic wealth (1-5)", "Police station",
                      "Hospital distance (km)", "Leadership (1-5)", "Floor area ratio",
                      "Street width (2-18)")

# rename colnames 
colnames(btable) <- c("Uzbek sample size", "Uzbek sample mean", "Uzbek treatment size", 
                      "Uzbek treatment mean", "Uzbek control size", "Uzbek control mean")

# print table A.3
btable
