##############################################
###### Table A2: Descriptive statistics ######
##############################################

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

# generate new variables
data$distance <- data$apc_min_distance
data$wealth_psu_historic <- ifelse(data$neigh_status=="Very badly off", 1, 
                                   ifelse(data$neigh_status=="Badly off", 2, 
                                          ifelse(data$neigh_status=="Neither well off, nor badly off", 3, 
                                                 ifelse(data$neigh_status=="Well off", 4, 
                                                        ifelse(data$neigh_status=="Very well off", 5,99)))))
data$wealth_psu_historic <- as.double(data$wealth_psu_historic)
data$police_station_historic <- data$neigh_places_3
data$female <- as.integer(data$sex - 1)

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
data$coethnic_employer <- ifelse(data$employer==1 & data$ethnicity=="Uzbek", 1, 
                                 ifelse(data$employer==2 & data$ethnicity=="Kyrgyz", 1, 0))
data$coethnic_employer[is.na(data$coethnic_employer)] <- 0
data$risk <- scale(data$risk) 
data$comm_relations <- scale(as.numeric(data$comm_relations))

# generate new variables for table
data$sample <- 1
data$treat_kyrgyz <- ifelse(data$ethnicity=="Kyrgyz" & data$affected==1,1,0)
data$control_kyrgyz <- ifelse(data$ethnicity=="Kyrgyz" & data$affected==0,1,0)
data$treat_uzbek <- ifelse(data$ethnicity=="Uzbek" & data$affected==1,1,0)
data$control_uzbek <- ifelse(data$ethnicity=="Uzbek" & data$affected==0,1,0)

# recode some vars
data$female <- as.integer(data$sex)-1
data$education_primary <- ifelse(data$education=="No formal education" | data$education=="Incomplete primary school" | data$education=="Complete primary school", 1,0)
data$education_secondary <- ifelse(data$education=="Religious school / Madrasa" | data$education=="Incomplete secondary" | data$education=="Complete secondary", 1,0)
data$education_tertiary <- ifelse(data$education=="Some university, no degree" | data$education=="University-level education, with degree", 1,0)
data$marital_married <- ifelse(data$marital_status=="Married",1,0)
data$marital_single <- ifelse(data$marital_status=="Single",1,0)
data$marital_other <- ifelse(data$marital_status=="Divorced/separated" | data$marital_status=="Widowed",1,0)
data$job_employee <- ifelse(data$occupation=="Full time employee (30 hours a week or more)" | data$occupation=="Part time employee (less than 30 hours a week)", 1,0)
data$job_self_employed <- ifelse(data$occupation=="Self-employed / business", 1,0)
data$job_retired <- ifelse(data$occupation=="Retired",1,0)
data$job_housewife <- ifelse(data$occupation=="Housewife",1,0)
data$job_student <- ifelse(data$occupation=="Student",1,0)
data$job_unemployed <- ifelse(data$occupation=="Unemployed",1,0)
data$job_other <- ifelse(data$occupation=="Other",1,0)
data$lived_abroad <- ifelse(data$livedabroad=="Yes", 1,0)
data$owns_accommodation <- ifelse(data$owns_house_apartment=="Yes",1,0)
data$lives_apartment <- ifelse(data$housing=="Apartment",1,0)
data$income <- data$income/70

# subset data set for descriptives
data_uzbek <- data[which(data$ethnicity=="Uzbek"),]
data_kyrgyz <- data[which(data$ethnicity=="Kyrgyz"),]

### Select vars
treats <- c("sample", "treat_uzbek", "control_uzbek", "treat_kyrgyz", "control_kyrgyz")
vars <- c("female", "age", "education_primary", "education_secondary", "education_tertiary", 
          "marital_married", "marital_single", "marital_other", "children",
          "job_employee", "job_self_employed", "job_retired", "job_housewife", "job_student", "job_unemployed", "job_other",
          "household_size", "migrants", "income", "owns_accommodation", "lived_abroad", "lives_apartment", "state_index", "economy_index")

# helper function
get_stats <- function(var, treat, data) {
  c(sum(!is.na(data[, var][data[, treat] == 1])), mean(data[, var][data[, treat] ==1 ], na.rm = T))
}

# loop
btable <- lapply(treats, function(y) t(sapply(vars, function(x) {
  get_stats(x, y, data = data)
})))

# to data, add n, colnames
btable <- data.frame(do.call(cbind, btable))

# times 100
perc <- c("female", "education_primary", "education_secondary", "education_tertiary", 
          "marital_married", "marital_single", "marital_other",
          "job_employee", "job_self_employed", "job_retired", "job_housewife", "job_student", "job_unemployed", "job_other",
          "owns_accommodation", "lived_abroad", "lives_apartment")
btable[rownames(btable) %in% perc, c(2,4,6,8,10)] <- btable[rownames(btable) %in% perc, c(2,4,6,8,10)] * 100

# add varnames
btable$variable <- rownames(btable)

# change colnames
cn <- paste0(rep(treats, each = 2), rep(c("_N", "_mean"), 2))
colnames(btable) <- c(cn)
btable <- btable[, -ncol(btable)]
colnames(btable) <- c("Sample size", "Sample mean","Uzbek Treatment size", "Uzbek Treatment mean", "Uzbek Control size", "Uzbek Control mean", "Kyrgyz Treatment size", "Kyrgyz Treatment mean", "Kyrgyz Control size", "Kyrgyz Control mean")

# change row names
rownames(btable) <- c("Female", "Age", "Education: Primary", "Education: Secondary", "Education: Tertiary", 
                      "Marital: Married", "Marital: Single", "Marital: Other", "Children",
                      "Job: Employee", "Job: Self-Employed", "Job: Retired", "Job: Housewive", "Job: Student", "Job: Unemployed", "Job: Other",
                      "Household size", "Migrants", "Income", "Owns residence", "Has lived abroad", "Lives in apartment", "State index", "Economy index")

# show table 
btable
