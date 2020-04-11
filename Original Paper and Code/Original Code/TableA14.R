############################################################
###### Table A14: Descriptive statistics (follow-up) #######
############################################################


# read data
setwd("/Users/McKarlo/Dropbox/Research/Kyrgyzstan/2. Analysis/apsr_replication/new_files/")
data <- read.dta13("kyrgyzstan.dta")


rm(list=ls())

# load required libraries
library(readstata13)
library(stargazer)
library(stringr)

# read data
followup <- read.dta13("kyrgyzstan.dta")
followup = subset(followup, select = c(ethnicity, affected, 
                                 tsbarracks, tsfamfled, tsfamreturn, tsmarryotherethn,
                                 tsoutsideosh, tssuppcoethn, tssuppgov, tssuppngo) )
followup_uzbek <- followup[which(followup$ethnicity == "Uzbek"),]

tableA14 <- mapply(followup_uzbek[, c("tsbarracks", "tsmarryotherethn", "tssuppcoethn", 
                          "tsoutsideosh", "tsfamfled", "tsfamreturn")],
       FUN = function(x){c(sum(!is.na(x)),
                           mean(x, na.rm = T), 
                           sum(!is.na(x[followup_uzbek$affected == 1])), 
                           mean(x[followup_uzbek$affected == 1], na.rm = T),
                           sum(!is.na(x[followup_uzbek$affected == 0])), 
                           mean(x[followup_uzbek$affected == 0], na.rm = T))}) %>% t()

tableA14 <- as.data.frame(tableA14)
rownames(tableA14) <- c("Aware of barracks in vicinity", 
                        "Non-coethnic Muslim spouse (0-3)",
                        "Financially helped by coethnics",
                        "Fled from Osh", "Family member fled (#)",
                        "Family member returned (#)")
stargazer(tableA14, summary = F, digits = 2)
