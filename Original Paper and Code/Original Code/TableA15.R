###########################################################
#### Tabelle A15: Vote shares in 2010 elections in    #####
####              victimized vs. non-victimized areas #####
###########################################################

rm(list=ls())

# load required libraries
library(stargazer)
library(eiCompare)
library(stringr)

## load data
data_voting <- read.dta13("voting_covered_precincts.dta")

# PANEL A: Simple comparison
A1 <- mapply(data_voting[, c("pct_AN","pct_AJ","pct_SDPK", "pct_AM", "pct_Res", "pct_BK", "pct_AS", "pct_Other")], FUN = function(x){
  weighted.mean(x[data_voting$affected == 0], 
                w = data_voting$totvote[data_voting$affected == 0], 
                na.rm = T)
})

A2 <- mapply(data_voting[, c("pct_AN","pct_AJ","pct_SDPK", "pct_AM", "pct_Res", "pct_BK", "pct_AS", "pct_Other")], FUN = function(x){
  weighted.mean(x[data_voting$affected == 1], 
                w = data_voting$totvote[data_voting$affected == 1], 
                na.rm = T)
})


# PANEL B: Uzbek majority
B1 <- mapply(data_voting[, c("pct_AN","pct_AJ","pct_SDPK", "pct_AM", "pct_Res", "pct_BK", "pct_AS", "pct_Other")], FUN = function(x){
  weighted.mean(x[data_voting$pct_uzb >= 0.5 & data_voting$affected == 0], 
                w = data_voting$totvote[data_voting$pct_uzb >= 0.5 & data_voting$affected == 0], 
                na.rm = T)
})


B2 <- mapply(data_voting[, c("pct_AN","pct_AJ","pct_SDPK", "pct_AM", "pct_Res", "pct_BK", "pct_AS", "pct_Other")], FUN = function(x){
  weighted.mean(x[data_voting$pct_uzb >= 0.5 & data_voting$affected == 1], 
                w = data_voting$totvote[data_voting$pct_uzb >= 0.5 & data_voting$affected == 1], 
                na.rm = T)
})

A1 <- A1*100
A2 <- A2*100
B1 <- B1*100
B2 <- B2*100


# PANEL C: Ecological inference
data_victimized <- subset(data_voting, data_voting$affected == 1)
data_nonvictimized <- subset(data_voting, data_voting$affected == 0)

# create character vectors required for function
parties <- c("pct_AN", "pct_AJ", "pct_SDPK", "pct_AM", "pct_Res", "pct_BK", "pct_AS", "pct_Other")
ethnic_groups <- c("~ pct_uzb", "~ pct_krg")
table_names <- c("EI: Pct UZB", "EI: Pct KRG")

# set seed for reproducibility
set.seed(12049)

# run ei function, first for non-victimized
C1 <- ei_est_gen(cand_vector=parties, race_group = ethnic_groups,
                 total = "totvote", data = data_nonvictimized, table_names = table_names)

# run ei function, for victimized
C2 <- ei_est_gen(cand_vector=parties, race_group = ethnic_groups,
                 total = "totvote", data = data_victimized, table_names = table_names)

# extract means
means_C1 = C1[c(1,3,5,7,9,11,13,15), 2]
means_C2 = C2[c(1,3,5,7,9,11,13,15), 2]

# extract se
se_C1 = C1[c(2,4,6,8,10,12,14,16), 2]
se_C2 = C2[c(2,4,6,8,10,12,14,16), 2]

# generate table
tableA15 <- cbind(A1, A2, B1, B2, means_C1, se_C1, means_C2, se_C2)

# clean table 
rownames(tableA15) <- str_replace(rownames(tableA15), "pct\\_", "")

# stargazer output
stargazer(tableA15, summary = F, digits = 1)
