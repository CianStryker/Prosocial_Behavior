#####################################################
###### Figure A.11: Effect of Riot Destruction ######
######    on Prosocial Behavior (matching)     ######
#####################################################

rm(list=ls())

# load required libraries
library(ggplot2)
library(MatchIt)
library(readstata13)
library(gridExtra)

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



##### Figure
# set seed 
set.seed(10)

# Estimate propensity scores
# Run logit
m_ps <- glm(affected ~ economy_index + state_index + social_cap_retro + access_index, data=data_uzbek)

# Extract propensities
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     affected = m_ps$model$affected)

# Look at propensity scores by treatment type
# labs <- paste("", c("Victimized", "Not Victimized"))
# prs_df %>%
#   mutate(affected = ifelse(affected == 1, labs[1], labs[2])) %>%
#   ggplot(aes(x = pr_score)) +
#   geom_histogram(color = "white") +
#   facet_wrap(~affected) +
#   xlab("Probability of being victimized") +
#   theme_bw()

# Throw out missings
ecls_nomiss <- data_uzbek %>%  # MatchIt does not allow missing values
  dplyr::select(cooperation_index, affected, economy_index, state_index, social_cap_retro, access_index) %>%
  na.omit()

# clean data
data_uzbek_nonmiss <- data_uzbek[,c("cooperation_index", "affected", "economy_index", "state_index", "social_cap_retro", "access_index", "pd_in_scale", "dg_in_scale", "pd_out_scale", "dg_out_scale")] 
data_uzbek_nonmiss <- data_uzbek_nonmiss[complete.cases(data_uzbek_nonmiss), ]

# match observations 
mod_match <- matchit(affected ~ economy_index + state_index + social_cap_retro + access_index,
                     method = "nearest", data=data_uzbek_nonmiss, caliper=0.05)
dta_m <- match.data(mod_match)
dim(dta_m)

### Estimate models, using difference in means
model1 <- lm(pd_in_scale ~ affected, data = dta_m)
model2 <- lm(dg_in_scale ~ affected, data = dta_m)
model3 <- lm(pd_out_scale ~ affected, data = dta_m)
model4 <- lm(dg_out_scale ~ affected, data = dta_m)
model5 <- lm(cooperation_index ~ affected, data = dta_m)

# extract coefficients and standard errors
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "PD ingroup")[2,]
model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "DG ingroup")[2,]
model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "PD out-group")[2,]
model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "DG out-group")[2,]
model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Index")[2,]

# rowbind data (coefficients and standard errors) from regressions
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame, model4Frame, model5Frame))
allModelFrame$Variable <- c(1,2,3,4, 5)
allModelFrame$Variable <- factor(allModelFrame$Variable, labels=c("Prisoner's Dilemma  Ingroup", "Dictator Game  Ingroup", "Prisoner's Dilemma  Outgroup", "Dictator Game  Outgroup", "Prosociality-  index"))
levels(allModelFrame$Variable) <- gsub("  ", "\n", levels(allModelFrame$Variable))

# set parameters for confidence interval
interval1 <- -qnorm((1-0.90)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# set color for plot
myColors <- c("#000000", "#000000", "#000000", "#000000", "#000000")

# Plot
figureA11 <- ggplot(allModelFrame, aes(colour = as.factor(Variable))) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/4, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  coord_flip(ylim = c(-0.8,0.2)) + theme_bw() + theme(legend.position="none") + 
  ylab("")  + xlab("") +     
  theme(text = element_text(size=18, family="Times")) +
  scale_color_manual(values=myColors)

# output
figureA11