########## SURVEY COMPARISON PROJECT MODELS WITH ALTERNATE SIZE BINS ##########
########## 
##########
# This file creates global linear mixed effects models to compare fish density 
# differences between SVC and transect surveys and SVC and roving surveys using 
# species, habitat, and survey traits as predictors. It then performs a dredge 
# on each global model to determine which predictors result in the best model
# fit, based on AICc values. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-03
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(nlme)
library(car)
library(arm)
library(MuMIn)
library(here)

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_model_data.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_model_data.csv"))
SVC_lengths <- read_csv(here("./clean_data/SVC_data.csv"))
prey_lengths <- read_csv(here("./clean_data/prey_fish_data.csv"))
pred_lengths <- read_csv(here("./clean_data/pred_fish_data.csv"))


# Determining Size Bin 6 Classification ========================================

# The following determines the median lenth of size class 6 (>30cm).

# extract lengths > 30cm 
SVC_6 <- SVC_lengths %>% filter(SVC_lengths$SVC_max_tl > 30)
prey_6 <- prey_lengths %>% filter(prey_lengths$prey_tl > 30)
pred_6 <- pred_lengths %>% filter(pred_lengths$pred_tl > 30)

# select length and species columns
SVC_6 <- SVC_6[,c(37,41)]
prey_6 <- prey_6[,3:4]
pred_6 <- pred_6[,3:4]

# re-name length columns
SVC_6 <- rename(SVC_6, length = SVC_max_tl)
prey_6 <- rename(prey_6, length = prey_tl)
pred_6 <- rename(pred_6, length = pred_tl)

# bind together
size_class_6 <- bind_rows(SVC_6, prey_6, pred_6)

# find median
median(size_class_6$length) # = 45cm 


# Size Bin Reclassifying =======================================================

# The following reclassifies size bins using the median length that each 
# contains. 

# size bin 1 = 3 cm
# size bin 2 = 8 cm
# size bin 3 = 13 cm
# size bin 4 = 18 cm 
# size bin 5 = 25 cm
# size bin 6 = 45 cm

# SVC vs. transect survey
SVCprey_model_data$size_bin_lengths <- 
  ifelse(SVCprey_model_data$size_bin == 1, 3, 
  ifelse(SVCprey_model_data$size_bin == 2, 8, 
  ifelse(SVCprey_model_data$size_bin == 3, 13, 
  ifelse(SVCprey_model_data$size_bin == 4, 18, 
  ifelse(SVCprey_model_data$size_bin == 5, 25, 
  ifelse(SVCprey_model_data$size_bin == 6, 45, NA))))))

# SVC vs. roving survey
SVCpred_model_data$size_bin_lengths <- 
  ifelse(SVCpred_model_data$size_bin == 1, 3, 
  ifelse(SVCpred_model_data$size_bin == 2, 8, 
  ifelse(SVCpred_model_data$size_bin == 3, 13, 
  ifelse(SVCpred_model_data$size_bin == 4, 18, 
  ifelse(SVCpred_model_data$size_bin == 5, 25, 
  ifelse(SVCpred_model_data$size_bin == 6, 45, NA))))))


# SVC vs. Transect: Global Model ===============================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCpreylengths_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                        size_bin_lengths*colouration2+nocturnal+position+
                        max_length+behavior+cryptic_behaviour+average_depth+
                        size_bin_lengths*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 
# response is log density difference, random effects are site and species order

# model summary
summary(SVCpreylengths_global) 

# covariate VIF values
vif(SVCpreylengths_global) 

# random effects plot
plot(ranef(SVCpreylengths_global))

# residuals plot
res_SVCprey_global = residuals(SVCprey_global)
plot(res_SVCprey_global) 

# qq plot
qqnorm(res_SVCprey_global) 
qqline(res_SVCprey_global)

# model plot
plot(SVCprey_global) 


# SVC vs. Transect: Dredging ===================================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to transect survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCprey_global_dredge <- dredge(SVCprey_global)
SVCprey_global_dredge
# First model (AICc = 29562.4): colouration, cryptic behaviour, habitat, max 
# length, shape, size bin, colouration*size bin, shape*size bin
# Second model ( AICc = : aggregation behaviour, colouration, cryptic behaviour, 
# habitat, max length, shape, size bin, colouration*size bin, shape*size bin
# Delta AIC = 1.82 between them and 2.10 between second and third model (third 
# has position)

# subset dredge
SVCprey_dredge_sub <- subset(SVCprey_global_dredge, delta < 4) 

# model average 
SVCprey_model_average <- model.avg(SVCprey_dredge_sub)
summary(SVCprey_model_average)

# confidence intervals of predictors
confint(SVCprey_model_average)

# save dredge output 
saveRDS(SVCprey_global_dredge, here("./outputs/SVCprey_global_dredge.rds"))


# SVC vs. Roving: Global Model =================================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and roving surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCpred_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                        nocturnal+max_length+cryptic_behaviour+ average_depth+
                        colouration2+size_bin_lengths, 
                      random = list(~1|site, ~1|species_order),
                      SVCpred_model_data) 

# model summary 
summary(SVCpred_global) 
# AIC = 1695.676
# sig pos covariates = silvering
# sig neg covariates = stony, size bin

# covariate VIF values
vif(SVCpred_global)
# habitat GVIF = 6.832051
# position GVIF = 6.634015
# depth GVIF = 5.079790
# colouration GVIF = 21.218479
# shape GVIF = 9.864492

# random effects plot
plot(ranef(SVCpred_global))

# residuals plot
res_SVCpred_global = residuals(SVCpred_global)
plot(res_SVCpred_global)

# qq plot
qqnorm(res_SVCpred_global) 
qqline(res_SVCpred_global) 

# model plot
plot(SVCpred_global) 


# SVC vs. Roving: Dredging =====================================================

# The following performs a dredge on the global linear mixed effects model 
# comparing SVC fish densities to roving survey fish densities in order to 
# determine which combination of predictors results in the most likely model.

# dredge
SVCpred_dredge <- dredge(SVCpred_global)
SVCpred_dredge
# First model (AICc = 1631.5): colouration, shape
# Second model (AICc = 1631.6, delta AICc = 0.11): cryptic behaviour, shape 
# Third model (AICc = 1633.1, delta AICc = 1.64): colouration, position, shape 
# Fourth model (AICc = 1633.4, delta AICc = 1.90): colouration  

# subset dredge
SVCpred_dredge_sub <- subset(SVCpred_dredge, delta < 4) 

# model average 
SVCpred_model_average <- model.avg(SVCpred_dredge_sub)
summary(SVCpred_model_average)

# covariate confidence intervals
confint(SVCpred_model_average)

# save dredge results 
saveRDS(SVCpred_dredge, here("./outputs/SVCpred_dredge.rds"))