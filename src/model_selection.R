########## SURVEY COMPARISON PROJECT MODEL CREATION AND SELECTION ##########
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
# DATE OF CREATION: 2021-05-28
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
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_dataframe.csv"))


# SVC vs. Transect: Global Model ===============================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCprey_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                      size_bin_lengths*colouration+nocturnal+position+
                      max_length+behavior+cryptic_behaviour+average_depth+
                      size_bin_lengths*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 

# model summary
summary(SVCprey_global) 

# covariate VIF values
vif(SVCprey_global) 

# random effects plot
plot(ranef(SVCprey_global))

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
SVCprey_dredge <- dredge(SVCprey_global)
SVCprey_dredge

# save dredge output 
saveRDS(SVCprey_dredge, here("./outputs/SVCprey_global_dredge.rds"))

# subset dredge
SVCprey_dredge_sub <- subset(SVCprey_dredge, delta < 4) 

# model average 
SVCprey_model_average <- model.avg(SVCprey_dredge_sub)
summary(SVCprey_model_average)

# save model average
saveRDS(SVCprey_model_average, here("./outputs/SVCprey_drege_average.rds"))

# confidence intervals of predictors
SVCprey_confidence <- confint(SVCprey_model_average)
summary(SVCprey_confidence )

# save confidence intervals
saveRDS(SVCprey_confidence, here("./outputs/SVCprey_dredge_CI.rds"))


# SVC vs. Roving: Global Model =================================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and roving surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCpred_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                      nocturnal+max_length+cryptic_behaviour+colouration+
                      size_bin_lengths, 
                      random = list(~1|site, ~1|species_order),
                      SVCpred_model_data) 

# model summary 
summary(SVCpred_global) 

# covariate VIF values
vif(SVCpred_global)

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

# save dredge results 
saveRDS(SVCpred_dredge, here("./outputs/SVCpred_dredge.rds"))

# subset dredge
SVCpred_dredge_sub <- subset(SVCpred_dredge, delta < 4) 

# model average 
SVCpred_model_average <- model.avg(SVCpred_dredge_sub)
summary(SVCpred_model_average)

# save model average
saveRDS(SVCpred_model_average, here("./outputs/SVCpred_dredge_average.rds"))

# covariate confidence intervals
SVCpred_confidence <- confint(SVCpred_model_average)
summary(SVCpred_confidence)

# save confidence intervals
saveRDS(SVCpred_confidence, here("./outputs/SVCpred_dredge_CI.rds"))