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
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_model_data.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_model_data.csv"))


# SVC vs. Transect: Global Model ===================================================

# In the following, a linear mixed model is created to compare fish density 
# differences between SVC and transect surveys in response to predictors 
# including species traits, habitat traits, and survey traits. All traits of 
# interest are included in the global model. Collinearity is explored through 
# the VIF values for each predictor, and model fit is determined through 
# random effects plots, residual plots, qq plots, and model plots. 

# global lme model
SVCprey_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                      size_bin*colouration2+nocturnal+position+max_length+
                      behavior+cryptic_behaviour+average_depth+size_bin*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 
# response is log density difference, random effects are site and species order

# model summary
summary(SVCprey_global) 
# AIC = 29598.41
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, silvering, 
# max length, shoaling, size bin*elongated, size bin*fusiform
# sig neg covariates = habitat, cryptic behaviour, elongated, fusiform, 
# size bin*colourful, size bin*neutral, size bin*silvering

# covariate VIF values
vif(SVCprey_global) 
# size bin GVIF = 24.897697 
# colouration GVIF = 409.450179 
# shape GVIF = 40.288049
# size bin*colouration GVIF = 1266.492002
# size bin*shape GVIF = 31.333791

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
SVCprey_global_dredge <- dredge(SVCprey_global)
SVCprey_global_dredge
# First model (AICc = 29562.4): colouration, cryptic behaviour, habitat, max 
# length, shape, size bin, colouration*size bin, shape*size bin
# Second model ( AICc = : aggregation behaviour, colouration, cryptic behaviour, 
# habitat, max length, shape, size bin, colouration*size bin, shape*size bin
# Delta AIC = 1.82 between them and 2.10 between second and third model (third 
# has position)

# model average 
SVCprey_model_average <- model.avg(SVCprey_global_dredge)
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
                      colouration2+size_bin, 
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

# model average 
SVCpred_model_average <- model.avg(SVCpred_global_dredge)
summary(SVCpred_model_average)

# covariate confidence intervals
confint(SVCpred_model_average)

# save dredge results 
saveRDS(SVCpred_dredge, here("./outputs/SVCprey_dredge.rds"))