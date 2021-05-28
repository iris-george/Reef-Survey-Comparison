########## SURVEY COMPARISON PROJECT HERBIVORE SUBSET ##########
########## 
##########
# This file subsets available survey data to look specifically at herbivorous 
# fish species and compares their density estimates between SVC and transect
# surveys. 
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


# Select Herbiviours Species ===================================================

# The following section filters the dataframe comparing all fish species 
# detected on SVC and transect surveys to contain only herbivorous species for
# further analysis.

# filter whole dataset for herbivores
herbivores <- filter(SVCprey_model_data, fnt_group == "herbivore")

# change size bin to categorical
herbivores$size_bin_char <- as.character(herbivores$size_bin_char)

# change cryptic behaviour to categorical
herbivores$cryptic_behaviour <- as.character(herbivores$cryptic_behaviour)


# Global Herbivore Linear Mixed Effects Model ==================================

# The following section creates a global linear mixed effects model to compare 
# herbivore density differences between SVC and transect surveys in relation to
# species, habitat, and survey traits. It explores collinearity by examining 
# VIF values of predictors and determines model fit using random effects plots,
# residual plots, qq plots, and model plots. 

# global lme model
herbivore_global <- lme(log_difference~habitat+octocoral+stony+relief_cm
                      +max_length+behavior+cryptic_behaviour
                      +average_depth+size_bin*shape+colouration2, 
                      random = list(~1|site, ~1|species_order), 
                      herbivores) 
# response is log density difference, random effects are site and species order
# removed colouration*size_bin interaction because of single levels in camo
# removed nocturnal because all are not
# removed position because all are demersal 

# model summary
summary(herbivore_global) 
# AIC = 6325.85
# significant predictors: octocoral, stony, max_length, cryptic_behaviour, 
# size_bin, fusiform, colourful, neutral, size_bin*fusiform

# covariate VIF values
vif(herbivore_global)
# habitat GVIF = 5.027517
# behaviour GVIF = 28.228145
# size bin GVIF = 5.784482
# shape GVIF = 38.104405


# Alternate Models =============================================================

# The following creates alternative models to the global model in order to 
# attempt to improve collinearity and model performance. 

# remove shape
herbivore_no_shape <- lme(log_difference~habitat+octocoral+stony+relief_cm
                        +max_length+behavior+cryptic_behaviour
                        +average_depth+colouration2, 
                        random = list(~1|site, ~1|species_order), 
                        herbivores) 
summary(herbivore_no_shape) # AIC = 6367.505 (worse performance)
vif(herbivore_no_shape) # habitat GVIF = 5.146758; all others <5
# DECREASES MODEL PERFORMANCE BUT IMPROVES COLLINEARITY 

# remove behaviour
herbivore_no_behav <- lme(log_difference~habitat+octocoral+stony+relief_cm
                        +max_length+cryptic_behaviour
                        +average_depth+size_bin*shape+colouration2, 
                        random = list(~1|site, ~1|species_order), 
                        herbivores) 
summary(herbivore_no_behav) # AIC = 6327.085 (similar performance)
vif(herbivore_no_behav) # shape GVIF = 9.891090 and size_bin GVIF = 9.891090
# DOES NOT CHANGE MODEL FIT AND IMPROVED COLLINEARITY 

# USE MODEL WITHOUT BEHAVIOUR! 


# Covariate Plots ==============================================================

# The following creates scatterplots and boxplots of all significant predictors 
# from the mixed linear effects model. 

# octocoral plot
plot(herbivores$log_difference ~ herbivores$octocoral)
# significantly positive

# stony coral plot
plot(herbivores$log_difference ~ herbivores$stony)
# significantly positive

# maximum length plot
plot(herbivores$log_difference ~ herbivores$max_length)
# significantly positive

# cryptic behaviour boxplot
ggplot(herbivores, aes(x = cryptic_behaviour, y = log_difference, 
                       fill = cryptic_behaviour)) +
  geom_boxplot() +
  theme_classic() + xlab("Presence of Cryptic Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# significantly negative

# size bin boxplot
ggplot(herbivores, aes(x = size_bin_char, y = log_difference, 
                       fill = size_bin_char)) +
  geom_boxplot() +
  theme_classic() + xlab("Size Bin") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# significantly negative (?)

# shape boxplot
ggplot(herbivores, aes(x = shape, y = log_difference, 
                       fill = shape)) +
  geom_boxplot() +
  theme_classic() + xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# fusiform significantly negative

# colouration boxplot
ggplot(herbivores, aes(x = colouration2, y = log_difference, 
                       fill = colouration2)) +
  geom_boxplot() +
  theme_classic() + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# colourful and neutral significantly negative (?)

# shape*size_bin interaction boxplot
ggplot(herbivores, aes(shape, log_difference, fill = size_bin_char)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "blue", "red", "green")) + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# size_bin*fusiform significantly positive
