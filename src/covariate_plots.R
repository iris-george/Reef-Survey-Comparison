########## SURVEY COMPARISON PROJECT COVARIATE PLOTS ##########
########## 
##########
# This file creates scatterplots and boxplots of predictors from linear mixed
# effects models run to compare fish density differences between SVC, transect,
# and roving reef surveys in relation to species, habitat, and survey traits.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-31
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(ggplot2)
library(here)

# data
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_model_data.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_model_data.csv"))


# Dataframe Edits ==============================================================

# The following performs dataframe edits necessary for creation of accurate 
# plots. 

# change size bin to character variable
SVCprey_model_data$size_bin_char <- 
  as.character(SVCprey_model_data$size_bin_char)
SVCpred_model_data$size_bin_char <- 
  as.character(SVCpred_model_data$size_bin_char)

# change cryptic behaviour to character variable
SVCprey_model_data$cryptic_behaviour <- 
  as.character(SVCprey_model_data$cryptic_behaviour)
SVCpred_model_data$cryptic_behaviour <- 
  as.character(SVCpred_model_data$cryptic_behaviour)

# re-name cryptic behaviour column
SVCprey_model_data$cryptic_behaviour2 <- 
  ifelse(SVCprey_model_data$cryptic_behaviour == 1, 
                                 "cryptic behaviour", "none")

# re-order colouration levels
SVCprey_model_data$colouration2 <- 
  factor(SVCprey_model_data$colouration2, 
         levels = c("camouflage", "neutral", "silvering", "colourful"))


# SVC vs. Transect Survey Plots ================================================

# The following creates boxplots of significant predictors from the SVC vs. 
# transect survey linear mixed effects model. 

# cryptic behaviour boxplot
ggplot(SVCprey_model_data, aes(x = cryptic_behaviour2, y = log_difference, 
                               fill = cryptic_behaviour2)) + 
  geom_boxplot() +
  theme_classic() + xlab("Cryptic Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Set3") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# size bin boxplot
ggplot(SVCprey_model_data, aes(x = size_bin_char, y = log_difference, 
                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = FALSE) + 
  theme_classic() + 
  xlab("Size Bin") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) + 
  theme(axis.text= element_text(size = 18)) + 
  theme(legend.text = element_text(size = 18)) + 
  theme(legend.title = element_text(size = 20)) + 
  scale_fill_brewer(palette = "YlGnBu") + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40")

# colouration*size bin boxplot
ggplot(SVCprey_model_data, aes(colouration2, log_difference, 
                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(name = "Size Bin", palette = "YlGnBu") +
  geom_hline(yintercept = 0,
               linetype = "dashed",
               colour = "grey40")

# shape*size bin boxplot
ggplot(SVCprey_model_data, aes(shape, log_difference, 
                               fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(name = "Size Bin", palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


# SVC vs. Roving Survey Plots ==================================================

# The following creates scatter plots and boxplots of significant predictors 
# from the SVC vs. roving survey linear mixed effects model.

# stony coral scatterplot
ggplot(SVCpred_model_data, aes(x = stony, y = log_difference)) + 
  geom_point(size = 2) +
  theme_classic() + xlab("Percent Stony Coral") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  geom_smooth(method=lm, se = FALSE) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# size bin boxplot
ggplot(SVCpred_model_data, aes(x = size_bin_char, y = log_difference, 
                               fill = size_bin_char)) + 
  geom_boxplot() +
  theme_classic() + xlab("Size Bin") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "YlGnBu") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# colouration boxplot
ggplot(SVCpred_model_data, aes(x = colouration2, y = log_difference, 
                               fill = colouration2)) + 
  geom_boxplot() +
  theme_classic() + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_manual(name = "Colouration", values = c("goldenrod4", "wheat1", 
                                                     "paleturquoise3")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
