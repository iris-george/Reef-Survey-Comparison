########## Survey Comparison: Linear Mixed Effects Models ##########


#### Set Up ####

setwd("/Users/irisgeorge/Documents/GitHub/Reef-Survey-Comparison/data")

# Data:
SVCprey <- read.csv("SVCprey_dataframe.csv")
SVCpred <- read.csv("SVCpred_dataframe.csv")
colouration <- read.csv("species_colouration.csv")
shape <- read.csv("species_shape.csv")
fnt_group <- read.csv("species_trophic.csv")

# Packages:
library(plyr)
library(dplyr)
library(nlme)
library(car)
library(ggplot2)
library(dotwhisker)
library(arm)
library(MuMIn)


#### Dataframe Edits ####

# Remove Juveniles:
colouration <- filter(colouration, lifestage == "adult") # remove juveniles
colouration <- colouration[,c(1,3)] # remove lifestage column

# Join Colouration:
SVCprey_data <- join(SVCprey, colouration, by = NULL, type = "left", match = "all")
SVCpred_data <- join(SVCpred, colouration, by = NULL, type = "left", match = "all")

# Join Shape:
shape <- filter(shape, lifestage == "adult") # remove juveniles
shape <- shape[,c(1,3)]
SVCprey_data <- join(SVCprey_data, shape, by = NULL, type = "left", match = "all")
SVCpred_data <- join(SVCpred_data, shape, by = NULL, type = "left", match = "all")

# Join Functional Group:
fnt_group <- filter(fnt_group, lifestage == "adult") # remove juveniles
fnt_group <- fnt_group[,c(1,3)]
SVCprey_data <- join(SVCprey_data, fnt_group, by = NULL, type = "left", match = "all")
SVCpred_data <- join(SVCpred_data, fnt_group, by = NULL, type = "left", match = "all")

# Rename Order:
SVCprey_model_data <- SVCprey_data %>% rename(species_order = order)
SVCpred_model_data <- SVCpred_data

# Remove Silversides, Trumpetfish, and Flounder:
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order !="Syngnathiformes",] # remove trumpetfish
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order !="Atheriniformes",] # remove silversides
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order !="Pleuronectiformes",] # remove flounders
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species_order !="Syngnathiformes",] # remove trumpetfish

# Remove Gray Snapper and Amberjack from SVC vs. Predator:
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species !="gray snapper",] # not consistently recorded in predator surveys
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species !="amberjack",] # only schooling species

# Remove NA Values:
SVCprey_model_data <- na.omit(SVCprey_model_data)
SVCpred_model_data <- na.omit(SVCpred_model_data)

# Remove Size Bin = 0:
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$size_bin !=0,]
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$size_bin !=0,]

# Change Variables to Character:
summary(SVCprey_model_data)
SVCprey_model_data$size_bin <- as.character(SVCprey_model_data$size_bin)
summary(SVCpred_model_data)
SVCpred_model_data$size_bin <- as.character(SVCpred_model_data$size_bin)

write.csv(SVCprey_model_data, "SVCprey_model_data.csv")
write.csv(SVCpred_model_data, "SVCpred_model_data.csv")



#### SVC vs. Prey: Linear Mixed Effects Model ####

# Full Model:
SVCprey_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model) 
# model summary: 
# AIC = 29939.76
# sig pos covariates = octocoral, stony, size bin, demersal, max_length, colourful, neutral, silvering, solitary
# sig neg covariates = cryptic behaviour 
vif(SVCprey_model) 
# all under GVIF = 5

# Random Effects Plot:
plot(ranef(SVCprey_model))

# Residuals Plots:
res_SVCprey_model = residuals(SVCprey_model)
plot(res_SVCprey_model) # residuals plot
qqnorm(res_SVCprey_model) 
qqline(res_SVCprey_model) # qq plot
plot(SVCprey_model) 

# Significant Covariate Plots:
plot(SVCprey_model_data$log_difference~SVCprey_model_data$octocoral)
plot(SVCprey_model_data$log_difference~SVCprey_model_data$stony)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$size_bin)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$nocturnal)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$position)
plot(SVCprey_model_data$log_difference~SVCprey_model_data$max_length)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$colouration2)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$behavior)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$cryptic_behaviour)

# Dot-and-Whisker Plot:
dwplot(SVCprey_model)
# link for aesthetics: https://cran.r-project.org/web/packages/dotwhisker/vignettes/dotwhisker-vignette.html


#### SVC vs. Predator: Linear Mixed Effects Model ####

# Full Model:
SVCpred_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) # behaviour not included because all species are solitary 
summary(SVCpred_model) 
# model summary:
# AIC = 1696.275
# sig pos covariates = silvering
# sig neg covariates = stony, size bin
vif(SVCpred_model) 
# habitat GVIF = 6.779778
# colouration GVIF = 17.144453
# depth GVIF = 5.029028

# Random Effects Plot:
plot(ranef(SVCpred_model))

# Residuals Plots:
res_SVCpred_model = residuals(SVCpred_model)
plot(res_SVCpred_model) # residuals plot
qqnorm(res_SVCpred_model) 
qqline(res_SVCpred_model) # qq plot
plot(SVCpred_model) 

# Significant Covariate Plots:
plot(SVCpred_model_data$log_difference~SVCpred_model_data$stony)
boxplot(SVCpred_model_data$log_difference~SVCpred_model_data$size_bin)
boxplot(SVCpred_model_data$log_difference~SVCpred_model_data$colouration2)

# Dot-and-Whisker Plot:
dwplot(SVCpred_model)


#### SVC vs. Prey: Shape Model ####

SVCprey_shape_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_shape_model) 
# model summary:
# AIC = 29625.5 --> better fit than model without shape
# sig pos covariates = octocoral, stony, size bin, max length, colourful, neutral, silvering, shoaling, solitary, compressiform, elongated, fusiform, globiform
# sig neg covariates = habitat type, cryptic behaviour
vif(SVCprey_shape_model) 
# colouration GVIF = 6.583216
# behaviour GVIF = 5.076665
# shape GVIF = 6.310282

# Shape Boxplot: 
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$shape)



#### SVC vs. Predator: Shape Model ####

SVCpred_shape_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_shape_model) 
# model summary:
# AIC = 1683.789 --> better fit than previous model without shape
# sig pos covariates = silvering, fusiform
# sig neg covariates = stony, size bin
vif(SVCpred_shape_model) 
# habitat GVIF = 6.905395
# position GVIF = 6.696934
# max length = 5.106350
# colouration GVIF = 21.925347
# depth GVIF = 5.106133
# shape GVIF = 10.005806

# Shape Boxplot: 
boxplot(SVCpred_model_data$log_difference~SVCpred_model_data$shape)


#### SVC vs. Prey: Model without Eels ####

# Remove Eels:
SVCprey_noeels <- SVCprey_model_data[SVCprey_model_data$species_order !="Anguilliformes",] # remove eels

# Model:
SVCprey_noeel_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_noeels) 
summary(SVCprey_noeel_model) 
# model summary:
# AIC = 29457.02 --> better fit than models with eels
# sig pos covariates = octocoral, stony, size bin, max length, colourful, neutral, silvering, shoaling, solitary
# sig neg covariates = habitat type, cryptic behaviour, elongated, fusiform, globiform
vif(SVCprey_shape_model) 
# colouration GVIF = 6.583216
# behaviour GVIF = 5.076665
# shape GVIF = 6.310282


#### SVC vs. Prey: Functional Group Model ####

# Model:
SVCprey_functional_model <- lme(log_difference~size_bin+fnt_group, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_functional_model) 
# model summary:
# AIC = 30262.5
# sig pos covariates = size bin, herbivore
# sig neg covariates = omnivore, planktivore
vif(SVCprey_functional_model) 
# all under GVIF = 5

# Random Effects Plot:
plot(ranef(SVCprey_functional_model))

# Residuals Plots:
res_SVCprey_fnt_model = residuals(SVCprey_functional_model)
plot(res_SVCprey_fnt_model) # residuals plot
qqnorm(res_SVCprey_fnt_model) 
qqline(res_SVCprey_fnt_model) # qq plot
plot(SVCprey_functional_model) 

# Covariate Boxplots:
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$size_bin)
boxplot(SVCprey_model_data$log_difference~SVCprey_model_data$fnt_group)



#### SVC vs. Predator: Functional Group Model ####

# ALL SPECIES ARE CARNIVORES! 


#### SVC vs. Prey: Size Bin Interactions ####

# Size Bin x Colouration (without shape):
SVCprey_sxc_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_sxc_model) 
# model summary
# AIC = 29898.08
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, silvering, demersal, max length, solitary
# sig neg covariates = cryptic behaviour, size bin*colourful, size bin*neutral, size bin*silvering
vif(SVCprey_sxc_model)
# size bin GVIF = 13.645754
# colouration GVIF = 234.809002

# Size Bin x Colouration (with shape):
SVCprey_sxc_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_sxc_model2) 
# model summary:
# AIC = 29607.8
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, silvering, max length, shoaling, solitary, compressiform, elongated, fusiform, globiform
# sig neg covariates = cryptic behaviour, eel-like, elongated, fusiform, size bin*colourful, size bin*neutral, size bin*silvering
vif(SVCprey_sxc_model2) 
# size bin GVIF = 4.658874e+06
# colourful GVIF = 8.250041e+02
# behaviour GVIF = 5.452498e+00
# shape GVIF = 9.540549e+00

# Size Bin x Colouration Boxplot:
boxplot(SVCprey_model_data$log_difference ~ (SVCprey_model_data$colouration2)*(SVCprey_model_data$size_bin))

ggplot(SVCprey_model_data, aes(colouration2, log_difference, fill = size_bin)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "blue", "red", "green")) + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# Size Bin x Shape (without colouration):
SVCprey_sxs_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_noeels) 
summary(SVCprey_sxs_model) # model summary
vif(SVCprey_sxs_model) # don't want GVIF > 5

# Size Bin x Shape (with colouration):
SVCprey_sxs_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+colouration2, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_noeels) 
summary(SVCprey_sxs_model2) # model summary
vif(SVCprey_sxs_model2) # don't want GVIF > 5

# Size Bin x Shape Boxplot:
boxplot(SVCprey_model_data$log_difference ~ (SVCprey_model_data$shape)*(SVCprey_model_data$size_bin))

ggplot(SVCprey_noeels, aes(shape, log_difference, fill = size_bin)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "blue", "red", "green")) + xlab("Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


#### SVC vs. Predator: Size Bin Interactions ####

# Size Bin x Colouration (without shape):
SVCpred_sxc_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxc_model) # model summary
vif(SVCpred_sxc_model) # don't want GVIF > 5

# Size Bin x Colouration (with shape):
SVCpred_sxc_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxc_model2) # model summary
vif(SVCpred_sxc_model2) # don't want GVIF > 5

# Size Bin x Colouration Boxplot:
boxplot(SVCpred_model_data$log_difference ~ (SVCpred_model_data$colouration2)*(SVCpred_model_data$size_bin))

ggplot(SVCpred_model_data, aes(colouration2, log_difference, fill = size_bin)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "red", "blue", "green")) + xlab("Colouration") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# Size Bin x Shape (without colouration):
SVCpred_sxs_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : contrasts can be applied only to factors with 2 or more levels
# only one observation of anguilliform of size bin 4
summary(SVCpred_sxs_model) # model summary
vif(SVCpred_sxs_model) # don't want GVIF > 5

# Size Bin x Shape (with colouration):
SVCpred_sxs_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+colouration2, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxs_model2) # model summary
vif(SVCpred_sxs_model2) # don't want GVIF > 5

# Size Bin x Shape Boxplot:
boxplot(SVCpred_model_data$log_difference ~ (SVCpred_model_data$shape)*(SVCpred_model_data$size_bin))

ggplot(SVCprey_model_data, aes(shape, log_difference, fill = size_bin)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Size Bin", values = c("yellow1", "gray65", "white", "blue", "red", "green")) + xlab("Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")


#### SVC vs. Prey: Backwards Model Selection ####

# Full Model:
SVCprey_model1 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), SVCprey_model_data) 
summary(SVCprey_model1) # model summary
vif(SVCprey_model1) # don't want GVIF > 5

# Remove Nocturnal:
SVCprey_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model2) # model summary
vif(SVCprey_model2) # don't want GVIF > 5

# Remove Position:
SVCprey_model3 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model3) # model summary
vif(SVCprey_model3) # don't want GVIF > 5

# Remove Relief:
SVCprey_model4 <- lme(log_difference~habitat+octocoral+stony+size_bin+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model4) # model summary
vif(SVCprey_model4) # don't want GVIF > 5

# Remove Average Depth:
SVCprey_model5 <- lme(log_difference~habitat+octocoral+stony+size_bin+max_length+colouration2+behavior+cryptic_behaviour+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model5) # model summary
vif(SVCprey_model5) # don't want GVIF > 5

# Remove Aggregation Behaviour:
SVCprey_model6 <- lme(log_difference~habitat+octocoral+stony+size_bin+max_length+colouration2+cryptic_behaviour+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_model6) # model summary
vif(SVCprey_model6) # don't want GVIF > 5

# ALL COVARIATES ARE SIGNIFICANT NOW, STOP HERE?


#### SVC vs. Predator: Backwards Model Selection ####

# Full Model:
SVCpred_model1 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), SVCpred_model_data) 
summary(SVCpred_model1) # model summary
vif(SVCpred_model1) 

# Remove Nocturnal:
SVCpred_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model2) # model summary
vif(SVCpred_model2) 

# Remove Cryptic Behaviour:
SVCpred_model3 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+position+max_length+colouration2+behavior+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model3) # model summary
vif(SVCpred_model3) 
# Difference between this and previous model's AIC is 2.113; very close 

# Remove Relief:
SVCpred_model4 <- lme(log_difference~habitat+octocoral+stony+size_bin+position+max_length+colouration2+behavior+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model4) # model summary
vif(SVCpred_model4) 

# Remove Habitat:
SVCpred_model5 <- lme(log_difference~octocoral+stony+size_bin+position+max_length+colouration2+behavior+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model5) # model summary
vif(SVCpred_model5) 

# Remove Shape:
SVCpred_model6 <- lme(log_difference~octocoral+stony+size_bin+position+max_length+colouration2+behavior+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_model6) # model summary
vif(SVCpred_model6)
# Removing shape decreases model fit (increases AIC) --> should I stop there?


#### SVC vs. Prey: Dredging ####

# Global Model:
SVCprey_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+size_bin*shape, random = list(~1|site, ~1|species_order), SVCprey_noeels) 
summary(SVCprey_global) 
# model summary:
# AIC = 29598.41
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, silvering, max length, shoaling, size bin*elongated, size bin*fusiform
# sig neg covariates = habitat, cryptic behaviour, elongated, fusiform, size bin*colourful, size bin*neutral, size bin*silvering
vif(SVCprey_global) 
# size bin GVIF = 24.897697 
# colouration GVIF = 409.450179 
# shape GVIF = 40.288049
# size bin*colouration GVIF = 1266.492002
# size bin*shape GVIF = 31.333791

# Dredging: 
SVCprey_dredge <- dredge(SVCprey_model1) # model without all covariates but no interactions
SVCprey_dredge
# Best model: colouration, cryptic behaviour, habitat type, max length, shape, and size bin
# AICc = 29807.3, delta AIC = 2.42 (next model includes position)
saveRDS(SVCprey_dredge, "SVCprey_mini_dredge.rds")
saveRDS(setosa_model, here('./output/setosa_model_object.rds'))

SVCprey_global_dredge <- dredge(SVCprey_global)
SVCprey_global_dredge
model_average <- model.avg(SVCprey_global_dredge)
summary(model_average)
confint(model_average)

# First model (AICc = 29562.4): colouration, cryptic behaviour, habitat, max length, shape, size bin, colouration*size bin, shape*size bin
# Second model ( AICc = : aggregation behaviour, colouration, cryptic behaviour, habitat, max length, shape, size bin, colouration*size bin, shape*size bin
# Delta AIC = 1.82 between them and 2.10 between second and third model (third has position)
saveRDS(SVCprey_global_dredge, "SVCprey_global_dredge.rds")

#### SVC vs. Predator: Dredging ####

# Global Model:
SVCpred_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+colouration2+shape+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) 
summary(SVCpred_global) 
# model summary:
# AIC = 1695.676
# sig pos covariates = silvering
# sig neg covariates = stony, size bin
vif(SVCpred_global)
# habitat GVIF = 6.832051
# position GVIF = 6.634015
# depth GVIF = 5.079790
# colouration GVIF = 21.218479
# shape GVIF = 9.864492

# Dredging: 
SVCpred_dredge <- dredge(SVCpred_global)
SVCpred_dredge
# First model (AICc = 1631.5): colouration, shape
# Second model (AICc = 1631.6, delta AICc = 0.11): cryptic behaviour, shape 
# Third model (AICc = 1633.1, delta AICc = 1.64): colouration, position, shape 
# Fourth model (AICc = 1633.4, delta AICc = 1.90): colouration  
saveRDS(SVCpred_dredge, "SVCprey_dredge.rds")


#### SVC vs. Predator: Presence/Absence Model ####

# Creating presence/absence values:
SVCpred_model_data$pres_abs <- ifelse(SVCpred_model_data$SVC_density > 0 & SVCpred_model_data$pred_density > 0, 0, ifelse(SVCpred_model_data$SVC_density > 0 & SVCpred_model_data$pred_density <= 0, 1, ifelse(SVCpred_model_data$SVC_density <= 0 & SVCpred_model_data$pred_density > 0, -1, NA)))
# 0 = observed on SVC and pred surveys
# 1 = observed only on SVC surveys
# -1 = observed only on pred surveys 

# Model:
SVCpred_presabs <- lme(pres_abs~habitat+octocoral+stony+relief_cm+nocturnal+position+max_length+cryptic_behaviour+average_depth+colouration2+shape+size_bin, random = list(~1|site, ~1|species_order), SVCpred_model_data) 
summary(SVCpred_presabs) 
vif(SVCpred_presabs)

# Boxplots:
boxplot(SVCpred_model_data$pres_abs ~ SVCpred_model_data$colouration2)
boxplot(SVCpred_model_data$pres_abs ~ SVCpred_model_data$shape)
boxplot(SVCpred_model_data$pres_abs ~ SVCpred_model_data$size_bin)
