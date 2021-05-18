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

# Remove Silversides & Trumpetfish:
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order !="Syngnathiformes",] # remove trumpetfish
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$species_order !="Atheriniformes",] # remove silversides
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species_order !="Syngnathiformes",] # remove trumpetfish

# Remove Gray Snapper and Amberjack from SVC vs. Predator:
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species !="gray snapper",] # not consistently recorded in predator surveys
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$species !="amberjack",] # only schooling species

# Remove NA Values:
SVCprey_model_data <- na.omit(SVCprey_model_data)
SVCpred_model_data <- na.omit(SVCpred_model_data)

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
# AIC = 29847.64 --> better fit than model without shape
# sig pos covariates = octocoral, stony, size bin, max length, colourful, neutral, silvering
# sig neg covariates = cryptic behaviour, eel-like, elongated, fusiform 
vif(SVCprey_shape_model) 
# colouration GVIF = 5.382886
# shape GVIF = 5.650639

# Shape Boxplot: 
boxplot(SVCpred_model_data$log_difference~SVCpred_model_data$shape)



#### SVC vs. Predator: Shape Model ####

SVCpred_shape_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_shape_model) 
# model summary:
# AIC = 1692.525 --> better fit than previous model without shape
# sig pos covariates = silvering
# sig neg covariates = stony, size bin
vif(SVCpred_shape_model) 
# habitat GVIF = 6.832051
# position GVIF = 6.634015
# colouration GVIF = 21.218479
# depth GVIF = 5.079790
# shape GVIF = 9.864492


#### SVC vs. Prey: Model without Eels ####

# Remove Eels:
SVCprey_noeels <- SVCprey_model_data[SVCprey_model_data$species_order !="Anguilliformes",] # remove eels

# Model:
SVCprey_noeel_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin+nocturnal+position+max_length+colouration2+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_noeels) 
summary(SVCprey_noeel_model) 
# model summary:
# AIC = 29676.49 --> better fit than models with eels
# sig pos covariates = octocoral, stony, size bin, max length, colourful, neutral, silvering
# sig neg covariates = cryptic behaviour, elongated, fusiform 
vif(SVCprey_shape_model) 
# colouration GVIF = 5.382886
# shape GVIF = 5.650639

#### SVC vs. Prey: Functional Group Model ####

# Model:
SVCprey_functional_model <- lme(log_difference~size_bin+fnt_group, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_functional_model) # model summary
vif(SVCprey_functional_model) 

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
summary(SVCprey_sxc_model) # model summary
vif(SVCprey_sxc_model) # don't want GVIF > 5

# Size Bin x Colouration (with shape):
SVCprey_sxc_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_sxc_model2) # model summary
vif(SVCprey_sxc_model2) # don't want GVIF > 5

# Size Bin x Colouration Boxplot:
boxplot(SVCprey_model_data$log_difference ~ (SVCprey_model_data$colouration2)*(SVCprey_model_data$size_bin))
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$size_bin !=0,]

ggplot(SVCprey_model_data, aes(colouration2, log_difference, fill = size_bin)) + geom_boxplot(show.legend = TRUE) + theme_classic() + scale_fill_manual(name = "Colouration", values = c("yellow1", "gray65", "white", "blue", "red", "purple")) + xlab("Size Bin") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

ggplot(SVCprey_model_data, aes(behavior, log_difference, fill = colouration)) + geom_boxplot() + theme_classic() + scale_fill_manual(name = "Colouration", labels = c("Conspicuous", "Cryptic", "None"), values = c("yellow1", "gray65", "white")) + xlab("Aggregation Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")

# Size Bin x Shape (without colouration):
SVCprey_sxs_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_sxs_model) # model summary
vif(SVCprey_sxs_model) # don't want GVIF > 5

# Size Bin x Shape (with colouration):
SVCprey_sxs_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+colouration2, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCprey_model_data) 
summary(SVCprey_sxs_model2) # model summary
vif(SVCprey_sxs_model2) # don't want GVIF > 5

# Size Bin x Shape Boxplot:
boxplot(SVCprey_model_data$log_difference ~ (SVCprey_model_data$shape)*(SVCprey_model_data$size_bin))


#### SVC vs. Predator: Size Bin Interactions ####

# Size Bin x Colouration (without shape):
SVCpred_sxc_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxc_model) # model summary
vif(SVCpred_sxc_model) # don't want GVIF > 5

# Size Bin x Colouration (with shape):
SVCpred_sxc_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*colouration2+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+shape, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxc_model2) # model summary
vif(SVCpred_sxc_model2) # don't want GVIF > 5

# Size Bin x Colouration Boxplot:
boxplot(SVCpred_model_data$log_difference ~ (SVCpred_model_data$colouration2)*(SVCpred_model_data$size_bin))

# Size Bin x Shape (without colouration):
SVCpred_sxs_model <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
# NOT LIKING THIS
summary(SVCpred_sxs_model) # model summary
vif(SVCpred_sxs_model) # don't want GVIF > 5

# Size Bin x Shape (with colouration):
SVCpred_sxs_model2 <- lme(log_difference~habitat+octocoral+stony+relief_cm+size_bin*shape+nocturnal+position+max_length+behavior+cryptic_behaviour+average_depth+colouration2, random = list(~1|site, ~1|species_order), na.action = na.omit, SVCpred_model_data) 
summary(SVCpred_sxs_model2) # model summary
vif(SVCpred_sxs_model2) # don't want GVIF > 5

# Size Bin x Shape Boxplot:
boxplot(SVCpred_model_data$log_difference ~ (SVCpred_model_data$shape)*(SVCpred_model_data$size_bin))


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
SVCprey_dredge <- dredge(SVCprey_model1)
SVCprey_dredge
# Best model contains colouration, cryptic behaviour, habitat type, max length, shape, and size bin
# AICc = 29807.3, delta AIC = 2.42 (next model includes position)


#### SVC vs. Predator: Dredging ####
SVCpred_dredge <- dredge(SVCpred_model1)
SVCpred_dredge
# results are exactly the same as SVC vs. prey model???







