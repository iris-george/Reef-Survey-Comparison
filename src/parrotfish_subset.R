########## Parrotfish Subset ##########


#### Set-Up ####
setwd("/Users/irisgeorge/Documents/Green Lab/Lionfish/USRA 2021/Survey Comparison")

# Data:
SVC <- read.csv("SVC_sizebin_abundance.csv")
prey <- read.csv("prey_sizebin_abundance.csv")
traits <- read.csv("fish_traits.csv")
SVCprey_data <- read.csv("SVCprey_dataframe.csv")
SVC_lengths <- read.csv("SVC_lengths.csv")
prey_lengths <- read.csv("prey_fish_data.csv")

# Packages:
library(plyr)
library(dplyr)
library(tidyverse)


#### Abundance Differences ####

# Merge Binomial to Survey Data:
binomial <- traits[,c(3:4)]
binomial <- binomial %>% rename(species = common_name)
SVC <- join(SVC, binomial, by = NULL, typ = "full", match = "first")
prey <- join(prey, binomial, by = NULL, typ = "full", match = "first")

# Extract Genus:
SVC$genus <- word(SVC$Binomial, 1)
prey$genus <- word(prey$Binomial, 1)

# Extract Parrotfish:
SVC_parrotfish <- filter(SVC, genus == "Scarus"| genus == "Sparisoma")
prey_parrotfish <- filter(prey, genus == "Scarus"| genus == "Sparisoma")

# Extract Parrotfish Size Bin = 1:
SVC_parrotfish_s1 <- filter(SVC_parrotfish, size_bin == 1)
sum(SVC_parrotfish_s1$SVC_abundance)
prey_parrotfish_s1 <- filter(prey_parrotfish, size_bin == 1)
sum(prey_parrotfish_s1$prey_abundance)

# Extract Parrotfish Size Bin = 1 & 2:
SVC_parrotfish_s2 <- filter(SVC_parrotfish, size_bin == 1 | size_bin == 2)
sum(SVC_parrotfish_s2$SVC_abundance)
prey_parrotfish_s2 <- filter(prey_parrotfish, size_bin == 1 | size_bin == 2)
sum(prey_parrotfish_s2$prey_abundance)


#### Density Differences #### 

# Extract Genus:
SVCprey_data$genus <- word(SVCprey_data$binomial, 1)

# Extract Parrotfish:
SVCprey_parrotfish <- filter(SVCprey_data, genus == "Scarus"| genus == "Sparisoma")

# Extract Parrotfish Size Bin = 1:
SVCprey_parrotfish_s1 <- filter(SVCprey_parrotfish, size_bin == 1)
mean(SVCprey_parrotfish_s1$SVC_density)
mean(SVCprey_parrotfish_s1$prey_density)
mean(SVCprey_parrotfish_s1$SVC_prey_difference)

# Extract Parrotfish Size Bin = 1 & 2:
SVCprey_parrotfish_s2 <- filter(SVCprey_parrotfish, size_bin == 1 | size_bin ==2)
mean(SVCprey_parrotfish_s2$SVC_density)
mean(SVCprey_parrotfish_s2$prey_density)
mean(SVCprey_parrotfish_s2$SVC_prey_difference)


#### < 3 cm Individuals ####

# Merge Binomial:
SVC_lengths <- join(SVC_lengths, binomial, by = NULL, typ = "full", match = "first")
prey_lengths <- join(prey_lengths, binomial, by = NULL, typ = "full", match = "first")

# Extract Genus:
SVC_lengths$genus <- word(SVC_lengths$Binomial, 1)
prey_lengths$genus <- word(prey_lengths$Binomial, 1)

# Extract Parrotfish:
SVC_parrotfish_lenghts <- filter(SVC_lengths, genus == "Scarus"| genus == "Sparisoma")
prey_parrotfish_lenghts <- filter(prey_lengths, genus == "Scarus"| genus == "Sparisoma")

# Extract <3cm Individuals:
SVC_parrotfish_3 <- filter(SVC_parrotfish_lenghts, SVC_tl <= 3)
sum(SVC_parrotfish_3$SVC_abundance)
prey_parrotfish_3 <- filter(prey_parrotfish_lenghts, prey_tl <= 3)
nrow(prey_parrotfish_3)








