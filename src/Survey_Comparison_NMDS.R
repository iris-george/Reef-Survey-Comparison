########## Survey Comparison NMDS Plots ##########


#### Set Up ####

setwd("/Users/irisgeorge/Documents/Green Lab/Lionfish/USRA 2021/Survey Comparison")

# Data:
SVCprey_data <- read_csv("SVCprey_model_data.csv")
SVCpred_data <- read_csv("SVCpred_model_data.csv")

# Packages:
library(dplyr)
library(tidyverse)
library(vegan)
library(klaR)


#### Ordination ####

# Select Columns Used in Models:
SVCprey_NMDS_data <- SVCprey_data[,c(2:8,12,14:17,19,32:36)]
SVCpred_NMDS_data <- SVCpred_data[,c(2:8,12,14:17,19,32:36)]

# not sure whether Gower or Bray-Curtis difference is going to be better here...

# Gower Distance:
SVCprey_NMDS <- metaMDS(SVCprey_NMDS_data, distance="gower", k=3)
trait_NMDS[["stress"]] #stress = 0.1012489
#check stress on the ordination

# Bray-Curtis Dissimilarity:
trait_NMDS <- metaMDS(NMDS_data, distance="bray", k=3)
trait_NMDS[["stress"]] #stress = 0.09647226
#stress < 0.1 is great, it means that our nMDS plots are meaningful and that we're not trying to fit a square peg through a round hole
#stress of > 0.2 means we have problems and we need to transform the data, reduce the data (remove species that might have vastly different abundances) 

























