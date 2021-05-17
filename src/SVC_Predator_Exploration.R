########## SVC vs. Predator Survey Exploration ##########


#### Set Up ####

setwd("/Users/irisgeorge/Documents/Green Lab/Lionfish/USRA 2021/Survey Comparison")

# Data:
SVC <- read.csv("SVC_lengths.csv")
pred <- read.csv("pred_fish_data.csv")

# Packages:
library(plyr)


#### Silvering Species ####

# Species List:
unique(pred[c("species")])
# mutton snapper, cubera snapper, black margate, barracuda, gray snapper, amberjack 

# Mutton Snapper: 24 in predator, 13 in SVC
# Cubera Snapper: 1 in predator, 0 in SVC
# Black Margate: 2 in predator, 0 in SVC
# Barracuda: 17 in predator, 8 in SVC
# Gray Snapper: 4 in predator, 114 in SVC
# Amberjack: 6 in predator, 0 in SVC
