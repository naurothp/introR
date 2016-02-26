library(ggplot2)
library(dplyr)

subsample <- filter(data, aq1>=0, !is.na(sex1)) #Folie 25
subsample <- filter(data, norm1>=0, !is.na(typ1), !is.na(sex1)) #Folie 43