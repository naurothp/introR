###### --------------- Science and Religion PEW-Data --------------- ###### 
###### ---------------   Introduction to R Workshop  --------------- ###### 
###### ---------------          Peter Nauroth        --------------- ###### 

library(foreign)
library(dplyr)

# 1. Read in Data

pewData <- read.spss("data/Science09c.sav", to.data.frame = TRUE)

# 2. Select relevant Data
pewData <- dplyr::select(pewData,                    #data
                         q26,                        #science conflict
                         sciknow5,                   #science knwoledge
                         age, sex, educ, income, ideo, relig)  #demographics

str(pewData)

# 3. Datatype Transformations and Renaming
names(pewData)[1] <- "srconflict"
pewData$srconflict <- as.numeric(pewData$srconflict)
pewData$srconflict[pewData$srconflict==3] <- NA

pewData$sciknow <- as.numeric(pewData$sciknow5)
pewData$sciknow5 <- NULL # delete variable

pewData$age[pewData$age==99] <- NA
pewData$ideo <- as.numeric(pewData$ideo)
pewData$ideo[pewData$ideo==9] <- NA
pewData$educ <- as.numeric(pewData$educ)
pewData$educ[pewData$educ==9] <- NA
pewData$income <- as.numeric(pewData$income)
pewData$income[pewData$income==10] <- NA
pewData$relig[pewData$relig==99] <- NA
pewData$relig <- as.factor(ifelse(is.na(pewData$relig), NA, 
                           ifelse(pewData$relig %in% levels(pewData$relig)[c(9,10)], "non-religious", 
                           "religious")))
str(pewData)
summary(pewData)


# 4. Inference statistics

# a) 
myTTest <- with(pewData, t.test(srconflict~relig))
myTTest

# b)
myRegression <- lm(srconflict ~ age + sex + educ + income + ideo + sciknow + relig, data=pewData)
summary(myRegression)

# c)
myInteraction <- lm(srconflict ~ age + sex + educ + income + ideo + scale(sciknow)*relig, data=pewData)
summary(myInteraction)
