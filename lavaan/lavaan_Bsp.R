###### lavaan - KFA/latente Modelle ######

#set wd
setwd("C:/Users/Alpha Omega/Documents/Studium/Psychologie/Master of Science Psychologie/2. FM - SS 2016/Datenanalyse/R/Sitzung 2/basic statistics")
rm(list=ls())


#Paketinstallation
install.packages("lavaan", dependencies = TRUE)

library(lavaan) 

## Beispiel 1: Konfirmatorische Faktorenanalyse ##
?HolzingerSwineford1939

#Modellspezifizierung
cfaModel <- '	visual	=~ x1 + x2 + x3
          		textual	=~ x4 + x5 + x6
          		speed 	=~ x7 + x8 + x9 '

#Fit indizes berechnen
fit <- cfa(cfaModel, data=HolzingerSwineford1939)

#Ergebnisausgabe
summary(fit, fit.measures=TRUE)

## Beispiel 2: Strukturgleichungsmodelle ##
?PoliticalDemocracy

semModel <- '
		          #measurement model
          			ind60 =~ x1 + x2 + x3
                dem60 =~ y1 + y2 + y3 + y4
          	    dem65 =~ y5 + y6 + y7 + y8
		          #regressions
                dem60 ~ ind60
                dem65 ~ ind60 + dem60
		          #residual correlations
                y1 ~~ y5
                y2 ~~ y4 + y6
                y3 ~~ y7
                y4 ~~ y8
                y6 ~~ y8
	          '

#Berechnung der Indizes + Ausgabe
fit <- sem(semModel, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)
