###########Working Directory bestimmen und Bibliotheken einlesen###########

#(setwd("E:\\Dropbox\\Grundlegende_Statistik")) 


#l?sche den Workspace
rm(list=ls()) 

library(Hmisc)
library(foreign)
library(dplyr) 
library(abind)

###################################################################################################
############################################Vorbereitung###########################################
###################################################################################################

#Einlesen des Datensatzes
dfGedaechtnis <- read.spss(file="gedaechtnis.sav", to.data.frame = TRUE)

#Umbenennen der einzelnen Variablen
names(dfGedaechtnis)[1] <- "bed"
names(dfGedaechtnis)[2] <- "sex"
names(dfGedaechtnis)[3] <- "abinote"
names(dfGedaechtnis)[4] <- "anzWoerter"
names(dfGedaechtnis)[5] <- "alter"
names(dfGedaechtnis)[6] <- "leistungsOri"

#Umkodieren des Geschlechts wegen Problemen mit Umlauten in R
dfGedaechtnis$sex <- as.factor(ifelse(dfGedaechtnis$sex=="weiblich", "weiblich", "maennlich"))

#Verk?rzung der Bedingung zur ?bersichtlicheren Darstellung
dfGedaechtnis$bed <- {as.factor(ifelse(dfGedaechtnis$bed=="strukturelle Verarbeitung","strukturell",
                                       ifelse(dfGedaechtnis$bed=="emotionale Verarbeitung", "emotional",
                                              "bildhaft")))}



###################################################################################################
########################################Deskriptive Statistik######################################
###################################################################################################

# Mittelwert
mean(dfGedaechtnis$anzWoerter)

# getrimmter Mittelwert
# Mittelwert ohne die oberen und unteren 10% der Daten
mean(dfGedaechtnis$anzWoerter, trim=.1)
# Mittelwert ohne die oberen und unteren 25% der Daten
mean(dfGedaechtnis$anzWoerter, trim=.25)

# Median
median(dfGedaechtnis$anzWoerter)

# Modalwert
which.max(table(dfGedaechtnis$anzWoerter))

# 25% und 75% Quantil
quantile(dfGedaechtnis$anzWoerter, probs=.25)
quantile(dfGedaechtnis$anzWoerter, probs=.75)

# Zusammenfassung der deskriptiven Statistik
summary(dfGedaechtnis$anzWoerter)

# Varianz und Standardabweichung der Anzahl erinnerter W?rter
var(dfGedaechtnis$anzWoerter)
sd(dfGedaechtnis$anzWoerter)


###################################################################################################
############################################H?ufigkeiten###########################################
###################################################################################################


# H?ufigkeitstabelle f?r die Variable Geschlecht
table(dfGedaechtnis$sex)

# H?ufigkeitstabelle f?r die Variable Anzahl erinnerter W?rter
table(dfGedaechtnis$anzWoerter)

# Zur ?bersichtlicheren Darstellung bei kontinuierlichen Variablen: Kategorisierung der Variable in Gruppen
# Kategorisierung der Variable Anzahl erinnerter W?rter in 4 Gruppen nach Quartilen
anzWoerter_kategorien <- cut(x=dfGedaechtnis$anzWoerter, breaks=c(0,10,14,17,27),labels=c("0-10 Woerter", "11-14 Woerter", "15-17 Woerter", "17-27 Woerter"))
table(anzWoerter_kategorien)

# Aufgabe1: Erstelle eine H?ufigkeitstabelle der Variable Abinote kategorisiert nach Quartilen.

breaksNote <- c(1,1.4,1.8,2.3,3.3)
abinote_kategorien <- cut(x=dfGedaechtnis$abinote, breaks=breaksNote)#,labels=as.character(breaksNote))
table(abinote_kategorien)

#####################################################################################################
############################################Kreuztabellen############################################
#####################################################################################################


###########Erl?uterung der Funktion f?r Kreuztabellen durch schrittweise Erweiterung###########

#einfache Kreuztabelle ohne Erweiterungen
myContingencyTableRow <- function (x,y) {table(x, y)}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Aus absoluten Werten werden relative H?ufigkeiten in Bezug auf die einzelne Zeile durch prop.table mit dem 
#Parameter 1 (eine 2 w?rde die relative H?ufigkeit bezogen auf die Spalte angeben)
myContingencyTableRow <- function (x,y) {prop.table(table(x, y),1)}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Erg?nzung um die Spalten- und Zeilensummen durch addmargins
myContingencyTableRow <- function (x,y) {addmargins(prop.table(table(x, y),1))}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Runden auf vier Stellen nach dem Komma durch round mit dem Paremter digits=4
myContingencyTableRow <- function (x,y) {round(addmargins(prop.table(table(x, y),1)), digits =4)}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Multiplikation mit 100 zur ?bersichtlicheren Darstellung in Prozentwerten
myContingencyTableRow <- function (x,y) {round(addmargins(prop.table(table(x, y),1)), digits =4)*100}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Erweiterung um die M?glichkeit einer dritten Dimension -->Wenn "z" fehlt wird die gleiche Funktion wie bisher aufgerufen
#wenn "z" vorhanden ist wird die bisher bekannte Funktion einfach um einen dritten Parameter erg?nzt
myContingencyTableRow <- function (x,y,z) {if(missing(z))
{round(addmargins(prop.table(table(x, y),1)), digits =4)*100}else
{round(addmargins(prop.table(table(x, y, z),1)), digits =4)*100}}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Verteilung der Abinoten aufgeteilt nach Geschlecht
#Problem: sehr un?bersichtlich durch viele Kategorien
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$abinote)

#?bersichtlicher durch Verwendung einer in Quartile unterteilten Variablen
myContingencyTableRow (dfGedaechtnis$sex, abinote_kategorien)


#Aufgabe2: Erstellt eine ?bersichtliche Kreuztabelle zum Zusammenhang
#von Geschlecht und Anzahl der erinnerten W?rter
anzWoerterKategorien <- cut(x=dfGedaechtnis$anzWoerter, breaks=quantile(dfGedaechtnis$anzWoerter))#,labels=as.character(breaksNote))
table(anzWoerterKategorien)
myContingencyTableRow (dfGedaechtnis$sex, anzWoerterKategorien)





#####################################################################################################
############################################Korrelationen############################################
#####################################################################################################

#Neue Pakete installieren und laden
#install.packages("ppcor")
#install.packages("stats")
library(ppcor)
#library(stats)

#?berpr?fen auf Missing Values
unique(dfGedaechtnis$bed)
unique(dfGedaechtnis$sex)
unique(dfGedaechtnis$anzWoerter)
unique(dfGedaechtnis$alter)
unique(dfGedaechtnis$leistungsOri)
unique(dfGedaechtnis$abinote)

#Geschlecht numerisch
dfGedaechtnis$sex_numerisch <- as.numeric(dfGedaechtnis$sex)

##Korrelationen

#Korrelation Abinote und Alter
cor.test(dfGedaechtnis$abinote, dfGedaechtnis$alter)

#andere M?glichkeit
x <- dfGedaechtnis$abinote
y <- dfGedaechtnis$alter
z <- dfGedaechtnis$anzWoerter
cor.test(x, y)

#Ver?nderung der Argumente -> z.B. einseitige Testung
cor.test(x, y, alternative = c("greater"), method = c("pearson"))


#Aufgabe3.1: Wie korreliert die Abinote mit der Anzahl erinnerter W?rter? 
#Beachte: Das Konfidenzniveau soll bei 90% liegen.
cor.test(x,z, conf.level	= .90)

#Aufgabe 3.2: Wie korreliert das Geschlecht mit der Anzahl der erinnerten W?rter?
#Beachte: Das Konfidenzniveau soll bei 95% liegen.
cor.test(y,z)

##Partialkorrelationen
#Partialkorrelation von zwei Variablen (x, y) bei Kontrolle f?r eine dritte Variable (z)
m <- as.data.frame(na.omit(cbind(x,y,z)))

pcor.test(m$x, m$y, m$z)


#Aufgabe 3.3.: Wie ?ndert sich der Zusammenhang von Geschlecht und Anzahl erinnerter W?rter
#bei Kontrolle f?r Abinote?


#Aufgabe 3.4: Wie sieht der Zusammenhang von Alter und Anzahl erinnerter W?rter aus?
#Aufgabe 3.5: Wie ?ndert sich der Zusammenhang bei Kontrolle f?r Abinote?

#Tipp: sucht den Befehl na.omit in der Hilfe




######################################################################################################
######################################Nicht-parametrische Tests#######################################
######################################################################################################







#################################?berpr?fung der Normalverteilung#####################################

#Wir nutzen den shapiro wilk Test zur ?berpr?fung der Normalverteilung
install.packages("nortest") #ben?tigt f?r shapiro wilk test
library(nortest)

#allgemeiner Befehl f?r shapiro Wilk test: shapiro.test(Daten) 

#Aufgabe 4.1: Liegt eine Normalverteilung der Abi Note vor?
#Aufgabe 4.2: Liegt eine Normalverteilung der W?rtanzahl vor?
#Aufgabe 4.3: Liegt eine Normalverteilung des Alters vor?



######################Berechnung von Korrelationen: Rangkorrelationen#######################

#Berechnung der Korrelation von Abinote und Alter

#Mit Spearmans Roh
cor.test(dfGedaechtnis$alter, dfGedaechtnis$abinote, method="spearman")
#Mit Kendalls Tau 
cor.test(dfGedaechtnis$alter, dfGedaechtnis$abinote, method="kendall")

#Aufgabe 4.4:Berechnet die Korrelation der Abinote mit der Anzahl erinnerter W?rter

#Aufgabe 4.5: Berechnet die Korrelation der Variable Kategorie mit der Anzahl erinnerter W?rter



##########################################Chi-Quadrat-Test############################################


library(MASS)       # MASS package laden

# Kreuztabelle inklusive Zeilen- und Spaltensummen erstellen, um Voraussetzungen bez?glich der absoluten H?ufigkeiten in den Zellen zu pr?fen
# Voraussetzung: In keiner der Zellen eine H?ufigkeit < 1 und in mindestens 80% der Zellen eine H?ufigkeit von mindestens 5

myContingencyTable (dfGedaechtnis$sex, abinote_kategorien)

# Ist die Abinote stochastisch unabh?ngig vom Geschlecht der Studenten?
chisq.test(table(dfGedaechtnis$sex, abinote_kategorien)) # wird auf einem Signifikanzlevel von .05 getestet

# Aufgabe 4.6: Erstellt eine Kreuztabelle inklusive Zeilen- und Spaltensummen f?r die Variablen Geschlecht und Bedingung
# Aufgabe 4.7: Ist die Aufgabenbedingung stochastisch unabh?ngig vom Geschlecht der Studenten?

#####################################################################################################
##########################################Weitere Aufgaben###########################################
#####################################################################################################


# Aufgabe 5.1: Welcher Abischnitt ist am h?ufigsten vertreten? Finde zwei verschiedene L?sungswege.  

# Aufgabe 5.2: Teile die Variable Leistungsorientierung nach Marbuger Konventionen in Kategorien ein und erstelle eine H?ufigkeitstabelle. 

# Aufgabe 5.3: Wie verteilen sich die Teilnehmer auf die Kategorien der Leistungsorientierung?
# trennt dabei nach Geschlecht

# Es besteht ein kleiner Zusammenhang zwischen Abinote und Anzahl erinnerter W?rter
# Aufgabe 5.4: ?ndert sich der Zusammenhang bei Kontrolle f?r Leistungsorientierung?

# Aufgabe 5.5:Erstellt eine Kreuztabelle zum Zusammenhang von Leistungsorientierung und Anzahl der erinnerten W?rter
# stellt diesen Zusammenhang aufgeteilt nach der Bedingung dar

# Aufgabe 5.6: Liegt eine Normalverteilung der Leistungsorientierung vor?

# Aufgabe 5.7: Sind Leistungsorientierung und Abinote miteinander korreliert?

# Aufgabe 5.8: Ist die Leistungsorientierung stochastisch unabh?ngig von der Abinote der Studenten?

# Aufgabe 5.9: Ist die Leistungsorientierung stochastisch unabh?ngig vom Geschlecht der Studenten?
