###########Working Directory bestimmen und Bibliotheken einlesen###########

(setwd("E:\\Dropbox\\Grundlegende_Statistik")) 
#(setwd("D:\\Dropbox\\Grundlegende_Statistik"))

#lösche den Workspace
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

#Verkürzung der Bedingung zur übersichtlicheren Darstellung
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

# Varianz und Standardabweichung der Anzahl erinnerter Wörter
var(dfGedaechtnis$anzWoerter)
sd(dfGedaechtnis$anzWoerter)


###################################################################################################
############################################Häufigkeiten###########################################
###################################################################################################


# Häufigkeitstabelle für die Variable Geschlecht
table(dfGedaechtnis$se)

# Häufigkeitstabelle für die Variable Anzahl erinnerter Wörter
table(dfGedaechtnis$anzWoerter)

# Zur übersichtlicheren Darstellung bei kontinuierlichen Variablen: Kategorisierung der Variable in Gruppen
# Kategorisierung der Variable Anzahl erinnerter Wörter in 4 Gruppen nach Quartilen
anzWoerter_kategorien <- cut(x=dfGedaechtnis$anzWoerter, breaks=c(0,10,14,17,27),labels=c("0-10 Woerter", "11-14 Woerter", "15-17 Woerter", "18-27 Woerter"))
table(anzWoerter_kategorien)

# Aufgabe1: Erstelle eine Häufigkeitstabelle der Variable Abinote kategorisiert nach Quartilen.
















# Lösung 1:
summary(dfGedaechtnis$abinote)
abinote_kategorien <- {cut(x=dfGedaechtnis$abinote, 
                          breaks=c(1.0,1.4,1.8,2.3,3.3),labels=c
                          ("Abinote bis 1,4", "Abinote bis 1,8", "Abinote bis 2,3", "Abinote bis 3,3"))}
table(abinote_kategorien)



#####################################################################################################
############################################Kreuztabellen############################################
#####################################################################################################


###########Erläuterung der Funktion für Kreuztabellen durch schrittweise Erweiterung###########

#einfache Kreuztabelle ohne Erweiterungen
myContingencyTableRow <- function (x,y) {table(x, y)}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Aus absoluten Werten werden relative Häufigkeiten in Bezug auf die einzelne Zeile durch prop.table mit dem 
#Parameter 1 (eine 2 würde die relative Häufigkeit bezogen auf die Spalte angeben)
myContingencyTableRow <- function (x,y) {prop.table(table(x, y),1)}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Ergänzung um die Spalten- und Zeilensummen durch addmargins
myContingencyTableRow <- function (x,y) {addmargins(prop.table(table(x, y),1))}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Runden auf vier Stellen nach dem Komma durch round mit dem Paremter digits=4
myContingencyTableRow <- function (x,y) {round(addmargins(prop.table(table(x, y),1)), digits =4)}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Multiplikation mit 100 zur übersichtlicheren Darstellung in Prozentwerten
myContingencyTableRow <- function (x,y) {round(addmargins(prop.table(table(x, y),1)), digits =4)*100}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#Erweiterung um die Möglichkeit einer dritten Dimension -->Wenn "z" fehlt wird die gleiche Funktion wie bisher aufgerufen
#wenn "z" vorhanden ist wird die bisher bekannte Funktion einfach um einen dritten Parameter ergänzt
myContingencyTableRow <- function (x,y,z) {if(missing(z))
{round(addmargins(prop.table(table(x, y),1)), digits =4)*100}else
{round(addmargins(prop.table(table(x, y, z),1)), digits =4)*100}}
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$bed)

#weitere Funktionen für Kreuztabellen in extra Datei (MyContingencyTable_Functions)



#Verteilung der Abinoten aufgeteilt nach Geschlecht
#Problem: sehr unübersichtlich durch viele Kategorien
myContingencyTableRow (dfGedaechtnis$sex, dfGedaechtnis$abinote)

#Übersichtlicher durch Verwendung einer in Quartile unterteilten Variablen
myContingencyTableRow (dfGedaechtnis$sex, abinote_kategorien)


#Aufgabe2: Erstellt eine übersichtliche Kreuztabelle zum Zusammenhang
#von Geschlecht und Anzahl der erinnerten Wörter















#Lösung 2
myContingencyTableRow (dfGedaechtnis$sex, anzWoerter_kategorien)


#####################################################################################################
############################################Korrelationen############################################
#####################################################################################################

#Neue Pakete installieren und laden
#install.packages("ppcor")
#install.packages("stats")
library(ppcor)
library(stats)

#Überprüfen auf Missing Values
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

#andere Möglichkeit
x <- dfGedaechtnis$abinote
y <- dfGedaechtnis$alter
cor.test(x, y)

#Veränderung der Argumente -> z.B. einseitige Testung
cor.test(x, y, alternative = c("greater"), method = c("pearson"))


#Aufgabe3.1: Wie korreliert die Abinote mit der Anzahl erinnerter Wörter? 
#Beachte: Das Konfidenzniveau soll bei 90% liegen.


#Aufgabe 3.2: Wie korreliert die Leistungsorientierung mit der Anzahl der erinnerten Wörter?
#Beachte: Das Konfidenzniveau soll bei 95% liegen.
















#Lösung 3.1:
cor.test(dfGedaechtnis$abinote, dfGedaechtnis$anzWoerter, conf.level = 0.90)

#Lösung 3.2
cor.test(dfGedaechtnis$leistungsOri, dfGedaechtnis$anzWoerter)




##Partialkorrelationen
#Partialkorrelation von zwei Variablen (x, y) bei Kontrolle für eine dritte Variable (z)
pcor.test(x, y, z)


#Aufgabe 3.3.: Wie ändert sich der Zusammenhang von Leistungsorientierung und Anzahl erinnerter Wörter
#bei Kontrolle für Abinote?


#Aufgabe 3.4: Wie sieht der Zusammenhang von Alter und Anzahl erinnerter Wörter aus?
#Aufgabe 3.5: Wie ändert sich der Zusammenhang bei Kontrolle für Abinote?

#Tipp: sucht den Befehl na.omit in der Hilfe


















#Lösung 3.3:

#Entfernen von Objekten mit Missing Values
dfGedaechtnisNA<- na.omit(dfGedaechtnis)

cor.test(dfGedaechtnisNA$leistungsOri, dfGedaechtnisNA$anzWoerter)
cor.test(dfGedaechtnisNA$leistungsOri, dfGedaechtnisNA$anzWoerter)
pcor.test(dfGedaechtnisNA$leistungsOri, dfGedaechtnisNA$anzWoerter, dfGedaechtnisNA$abinote)




#Lösung 3.4 und 3.5.:
cor.test(dfGedaechtnisNA$alter, dfGedaechtnisNA$anzWoerter)
pcor.test(dfGedaechtnisNA$alter, dfGedaechtnisNA$anzWoerter, dfGedaechtnisNA$abinote)



######################################################################################################
######################################Nicht-parametrische Tests#######################################
######################################################################################################


#################################Überprüfung der Normalverteilung#####################################

#Wir nutzen den shapiro wilk Test zur Überprüfung der Normalverteilung
install.packages("nortest") #benötigt für shapiro wilk test
library(nortest)

#allgemeiner Befehl für shapiro Wilk test: shapiro.test(Daten) 

#Aufgabe 4.1: Liegt eine Normalverteilung der Abi Note vor?
#Aufgabe 4.2: Liegt eine Normalverteilung der Wörtanzahl vor?
#Aufgabe 4.3: Liegt eine Normalverteilung des Alters vor?











#Lösungen 4.1, 4.2 und 4.3

shapiro.test(dfGedaechtnis$abinote)

shapiro.test(dfGedaechtnis$anzWoerter)

shapiro.test(dfGedaechtnis$alter)


######################Berechnung von Korrelationen: Rangkorrelationen#######################



#Berechnung der Korrelation von Abinote und Alter

#Spearmans Roh
cor.test(dfGedaechtnis$alter, dfGedaechtnis$abinote, method="spearman")
#Kendalls Tau 
cor.test(dfGedaechtnis$alter, dfGedaechtnis$abinote, method="kendall")

#Aufgabe 4.4:Korrelation Abinote-Anzahl Wörter

















#Lösung 4.4
#Spearmans Roh
cor.test(dfGedaechtnis$anzWoerter, dfGedaechtnis$abinote, method="spearman")
#Kendalls Tau
cor.test(dfGedaechtnis$anzWoerter, dfGedaechtnis$abinote, method="kendall")

#Aufgabe 4.5: Korrelation Kategorie-Anzahl Wörter
















#Lösung 4.5

#Spearmans Roh
cor.test(dfGedaechtnis$anzWoerter, dfGedaechtnis$bed, method="spearman")

#Warum gibt R eine Fehlermeldung aus und wie kann der Fehler behoben werden? 

#Spearmans Rho
cor.test(dfGedaechtnis$anzWoerter, (as.numeric(dfGedaechtnis$bed)), method="spearman")
#Kendalls Tau
cor.test(dfGedaechtnis$anzWoerter, (as.numeric(dfGedaechtnis$bed)), method="kendall")





##########################################Chi-Quadrat-Test############################################


library(MASS)       # MASS package laden

# Kreuztabelle inklusive Zeilen- und Spaltensummen erstellen, um Voraussetzungen bezüglich der absoluten Häufigkeiten in den Zellen zu prüfen
# Voraussetzung: In keiner der Zellen eine Häufigkeit < 1 und in mindestens 80% der Zellen eine Häufigkeit von mindestens 5

myContingencyTable (dfGedaechtnis$sex, abinote_kategorien)

# Ist die Abinote stochastisch unabhängig vom Geschlecht der Studenten?
chisq.test(table(dfGedaechtnis$sex, abinote_kategorien)) # wird auf einem Signifikanzlevel von .05 getestet

# Aufgabe 4.6: Erstellt eine Kreuztabelle inklusive Zeilen- und Spaltensummen für die Variablen Geschlecht und Bedingung
# Aufgabe 4.7: Ist die Aufgabenbedingung stochastisch unabhängig vom Geschlecht der Studenten?




















# Lösung 4.6:
myContingencyTable(dfGedaechtnis$sex, dfGedaechtnis$bed)

# Lösung 4.7: ja
chisq.test(table(dfGedaechtnis$sex, dfGedaechtnis$bed))



#####################################################################################################
##########################################Weitere Aufgaben###########################################
#####################################################################################################


# Aufgabe 5.1: Welcher Abischnitt ist am häufigsten vertreten? Finde zwei verschiedene Lösungswege.  

















# Lösung 5.1: 
table (dfGedaechtnis$abinote) # oder 
which.max(table(dfGedaechtnis$abinote))

# Aufgabe 5.2: Teile die Variable Leistungsorientierung nach Marbuger Konventionen in Kategorien ein und erstelle eine Häufigkeitstabelle. 



















# Lösung 5.2: 
mean_leistungsOri<-mean(dfGedaechtnis$leistungsOri,na.rm = TRUE)
sd_leistungsOri<-sd(dfGedaechtnis$leistungsOri,na.rm = TRUE)

leistungsOri_kategorien <- cut(x=dfGedaechtnis$leistungsOri, breaks=c(0,mean_leistungsOri-(3*sd_leistungsOri/2),mean_leistungsOri-(sd_leistungsOri/2),mean_leistungsOri+(sd_leistungsOri/2),mean_leistungsOri+(3*sd_leistungsOri/2),72.6),labels=c("sehr niedrig", "niedrig", "durchschnittlich", "hoch", "sehr hoch"))
table(leistungsOri_kategorien)


#Aufgabe 5.3: Wie verteilen sich die Teilnehmer auf die Kategorien der Leistungsorientierung?
#trennt dabei nach Geschlecht
























#Lösung 5.3
myContingencyTableRow (dfGedaechtnis$sex, leistungsOri_kategorien)

#Es besteht ein kleiner Zusammenhang zwischen Abinote und Anzahl erinnerter Wörter
#Aufgabe 5.4: Ändert sich der Zusammenhang bei Kontrolle für Leistungsorientierung?


















#Lösung 5.4:
cor.test(dfGedaechtnisNA$abinote, dfGedaechtnisNA$anzWoerter)
pcor.test(dfGedaechtnisNA$abinote, dfGedaechtnisNA$anzWoerter, dfGedaechtnisNA$leistungsOri)

#Aufgabe 5.5:Erstellt eine Kreuztabelle zum Zusammenhang von Leistungsorientierung und Anzahl der erinnerten Wörter
#stellt diesen Zusammenhang aufgeteilt nach der Bedingung dar














#Lösung 5.5
myContingencyTableColumn (anzWoerter_kategorien, leistungsOri_kategorien, dfGedaechtnis$bed)



#Aufgabe 5.6: Liegt eine Normalverteilung der Leistungsorientierung vor?






















#Lösung 5.6
shapiro.test(dfGedaechtnis$leistungsOri) 


#Aufgabe 5.7: Sind Leistungsorientierung und Abinote miteinander korreliert?





















#Lösung 5.7
#Spearmans Roh
cor.test(dfGedaechtnis$leistungsOri, dfGedaechtnis$abinote, method="spearman")
#Kendalls Tau 
cor.test(dfGedaechtnis$leistungsOri, dfGedaechtnis$abinote, method="kendall")


# Aufgabe 5.8: Ist die Leistungsorientierung stochastisch unabhängig von der Abinote der Studenten?





















# Lösung 5.8: nein
addmargins(table(leistungsOri_kategorien, abinote_kategorien)) # Voraussetzungen des Chi-Quadrat-Tests sind nicht erfüllt (absolute Häufigkeiten in Zellen) --> Lösung: Kategorien zusammenfassen 

abinote_kategorien2 <- cut(x=dfGedaechtnis$abinote, breaks=c(1.0,1.8,3.3),labels=c("Abinote bis 1,8", "Abinote bis 3,3"))

leistungsOri_kategorien2 <- cut(x=dfGedaechtnis$leistungsOri, breaks=c(0,mean_leistungsOri-(sd_leistungsOri/2),mean_leistungsOri+(sd_leistungsOri/2),72.6),labels=c("unterdurchschnittlich", "durchschnittlich", "überdurchschnittlich"))

addmargins(table(leistungsOri_kategorien2, abinote_kategorien2))

chisq.test(table(leistungsOri_kategorien2, abinote_kategorien2))

# Aufgabe 5.9: Ist die Leistungsorientierung stochastisch unabhängig vom Geschlecht der Studenten?






















# Lösung 5.9: ja
addmargins(table(dfGedaechtnis$sex, leistungsOri_kategorien2))
chisq.test(table(dfGedaechtnis$sex, leistungsOri_kategorien2))

