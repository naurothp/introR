#Eigene Funktion Kreuztabelle: Erweiterung um Spalten- und Zeilensummen durch den Befehl addmargins:
myContingencyTable <- function (x,y,z) {if(missing(z)) 
{addmargins(table (x,y))} else
{addmargins(table (x,y,z))}}

#Kreutabelle mit relativer Häufigkeit bezogen auf die Gesamthäufigkeit, gerundet auf 4 Stellen und anschließender
#Multiplikation mit 100 zur Darstellung von Prozentwerten
myContingencyTableAll <- function(x,y,z) { if(missing(z))
{round(addmargins(prop.table(table(x, y))), digits =4)*100} else
{round(addmargins(prop.table(table(x, y,z))), digits =4)*100}}


#relative Häufigkeit bezogen auf einzelne Zeile statt auf Gesamthäufigkeit, gerundet auf 4 Stellen und anschließender
#Multiplikation mit 100 zur Darstellung von Prozentwerten
#zeigt die Verteilung aller Leute einer Zeile auf die einzelnen Spalten
myContingencyTableRow <- function (x,y,z) {if(missing(z))
{round(addmargins(prop.table(table(x, y),1)), digits =4)*100}else
{round(addmargins(prop.table(table(x, y, z),1)), digits =4)*100}}


#relative Häufigkeit bezogen auf einzelne Zeile statt auf Gesamthäufigkeit, gerundet auf 4 Stellen und anschließender
#Multiplikation mit 100 zur Darstellung von Prozentwerten
#zeigt die Verteilung aller Leute einer Spalte auf die einzelnen Zeilen
myContingencyTableColumn <- function (x,y,z) {if(missing(z))
{round(addmargins(prop.table(table(x, y),2)), digits =4)*100}else
{round(addmargins(prop.table(table(x, y, z),2)), digits =4)*100}}


