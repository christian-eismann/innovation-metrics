#### Beschreibung###################################################################
##                                                                                ##
## Erstellt eine tabellarische Übersicht zu den Elementetypen und ihrem Auftreten ##
## in den jeweiligen Jahren.                                                      ##
##                                                                                ##
## Input                                                                          ##
##   - cart: Ein Element, wie es durch die Funktion extract.slides erstellt wurde ##
##                                                                                ##
## Output                                                                         ##
##   - Eine Liste                                                                 ##
##      * zu Häufigkeiten des Auftretens der Elemente (freA, freT, freU, freT)    ##
##      * der Fluktuation der Elemente (fluA, fluT, fluU, fluZ)                   ##
##      * der Gesamtkonstellation (freAll, fluAll)                                ##
####################################################################################




development <- function(cart){

################################################
#### Elementtypen ?ber Jahre zusammenf?hren ####
################################################

#### Akteure ####

akteure <- rbind(
  if(dim(subset(cart$SQ2015, cart$SQ2015$Element == "Akteur"))[1] > 0){
    cbind(subset(cart$SQ2015, cart$SQ2015$Element == "Akteur"))
  },
  
  if(dim(subset(cart$SQ2016, cart$SQ2016$Element == "Akteur"))[1] > 0){
    cbind(subset(cart$SQ2016, cart$SQ2016$Element == "Akteur"))
  },
  
  if(dim(subset(cart$SQ2017, cart$SQ2017$Element == "Akteur"))[1] > 0){
    cbind(subset(cart$SQ2017, cart$SQ2017$Element == "Akteur"))
  },
  
  if(dim(subset(cart$SQ2018, cart$SQ2018$Element == "Akteur"))[1] > 0){
    cbind(subset(cart$SQ2018, cart$SQ2018$Element == "Akteur"))
  },
  if(dim(subset(cart$SQ2020, cart$SQ2020$Element == "Akteur"))[1] > 0){
    cbind(subset(cart$SQ2020, cart$SQ2020$Element == "Akteur"))
  }
)

#### Technik ####

technik <- rbind(
  if(dim(subset(cart$SQ2015, cart$SQ2015$Element == "Technik"))[1] > 0){
    cbind(subset(cart$SQ2015, cart$SQ2015$Element == "Technik"))
  },
  
  if(dim(subset(cart$SQ2016, cart$SQ2016$Element == "Technik"))[1] > 0){
    cbind(subset(cart$SQ2016, cart$SQ2016$Element == "Technik"))
  },
  
  if(dim(subset(cart$SQ2017, cart$SQ2017$Element == "Technik"))[1] > 0){
    cbind(subset(cart$SQ2017, cart$SQ2017$Element == "Technik"))
  },
  
  if(dim(subset(cart$SQ2018, cart$SQ2018$Element == "Technik"))[1] > 0){
    cbind(subset(cart$SQ2018, cart$SQ2018$Element == "Technik"))
  },
  if(dim(subset(cart$SQ2020, cart$SQ2020$Element == "Technik"))[1] > 0){
    cbind(subset(cart$SQ2020, cart$SQ2020$Element == "Technik"))
  }
)

#### Umwelt ####

umwelt <- rbind(
  if(dim(subset(cart$SQ2015, cart$SQ2015$Element == "Umwelt"))[1] > 0){
    cbind(subset(cart$SQ2015, cart$SQ2015$Element == "Umwelt"))
  },
  
  if(dim(subset(cart$SQ2016, cart$SQ2016$Element == "Umwelt"))[1] > 0){
    cbind(subset(cart$SQ2016, cart$SQ2016$Element == "Umwelt"))
  },
  
  if(dim(subset(cart$SQ2017, cart$SQ2017$Element == "Umwelt"))[1] > 0){
    cbind(subset(cart$SQ2017, cart$SQ2017$Element == "Umwelt"))
  },
  
  if(dim(subset(cart$SQ2018, cart$SQ2018$Element == "Umwelt"))[1] > 0){
    cbind(subset(cart$SQ2018, cart$SQ2018$Element == "Umwelt"))
  },
  if(dim(subset(cart$SQ2020, cart$SQ2020$Element == "Umwelt"))[1] > 0){
    cbind(subset(cart$SQ2020, cart$SQ2020$Element == "Umwelt"))
  }
)

#### Zeichen ####

zeichen <- rbind(
  if(dim(subset(cart$SQ2015, cart$SQ2015$Element == "Zeichen"))[1] > 0){
    cbind(subset(cart$SQ2015, cart$SQ2015$Element == "Zeichen"))
  },
  
  if(dim(subset(cart$SQ2016, cart$SQ2016$Element == "Zeichen"))[1] > 0){
    cbind(subset(cart$SQ2016, cart$SQ2016$Element == "Zeichen"))
  },
  
  if(dim(subset(cart$SQ2017, cart$SQ2017$Element == "Zeichen"))[1] > 0){
    cbind(subset(cart$SQ2017, cart$SQ2017$Element == "Zeichen"))
  },
  
  if(dim(subset(cart$SQ2018, cart$SQ2018$Element == "Zeichen"))[1] > 0){
    cbind(subset(cart$SQ2018, cart$SQ2018$Element == "Zeichen"))
  },
  if(dim(subset(cart$SQ2020, cart$SQ2020$Element == "Zeichen"))[1] > 0){
    cbind(subset(cart$SQ2020, cart$SQ2020$Element == "Zeichen"))
  }
)


################################################
#### Veränderung über Jahre in Tabellenform ####
################################################

## globale Beschriftungen ##

rownames.fl <- c("Zugänge", "Konstante", "Abgänge", "Gesamtanzahl")
colnames.fl <- c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")

#### Akteur ####

akteure_namen <- sort(unique(akteure$Bezeichnung))
frA <- matrix(nrow = length(akteure_namen), ncol = 5)
rownames(frA) <- akteure_namen
colnames(frA) <- colnames.fl

x <- c(2015, 2016, 2017, 2018, 2020)


for(i in 1:length(akteure_namen)){
  x <- subset(akteure$Jahr, akteure$Bezeichnung == akteure_namen[i])
  frA[i, 1] <- "SQ2015" %in% x
  frA[i, 2] <- "SQ2016" %in% x
  frA[i, 3] <- "SQ2017" %in% x
  frA[i, 4] <- "SQ2018" %in% x
  frA[i, 5] <- "SQ2020" %in% x
}; rm(i)

frA[frA == TRUE]  <- 1
frA[frA == FALSE] <- 0

frA <- cbind("Akteur", frA)
frA <- as.data.frame(frA, stringsAsFactors = FALSE)
colnames(frA)[1] <- "Element"

frA[, 2] <- as.numeric(frA[, 2])
frA[, 3] <- as.numeric(frA[, 3])
frA[, 4] <- as.numeric(frA[, 4])
frA[, 5] <- as.numeric(frA[, 5])
frA[, 6] <- as.numeric(frA[, 6])

flA <- matrix(nrow = 4, ncol = 5)
rownames(flA) <- rownames.fl
colnames(flA) <- colnames.fl

# Konstant
# f?r 2015 keine Konstanten m?glich
flA[2, 2] <- dim(subset(frA, (frA[, 2] == 1  & (frA[, 3] == 1))))[1] #2016
flA[2, 3] <- dim(subset(frA, (frA[, 3] == 1  & (frA[, 4] == 1))))[1] #2017
flA[2, 4] <- dim(subset(frA, (frA[, 4] == 1  & (frA[, 5] == 1))))[1] #2018
flA[2, 5] <- dim(subset(frA, (frA[, 5] == 1  & (frA[, 6] == 1))))[1] #2020

# Zug?nge
flA[1, 1] <- 0                                                       #2015
flA[1, 2] <- dim(subset(frA, (frA[, 2] == 0  & (frA[, 3] == 1))))[1] #2016
flA[1, 3] <- dim(subset(frA, (frA[, 3] == 0  & (frA[, 4] == 1))))[1] #2017
flA[1, 4] <- dim(subset(frA, (frA[, 4] == 0  & (frA[, 5] == 1))))[1] #2018
flA[1, 5] <- dim(subset(frA, (frA[, 5] == 0  & (frA[, 6] == 1))))[1] #2020

# Abg?nge
flA[3, 1] <- 0                                                       #2015
flA[3, 2] <- dim(subset(frA, (frA[, 2] == 1  & (frA[, 3] == 0))))[1] #2016
flA[3, 3] <- dim(subset(frA, (frA[, 3] == 1  & (frA[, 4] == 0))))[1] #2017
flA[3, 4] <- dim(subset(frA, (frA[, 4] == 1  & (frA[, 5] == 0))))[1] #2018
flA[3, 5] <- dim(subset(frA, (frA[, 5] == 1  & (frA[, 6] == 0))))[1] #2020

flA[3, ] <- flA[3, ] * -1 #Abg?nge als negative Zahl
flA[4, ] <- colSums(frA[, c(2:6)])


#### Technik ####

technik_namen <- sort(unique(technik$Bezeichnung))
frT <- matrix(nrow = length(technik_namen), ncol = 5)
rownames(frT) <- technik_namen
colnames(frT) <- colnames.fl

for(i in 1:length(technik_namen)){
  x <- subset(technik$Jahr, technik$Bezeichnung == technik_namen[i])
  frT[i, 1] <- "SQ2015" %in% x
  frT[i, 2] <- "SQ2016" %in% x
  frT[i, 3] <- "SQ2017" %in% x
  frT[i, 4] <- "SQ2018" %in% x
  frT[i, 5] <- "SQ2020" %in% x
}; rm(i)

frT[frT == TRUE]  <- 1
frT[frT == FALSE] <- 0

frT <- cbind("Technik", frT)
frT <- as.data.frame(frT, stringsAsFactors = FALSE)
colnames(frT)[1] <- "Element"

frT[, 2] <- as.numeric(frT[, 2])
frT[, 3] <- as.numeric(frT[, 3])
frT[, 4] <- as.numeric(frT[, 4])
frT[, 5] <- as.numeric(frT[, 5])
frT[, 6] <- as.numeric(frT[, 6])

flT <- matrix(nrow = 4, ncol = 5)
rownames(flT) <- rownames.fl
colnames(flT) <- colnames.fl

# Konstant
# f?r 2015 keine Konstanten m?glich
flT[2, 2] <- dim(subset(frT, (frT[, 2] == 1  & (frT[, 3] == 1))))[1] #2016
flT[2, 3] <- dim(subset(frT, (frT[, 3] == 1  & (frT[, 4] == 1))))[1] #2017
flT[2, 4] <- dim(subset(frT, (frT[, 4] == 1  & (frT[, 5] == 1))))[1] #2018
flT[2, 5] <- dim(subset(frT, (frT[, 5] == 1  & (frT[, 6] == 1))))[1] #2020

# Zug?nge
flT[1, 1] <- 0                                                       #2015
flT[1, 2] <- dim(subset(frT, (frT[, 2] == 0  & (frT[, 3] == 1))))[1] #2016
flT[1, 3] <- dim(subset(frT, (frT[, 3] == 0  & (frT[, 4] == 1))))[1] #2017
flT[1, 4] <- dim(subset(frT, (frT[, 4] == 0  & (frT[, 5] == 1))))[1] #2018
flT[1, 5] <- dim(subset(frT, (frT[, 5] == 0  & (frT[, 6] == 1))))[1] #2020

# Abg?nge
flT[3, 1] <- 0                                                       #2015
flT[3, 2] <- dim(subset(frT, (frT[, 2] == 1  & (frT[, 3] == 0))))[1] #2016
flT[3, 3] <- dim(subset(frT, (frT[, 3] == 1  & (frT[, 4] == 0))))[1] #2017
flT[3, 4] <- dim(subset(frT, (frT[, 4] == 1  & (frT[, 5] == 0))))[1] #2018
flT[3, 5] <- dim(subset(frT, (frT[, 5] == 1  & (frT[, 6] == 0))))[1] #2020

flT[3, ] <- flT[3, ] * -1 #Abg?nge als negative Zahl
flT[4, ] <- colSums(frT[, c(2:6)])

#### Umwelt ####

if(is.null(umwelt) == FALSE){
  
  umwelt_namen <- sort(unique(umwelt$Bezeichnung))
  frU <- matrix(nrow = length(umwelt_namen), ncol = 5)
  rownames(frU) <- umwelt_namen
  colnames(frU) <- colnames.fl
  
  for(i in 1:length(umwelt_namen)){
    x <- subset(umwelt$Jahr, umwelt$Bezeichnung == umwelt_namen[i])
    frU[i, 1] <- "SQ2015" %in% x
    frU[i, 2] <- "SQ2016" %in% x
    frU[i, 3] <- "SQ2017" %in% x
    frU[i, 4] <- "SQ2018" %in% x
    frU[i, 5] <- "SQ2020" %in% x
    }; rm(i)
  
  frU[frU == TRUE]  <- 1
  frU[frU == FALSE] <- 0
  
  frU <- cbind("Umwelt", frU)
  frU <- as.data.frame(frU, stringsAsFactors = FALSE)
  colnames(frU)[1] <- "Element"
  
  frU[, 2] <- as.numeric(frU[, 2])
  frU[, 3] <- as.numeric(frU[, 3])
  frU[, 4] <- as.numeric(frU[, 4])
  frU[, 5] <- as.numeric(frU[, 5])
  frU[, 6] <- as.numeric(frU[, 6])
  
  flU <- matrix(nrow = 4, ncol = 5)
  rownames(flU) <- rownames.fl
  colnames(flU) <- colnames.fl
  
  # Konstant
  # f?r 2015 keine Konstanten m?glich
  flU[2, 2] <- dim(subset(frU, (frU[, 2] == 1  & (frU[, 3] == 1))))[1] #2016
  flU[2, 3] <- dim(subset(frU, (frU[, 3] == 1  & (frU[, 4] == 1))))[1] #2017
  flU[2, 4] <- dim(subset(frU, (frU[, 4] == 1  & (frU[, 5] == 1))))[1] #2018
  flU[2, 5] <- dim(subset(frU, (frU[, 5] == 1  & (frU[, 6] == 1))))[1] #2020

  # Zug?nge
  flU[1, 1] <- 0                                                       #2015
  flU[1, 2] <- dim(subset(frU, (frU[, 2] == 0  & (frU[, 3] == 1))))[1] #2016
  flU[1, 3] <- dim(subset(frU, (frU[, 3] == 0  & (frU[, 4] == 1))))[1] #2017
  flU[1, 4] <- dim(subset(frU, (frU[, 4] == 0  & (frU[, 5] == 1))))[1] #2018
  flU[1, 5] <- dim(subset(frU, (frU[, 5] == 0  & (frU[, 6] == 1))))[1] #2020

  # Abg?nge
  flU[3, 1] <- 0                                                       #2015
  flU[3, 2] <- dim(subset(frU, (frU[, 2] == 1  & (frU[, 3] == 0))))[1] #2016
  flU[3, 3] <- dim(subset(frU, (frU[, 3] == 1  & (frU[, 4] == 0))))[1] #2017
  flU[3, 4] <- dim(subset(frU, (frU[, 4] == 1  & (frU[, 5] == 0))))[1] #2018
  flU[3, 5] <- dim(subset(frU, (frU[, 5] == 1  & (frU[, 6] == 0))))[1] #2020

  flU[3, ] <- flU[3, ] * -1 #Abg?nge als negative Zahl
  flU[4, ] <- colSums(frU[, c(2:6)])

} else {
  frU <- rep(NA, 6)
  flU <- matrix(nrow = 4, ncol = 5, rep(0, 20))}

#### Zeichen ####

zeichen_namen <- sort(unique(zeichen$Bezeichnung))
frZ <- matrix(nrow = length(zeichen_namen), ncol = 5)
rownames(frZ) <- zeichen_namen
colnames(frZ) <- colnames.fl

for(i in 1:length(zeichen_namen)){
  x <- subset(zeichen$Jahr, zeichen$Bezeichnung == zeichen_namen[i])
  frZ[i, 1] <- "SQ2015" %in% x
  frZ[i, 2] <- "SQ2016" %in% x
  frZ[i, 3] <- "SQ2017" %in% x
  frZ[i, 4] <- "SQ2018" %in% x
  frZ[i, 5] <- "SQ2020" %in% x
}; rm(i)

frZ[frZ == TRUE]  <- 1
frZ[frZ == FALSE] <- 0

frZ <- cbind("Zeichen", frZ)
frZ <- as.data.frame(frZ, stringsAsFactors = FALSE)
colnames(frZ)[1] <- "Element"

frZ[, 2] <- as.numeric(frZ[, 2])
frZ[, 3] <- as.numeric(frZ[, 3])
frZ[, 4] <- as.numeric(frZ[, 4])
frZ[, 5] <- as.numeric(frZ[, 5])
frZ[, 6] <- as.numeric(frZ[, 6])

flZ <- matrix(nrow = 4, ncol = 5)
rownames(flZ) <- rownames.fl
colnames(flZ) <- colnames.fl

  # Konstant
# f?r 2015 keine Konstanten m?glich
flZ[2, 2] <- dim(subset(frZ, (frZ[, 2] == 1  & (frZ[, 3] == 1))))[1] #2016
flZ[2, 3] <- dim(subset(frZ, (frZ[, 3] == 1  & (frZ[, 4] == 1))))[1] #2017
flZ[2, 4] <- dim(subset(frZ, (frZ[, 4] == 1  & (frZ[, 5] == 1))))[1] #2018
flZ[2, 5] <- dim(subset(frZ, (frZ[, 5] == 1  & (frZ[, 6] == 1))))[1] #2020

# Zug?nge
flZ[1, 1] <- 0                                                       #2015
flZ[1, 2] <- dim(subset(frZ, (frZ[, 2] == 0  & (frZ[, 3] == 1))))[1] #2016
flZ[1, 3] <- dim(subset(frZ, (frZ[, 3] == 0  & (frZ[, 4] == 1))))[1] #2017
flZ[1, 4] <- dim(subset(frZ, (frZ[, 4] == 0  & (frZ[, 5] == 1))))[1] #2018
flZ[1, 5] <- dim(subset(frZ, (frZ[, 5] == 0  & (frZ[, 6] == 1))))[1] #2020

# Abg?nge
flZ[3, 1] <- 0                                                       #2015
flZ[3, 2] <- dim(subset(frZ, (frZ[, 2] == 1  & (frZ[, 3] == 0))))[1] #2016
flZ[3, 3] <- dim(subset(frZ, (frZ[, 3] == 1  & (frZ[, 4] == 0))))[1] #2017
flZ[3, 4] <- dim(subset(frZ, (frZ[, 4] == 1  & (frZ[, 5] == 0))))[1] #2018
flZ[3, 5] <- dim(subset(frZ, (frZ[, 5] == 1  & (frZ[, 6] == 0))))[1] #2020

flZ[3, ] <- flZ[3, ] * -1 #Abg?nge als negative Zahl
flZ[4, ] <- colSums(frZ[, c(2:6)])


#### Gesamtliste alle Elemente erstellen ####

frAll <- rbind(frA, frT, frU, frZ)
frAll <- na.omit(frAll)
flAll <- flA + flT + flU + flZ

#### Ergebnisse als Liste zusammenfassen ####

dynamik <- list(frA, flA, frT, flT, frU, flU, frZ, flZ, frAll, flAll)
names(dynamik) <- c("freA", "fluA", "freT", "fluT", "freU", "fluU", "freZ", "fluZ", "freAll", "fluAll")

return(dynamik)

rm(cart, akteure, technik, umwelt, zeichen, 
   akteure_namen, frA, flA,
   technik_namen, frT, flT,
   umwelt_namen, frU, flU,
   zeichen_namen, frZ, flZ, 
   x, i, diagramme,
   bar_A, bar_T, bar_U, bar_z,
   dynamik)
}