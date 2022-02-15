#### Beschreibung ###############################################################
##                                                                             ##
## Inputparameter                                                              ##
##   - devlist: ein Element, wie es von function development erstellt wird     ##
##   - plotting: bestimmt, ob ein Diagramm ausgegeben werden soll              ##
##                                                                             ##
## Output                                                                      ##
##                                                                             ##
##   - eine Liste mit den Verh?ltnissen von Neuen und verworfenen Elementen    ##
##     (Werte ?ber 1: Mehr Ver?nderung als Konstanz, Werte zw. 0 und 1: mehr   ##
##     Konstanz als Ver?nderung)                                               ##
##   - ein Plot zur grafischen Darstellung der Dynamik (falls plotting = TRUE) ##
#################################################################################

dynamic <- function(devlist){ 
  
  ixDA <- ixDU <- ixDT <- ixDZ <- ixDall <- rep(NA, 4)
  names(ixDA)   <- c(2016, 2017, 2018, 2020)
  names(ixDT)   <- c(2016, 2017, 2018, 2020)
  names(ixDU)   <- c(2016, 2017, 2018, 2020)
  names(ixDZ)   <- c(2016, 2017, 2018, 2020)
  names(ixDall) <- c(2016, 2017, 2018, 2020)
  
  for(i in 2:5){ixDA[i-1]   <- sum(abs(devlist$fluA[c(1, 3), i]))  / devlist$fluA[2, i] }
  for(i in 2:5){ixDT[i-1]   <- sum(abs(devlist$fluT[c(1, 3), i]))  / devlist$fluT[2, i] }
  for(i in 2:5){ixDU[i-1]   <- sum(abs(devlist$fluU[c(1, 3), i]))  / devlist$fluU[2, i] }
  for(i in 2:5){ixDZ[i-1]   <- sum(abs(devlist$fluZ[c(1, 3), i]))  / devlist$fluZ[2, i] }
  for(i in 2:5){ixDall[i-1] <- sum(abs(devlist$fluAll[c(1, 3), i])) / devlist$fluAll[2, i]}
  
  ## Dimensionen setzen (ylim)
  
  z <- c(ixDA, ixDT, ixDU, ixDZ, ixDall)
  u <- max(z[which(is.finite(z) == TRUE)])
  
  ## Ausgabe vorbereiten
  
  liste <- list(ixDA, ixDT, ixDU, ixDZ, ixDall)
  names(liste) <- c("Akteure", "Technik", "Umwelt", "Zeichen", "gesamt")
  
  return(liste)
  }