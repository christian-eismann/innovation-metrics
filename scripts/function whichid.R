### function whichid ###########################################################
##                                                                            ##
## Diese Funktion findet Elemente entsprechend ihrer ID oder der Bezeichnung. ##
## Setzt ein Element, wie es von assign.id generiert wird, voraus (id_index). ##
##                                                                            ##
################################################################################

whichid <- function(id_index, element){
  char <- is.character(element)
  num  <- is.numeric(element)
  
  if(char == TRUE){
    z <- id_index[id_index$Bezeichnung == element, ]
  }
  else if(num == TRUE){
    z <- id_index[id_index$id == element, ]
  }
  
  z <- data.frame(z, stringsAsFactors = FALSE)
  z[1] <- as.numeric(z[1])
  z[2] <- as.character(z[2])
  return(z)
}