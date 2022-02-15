### function assign.id ###############################################################################
##                                                                                                  ##
## Diese funktion weist allen Elementen der Kartierungen eine eindeutige ID zu. Diese kann dann mit ##
## 'merge by = Bezeichnung' den Rohdaten zugewiesen werden.                                         ##
##                                                                                                  ##
######################################################################################################


assign.id <- function(cart){
  
  l <- length(cart)
  
  elemente <- NULL
  
  for(i in 1:l){    
    elemente <- c(elemente, cart[[i]]$Bezeichnung)
  }
  
  elemente <- unique(elemente)
  
  id_index <- as.data.frame(cbind(c(1:length(elemente)), elemente), stringsAsFactors = FALSE)
  colnames(id_index) <- c("id", "Bezeichnung")
  
  return(id_index)
}