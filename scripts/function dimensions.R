### function dimensions #################################################################
##                                                                                     ##
## Die Funktion berechnet für alle Kartierungen die Dimensionen auf der x- und y-Achse ##
## sowie deren Spannweite.                                                             ##
##                                                                                     ##
#########################################################################################

dimensions <- function(cart){
  l <- length(cart)
  
  minx <- miny <- maxx <- maxy <- rep(NA, l)
  
  
  for(i in 1:l){minx[i] <- min(as.numeric(cart[[i]]$x))}
  for(i in 1:l){miny[i] <- min(as.numeric(cart[[i]]$y))}
  
  for(i in 1:l){maxx[i] <- max(as.numeric(cart[[i]]$x))}
  for(i in 1:l){maxy[i] <- max(as.numeric(cart[[i]]$y))}
  
  rangeX <- abs(as.numeric(minx)) + as.numeric(maxx)
  rangeY <- abs(as.numeric(miny)) + as.numeric(maxy)
  
  dimensions <- cbind(c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020",
                        "Z2015", "Z2016", "Z2017", "Z2018"),
                      minx, maxx, miny, maxy, rangeX, rangeY)
  
  colnames(dimensions) <- c("Jahr", "minX", "maxX", "minY", "maxY", "rangeX", "rangeY")
  dimensions <- as.data.frame(dimensions, stringsAsFactors = FALSE)
  
  dimensions$minX <- as.numeric(dimensions$minX)
  dimensions$maxX <- as.numeric(dimensions$maxX)
  dimensions$minY <- as.numeric(dimensions$minY)
  dimensions$maxY <- as.numeric(dimensions$maxY)
  dimensions$rangeX <- as.numeric(dimensions$rangeX)
  dimensions$rangeY <- as.numeric(dimensions$rangeY)
  
  rm(l, i, minx, maxx, miny, maxy, rangeX, rangeY)
  
  return(dimensions)
}