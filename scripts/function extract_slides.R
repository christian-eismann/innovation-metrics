extract.slides <- function(file){
	wd <- getwd()
	file <- as.character(file)
	file.rename(file, "slides.zip")
	unzip("slides.zip")
	file.rename("slides.zip", file)

	slides.list <- list.files(paste0(wd, "/ppt/slides"))
	slides.list <- slides.list[grep("^slide", slides.list)]

	slides.no <- length(slides.list)

	setwd(paste0(wd, "/ppt/slides"))
	
	for (i in 1:slides.no){file.copy(slides.list[i], wd, overwrite = TRUE)}

	setwd(wd)

	unlink(paste0(wd, "/_rels"),    recursive=TRUE)
	unlink(paste0(wd, "/docProps"), recursive=TRUE)
	unlink(paste0(wd, "/ppt"),      recursive=TRUE)
	file.remove("[Content_Types].xml")

	message(slides.no, " Slides extrahiert")
	
	### Ab hier das Auslesen der Kartierungen ###

library(XML)
tabellen.list <- list()

# benötigt für die Tabellen und die Beschriftung der Liste:
jahre <- c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020", "Z2015", "Z2016", "Z2017", "Z2018")

for (no in 1:slides.no){

xmlfile <- as.character(slides.list[no])
slide <- xmlToList(xmlfile)

shapes <- slide[[1]][[1]]
shapes <- shapes[which(attributes(shapes)$names=="sp")]

shapes_count <- length(shapes)

valid_shapes <- rep(0, shapes_count)

#select only relevant shapes
#if-statement umgeht Fehlermeldung bei unpassendem Element (sonst: logical(0))
for(i in 1:shapes_count){
  if(length(shapes[[i]][[2]][[2]]$.attrs) == 1){
    valid_shapes[i] <- 
      (shapes[[i]][[2]][[2]]$.attrs=="rect") |
      (shapes[[i]][[2]][[2]]$.attrs=="roundRect") |
      (shapes[[i]][[2]][[2]]$.attrs=="octagon") |
      (shapes[[i]][[2]][[2]]$.attrs=="plaque")
  }
}

message(no, ": relevante shapes extrahiert")

shapes <- shapes[which(valid_shapes==1)]
shapes_count <- length(shapes)

element <- c(1:shapes_count)*NA
coord_x <- c(1:shapes_count)*NA
coord_y <- c(1:shapes_count)*NA
l       <- c(1:shapes_count)*NA
h       <- c(1:shapes_count)*NA
name    <- c(1:shapes_count)*NA

message(no, ": Variablen angelegt")

for(i in 1:shapes_count){
  element[i] <- as.character(shapes[[i]][[2]][[2]]$.attrs)
  coord_x[i] <- as.numeric(shapes[[i]][[2]][[1]][[1]][1])
  coord_y[i] <- as.numeric(shapes[[i]][[2]][[1]][[1]][2])
  l[i]       <- as.numeric(shapes[[i]][[2]][[1]][[2]][1])
  h[i]       <- as.numeric(shapes[[i]][[2]][[1]][[2]][2])

  #Ab hier werden die Bezeichnungen der Elemente (Text) zusammengeführt
    
  text <- ""
  textlines_p <- which(names(shapes[[i]]$txBody)=="p")
  
  for(p in 1:length(textlines_p)){
    textlines_r <- which(names(shapes[[i]]$txBody[[textlines_p[p]]])=="r")
    
    for(r in 1:length(textlines_r)){
      text <- paste0(text, shapes[[i]]$txBody[[textlines_p[p]]][[textlines_r[r]]]$t)
    }
    text <- paste(text)
  }
  
  name[i]    <- text
}

x  <- coord_x + 0.5 * l
mx <- (min(as.numeric(x)) + max(as.numeric(x))) / 2
x  <- as.numeric(x) - mx 

y  <- coord_y + 0.5 * h
y  <- max(y) - y #Achse umdrehen
my <- (min(as.numeric(y)) + max(as.numeric(y))) / 2
y  <- as.numeric(y) - my

Tabelle <- cbind(name, element, x, y, jahre[no])
Tabelle <- data.frame(Tabelle, stringsAsFactors = FALSE)

colnames(Tabelle) <- c("Bezeichnung", "Element", "x", "y", "Jahr")

Tabelle$Element[Tabelle$Element=="roundRect"] <- "Umwelt"
Tabelle$Element[Tabelle$Element=="rect"]      <- "Akteur"
Tabelle$Element[Tabelle$Element=="plaque"]    <- "Technik"
Tabelle$Element[Tabelle$Element=="octagon"]   <- "Zeichen"

tabellen.list[[no]] <- Tabelle

}

names(tabellen.list) <- jahre

sapply(slides.list, file.remove)

return(tabellen.list)
}
