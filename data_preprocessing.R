# initializing functions
root_path = "/home/christian/innovation-metrics"
scripts_path = "/scripts"
project_path = "/project"
setwd(paste0(root_path, scripts_path)); source("head.R")
setwd(paste0(root_path, project_path))

# Kartierung einlesen
project <- extract.slides("enahrgie_fin.pptx")

# ID generieren und mit Kartierungen verbinden
project.id <- assign.id(project)
for(i in 1:9){project[[i]] <- merge(project.id, project[[i]], by = "Bezeichnung")}

# Dimensionen der Kartierungen berechnen
project.dimensions <- dimensions(project)

# Gesamtliste aller Elemente erstellen
project.all <- NULL
for(i in 1:9){
  project.all <- rbind(project.all, project[[i]])
}; rm(i)

# Geordnete Liste der Elemente erstellen
project.elements <- list()
for(i in 1:dim(project.id)[1]){
  project.elements[[i]] <- project.all[project.all$id == i, ]
}; rm(i)
names(project.elements) <- project.id[, 2]

# x- und y-Koordinaten als numeric definieren
for(i in 1:9){
  project[[i]]$x <- as.numeric(project[[i]]$x)
  project[[i]]$y <- as.numeric(project[[i]]$y)
}; rm(i)

# neue Elemente an das Projektelement heften
project[[10]]  <- project.all
project[[11]] <- project.elements
project[[12]] <- project.dimensions
project[[13]] <- project.id

names(project)[c(10:13)] <- c("all", "elements", "dimensions", "id")

# Variablen bereinigen
rm(project.id, project.elements, project.dimensions, project.all)


save(list = "project", file = "project.RData")
