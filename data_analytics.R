#setwd("working dir")

source("scripts/head.R")
load("project.RData")

#### Häufigkeitsverteilungen der beteiligten Elemente ####
project.dev <- development(project)

Akteure <- project.dev$fluA[4, ]
Technik <- project.dev$fluT[4, ]
Umwelt  <- project.dev$fluU[4, ]
Zeichen <- project.dev$fluZ[4, ]

freq <- rbind(Akteure, Technik, Umwelt, Zeichen)

## Tabellen

freq <- rbind(freq, colSums(freq))
rownames(freq)[5] <- "gesamt"

freq_r <- freq 
for(i in 1:5){freq_r[, i] <- freq[, i] / freq[5, i]}

freq_c <- matrix(nrow = 5, ncol = 10)

freq_c[, 1] <- freq[, 1]
freq_c[, 3] <- freq[, 2]
freq_c[, 5] <- freq[, 3]
freq_c[, 7] <- freq[, 4]
freq_c[, 9] <- freq[, 5]

freq_c[, 2] <- round(freq_r[, 1], 3)
freq_c[, 4] <- round(freq_r[, 2], 3)
freq_c[, 6] <- round(freq_r[, 3], 3)
freq_c[, 8] <- round(freq_r[, 4], 3)
freq_c[,10] <- round(freq_r[, 5], 3)

rownames(freq_c) <- c("Akteure", "Technik", "Umwelt", "Zeichen", "gesamt")
colnames(freq_c) <- c("SQ2015", "%", "SQ2016", "%", "SQ2017", "%", "SQ2018", "%", "SQ2020", "%")

freq_c

# Absolute und relative Häufigkeiten als Tabelle
write.csv2(as.data.frame(freq_c), file = "project_haeufigkeiten.csv")

# Gesamtzahl der Elemente
table(project.dev$freAll$Element)
table(project.dev$freAll$Element) / sum(table(project.dev$freAll$Element))


## Balkendiagramm mit ggplot
freq    <- as.data.frame(freq, stringsAsFactors = F)
SQ      <- c(rep("SQ2015", 4), rep("SQ2016", 4), rep("SQ2017", 4), rep("SQ2018", 4), rep("Projektende", 4), rep("SQ2020", 4))
Element <- c(rep(c("Akteure", "Technik", "Umwelt", "Zeichen"), 6))
haeuf   <- c(freq$SQ2015[1:4], freq$SQ2016[1:4], freq$SQ2017[1:4], freq$SQ2018[1:4], rep(0, 4), freq$SQ2020[1:4])

fr <- as.data.frame(cbind(SQ, Element, haeuf), stringsAsFactors = F)
fr$haeuf <- as.numeric(fr$haeuf)
fr$SQ    <- factor(fr$SQ, levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"))

# Stacked barplot with multiple groups
dev.off()
bp <- ggplot(data=fr, aes(x=SQ, y=haeuf, fill=Element)) + 
  geom_bar(stat="identity", width = .75) + 
  scale_fill_manual(values=farben) +
  scale_x_discrete(breaks = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")) +
  ggtitle("Häufigkeitsverteilung von Elementtypen", subtitle = "project") +
  theme(
    #plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right", legend.title = element_blank()) +
  theme(legend.text = element_text(color = "grey30")); bp

ggsave("project_haeufigkeiten.png", device="png", dpi = 600,
       width = 200, height = 120, units = "mm")


rm(freq, freq_c, freq_r, Akteure, Technik, Umwelt, Zeichen,
   fr, bp, SQ, Element, haeuf)





#### Konsolidierung des Innovationsprozesses ####

cart <- project.dev$freAll

k5 <- subset(cart, (cart[, 2] == 1 & cart[, 3] == 1 & cart[, 4] == 1 & cart[, 5] == 1 & cart[, 6] == 1))
k4 <- subset(cart, (cart[, 2] == 0 & cart[, 3] == 1 & cart[, 4] == 1 & cart[, 5] == 1 & cart[, 6] == 1))
k3 <- subset(cart, (cart[, 2] == 0 & cart[, 3] == 0 & cart[, 4] == 1 & cart[, 5] == 1 & cart[, 6] == 1))
k2 <- subset(cart, (cart[, 2] == 0 & cart[, 3] == 0 & cart[, 4] == 0 & cart[, 5] == 1 & cart[, 6] == 1))
k1 <- subset(cart, (cart[, 2] == 0 & cart[, 3] == 0 & cart[, 4] == 0 & cart[, 5] == 0 & cart[, 6] == 1))

## leeren Data Frame erstellen
konst <- data.frame(
  SQ = factor(c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"),
              levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020")),
  val = as.numeric(c(colSums(cart[, 2:5]), NA, sum(cart[, 6]))),
  Akteure = NA, 
  Technik = NA, 
  Umwelt = NA, 
  Zeichen = NA,
  gesamt = NA
)


## Data Frame mit Werten füllen (konstante Elemente)
konst[2, c(3:6)] <- c(sum(k5$Element == "Akteur"), sum(k5$Element == "Technik"),
                      sum(k5$Element == "Umwelt"), sum(k5$Element == "Zeichen"))
konst[3, c(3:6)] <- c(sum(k4$Element == "Akteur"), sum(k4$Element == "Technik"),
                      sum(k4$Element == "Umwelt"), sum(k4$Element == "Zeichen"))
konst[4, c(3:6)] <- c(sum(k3$Element == "Akteur"), sum(k3$Element == "Technik"),
                      sum(k3$Element == "Umwelt"), sum(k3$Element == "Zeichen"))
konst[6, c(3:6)] <- c(sum(k2$Element == "Akteur"), sum(k2$Element == "Technik"),
                      sum(k2$Element == "Umwelt"), sum(k2$Element == "Zeichen"))

konst[3, c(3:6)] <- konst[2, c(3:6)] + konst[3, c(3:6)]
konst[4, c(3:6)] <- konst[3, c(3:6)] + konst[4, c(3:6)]
konst[6, c(3:6)] <- konst[4, c(3:6)] + konst[6, c(3:6)]

konst$gesamt[2:6] <- rowSums(konst[2:6, 3:6], na.rm=T)

lab <- round((konst$gesamt/konst$val*100), 1)
lab <- paste0(lab, "%")
lab[c(1, 5)] <- ""

## Werte für 2019 interpolieren
for(i in 3:7){konst[5, i] <- mean(c(konst[c(4, 6), i]))}

colors <- c("Akteure" = farben[1], "Technik" = farben[2], "Umwelt" = farben[3], "Zeichen" = farben[4], "gesamt" = "grey40")

kplot <- ggplot(data = konst, aes(x = SQ, group = 1)) +
  geom_bar(stat = "identity", aes(y = val), width = .75, fill = alpha("grey", .3)) +
  geom_line(aes(y = Akteure), col = "grey50", lwd = 2.5) +
  geom_line(aes(y = Technik), col = "grey50", lwd = 2.5) +
  geom_line(aes(y = Umwelt) , col = "grey50", lwd = 2.5) +
  geom_line(aes(y = Zeichen), col = "grey50", lwd = 2.5) +
  geom_line(aes(y = Akteure, color = "Akteure"), lwd = 2) +
  geom_line(aes(y = Technik, color = "Technik"), lwd = 2) +
  geom_line(aes(y = Umwelt , color = "Umwelt"), lwd = 2) +
  geom_line(aes(y = Zeichen, color = "Zeichen"), lwd = 2) +
  geom_line(aes(y = gesamt,  color = "gesamt"), lwd = 2.5) +
  scale_x_discrete(breaks = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")) +
  ggtitle("Verstetigung durch konsolidierte Elemente", subtitle = "project") +
  geom_text(aes(x = SQ, y = gesamt, label = lab, vjust = -1, hjust = 1)) +
  theme(
    #plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  labs(color = "") +
  scale_color_manual(values = colors, breaks=c("Akteure","Technik","Umwelt", "Zeichen", "gesamt"))

kplot

ggsave("project_verstetigung.png", device="png", dpi = 600,
       width = 200, height = 120, units = "mm")



#### Innovationsdynamik ####

all <- data.frame(cbind("SQ" = colnames(project.dev$fluAll),
                        "val"       = project.dev$fluAll[4, ],
                        "Zugänge"   = project.dev$fluAll[1, ],
                        "Konstante" = project.dev$fluAll[2, ],
                        "Abgänge"   = project.dev$fluAll[3, ]))

all <- rbind(all[1:4, ], c("Projektende", rep(NA, 4)), all[5, ])

all$SQ        <- factor(all$SQ, levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"))
all$val       <- as.numeric(all$val)
all$Zugänge   <- as.numeric(all$Zugänge)
all$Konstante <- as.numeric(all$Konstante)
all$Abgänge   <- as.numeric(all$Abgänge)

for(i in 3:5){all[5, i] <- mean(all[c(4, 6), i])} # fehlende Werte für 2019 interpolieren

dev.off()

colors <- c("wiederholt" = "darkorchid2", "neu" = "seagreen2", "verworfen" = "firebrick2")

pl <- ggplot(data = all, aes(x = SQ, group = 1)) +
  geom_bar(stat = "identity", aes(y = val), fill = alpha("grey", .3), width = .75) +
  geom_line(aes(y = Konstante), color = "grey50", size = 2.5) +
  geom_line(aes(y = Zugänge),   color = "grey50", size = 2.5) +
  geom_line(aes(y = Abgänge),   color = "grey50", size = 2.5) +
  geom_line(aes(y = Konstante,  color = "wiederholt"), size = 2) +
  geom_line(aes(y = Zugänge,    color = "neu"), size = 2) +
  geom_line(aes(y = Abgänge,    color = "verworfen"), size = 2) +
  scale_x_discrete(breaks = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")) +
  ggtitle("Innovationsdynamik", subtitle = "project") +
  theme(
    #plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) + 
  labs(color = "Elemente") +
  scale_color_manual(values = colors)

pl

ggsave("project_dynamik_gesamt.png", device="png", dpi = 600,
       width = 200, height = 120, units = "mm")




#### Veränderungsdynamik ####

project.dyn <- dynamic(project.dev)

dyn_table <- cbind(project.dyn$Akteure, project.dyn$Technik, project.dyn$Umwelt, project.dyn$Zeichen, project.dyn$gesamt)
dyn_table <- rbind(1, dyn_table[1:3, ], NA, dyn_table[4, ])
dyn_table <- cbind(c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"), dyn_table)

dyn_table[dyn_table == "NaN"] <- 0
dyn_table[dyn_table == "Inf"] <- 0

colnames(dyn_table) <- c("SQ", "Akteure", "Technik", "Umwelt", "Zeichen", "gesamt")
rownames(dyn_table) <- NULL

dyn_table <- data.frame(dyn_table)

for(i in 2:6){dyn_table[, i] <- as.numeric(dyn_table[, i])}
for(i in 2:6){dyn_table[5, i] <- mean(dyn_table[c(4, 6), i])}
dyn_table$SQ <- factor(dyn_table$SQ, levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"))

colors <- c("Akteure" = farben[1], "Technik" = farben[2], "Umwelt" = farben[3], "Zeichen" = farben[4], "gesamt" = "grey40")

dynplot <- ggplot(data = dyn_table, aes(x = SQ, group = 1)) +
  geom_hline(yintercept = 1, col = "forestgreen", lwd = .5, lty = 5) +
  geom_rect(xmin = 0, xmax = 10, ymin = log10(0), ymax = log10(1), fill = alpha("forestgreen", .03)) +
  geom_line(aes(y = Akteure), col = "grey50", lwd = 2.5) +
  #  geom_line(aes(y = Technik), col = "grey50", lwd = 2.5) +
  #  geom_line(aes(y = Umwelt),  col = "grey50", lwd = 2.5) +
  geom_line(aes(y = Zeichen), col = "grey50", lwd = 2.5) +
  geom_line(aes(y = Akteure, color = "Akteure"), lwd = 2) +
  geom_line(aes(y = Technik, color = "Technik"), lwd = 2, alpha = .3) +
  geom_line(aes(y = Umwelt,  color = "Umwelt"), lwd = 2, alpha = .3) +
  geom_line(aes(y = Zeichen, color = "Zeichen"), lwd = 2) +
  geom_line(aes(y = gesamt,  color = "gesamt"), lwd = 3) +
  ggtitle("Veränderungsdynamik", subtitle = "project") +
  scale_x_discrete(breaks = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")) +
  scale_y_continuous(trans="log10", breaks = c(1, 2, 4, 6, 8, 10)) +
  theme(
    #plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  labs(color = "") +
  scale_color_manual(values = colors, breaks=c("Akteure","Technik","Umwelt", "Zeichen", "gesamt"))

dynplot

ggsave("project_dynamik_durchsatz.png", device="png", dpi = 600,
       width = 200, height = 120, units = "mm")


#write.csv2(dyn_table, file = "project_dyn.csv")

#### Rauschen ####

project.dev$freAll

tempo <- project.dev$freAll[, 6] == 0
tempo <- table(tempo)[2]

gesamt <- dim(project.dev$freAll)[1]

tempo/(gesamt-tempo)

#### Gebundenheit ####

project.nxeltyp  <- nxeltyp(project)
project.nxeldist <- nxeldist(project.nxeltyp)

### Distanzen (Durchschnitt) zum nächsten Elementtyp in Prozent der Diagonale der Kartierung
project.dist <- list(
  "SQ2015" = project.nxeldist$SQ2015 / sqrt(project$dimensions$rangeX^2 + project$dimensions$rangeY^2)[1],
  "SQ2016" = project.nxeldist$SQ2016 / sqrt(project$dimensions$rangeX^2 + project$dimensions$rangeY^2)[2],
  "SQ2017" = project.nxeldist$SQ2017 / sqrt(project$dimensions$rangeX^2 + project$dimensions$rangeY^2)[3],
  "SQ2018" = project.nxeldist$SQ2018 / sqrt(project$dimensions$rangeX^2 + project$dimensions$rangeY^2)[4],
  "SQ2020" = project.nxeldist$SQ2020 / sqrt(project$dimensions$rangeX^2 + project$dimensions$rangeY^2)[5]
)

## Veränderung der Distanzen zwischen Akteuren und Zeichen

Daz <- Dza <- rep(NA, 5)
for(i in 1:5){Daz[i] <- project.dist[[i]]["Akteur", "Zeichen"]}
for(i in 1:5){Dza[i] <- project.dist[[i]]["Zeichen", "Akteur"]}

distanzen <- data.frame("SQ" = Jahr[1:5], "DAZ" = Daz, "DZA" = Dza)

rm(Daz, Dza)    

### Tabelle für Gebundenheit der Elementtypen erstellen

tab_bin <- vector(mode = "list", length = 5)
names(tab_bin) <- c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")

for(i in 1:5){
  
  nx <- project.nxeltyp[[i]]
  nx.a <- subset(nx, nx$id_el == "Akteur")
  nx.t <- subset(nx, nx$id_el == "Technik")
  nx.u <- subset(nx, nx$id_el == "Umwelt")
  nx.z <- subset(nx, nx$id_el == "Zeichen")
  
  an <- dim(nx.a)[1]
  tn <- dim(nx.t)[1]
  un <- dim(nx.u)[1] 
  zn <- dim(nx.z)[1] 
  
  # Bindung der Akteure an andere Elementtypen #
  aan <- length(unique(nx.a$id_a)) # gebundene Akteure
  atn <- length(unique(nx.a$id_t)) # gebundene Technik
  aun <- length(unique(nx.a$id_u)) # gebundene Umwelt
  azn <- length(unique(nx.a$id_z)) # gebundene Zeichen
  
  # Bindung der Technik an andere Elementtypen #
  tan <- length(unique(nx.t$id_a)) # gebundene Akteure
  ttn <- length(unique(nx.t$id_t)) # gebundene Technik
  tun <- length(unique(nx.t$id_u)) # gebundene Umwelt
  tzn <- length(unique(nx.t$id_z)) # gebundene Zeichen
  
  # Bindung der Umwelt an andere Elementtypen #
  uan <- length(unique(nx.u$id_a)) # gebundene Akteure
  utn <- length(unique(nx.u$id_t)) # gebundene Technik
  uun <- length(unique(nx.u$id_u)) # gebundene Umwelt
  uzn <- length(unique(nx.u$id_z)) # gebundene Zeichen
  
  # Bindung der Zeichen an andere Elementtypen #
  zan <- length(unique(nx.z$id_a)) # gebundene Akteure
  ztn <- length(unique(nx.z$id_t)) # gebundene Technik
  zun <- length(unique(nx.z$id_u)) # gebundene Umwelt
  zzn <- length(unique(nx.z$id_z)) # gebundene Zeichen
  
  
  # Akteur #
  x <- rep(NA, 4)
  if(an >= an){x[1] <- 1} else{x[1] <- an / an}
  if(an >= tn){x[2] <- 1} else{x[2] <- an / tn}
  if(an >= un){x[3] <- 1} else{x[3] <- an / un}
  if(an >= zn){x[4] <- 1} else{x[4] <- an / zn}
  
  y <- rep(NA, 4)
  y[1] <- aan / an
  y[2] <- atn / tn
  y[3] <- aun / un
  y[4] <- azn / zn
  
  bin_akt <- rbind(x, y)
  
  # Technik #
  x <- rep(NA, 4)
  if(tn >= an){x[1] <- 1} else{x[1] <- tn / an}
  if(tn >= tn){x[2] <- 1} else{x[2] <- tn / tn}
  if(tn >= un){x[3] <- 1} else{x[3] <- tn / un}
  if(tn >= zn){x[4] <- 1} else{x[4] <- tn / zn}
  
  y <- rep(NA, 4)
  y[1] <- tan / an
  y[2] <- ttn / tn
  y[3] <- tun / un
  y[4] <- tzn / zn
  
  bin_tec <- rbind(x, y)
  
  # Umwelt #
  x <- rep(NA, 4)
  if(un >= an){x[1] <- 1} else{x[1] <- un / an}
  if(un >= tn){x[2] <- 1} else{x[2] <- un / tn}
  if(un >= un){x[3] <- 1} else{x[3] <- un / un}
  if(un >= zn){x[4] <- 1} else{x[4] <- un / zn}
  
  y <- rep(NA, 4)
  y[1] <- uan / an
  y[2] <- utn / tn
  y[3] <- uun / un
  y[4] <- uzn / zn
  
  bin_umw <- rbind(x, y)
  
  # Zeichen #
  x <- rep(NA, 4)
  if(zn >= an){x[1] <- 1} else{x[1] <- zn / an}
  if(zn >= tn){x[2] <- 1} else{x[2] <- zn / tn}
  if(zn >= un){x[3] <- 1} else{x[3] <- zn / un}
  if(zn >= zn){x[4] <- 1} else{x[4] <- zn / zn}
  
  y <- rep(NA, 4)
  y[1] <- zan / an
  y[2] <- ztn / tn
  y[3] <- zun / un
  y[4] <- zzn / zn
  
  bin_zei <- rbind(x, y)
  
  tab_bin[[i]] <- rbind(
    (bin_akt[2, ] / bin_akt[1, ]),
    (bin_tec[2, ] / bin_tec[1, ]),
    (bin_umw[2, ] / bin_umw[1, ]),
    (bin_zei[2, ] / bin_zei[1, ])); 
  
  tab_bin[[i]] <- round(tab_bin[[i]], 3)
  
  colnames(tab_bin[[i]]) <- c("Akteur", "Technik", "Umwelt", "Zeichen")
  rownames(tab_bin[[i]]) <- c("Akteur", "Technik", "Umwelt", "Zeichen")
  
}

## Variablen entfernen
rm(nx, nx.a, nx.t, nx.u, nx.z,
   an, tn, un, zn,
   aan, atn, aun, azn,
   tan, ttn, tun, tzn,
   uan, utn, uun, uzn,
   zan, ztn, zun, zzn,
   x, y, 
   bin_akt, bin_tec, bin_umw, bin_zei)

## Inf und Nan bereinigen #
for(i in 1:5){
  tab_bin[[i]][which(is.infinite(tab_bin[[i]]) == T)] <- NA
  tab_bin[[i]][which(is.nan(tab_bin[[i]]) == T)] <- NA
}


### beides plotten

library(gtable)

## Plot für Distanzen
colors <- c("DAZ" = farben[1], "DZA" = farben[4])

distan <- rbind(distanzen[1:4,], NA, distanzen[5, ])
distan$SQ <- factor(c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"),
                    levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"))
distan$DAZ[5] <- mean(distan$DAZ[4:6], na.rm=T)
distan$DZA[5] <- mean(distan$DZA[4:6], na.rm=T)

distpl <- ggplot(data = distan, aes(x = SQ, group = 1)) +
  geom_line(aes(y = DAZ, color = "DAZ"), lwd = 2) +
  geom_line(aes(y = DZA, color = "DZA"), lwd = 2) +
  ggtitle("Distanzen zwischen Elementen", subtitle = "project") +
  ylab("Distanzen") +
  theme(
    #plot.title = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom") +
  labs(color = "") +
  scale_color_manual(values = colors, breaks=c("DAZ", "DZA"), 
                     labels = c("Akteure \u21d2 Zeichen", "Zeichen \u21d2 Akteure")) +
  scale_x_discrete(breaks = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020"))


## Plot für Gebundenheit
ght <- data.frame(
  "SQ" = Jahr[1:5],
  "geb" = c(
    mean(tab_bin$SQ2015, na.rm = T), 
    mean(tab_bin$SQ2016, na.rm = T), 
    mean(tab_bin$SQ2017, na.rm = T), 
    mean(tab_bin$SQ2018, na.rm = T),
    mean(tab_bin$SQ2020, na.rm = T)))

ght <- rbind(ght[1:4, ], NA, ght[5, ])
ght$SQ <- factor(c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"),
                 levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"))
ght$geb[5] <- mean(ght$geb[4:6], na.rm=T)

ghtpl <- ggplot(data = ght, aes(x = SQ, group = 1)) +
  geom_line(aes(y = geb), lwd = 2) +
  ylab("Gebundenheit") +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank()
  ) +
  scale_x_discrete(breaks = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020"))


## Plots zusammenführen

g <- rbind(ggplotGrob(distpl), ggplotGrob(ghtpl))

plot(g)

ggsave(g, filename = "project_distanzen.png", device = "png", dpi = 600,
       width = 200, height = 120, units = "mm")
