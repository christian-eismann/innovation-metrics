dsbTable <- function(cart){
  ##### Häufigkeiten #####
  
  
  cart.dev <- development(cart)
  
  Akteure <- cart.dev$fluA[4, ]
  Technik <- cart.dev$fluT[4, ]
  Umwelt  <- cart.dev$fluU[4, ]
  Zeichen <- cart.dev$fluZ[4, ]
  
  freq <- rbind(Akteure, Technik, Umwelt, Zeichen)
  freq <- rbind(freq, colSums(freq))
  freq <- as.data.frame(freq, stringsAsFactors = F)
  rownames(freq)[5] <- "gesamt"
  
  SQ      <- c(rep("SQ2015", 4), rep("SQ2016", 4), rep("SQ2017", 4), rep("SQ2018", 4), rep("Projektende", 4), rep("SQ2020", 4))
  Element <- c(rep(c("Akteure", "Technik", "Umwelt", "Zeichen"), 6))
  haeuf   <- c(freq$SQ2015[1:4], freq$SQ2016[1:4], freq$SQ2017[1:4], freq$SQ2018[1:4], rep(0, 4), freq$SQ2020[1:4])
  
  fr       <- as.data.frame(cbind(SQ, Element, haeuf), stringsAsFactors = F)
  fr$haeuf <- as.numeric(fr$haeuf)
  fr$SQ    <- factor(fr$SQ, levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"))
  
  
  ##### Konsolidierung des Innovationsprozesses #####
  
  cart_ko <- cart.dev$freAll
  
  k5 <- subset(cart_ko, (cart_ko[, 2] == 1 & cart_ko[, 3] == 1 & cart_ko[, 4] == 1 & cart_ko[, 5] == 1 & cart_ko[, 6] == 1))
  k4 <- subset(cart_ko, (cart_ko[, 2] == 0 & cart_ko[, 3] == 1 & cart_ko[, 4] == 1 & cart_ko[, 5] == 1 & cart_ko[, 6] == 1))
  k3 <- subset(cart_ko, (cart_ko[, 2] == 0 & cart_ko[, 3] == 0 & cart_ko[, 4] == 1 & cart_ko[, 5] == 1 & cart_ko[, 6] == 1))
  k2 <- subset(cart_ko, (cart_ko[, 2] == 0 & cart_ko[, 3] == 0 & cart_ko[, 4] == 0 & cart_ko[, 5] == 1 & cart_ko[, 6] == 1))
  k1 <- subset(cart_ko, (cart_ko[, 2] == 0 & cart_ko[, 3] == 0 & cart_ko[, 4] == 0 & cart_ko[, 5] == 0 & cart_ko[, 6] == 1))
  
  ## leeren Data Frame erstellen
  konst <- data.frame(
    SQ = factor(c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"),
                levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020")),
    val = as.numeric(c(colSums(cart_ko[, 2:5]), NA, sum(cart_ko[, 6]))),
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
  
  ## Werte für 2019 interpolieren
  for(i in 3:7){konst[5, i] <- mean(c(konst[c(4, 6), i]))}
  
  rm(k1, k2, k3, k4, k5, cart_ko)
  
  ##### Innovationsdynamik #####
  
  idyn <- data.frame(cbind("SQ" = colnames(cart.dev$fluAll),
                           "val"        = cart.dev$fluAll[4, ],
                           "Zugaenge"   = cart.dev$fluAll[1, ],
                           "Konstante"  = cart.dev$fluAll[2, ],
                           "Abgaenge"   = cart.dev$fluAll[3, ]))
  
  idyn <- rbind(idyn[1:4, ], c("Projektende", rep(NA, 4)), idyn[5, ])
  
  idyn$SQ         <- factor(idyn$SQ, levels = c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "Projektende", "SQ2020"))
  idyn$val        <- as.numeric(idyn$val)
  idyn$Zugaenge   <- as.numeric(idyn$Zugaenge)
  idyn$Konstante  <- as.numeric(idyn$Konstante)
  idyn$Abgaenge   <- as.numeric(idyn$Abgaenge)
  
  for(i in 3:5){idyn[5, i] <- mean(idyn[c(4, 6), i])} # fehlende Werte für 2019 interpolieren
  ##### Veränderungsdynamik #####
  
  cart.dyn <- dynamic(cart.dev)
  
  dyn_table <- cbind(cart.dyn$Akteure, cart.dyn$Technik, cart.dyn$Umwelt, cart.dyn$Zeichen, cart.dyn$gesamt)
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
  
  ##### Distanzen und Gebundenheit #####
  
  cart.nxeltyp  <- nxeltyp(cart)
  cart.nxeldist <- nxeldist(cart.nxeltyp)
  
  ### Distanzen (Durchschnitt) zum nächsten Elementtyp in Prozent der Diagonale der Kartierung
  cart.dist <- list(
    "SQ2015" = cart.nxeldist$SQ2015 / sqrt(cart$dimensions$rangeX^2 + cart$dimensions$rangeY^2)[1],
    "SQ2016" = cart.nxeldist$SQ2016 / sqrt(cart$dimensions$rangeX^2 + cart$dimensions$rangeY^2)[2],
    "SQ2017" = cart.nxeldist$SQ2017 / sqrt(cart$dimensions$rangeX^2 + cart$dimensions$rangeY^2)[3],
    "SQ2018" = cart.nxeldist$SQ2018 / sqrt(cart$dimensions$rangeX^2 + cart$dimensions$rangeY^2)[4],
    "SQ2020" = cart.nxeldist$SQ2020 / sqrt(cart$dimensions$rangeX^2 + cart$dimensions$rangeY^2)[5]
  )
  
  ## Veränderung der Distanzen zwischen Akteuren und Zeichen
  
  Daz <- Dza <- rep(NA, 5)
  for(i in 1:5){Daz[i] <- cart.dist[[i]]["Akteur", "Zeichen"]}
  for(i in 1:5){Dza[i] <- cart.dist[[i]]["Zeichen", "Akteur"]}
  
  distanzen <- data.frame("SQ" = Jahr[1:5], "DAZ" = Daz, "DZA" = Dza)
  
  rm(Daz, Dza)    
  
  ### Tabelle für Gebundenheit der Elementtypen erstellen
  
  tab_bin <- vector(mode = "list", length = 5)
  names(tab_bin) <- c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")
  
  for(i in 1:5){
    
    nx <- cart.nxeltyp[[i]]
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
  
  
  
  ##### Liste erstellen #####
  
  cart.dsb <- vector(mode = 'list', length = 6)
  cart.dsb [[1]] <- fr
  cart.dsb [[2]] <- konst
  cart.dsb [[3]] <- idyn
  cart.dsb [[4]] <- dyn_table
  cart.dsb [[5]] <- distanzen
  cart.dsb [[6]] <- ght
  names(cart.dsb) <- c("freq", "konst", "idyn", "vdyn", "dist", "bin")
  
  rm(fr, freq, konst, idyn, dyn_table, distanzen, tab_bin)
  
  return(cart.dsb)
}


