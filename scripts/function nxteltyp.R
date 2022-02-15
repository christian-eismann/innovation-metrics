nxeltyp <- function(innogruppe){
  
  list_nxel <- vector(mode = "list", length = 5)
  
  for(j in 1:5){
    cart <- innogruppe[[j]]
    
    n <- dim(cart)[1]
    
    # Distanzen zwischen allen Elementen berechnen
    {dist_raw <- dist(cbind(cart$x, cart$y), upper = T)
      dist_raw <- as.matrix(dist_raw)
      dist_raw[dist_raw == 0] <- NA}
    
    # Filter für Elementtypen
    {akt <- cart$Element == "Akteur"
      tec <- cart$Element == "Technik"
      umw <- cart$Element == "Umwelt"
      zei <- cart$Element == "Zeichen"}
    
    # Informationen zum Basiselement
    {id    <- cart$id
      id_el <- cart$Element
      id_x  <- cart$x
      id_y  <- cart$y}
    
    # Informationen zum Referenzobjekt: Akteure
    {dist_akt <- dist_raw[, akt]
      n_di <- rep(NA, n)
      n_lf <- rep(NA, n)
      
      for(i in 1:n){
        n_di[i] <- min(dist_akt[i, ], na.rm = T)
        n_lf[i] <- names(which(dist_akt[i, ] == n_di[i]))
      }
      
      n_lf <- as.numeric(n_lf)
      
      id_a  <- cart$id[n_lf]
      id_ae <- cart$Element[n_lf]
      id_ax <- cart$x[n_lf]
      id_ay <- cart$y[n_lf]
      
      nxteltyp <- cbind(id, id_el, id_x, id_y, id_a, id_ae, id_ax, id_ay)
      rm(id, id_el, id_x, id_y, id_a, id_ae, id_ax, id_ay, dist_akt)}
    
    # Informationen zum Referenzobjekt: Technik
#    {dist_tec <- dist_raw[, tec]
#      n_di <- rep(NA, n)
#      n_lf <- rep(NA, n)
#      
#      for(i in 1:n){
#        n_di[i] <- min(dist_tec[i, ], na.rm = T)
#        n_lf[i] <- names(which(dist_tec[i, ] == n_di[i]))
#      }
#      
#      n_lf <- as.numeric(n_lf)
#      
#      id_t  <- cart$id[n_lf]
#      id_te <- cart$Element[n_lf]
#      id_tx <- cart$x[n_lf]
#      id_ty <- cart$y[n_lf]
#      
#      nxteltyp <- cbind(nxteltyp, id_t, id_te, id_tx, id_ty)
#      rm(id_t, id_te, id_tx, id_ty, dist_tec)}
    
    
    
    
    
    
    if(sum(tec) == 0){
      id_t <- id_tx <- id_ty <- NA
      id_te <- "Technik"
    } else if(sum(tec) == 1){
      n_di <- dist_raw[, tec]
      id_t  <- cart$id[tec]
      id_te <- cart$Element[tec]
      id_tx <- cart$x[tec]
      id_ty <- cart$y[tec]
    } else {
      dist_tec <- dist_raw[, tec]
      n_di <- NA
      n_lf <- NA
      
      for(i in 1:n){
        n_di[i] <- min(dist_tec[i, ], na.rm = T)
        n_lf[i] <- names(which(dist_tec[i, ] == n_di[i]))
      }
      n_lf <- as.numeric(n_lf)
      id_t  <- cart$id[n_lf]
      id_te <- cart$Element[n_lf]
      id_tx <- cart$x[n_lf]
      id_ty <- cart$y[n_lf]
      rm(dist_tec)
    }
    
    nxteltyp <- cbind(nxteltyp, id_t, id_te, id_tx, id_ty)
    rm(id_t, id_te, id_tx, id_ty)
    
    
    
    
    
    
    
    
    
    
    
    # Informationen zum Referenzobjekt: Umwelt
    if(sum(umw) == 0){
      id_u <- id_ux <- id_uy <- NA
      id_ue <- "Umwelt"
    } else if(sum(umw) == 1){
      n_di <- dist_raw[, umw]
      id_u  <- cart$id[umw]
      id_ue <- cart$Element[umw]
      id_ux <- cart$x[umw]
      id_uy <- cart$y[umw]
    } else {
      dist_umw <- dist_raw[, umw]
      n_di <- NA
      n_lf <- NA
      
      for(i in 1:n){
        n_di[i] <- min(dist_umw[i, ], na.rm = T)
        n_lf[i] <- names(which(dist_umw[i, ] == n_di[i]))
      }
      n_lf <- as.numeric(n_lf)
      id_u  <- cart$id[n_lf]
      id_ue <- cart$Element[n_lf]
      id_ux <- cart$x[n_lf]
      id_uy <- cart$y[n_lf]
      rm(dist_umw)
    }
    
    nxteltyp <- cbind(nxteltyp, id_u, id_ue, id_ux, id_uy)
    rm(id_u, id_ue, id_ux, id_uy)
    
    # Informationen zum Referenzobjekt: Zeichen
    {dist_zei <- dist_raw[, zei]
      n_di <- rep(NA, n)
      n_lf <- rep(NA, n)
      
      for(i in 1:n){
        n_di[i] <- min(dist_zei[i, ], na.rm = T)
        n_lf[i] <- names(which(dist_zei[i, ] == n_di[i]))
      }
      
      n_lf <- as.numeric(n_lf)
      
      id_z  <- cart$id[n_lf]
      id_ze <- cart$Element[n_lf]
      id_zx <- cart$x[n_lf]
      id_zy <- cart$y[n_lf]
      
      nxteltyp <- cbind(nxteltyp, id_z, id_ze, id_zx, id_zy)
      rm(id_z, id_ze, id_zx, id_zy, dist_zei)
    }
    
    # Informationen zusammenführen
    nxteltyp <- data.frame(nxteltyp, stringsAsFactors = F)
    
    nxteltyp$id_x <- as.numeric(nxteltyp$id_x)
    nxteltyp$id_y <- as.numeric(nxteltyp$id_y)
    
    nxteltyp$id_ax <- as.numeric(nxteltyp$id_ax)
    nxteltyp$id_ay <- as.numeric(nxteltyp$id_ay)
    
    nxteltyp$id_tx <- as.numeric(nxteltyp$id_tx)
    nxteltyp$id_ty <- as.numeric(nxteltyp$id_ty)
    
    nxteltyp$id_ux <- as.numeric(nxteltyp$id_ux)
    nxteltyp$id_uy <- as.numeric(nxteltyp$id_uy)
    
    nxteltyp$id_zx <- as.numeric(nxteltyp$id_zx)
    nxteltyp$id_zy <- as.numeric(nxteltyp$id_zy)
    
    list_nxel[[j]] <- nxteltyp
    names(list_nxel) <- c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020")
  }
  
  rm(nxteltyp, i, j, n, dist_raw, akt, tec, umw, zei, n_di, n_lf)
  
  return(list_nxel)
}


 
  

  

