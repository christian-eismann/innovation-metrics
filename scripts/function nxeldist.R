nxeldist <- function(nxeltyp_innogruppe){
  
  run <- c("Akteur", "Technik", "Umwelt", "Zeichen")
  
  nxteldist <- matrix(nrow = 4, ncol = 4)
  rownames(nxteldist) <- run
  colnames(nxteldist) <- run
  
  list_nxeldist <- vector(mode = "list", length = 5)
  names(list_nxeldist) <- Jahr[1:5]
  
  for(k in 1:5){
    
    for(j in 1:4){
      
      nxel_sub <- as.data.frame(nxeltyp_innogruppe[[k]], stringsAsFactors = F)
      nxel_sub <- subset(nxel_sub, nxel_sub$id_el == run[j])
      
      n   <- dim(nxel_sub)[1]
      dis <- rep(NA, n)
      
      attach(nxel_sub)
      
      # Akteur
      for(i in 1:n){
        x <- sort(c(id_x[i], id_ax[i]), decreasing = F)
        y <- sort(c(id_y[i], id_ay[i]), decreasing = F)
        dx <- x[2] - x[1]
        dy <- y[2] - y[1]
        dis[i] <- sqrt(dx^2 + dy^2)
      }; nxteldist[j, 1] <- mean(dis, na.rm = T)
      
      # Technik
      for(i in 1:n){
        x <- sort(c(id_x[i], id_tx[i]), decreasing = F)
        y <- sort(c(id_y[i], id_ty[i]), decreasing = F)
        dx <- x[2] - x[1]
        dy <- y[2] - y[1]
        dis[i] <- sqrt(dx^2 + dy^2)
      }; nxteldist[j, 2] <- mean(dis, na.rm = T)
      
      
      # Umwelt
      for(i in 1:n){
        x <- sort(c(id_x[i], id_ux[i]), decreasing = F)
        y <- sort(c(id_y[i], id_uy[i]), decreasing = F)
        dx <- x[2] - x[1]
        dy <- y[2] - y[1]
        dis[i] <- sqrt(dx^2 + dy^2)
      }; nxteldist[j, 3] <- mean(dis, na.rm = T)
      
      
      # Zeichen
      for(i in 1:n){
        x <- sort(c(id_x[i], id_zx[i]), decreasing = F)
        y <- sort(c(id_y[i], id_zy[i]), decreasing = F)
        dx <- x[2] - x[1]
        dy <- y[2] - y[1]
        dis[i] <- sqrt(dx^2 + dy^2)
      }; nxteldist[j, 4] <- mean(dis, na.rm = T)
      
      detach(nxel_sub)
      
    }
    
    list_nxeldist[[k]] <- nxteldist
    
  }
  
  return(list_nxeldist)
  
}

