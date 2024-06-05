
library(bioclim)
library(ggplot2)

# Função para calcular o balanço hídrico
watball_daily <- function(t, p, lat, CC){
  n <- length(t)
  balhid <- matrix(NA, ncol = 12, nrow = n)
  colnames(balhid) <-  c('Tmp', 'Pcp', 'PET', 'P_PET', 'ppa', 'ST', 'i_ST', 'ETR', 'Dh', 'S', 'r', 'rP')
  
  balhid[, 1] <- t
  balhid[, 2] <- p
  balhid[, 3] <- thornthwaite(t, lat)
  balhid[, 4] <- balhid[, 2] - balhid[, 3]
  
  p_pet <- balhid[, 4]
  ppa <- p_pet
  w1 <- which(p_pet > 0)
  w2 <- which(p_pet <= 0)
  if(length(w1) > 0){
    ppa[w1] <- 0
  }
  if(length(w2) > 0){
    for(h in w2){
      if(h == 1){
        ppa[h] <- p_pet[h]
      } else {
        ppa[h] <- ppa[h - 1] + p_pet[h]
      }
    }
  }
  
  st <- rep(NA, n)
  for(i in 1:n){
    if(i == 1){
      if(p_pet[i] > 0){
        if(p_pet[i] > CC) {st[i] <- CC}
        else {st[i] <- p_pet[i]}
      } else {
        st[i] <- CC * exp(ppa[i] / CC)
      }
    } else {
      if(p_pet[i] > 0){
        if(p_pet[i] + st[i - 1] > CC) {st[i] <- CC}
        else {st[i] <- p_pet[i] + st[i - 1]}
      } else {
        st[i] <- CC * exp(ppa[i] / CC)
      }
    }
  }
  
  k <- cbind(ppa, st)
  seg <- TRUE
  it <- 0
  while(it < 20) {
    it <- it + 1
    ppa2 <- ppa
    for(h in 1:n){
      if(h == n){
        if(p_pet[1] >= 0) {
          if(p_pet[h] >= 0){ppa2[h] <- 0}
          else {
            ppa2[h] <- ppa2[h - 1] + p_pet[h]
          }
        } else {
          if(p_pet[h] >= 0){if(st[h - 1] + p_pet[h] < CC) {ppa2[h] <- log(st[h] / CC) * CC} else 0}
          else {
            ppa2[h] <- ppa2[h - 1] + p_pet[h]
          }
        }
      } else {
        if(p_pet[h + 1] >= 0) {
          if(p_pet[h] >= 0){ppa2[h] <- 0}
          else {
            if(h == 1){ppa2[h] <- ppa[length(ppa)] + p_pet[h]} else {ppa2[h] <- ppa2[h - 1] + p_pet[h]}
          }
        } else {
          if(p_pet[h] >= 0){if(st[length(st)] + p_pet[h] < CC) ppa2[h] <- log(st[h] / CC) * CC else 0}
          else {
            if(h == 1){ppa2[h] <- ppa[length(ppa)] + p_pet[h]} else {ppa2[h] <- ppa2[h - 1] + p_pet[h]}
          }
        }
      }
    }
    
    st2 <- st
    for(i in 1:n){
      if(h == n) ini_p_pet <- p_pet[1] else ini_p_pet <- p_pet[i + 1]
      if(ini_p_pet >= 0) {
        if(p_pet[i] >= 0){
          if(i == 1){if((st[length(st)] + p_pet[i]) > CC) {st2[i] <- CC} else {st2[i] <- (st[length(st)] + p_pet[i])}}
          else {
            if((st2[i - 1] + p_pet[i]) > CC) {st2[i] <- CC} else {st2[i] <- (st2[i - 1]  + p_pet[i])}
          }
        } else {st2[i] <- CC * exp(ppa2[i] / CC)}
      } else {
        if(p_pet[i] >= 0){
          if(i == 1){if((st[length(st)] + p_pet[i]) > CC) {st2[i] <- CC} else {st2[i] <- (st[length(st)] + p_pet[i])}}
          else {
            if((st2[i - 1] + p_pet[i]) > CC) {st2[i] <- CC} else {st2[i] <- (st2[i - 1]  + p_pet[i])}
          }
        } else {st2[i] <- CC * exp(ppa2[i] / CC)}
      }
    }
    k1 <- cbind(ppa2, st2)
    k <- k1
    ppa <- ppa2
    st <- st2
    rm(ppa2, st2)
  }
  
  balhid[, 5] <- k[, 1]
  balhid[, 6] <- k[, 2]
  balhid[, 7] <-  c((balhid[1, 6] - balhid[n, 6]), (balhid[2:n, 6] - balhid[1:(n-1), 6]))
  w1 <- which(p_pet >= 0)
  balhid[w1, 8] <- balhid[w1, 3]
  w2 <- which(p_pet < 0)
  balhid[w2, 8] <- balhid[w2, 2] + abs(balhid[w2, 7])
  balhid[, 9] <- balhid[, 8] - balhid[, 3]
  balhid[, 10] <- 0
  w1 <- which(p_pet >= 0)
  balhid[w1, 10] <- balhid[w1, 2] - (balhid[w1, 8] + balhid[w1, 7])
  balhid[, 11] <- 0
  w <- which(p_pet >= 0 & balhid[, 6] >= CC)
  if(length(w) > 0){
    if(w[1] == 1) {rec <- 1:n} else {rec <- c(w[1]:n, 1:(w[1]-1))}
    rnf <- rep(0, n)
    seguir <- TRUE
    for(i in rec){
      if(i == 1) {
        rnf[i] <- 0.5 * (balhid[i, 10] + rnf[n])
      } else {
        rnf[i] <- 0.5 * (balhid[i, 10] + rnf[i - 1])
      }
    }
    while(seguir == TRUE){
      rnf2 <- rnf
      for(i in rec){
        if(i == 1) {
          rnf2[i] <- 0.5 * (balhid[i, 10] + rnf2[n])
        } else {
          rnf2[i] <- 0.5 * (balhid[i, 10] + rnf2[i - 1])
        }
      }
      if(identical(rnf, rnf2)){seguir <- FALSE} else{
        rnf <- rnf2
      }
    }
    r <- rnf
  } else { r <- rep(0, n) }
  balhid[, 11] <- r
  balhid[, 12] <- (balhid[, 11] * 100) / balhid[, 2]
  w <- which(is.nan(balhid[, 12]) | is.infinite(balhid[, 12]))
  if(length(w) > 0) balhid[w, 12] <- 0
  
  colnames(balhid) <-  c
  
  tabela <- read.csv('/home/ramoraes/R_scripts/dados2.csv')
  
  watball_daily(t )