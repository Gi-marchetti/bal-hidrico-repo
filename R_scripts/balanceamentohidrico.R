
#' Function to plot water balance
#'
#' @description Function to plot water balance.
#' @param bh Water balance in data.frame format from watbal() function.
#' @return Plot of water balance
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual theme_minimal theme guides xlab ylab scale_x_discrete ggtitle  element_text element_blank guide_legend  rel unit
#' @import reshape2
#' @importFrom grDevices rgb
#' @examples
#' wb <- watbal(t = c(10, 11.5, 14, 16.5, 20, 24.5, 27.5, 28, 24.5, 19.5, 14.5, 11),
#' p = c(55, 73, 84, 58, 33, 23, 2, 2, 28, 66, 94, 71), lat = 35, CC = 400)
#' plotWatbal(wb)
#' @export
#'

library(bioclim)
library(ggplot2)

# Função para calcular o balanço hídrico
watball_daily <- function(t, p, lat, CC){
  t <- na.omit(t)
  p <- na.omit(p)
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
}  

plotWatbal <- function(bh){
  #
  n <- nrow(bh)
  days <- seq.Date(from = as.Date("2024-01-05"), by = "day", length.out = n)
  dbh <- data.frame(
    days = days,
    white = NA,
    water_exc = NA,
    soil_moist = NA,
    wat_def = NA,
    soil_rech = NA
  )

  for(i in 1:n){
    dbh$white[i] <- min(bh$PET[i], bh$P[i])
    if(bh$RET[i] == bh$PET[i]) {
      if(round(bh$ME[i], 2) > 0) {
        dbh$water_exc[i] <- bh$P[i] - dbh$white[i]
      } else {
        dbh$water_exc[i] <- 0
      }
    } else {
      dbh$water_exc[i] <- 0
    }

    if(bh$PET[i] > bh$P[i]) {
      dbh$soil_moist[i] <- bh$RET[i] - sum(dbh$white[i], dbh$water_exc[i], na.rm = TRUE)
    } else {
      dbh$soil_moist[i] <- 0
    }

    if(bh$PET[i] > bh$RET[i]) {
      dbh$wat_def[i] <- bh$PET[i] - sum(dbh$white[i], dbh$soil_moist[i], dbh$water_exc[i], na.rm = TRUE)
    } else {
      dbh$wat_def[i] <- 0
    }

    if(bh$PET[i] > bh$P[i]) {
      dbh$soil_rech[i] <- 0
    } else {
      if(bh$RET[i] == bh$PET[i]) {
        if(round(bh$ME[i], 2) == 0) {
          dbh$soil_rech[i] <- bh$P[i] - sum(dbh$white[i], dbh$soil_moist[i], dbh$wat_def[i], dbh$water_exc[i], na.rm = TRUE)
        } else {
          dbh$soil_rech[i] <- 0
        }
      } else {
        dbh$soil_rech[i] <- 0
      }
    }
  }

  dbh <- dbh[, c('days', 'wat_def', 'soil_moist', 'soil_rech', 'water_exc', 'white')]
  names(dbh) <- c('days', 'Water deficit', 'Soil water use', 'Soil water recharge', 'Water exceedance', ' ')

  dbh_melted <- melt(dbh, id.vars = "days")

  ggplot(dbh_melted[order(dbh_melted$variable),], aes(x = days, y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    scale_fill_manual("legend", values = c(" " = rgb(1, 1, 1, 0),
                                           "Water exceedance" = "darkblue",
                                           "Soil water use" = "orange",
                                           'Water deficit' = 'red',
                                           'Soil water recharge' = 'lightblue')) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_text(size = rel(0.8)),
          plot.title = element_text(size = rel(0.9), face = 'bold'),
          legend.spacing.x = unit(0.15, 'cm')) +
    xlab('') + ylab('mm') +
    scale_x_date(date_labels = "%b-%d", date_breaks = "1 month") +
    ggtitle("Daily Water Balance")
}


data <- read.csv("../data_archives/temp_prec.csv", skip = 10, header = FALSE)
colnames(data) <- c("Year", "DOY", "T2M", "PRECTOTCORR")
temperatura_lista <- unlist(data$T2M)
precipitacao_lista <- unlist(data$PRECTOTCORR)
temperatura_lista <- as.numeric(temperatura_lista)
precipitacao_lista <- as.numeric(precipitacao_lista)
print(temperatura_lista)
wb <- watball_daily(t = temperatura_lista, p = precipitacao_lista, lat = 22, CC = 400)
plotWatbal(wb)
