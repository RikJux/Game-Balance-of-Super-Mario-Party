#get_dice_data() estrae dal database con le facce dei dadi le risorse
#media movimento, varianza movimento, media monete, varianza monete e varietà
get_dice_data <- function(){

  marioparty <- read.csv("dice.csv")
  
  values <- marioparty[,-1]
  mov_values <- values[,1:6]
  coin_values <- values[,7:12]
  
  character <- marioparty$Character
  mov_variety <- marioparty$mov_variety
  
  mov_mean <- rowMeans(mov_values)
  coin_mean <- rowMeans(coin_values)
  
  mov_var <- rowVars(as.matrix(mov_values))
  coin_var <- rowVars(as.matrix(coin_values))
  
  dice_data <- data.frame(character, mov_mean, mov_var, coin_mean, coin_var,
                          mov_variety)
  
  return(dice_data)

}

basic_univariate_stats <- function(data){
    
    comp.min <- min(data)
    comp.max <- max(data)
    comp.mean <- mean(data)
    comp.var <- var(data)
    comp.sd <- sd(data)
    comp.skw <- skewness(data)
    comp.krt <- kurtosis(data)
    
    return(data.frame(comp.mean, comp.var, comp.sd, comp.skw, comp.krt))

}

#all_2_resources_preferences() ritorna un array con le formule di tutti i trade-off 
#possibili tra due risorse
all_2_resources_preferences <- function(){
  
  preferences <- c(high(mov_mean), high(mov_var), low(mov_var),
                   high(coin_mean), high(coin_var), low(coin_var),
                   high(mov_variety))
  
  
  prod.preferences <- c()
  for(i in 1:length(preferences)){
    if(i < length(preferences)){
      k <- i+1
      for(j in k:length(preferences)){
        p <- preferences[[i]] * preferences[[j]]
        prod.preferences <- c(prod.preferences, p)
      }
    }
  }
  
  prod.preferences[c(7,19)] <- NULL #elimino high(mov/coin_mean)*low(mov/coin_mean)
  
  return(prod.preferences)
} 

#data una preferenza, ritorna i livelli normalizzati in scala [0,1]
#con personaggi ordinati come nel dataset dice.data
get_levels <- function(data, preference, order){
  
  res.preference <- psel(data, preference, top = nrow(data))
  character.level <- data.frame(res.preference$character, res.preference$.level) 
  
  order.characters <- order(match(character.level$res.preference.character, 
                                  order))
  
  character.level <- character.level[order.characters,]
  
  return(norm(character.level$res.preference..level))
  
}

#get_levels_data ritrorna il data frame con personaggio e livello (normalizzato) 
#di preferenza su tutti i trade-off
get_levels_data <- function(dice.data){
  
  prod.preferences <- all_2_resources_preferences()
  levels.data <- data.frame(dice.data$character)
  
  for(i in 1:length(prod.preferences)){
    lv <- get_levels(dice.data, prod.preferences[[i]], dice.data$character)
    levels.data <- data.frame(levels.data, lv)
  }
  
  pref.names <- c()
  for(i in 1:length(prod.preferences)){
    pref.names <- c(pref.names, as.character(prod.preferences[[i]]))
  }
  
  colnames(levels.data) <- c("character", pref.names)
  
  return(levels.data)
}

#conta il numero di partite giocate da ogni personaggio
get_n_matches_played <- function(data, character){
  gm <- games_matrix(data, character)
  usage <- c()
  for(i in 1:length(gm[1,])){
    ch.usage <- sum(gm[,i])
    usage <- c(usage, ch.usage)
  }
  
  usage <- data.frame(character,usage)
  return(usage)
  
}

get_outliers <- function(data){
  iqr <- IQR(data)
  quartiles <- quantile(data)[c(2,4)]
  
  outliers <- data < (quartiles[1] - 1.5*iqr) |(data > (quartiles[2] + 1.5*iqr))
  
  return(outliers)
}

#elimino da levels.data i livelli correlati (positivamente o negativamente)
#con quelli dei trade-off "fundamentals"
delete_fund_correlated_trade_offs <- function(data){
  
  cor.data <- cor(data)
  
  fund <- c("high(mov_mean) * high(mov_var)", "high(mov_mean) * low(mov_var)",
    "high(coin_mean) * high(coin_var)", "high(coin_mean) * low(coin_var)",
    "high(mov_mean) * high(mov_variety)", "high(mov_mean) * high(coin_mean)")

  for(i in 1:length(fund)){
    correlated <- which((cor.data[fund[i],] > 0.70 | cor.data[fund[i],] < -0.70) 
                 & cor.data[fund[i],] != 1)
    if(length(correlated > 0)){
      data <- data[,-correlated]
      cor.data <- cor(data)
    }

  }
  
  return(data)
  
}

games_matrix <- function(data, order){
  
  players <- matrix(nrow = length(data[,1]), ncol = length(order))
  for(i in 1:length(data[,1])){ #i sono le partite
    for(j in 1:length(data[1,])){#j sono i giocatori
      char <- which(data[i,j] == order)
      players[i,char] <- 1
    }
  }
  players[is.na(players)] <- 0
  colnames(players) <- order
  
  return(players)
  
}


norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}