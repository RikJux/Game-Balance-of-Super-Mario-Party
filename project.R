  library("rPref")
  library("matrixStats")
  library("ggplot2")
  library("RColorBrewer")
  library("corrplot")
  library("coefplot")
  library("PerformanceAnalytics")

  
  source("functions.R")
  dice.data <- get_dice_data()
  levels.data <- get_levels_data(dice.data)
  games.data <- read.csv("gamesResults.csv")
  
  par(mfrow=c(3,2))
  hist(dice.data$mov_mean, main="Movement Mean", 
       breaks = 10, col="green", xlab = "mean")
  hist(dice.data$mov_var, main="Movement Variance",
       breaks = 10, col="darkgreen", xlab = "variance")
  hist(dice.data$coin_mean, main="Coins Mean",
       breaks = 10, col="yellow", xlab = "mean")
  hist(dice.data$coin_var, main="Coins Variance",
       breaks = 10, col="gold", xlab = "variance")
  hist(dice.data$mov_variety, main="Variety",
       breaks = 7, col="red", xlab = "Variety", xlim = c(1,5))
  par(mfrow=c(1,1))
  
  corrplot.mixed(corr=cor(dice.data[,-1]))
  
  mov_mean.uni.stats <- basic_univariate_stats(dice.data$mov_mean)
  mov_var.uni.stats <- basic_univariate_stats(dice.data$mov_var)
  coin_mean.uni.stats <- basic_univariate_stats(dice.data$coin_mean)
  coin_var.uni.stats <- basic_univariate_stats(dice.data$coin_var)
  mov_variety.uni.stats <- basic_univariate_stats(dice.data$mov_variety)

  #given all possible trade-offs, only those significantly covariant with
  #some "foundamental trade-offs" are mantained
  corrplot.mixed(corr=cor(levels.data[,-1]),tl.pos = "n",upper = "circle")
  
  fundamentals <- data.frame(levels.data[,"high(mov_mean) * high(mov_var)"], levels.data[,"high(mov_mean) * low(mov_var)"],
                             levels.data[,"high(coin_mean) * high(coin_var)"],levels.data[,"high(coin_mean) * low(coin_var)"],
                             levels.data[,"high(mov_mean) * high(mov_variety)"],levels.data[,"high(mov_mean) * high(coin_mean)"])
  
  corrplot.mixed(corr=cor(fundamentals), tl.pos = "n")
  
  levels.data <- data.frame(levels.data$character, delete_fund_correlated_trade_offs(levels.data[,-1]))
  corrplot.mixed(corr=cor(levels.data[,-1]), tl.pos = "n")
  colnames(levels.data) <- c("character","high.mov_meanXhigh.mov_var", "high.mov_meanXlow.mov_var",
                             "high.mov_meanXhigh.coin_mean","high.mov_meanXlow.coin_var",
                             "high.mov_meanXhigh.mov_variety", "low.mov_varXhigh.coin_mean",
                             "low.mov_varXhigh.coin_var",
                             "high.coin_meanXhigh.coin_var", "high.coin_meanXlow.coin_var",
                             "high.coin_meanXhigh.mov_variety", "high.coin_varXhigh.mov_variety",
                             "low.coin_varXhigh.mov_variety")
 
   #plot off all the mantained trade-offs
    trade.off.1 <- psel(dice.data, high(mov_mean) * high(mov_var), top = nrow(dice.data))
    trade.off.2 <- psel(dice.data, high(mov_mean) * low(mov_var), top = nrow(dice.data))
    trade.off.3 <- psel(dice.data, high(mov_mean) * high(coin_mean), top = nrow(dice.data))
    trade.off.4 <- psel(dice.data, high(mov_mean) * low(coin_var), top = nrow(dice.data))
    trade.off.5 <- psel(dice.data, high(mov_mean) * high(mov_variety), top = nrow(dice.data))
    trade.off.6 <- psel(dice.data, low(mov_var) * high(coin_mean), top = nrow(dice.data))
    trade.off.7 <- psel(dice.data, low(mov_var) * high(coin_var), top = nrow(dice.data))
    trade.off.8 <- psel(dice.data, high(coin_mean) * high(coin_var), top = nrow(dice.data))
    trade.off.9 <- psel(dice.data, high(coin_mean) * low(coin_var), top = nrow(dice.data))
    trade.off.10 <- psel(dice.data, high(coin_mean) * high(mov_variety), top = nrow(dice.data))
    trade.off.11 <- psel(dice.data, high(coin_var) * high(mov_variety), top = nrow(dice.data))
    trade.off.12 <- psel(dice.data, low(coin_var) * high(mov_variety), top = nrow(dice.data))

      gp_trade.off.1 <- ggplot(trade.off.1, aes(x=mov_var, y=mov_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.mov_meanXhigh.mov_var")
      gp_trade.off.2 <- ggplot(trade.off.2, aes(x=mov_var, y=mov_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.mov_meanXlow.mov_var")
      gp_trade.off.3 <- ggplot(trade.off.3, aes(x=coin_mean, y=mov_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.mov_meanXhigh.coin_mean")
      gp_trade.off.4 <- ggplot(trade.off.4, aes(x=coin_var, y=mov_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") + 
        ggtitle("high.mov_meanXlow.coin_var")
      gp_trade.off.5 <- ggplot(trade.off.5, aes(x=mov_variety, y=mov_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.mov_meanXhigh.mov_variety")
      gp_trade.off.6 <- ggplot(trade.off.6, aes(x=mov_var, y=coin_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") + 
        ggtitle("low.mov_varXhigh.coin_mean")
      gp_trade.off.7 <- ggplot(trade.off.7, aes(x=coin_var, y=mov_var,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("low.mov_varXhigh.coin_var")
      gp_trade.off.8 <- ggplot(trade.off.8, aes(x=coin_var, y=coin_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.coin_meanXhigh.coin_var")
      gp_trade.off.9 <- ggplot(trade.off.9, aes(x=coin_var, y=coin_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.coin_meanXlow.coin_var")
      gp_trade.off.10 <- ggplot(trade.off.10, aes(x=mov_variety, y=coin_mean,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.coin_meanXhigh.mov_variety")
      gp_trade.off.11 <- ggplot(trade.off.11, aes(x=mov_variety, y=coin_var,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("high.coin_varXhigh.mov_variety")
      gp_trade.off.12 <- ggplot(trade.off.12, aes(x=mov_variety, y=coin_var,color=factor(.level))) + geom_point(size=3) + geom_step(direction = "vh") +
        ggtitle("low.coin_varXhigh.mov_variety")

      #given games.data, winning frequency is calculated for all characters.
      #Subsequently, 'performance' is defined as 1/(winn_freq +1)
      n.matches <- get_n_matches_played(games.data[,-5],dice.data$character)
      
      table(games.data$winner)
      wins <- c(0,3,2,3,2,1,4,0,14,4,4,0,1,5,3,2,6,5,1,0)
      
      performance.data <- cbind(n.matches,wins)
      performance.data$freq <- performance.data$wins/performance.data$usage
      
      boxplot(performance.data$freq, col=c("red"), main="winnings/games played")
      
      #outliers are transformed
      performance.data[which(get_outliers(performance.data$freq)),"freq"] <- 
        quantile(performance.data$freq)[4] + 1.5*IQR(performance.data$freq) 
    
    performance.level <- performance.data$freq
   
    names <- colnames(levels.data)
    
    levels.data <- data.frame(levels.data, performance.level)
    colnames(levels.data) <- c(names, "performance")
    levels.data$performance <- 1/(levels.data$performance + 1)
    
    #mean and standard deviation of character preference levels in all trade-offs
    ch.levels.mean <- rowMeans(levels.data[,-c(1,14)])
    ch.levels.std <- sqrt(rowVars(as.matrix(levels.data[,-c(1,14)])))
    
    ch.levels.mean.stats <- basic_univariate_stats(ch.levels.mean)
    ch.levels.std.stats <- basic_univariate_stats(ch.levels.std)
    performance.stats <- basic_univariate_stats(levels.data$performance)
    
    par(mfrow=c(2,1))
    hist(ch.levels.mean, main= "preference mean of each character", 
         xlab= "mean", col= "green")
    plot(density(ch.levels.mean), main = "density")
    hist(ch.levels.std, main= "preference standard dev of each characte", 
         xlab= "standard deviation", col= "darkgreen")
    plot(density(ch.levels.std), main = "density")
    hist(levels.data$performance, main= "'performance' of each character personaggio", 
         xlab= "performance", col= "lightblue")
    plot(density(levels.data$performance), main = "density")
    par(mfrow=c(1,1))
    
    #regression model: dependent variable <- performance
      # independent variables <- levels
    #the final model is determined through backward elimination until
    #all variables have signficance
    
    m1.formula <- performance ~ .
    m1 <- lm(formula = m1.formula, data = levels.data[,-1])
    s1 <-summary(m1)
    
    m2.formula <- performance ~ high.mov_meanXhigh.mov_var + high.mov_meanXlow.mov_var +
      high.mov_meanXhigh.coin_mean + high.mov_meanXlow.coin_var + high.mov_meanXhigh.mov_variety +
      low.mov_varXhigh.coin_mean + low.mov_varXhigh.coin_var + high.coin_meanXhigh.coin_var + 
      high.coin_meanXlow.coin_var + high.coin_varXhigh.mov_variety + low.coin_varXhigh.mov_variety
    m2 <- lm(formula = m2.formula, data = levels.data[,-1])
    s2 <-summary(m2)
    
    m3.formula <- performance ~ high.mov_meanXhigh.mov_var + high.mov_meanXlow.mov_var +
      high.mov_meanXhigh.coin_mean + high.mov_meanXhigh.mov_variety +
      low.mov_varXhigh.coin_mean + low.mov_varXhigh.coin_var + high.coin_meanXhigh.coin_var + 
      high.coin_meanXlow.coin_var + high.coin_varXhigh.mov_variety + low.coin_varXhigh.mov_variety
    m3 <- lm(formula = m3.formula, data = levels.data[,-1])
    s3 <-summary(m3)
    
    m4.formula <- performance ~ high.mov_meanXhigh.mov_var + high.mov_meanXlow.mov_var +
      high.mov_meanXhigh.coin_mean + high.mov_meanXhigh.mov_variety +
      low.mov_varXhigh.coin_mean + high.coin_meanXhigh.coin_var + 
      high.coin_meanXlow.coin_var + high.coin_varXhigh.mov_variety + low.coin_varXhigh.mov_variety
    m4 <- lm(formula = m4.formula, data = levels.data[,-1])
    s4 <-summary(m4)
    
    m5.formula <- performance ~ high.mov_meanXhigh.mov_var + high.mov_meanXlow.mov_var +
      high.mov_meanXhigh.coin_mean + high.mov_meanXhigh.mov_variety +
      low.mov_varXhigh.coin_mean + high.coin_meanXhigh.coin_var + 
      high.coin_meanXlow.coin_var + high.coin_varXhigh.mov_variety
    m5 <- lm(formula = m5.formula, data = levels.data[,-1])
    s5 <-summary(m5)
    
    m6.formula <- performance ~ high.mov_meanXhigh.mov_var + high.mov_meanXlow.mov_var +
      high.mov_meanXhigh.coin_mean + high.mov_meanXhigh.mov_variety +
      low.mov_varXhigh.coin_mean + high.coin_meanXhigh.coin_var + 
      high.coin_varXhigh.mov_variety
    m6 <- lm(formula = m6.formula, data = levels.data[,-1])
    s6 <-summary(m6)
    
    m7.formula <- performance ~ high.mov_meanXhigh.mov_var + high.mov_meanXlow.mov_var +
      high.mov_meanXhigh.coin_mean + high.mov_meanXhigh.mov_variety +
      low.mov_varXhigh.coin_mean +  high.coin_varXhigh.mov_variety
    m7 <- lm(formula = m7.formula, data = levels.data[,-1])
    s7 <-summary(m7)
    print(s7)
    
    #the 6 trade-offs of the final model, hence the significant descriptors of performance
    print(gp_trade.off.1)
    paste0(trade.off.1$character, " ", trade.off.1$.level)
    print(gp_trade.off.2)
    paste0(trade.off.2$character, " ", trade.off.2$.level)
    print(gp_trade.off.3)
    paste0(trade.off.3$character, " ", trade.off.3$.level)
    print(gp_trade.off.5)
    paste0(trade.off.5$character, " ", trade.off.5$.level)
    print(gp_trade.off.6)
    paste0(trade.off.6$character, " ", trade.off.6$.level)
    print(gp_trade.off.11)
    paste0(trade.off.11$character, " ", trade.off.11$.level)
    
    par(mfrow=c(2,2))
    plot(m7)
    par(mfrow=c(1,1))
    
    confint(m7)
    coefplot(m7)
    
    plot(levels.data$performance, fitted(m7), xlim = c(0.6,1), ylim = c(0.6,1),
         xlab = "performance", ylab = "fitted")
    points(levels.data$performance, levels.data$performance, col='red')
    legend("topleft", legend= c("model","actual"), col=c('black','red'), pch=1)
    
    plot(levels.data$character, fitted(m7), xlab = "character", ylab = "performance")
    points(levels.data$character, levels.data$performance, pch=19, col="red")
    legend("topleft", legend= c("modello","actual"), col=c('black','red'), pch=19)
  
  
  
  
