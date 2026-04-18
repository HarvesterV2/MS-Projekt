rysuj_pudelko <- function(market, num) {
  market_srednia = mean(market)
  market_mediana = median(market)
  market_moda = Mode(market)
  market_q1 = quantile(market,prob=0.25)
  market_q3 = quantile(market,prob=0.75)
  market_IQR = market_q3-market_q1
  boxplot(market, main =paste("Mediana i Kwartyle\nMarket ", num),ylab="Wydatki [zł]",horizontal = FALSE,ylim=c(globalmin,globalmax))
  text(0.6,market_q1,"Q1",col = "blue",cex = 0.7)
  text(0.6,market_mediana,"Mediana",col = "red",cex = 0.7)
  text(0.6,market_q3,"Q3",col = "blue",cex = 0.7)
}

rysuj_odchylenie <- function(market, num) {
  market_odchylenie_standardowe = sd(market)
  market_xx = c(min(market),mean(market)-sd(market),mean(market),mean(market)+sd(market),max(market))
  boxplot(market_xx, main = paste("Odchylenie standardowe\nMarket ", num),ylab="Wydatki [zł]",horizontal = FALSE,ylim=c(globalmin,globalmax))
}