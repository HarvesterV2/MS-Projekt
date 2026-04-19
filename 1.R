Zadanie1 <- function(markets)
{
  par(mfrow=c(1,2))
  boxplot(
    markets, 
    main = paste("Obliczona\nMediana oraz Kwartyle"),
    xlab = "Numer marketu: ",
    ylab="Wydatki [zł]",
    horizontal = FALSE,
    col = c("#11A10D", "#7F00FF"),
    ylim=c(globalmin,globalmax)
  )
  grid(nx = NA, ny = NULL, col = "black", lty = "dotted")
  rysuj_odchylenie(markets)
  market1_dane = wypisz_dane(markets[[1]])
  market2_dane = wypisz_dane(markets[[2]])
  returnValue(list(market1_dane,market2_dane))
}

wypisz_dane <- function(market) {
  market_srednia = mean(market)
  market_mediana = median(market)
  market_moda = Mode(market)
  market_q1 = quantile(market,prob=0.25)
  market_q3 = quantile(market,prob=0.75)
  market_IQR = market_q3-market_q1
  returnValue(list(market_srednia,market_mediana,market_moda,market_q1,market_q3,market_IQR))
}

rysuj_odchylenie <- function(markety) {
  market_xx_list <- lapply(markety, function(market) {
    market <- unlist(market)
    c(
      min(market),
      mean(market) - sd(market),
      mean(market),
      mean(market) + sd(market),
      max(market)
    )
  })
  
  boxplot(
    market_xx_list,
    main = "Obliczone\nOdchylenie standardowe",
    xlab = "Numer marketu: ",
    ylab = "Wydatki [zł]",
    ylim = c(globalmin, globalmax),
    col = c("#11A10D", "#7F00FF")
    )
  grid(nx = NA, ny = NULL, col = "black", lty = "dotted")
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

