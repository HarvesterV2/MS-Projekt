Zadanie1 <- function(markets)
{
  par(mfrow=c(1,2))
  rysuj_wykres_mediana(markets)
  rysuj_wykres_odchylenie(markets)
}

rysuj_wykres_mediana <- function(markets)
{
  boxplot(
    markets, 
    main = paste("Wykres prezentujący\nmedianę oraz kwartyle"),
    xlab = "Numer marketu: ",
    ylab="Wydatki [zł]",
    horizontal = FALSE,
    col = c("#11A10D", "#7F00FF"),
    ylim=range(unlist(markets))
  )
  grid(nx = NA, ny = NULL, col = "black", lty = "dotted")
}

rysuj_wykres_odchylenie <- function(markets) {
  market_xx_list <- lapply(markets, function(market) {
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
    main = "Wykres prezentujący\nodchylenie standardowe",
    xlab = "Numer marketu: ",
    ylab = "Wydatki [zł]",
    ylim = range(unlist(markets)),
    col = c("#11A10D", "#7F00FF")
    )
  grid(nx = NA, ny = NULL, col = "black", lty = "dotted")
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}