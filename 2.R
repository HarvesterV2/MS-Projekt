policz_miary <- function(dane) {
  srednia <- mean(dane)
  mediana <- median(dane)

  wyniki <- list(
    wariancja = var(dane),
    odchylenie_std = sd(dane),
    srednia = srednia,
    odchylenie_przecietne = mean(abs(dane - srednia)),
    mediana = mediana,
    odchylenie_od_mediany = mean(abs(dane - mediana)),
    odchylenie_cwiartkowe = IQR(dane)/2,
    wsp_zmiennosci = sd(dane)/srednia * 100,
    pozycyjny_wsp_zmiennosci = IQR(dane)/(2*mediana) * 100,
    skosnosc = skewness(dane),
    kurtoza = kurtosis(dane),
    eksces = kurtosis(dane) - 3
  )

  return(wyniki)
}

rysuj_histogram <- function(market, num) {
  hist(market, main = paste("Histogram rozkładu empirycznego\nSzereg szczegółowy. Market", num),xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))
  breaks_market = seq(20,45,by=5)
  freq_market = table(cut(market,breaks = breaks_market, right=FALSE))
  hist(market,breaks = breaks_market, main = paste("Histogram rozkładu empirycznego\nSzereg rozdzielczy. Market ", num),xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))
}