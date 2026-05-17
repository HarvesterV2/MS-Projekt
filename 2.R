Zadanie2 <-function(markets)
{
  miary1 <- policz_miary(markets[[1]])
  miary2 <- policz_miary(markets[[2]])
  miary1_szereg_rozdzielczy <- policz_miary_szereg_rozdzielczy(markets[[1]])
  miary2_szereg_rozdzielczy <- policz_miary_szereg_rozdzielczy(markets[[2]])
  
  par(mfrow=c(2,2))
  plot_range = range(unlist(markets))
  rysuj_histogram(markets[[1]], 1, plot_range)
  rysuj_histogram(markets[[2]], 2, plot_range)
  tabela1 <- data.frame(Nazwa = names(miary1_szereg_rozdzielczy),
                      Szereg_Szczegółowy = unname(unlist(miary1)),
                      Szereg_Rozdzielczy = unname(unlist(miary1_szereg_rozdzielczy)))
  
  tabela2 <- data.frame(Nazwa = names(miary2_szereg_rozdzielczy),
                        Szereg_Szczegółowy = unname(unlist(miary2)),
                        Szereg_Rozdzielczy = unname(unlist(miary2_szereg_rozdzielczy)))
  
  wyswietl_tabele(tabela1,1)
  wyswietl_tabele(tabela2,2)
  return(list(tabela1 = tabela1, tabela2 = tabela2))
}

policz_miary <- function(dane) {
  srednia <- mean(dane)
  mediana <- median(dane)
  
  wyniki <- list(
    suma = as.integer(length(dane)),
    srednia = srednia,
    mediana = mediana,
    moda = Mode(dane),
    Q1 = unname(quantile(dane, prob = 0.25)),
    Q3 = unname(quantile(dane, prob = 0.75)),
    
    wariancja = sum((dane - srednia)^2) / length(dane),
    wariancja_gwiazdka = var(dane),
    
    odchylenie_std = sqrt(sum((dane - srednia)^2) / length(dane)),
    odchylenie_std_gwiazdka = sd(dane),
    
    odchylenie_przecietne = mean(abs(dane - srednia)),
    odchylenie_przecietne_od_mediany = mean(abs(dane - mediana)),
    
    odchylenie_cwiartkowe = IQR(dane) / 2,
    
    wspolczynnik_zmiennosci = sd(dane) / srednia * 100,
    pozycyjny_wspolczynnik_zmiennosci = IQR(dane) / (2 * mediana) * 100,
    
    skosnosc = skosnosc(dane),
    kurtoza = kurtoza(dane),
    eksces = kurtoza(dane) - 3,
    min = min(dane),
    max = max(dane)
  )

  return(wyniki)
}

rysuj_histogram <- function(market, num, plot_range) {
  bb <- seq(min(market), max(market), length.out =ceiling(sqrt(length(market))))
  hist(market, main = paste("Histogram rozkładu empirycznego\nSzereg szczegółowy. Market", num),
       xlab="Wydatki[zł]",ylab="Częstość",xlim = plot_range)
  hist(market,breaks = bb, main = paste("Histogram rozkładu empirycznego\nSzereg rozdzielczy. Market ", num),
       xlab="Wydatki[zł]",ylab="Częstość",xlim = plot_range)
}

skosnosc <- function(x) {
  x <- unlist(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  
  sum((x - m)^3) / n / (s^3) * ((n * (n - 1))^0.5 / (n - 2))
  
}  

kurtoza <- function(x) {
  x <- unlist(x)
  n <- length(x)
  m <- mean(x)
  
  m4 <- sum((x - m)^4) / n
  m2 <- sum((x - m)^2) / n
  
  m4 / (m2^2)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

policz_miary_szereg_rozdzielczy <- function(market)
{
  market = unlist(market)
  
  liczba_przedzialow = ceiling(sqrt(length(market)))
  
  granice_przedzialow = seq(
    min(market),
    max(market),
    length.out = liczba_przedzialow
  )
  
  histogram_rozdzielczy = hist(
    market,
    breaks = granice_przedzialow,
    plot = FALSE
  )
  
  liczba_obserwacji = sum(histogram_rozdzielczy$counts)
  liczebnosci_przedzialow = histogram_rozdzielczy$counts
  srodki_przedzialow = histogram_rozdzielczy$mids
  
  skumulowane_liczebnosci = cumsum(liczebnosci_przedzialow)

  srednia = sum(srodki_przedzialow * liczebnosci_przedzialow) / liczba_obserwacji
  
  pozycja_mediany = liczba_obserwacji / 2
  
  indeks_klasy_medianowej = which(skumulowane_liczebnosci >= pozycja_mediany)[1]
  
  dolna_granica_klasy_medianowej = granice_przedzialow[indeks_klasy_medianowej]
  gorna_granica_klasy_medianowej = granice_przedzialow[indeks_klasy_medianowej + 1]
  
  szerokosc_klasy_medianowej = gorna_granica_klasy_medianowej - dolna_granica_klasy_medianowej
  
  liczebnosc_klasy_medianowej = liczebnosci_przedzialow[indeks_klasy_medianowej]
  
  skumulowane_przed_klasa_medianowa = ifelse(
    indeks_klasy_medianowej == 1,
    0,
    skumulowane_liczebnosci[indeks_klasy_medianowej - 1]
  )
  
  mediana = dolna_granica_klasy_medianowej +
    ((pozycja_mediany - skumulowane_przed_klasa_medianowa) /
       liczebnosc_klasy_medianowej) *
    szerokosc_klasy_medianowej
  
  indeks_klasy_modalnej = which.max(liczebnosci_przedzialow)
  
  dolna_granica_klasy_modalnej = granice_przedzialow[indeks_klasy_modalnej]
  szerokosc_klasy_modalnej = granice_przedzialow[indeks_klasy_modalnej + 1] -
    granice_przedzialow[indeks_klasy_modalnej]
  
  liczebnosc_klasy_modalnej = liczebnosci_przedzialow[indeks_klasy_modalnej]
  
  liczebnosc_poprzedniej_klasy = ifelse(
    indeks_klasy_modalnej == 1,
    0,
    liczebnosci_przedzialow[indeks_klasy_modalnej - 1]
  )
  
  liczebnosc_nastepnej_klasy = ifelse(
    indeks_klasy_modalnej == length(liczebnosci_przedzialow),
    0,
    liczebnosci_przedzialow[indeks_klasy_modalnej + 1]
  )
  
  moda = dolna_granica_klasy_modalnej +
    ((liczebnosc_klasy_modalnej - liczebnosc_poprzedniej_klasy) /
       ((liczebnosc_klasy_modalnej - liczebnosc_poprzedniej_klasy) +
          (liczebnosc_klasy_modalnej - liczebnosc_nastepnej_klasy))) *
    szerokosc_klasy_modalnej
  

  pozycja_Q1 = liczba_obserwacji / 4
  pozycja_Q3 = 3 * liczba_obserwacji / 4
  
  indeks_klasy_Q1 = which(skumulowane_liczebnosci >= pozycja_Q1)[1]
  indeks_klasy_Q3 = which(skumulowane_liczebnosci >= pozycja_Q3)[1]
  
  dolna_granica_klasy_Q1 = granice_przedzialow[indeks_klasy_Q1]
  dolna_granica_klasy_Q3 = granice_przedzialow[indeks_klasy_Q3]
  
  szerokosc_klasy_Q1 = granice_przedzialow[indeks_klasy_Q1 + 1] -
    granice_przedzialow[indeks_klasy_Q1]
  
  szerokosc_klasy_Q3 = granice_przedzialow[indeks_klasy_Q3 + 1] -
    granice_przedzialow[indeks_klasy_Q3]
  
  Q1 = dolna_granica_klasy_Q1 +
    ((pozycja_Q1 - ifelse(indeks_klasy_Q1 == 1, 0,
                          skumulowane_liczebnosci[indeks_klasy_Q1 - 1])) /
       liczebnosci_przedzialow[indeks_klasy_Q1]) *
    szerokosc_klasy_Q1
  
  Q3 = dolna_granica_klasy_Q3 +
    ((pozycja_Q3 - ifelse(indeks_klasy_Q3 == 1, 0,
                          skumulowane_liczebnosci[indeks_klasy_Q3 - 1])) /
       liczebnosci_przedzialow[indeks_klasy_Q3]) *
    szerokosc_klasy_Q3
  
  wariancja = sum(
    liczebnosci_przedzialow * (srodki_przedzialow - srednia)^2
  ) / liczba_obserwacji
  
  wariancja_nieobciazona = sum(
    liczebnosci_przedzialow * (srodki_przedzialow - srednia)^2
  ) / (liczba_obserwacji - 1)
  
  odchylenie_standardowe = sqrt(wariancja)
  odchylenie_standardowe_nieobciazone = sqrt(wariancja_nieobciazona)
  

  odchylenie_przecietne = sum(
    liczebnosci_przedzialow * abs(srodki_przedzialow - srednia)
  ) / liczba_obserwacji
  
  odchylenie_przecietne_od_mediany = sum(
    liczebnosci_przedzialow * abs(srodki_przedzialow - mediana)
  ) / liczba_obserwacji
  
  odchylenie_cwiartkowe = (Q3 - Q1) / 2
  
  wspolczynnik_zmiennosci = (odchylenie_standardowe_nieobciazone / srednia) * 100
  
  pozycyjny_wspolczynnik_zmiennosci = ((Q3 - Q1) / (2 * mediana)) * 100
  
  skosnosc = sum(
    liczebnosci_przedzialow * (srodki_przedzialow - srednia)^3
  ) / (liczba_obserwacji * odchylenie_standardowe_nieobciazone^3)
  
  kurtoza = sum(
    liczebnosci_przedzialow * (srodki_przedzialow - srednia)^4
  ) / (liczba_obserwacji * odchylenie_standardowe_nieobciazone^4)
  
  eksces = kurtoza - 3
  
  wyniki = list(
    "Liczba obserwacji" = liczba_obserwacji,
    "Średnia" = srednia,
    "Mediana" = mediana,
    "Moda" = moda,
    "Q1" = Q1,
    "Q3" = Q3,
    "Wariancja" = wariancja,
    "Wariancja nieobciążona" = wariancja_nieobciazona,
    "Odchylenie standardowe" = odchylenie_standardowe,
    "Odchylenie standardowe nieobciążone" = odchylenie_standardowe_nieobciazone,
    "Odchylenie przeciętne" = odchylenie_przecietne,
    "Odchylenie przeciętne od mediany" = odchylenie_przecietne_od_mediany,
    "Odchylenie ćwiartkowe" = odchylenie_cwiartkowe,
    "Współczynnik zmienności %" = wspolczynnik_zmiennosci,
    "Pozycyjny współczynnik zmienności %" = pozycyjny_wspolczynnik_zmiennosci,
    "Skośność" = skosnosc,
    "Kurtoza" = kurtoza,
    "Eksces" = eksces,
    "Min" = min(market),
    "Max" = max(market)
  )
  
  return(wyniki)
}

wyswietl_tabele <- function(tabelka,nr_market)
{
  cat(paste("Market ",nr_market))
  cat("\n")
  cat(format("Liczba obserwacji: ",width = 64,justify = "left"))
  cat(tabelka$Szereg_Szczegółowy[tabelka$Nazwa == "Liczba obserwacji"], "\n")
  
  headers <- c(
    format(colnames(tabelka)[1], width = 62, justify = "left"),
    format(colnames(tabelka)[2], width = 20, justify = "right"),
    format(colnames(tabelka)[3], width = 20, justify = "right")
  )
  cat(paste(headers, collapse = ""), "\n")

  cat("Miary położenia:")
  print_table(tabelka[2:6,])
  cat("Miary rozproszenia:\n")
  cat(paste(format("Zakres", width = 62, justify = "left"),"<",tabelka$Szereg_Szczegółowy[tabelka$Nazwa == "Min"],",",tabelka$Szereg_Szczegółowy[tabelka$Nazwa == "Max"],">"))
  print_table(tabelka[7:15,])
  cat("Miary asymetrii:")
  print_table(tabelka[16:18,])
  cat("\n")
}

print_table <- function(tabelka)
{
  tabelka$Nazwa <- format(tabelka$Nazwa, width = 50, justify = "left")
  tabelka$Szereg_Szczegółowy  <- format(tabelka$Szereg_Szczegółowy, width = 20, justify = "left")
  tabelka$Szereg_Rozdzielczy  <- format(tabelka$Szereg_Rozdzielczy, width = 20, justify = "left")
  colnames(tabelka) <- rep("", ncol(tabelka))
  print(tabelka,row.names = FALSE)
}