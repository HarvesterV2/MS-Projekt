Zadanie2 <-function(markets)
{
  miary1 <- policz_miary(markets[[1]])
  miary2 <- policz_miary(markets[[2]])
  miary1_szereg_rozdzielczy <- policz_miary_szereg_rozdzielczy(markets[[1]])
  miary2_szereg_rozdzielczy <- policz_miary_szereg_rozdzielczy(markets[[2]])
  
  # Opracować histogramy rozkładów empirycznych. Miary wyznaczyć dwoma 
  # sposobami: a) na podstawie szeregu szczegółowego, b) na podstawie szeregu rozdzielczego.
  par(mfrow=c(2,2))
  rysuj_histogram(markets[[1]], 1)
  rysuj_histogram(markets[[2]], 2)
  tabela1 <- data.frame(Nazwa = names(miary1_szereg_rozdzielczy),
                      Szereg_Szczegółowy = unname(unlist(miary1)),
                      Szereg_Rozdzielczy = unname(unlist(miary1_szereg_rozdzielczy)))
  
  tabela2 <- data.frame(Nazwa = names(miary2_szereg_rozdzielczy),
                        Szereg_Szczegółowy = unname(unlist(miary2)),
                        Szereg_Rozdzielczy = unname(unlist(miary2_szereg_rozdzielczy)))
  
  wyswietl_tabele(tabela1,1)
  wyswietl_tabele(tabela2,2)
  #return(list(miary1,miary1_szereg_rozdzielczy,miary2,miary2_szereg_rozdzielczy))
  return(list(tabela1 = tabela1,
              tabela2 = tabela2))
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

rysuj_histogram <- function(market, num) {
  bb <- seq(min(market), max(market), length.out =ceiling(sqrt(length(market))))
  hist(market, main = paste("Histogram rozkładu empirycznego\nSzereg szczegółowy. Market", num),xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))
  #breaks_market = seq(20,45,by=5)
  #freq_market = table(cut(market,breaks = breaks_market, right=FALSE))
  hist(market,breaks = bb, main = paste("Histogram rozkładu empirycznego\nSzereg rozdzielczy. Market ", num),xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))
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
  bb <- seq(min(market), max(market), length.out =ceiling(sqrt(length(market))))
  hh <- hist(market, breaks = bb, plot = FALSE)
  #srednia
  srednia <- sum(hh$mids * hh$counts) / sum(hh$counts)
  #mediana
  liczebnosci_w_przedzialach  <- hh$counts
  liczebnosci_skumulowane  <- cumsum(liczebnosci_w_przedzialach)
  pozycja_mediany <- length(market) / 2
  indeks_klasy_medianowej  <- which(liczebnosci_skumulowane  >= pozycja_mediany)[1]
  dolna_granica_klasy_medianowej  <- bb[indeks_klasy_medianowej]
  gorna_granica_klasy_medianowej  <- bb[indeks_klasy_medianowej  + 1]
  szerokosc_przedzialu <- gorna_granica_klasy_medianowej - dolna_granica_klasy_medianowej
  liczebnosc_klasy_medianowej <- liczebnosci_w_przedzialach[indeks_klasy_medianowej]
  liczebnosci_skumulowane_przed_klasa <- ifelse(
    indeks_klasy_medianowej == 1,
    0,
    liczebnosci_skumulowane[indeks_klasy_medianowej - 1]
  )
  mediana <- dolna_granica_klasy_medianowej +
    ((pozycja_mediany - liczebnosci_skumulowane_przed_klasa) /
       liczebnosc_klasy_medianowej) *
    szerokosc_przedzialu
  #Moda
  najwieksza_liczebosc <- which.max(liczebnosci_w_przedzialach)
  gorna_granica_klasy_modalnej <- bb[najwieksza_liczebosc + 1] - bb[najwieksza_liczebosc]
  dolna_granica_klasy_modalnej <- bb[najwieksza_liczebosc]
  liczebnosc_poprzednia  <- liczebnosci_w_przedzialach[najwieksza_liczebosc] - ifelse(najwieksza_liczebosc == 1, 0, liczebnosci_w_przedzialach[najwieksza_liczebosc - 1])
  liczebnosc_nastepna  <- liczebnosci_w_przedzialach[najwieksza_liczebosc] - ifelse(najwieksza_liczebosc == length(liczebnosci_w_przedzialach), 0, liczebnosci_w_przedzialach[najwieksza_liczebosc + 1])
  moda <- dolna_granica_klasy_modalnej + (liczebnosc_poprzednia / (liczebnosc_poprzednia + liczebnosc_nastepna)) * gorna_granica_klasy_modalnej
  #Kwantyle
  pozycja_Q1 <- length(market) / 4
  klasa_Q1 <- which(liczebnosci_skumulowane >= pozycja_Q1)[1]
  
  L_Q1 <- bb[klasa_Q1]
  h_Q1 <- bb[klasa_Q1 + 1] - bb[klasa_Q1]
  f_Q1 <- liczebnosci_w_przedzialach[klasa_Q1]
  F_prev_Q1 <- ifelse(klasa_Q1 == 1, 0, liczebnosci_skumulowane[klasa_Q1 - 1])
  
  Q1 <- L_Q1 + ((pozycja_Q1 - F_prev_Q1) / f_Q1) * h_Q1
  
  pozycja_Q3 <- 3 * length(market) / 4
  klasa_Q3 <- which(liczebnosci_skumulowane >= pozycja_Q3)[1]
  
  L_Q3 <- bb[klasa_Q3]
  h_Q3 <- bb[klasa_Q3 + 1] - bb[klasa_Q3]
  f_Q3 <- liczebnosci_w_przedzialach[klasa_Q3]
  F_prev_Q3 <- ifelse(klasa_Q3 == 1, 0, liczebnosci_skumulowane[klasa_Q3 - 1])
  
  Q3 <- L_Q3 + ((pozycja_Q3 - F_prev_Q3) / f_Q3) * h_Q3
  
  #wariancje
  wariancja <- sum(liczebnosci_w_przedzialach * (hh$mids - srednia)^2) / sum(liczebnosci_w_przedzialach)
  wariancja_gwiazdka <- sum(liczebnosci_w_przedzialach * (hh$mids - srednia)^2) / (sum(liczebnosci_w_przedzialach) - 1)
  #odchylenia
  odchylenie_std = sqrt(wariancja)
  odchylenie_std_gwiazdka = sqrt(wariancja_gwiazdka)
  #odchylenie przeciętne
  odchylenie_przecietne <- sum(liczebnosci_w_przedzialach * abs(hh$mids - srednia)) / length(market)
  #odchylenie przecietne od mediany
  odchylenie_przecietne_od_mediany <- sum(liczebnosci_w_przedzialach * abs(hh$mids - mediana)) / length(market)
  #odchylenie ćwiartkowe
  odchylenie_cwiartkowe <- (Q3 - Q1) / 2
  #wspolczynnik zmiennosci
  wspolczynnik_zmiennosci <- (odchylenie_std_gwiazdka / srednia) * 100
  #pozycyjny wspolczynnik zmiennosci
  pozycyjny_wspolczynnik_zmiennosci <- ((Q3 - Q1) / (2 * mediana)) * 100
  # skosnosc
  skosnosc <- sum(liczebnosci_w_przedzialach * (hh$mids - srednia)^3) / (length(market) * odchylenie_std_gwiazdka^3)
  #kurtoza i eksces
  kurtoza <- sum(liczebnosci_w_przedzialach * (hh$mids - srednia)^4) / (length(market) * odchylenie_std_gwiazdka^4)
  eksces <- kurtoza - 3
  
  #wyniki
  wyniki <- list(
    "Liczba obserwacji" = as.integer(length(market)),
    "Średnia" = srednia,
    "Mediana" = mediana,
    "Moda" = moda,
    "Q1" = Q1,
    "Q3" = Q3,
    "Wariancja" = wariancja,
    "Wariancja nieobciążona" = wariancja_gwiazdka,
    "Odchylenie standardowe"= odchylenie_std,
    "Odchylenie standardowe nieobciążone" = odchylenie_std_gwiazdka,
    "Odchylenie przeciętne" = odchylenie_przecietne,
    "Odchylenie przeciętne od mediany" = odchylenie_przecietne_od_mediany,
    "Odchylenie ćwiartkowe" = odchylenie_cwiartkowe,
    "Współczynnik zmienności w procentach" = wspolczynnik_zmiennosci,
    "Pozycyjny współczynnik zmienności w procentach" = pozycyjny_wspolczynnik_zmiennosci,
    "Skośność" = skosnosc,
    "Kurtoza" = kurtoza,
    "Eksces" = eksces,
    "Min" = min(market),
    "Max" = max(market)
    )
  return(wyniki)
  
}

wyswietl_tabele <- function(table,nr_market)
{
  cat(paste("Market ",nr_market))
  cat("\n")
  cat(format("Liczba obserwacji: ",width = 64,justify = "left"))
  cat(table$Szereg_Szczegółowy[table$Nazwa == "Liczba obserwacji"], "\n")
  #print_table(table[table$Nazwa == "Liczba obserwacji",])
  
  headers <- c(
    format(colnames(table)[1], width = 62, justify = "left"),
    format(colnames(table)[2], width = 20, justify = "right"),
    format(colnames(table)[3], width = 20, justify = "right")
  )
  cat(paste(headers, collapse = ""), "\n")

  cat("Miary położenia:")
  print_table(table[2:6,])
  cat("Miary rozproszenia:\n")
  cat(paste(format("Zakres", width = 62, justify = "left"),"<",table$Szereg_Szczegółowy[table$Nazwa == "Min"],",",table$Szereg_Szczegółowy[table$Nazwa == "Max"],">"))
  print_table(table[7:15,])
  cat("Miary asymetrii:")
  print_table(table[16:18,])
  cat("\n")
}

print_table <- function(table)
{
  table$Nazwa <- format(table$Nazwa, width = 50, justify = "left")
  table$Szereg_Szczegółowy  <- format(table$Szereg_Szczegółowy, width = 20, justify = "left")
  table$Szereg_Rozdzielczy  <- format(table$Szereg_Rozdzielczy, width = 20, justify = "left")
  colnames(table) <- rep("", ncol(table))
  print(table,row.names = FALSE)
}