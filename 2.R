policz_miary <- function(dane) {
  srednia <- mean(dane)
  mediana <- median(dane)

  wyniki <- list(
    wariancja_populacja = sum((dane-srednia)^2)/length(dane),
    odchylenie_std_populacja= sqrt(sum((dane-srednia)^2)/length(dane)),
    #n-1
    wariancja_proba = var(dane),
    odchylenie_std_proba = sd(dane),
    srednia = srednia,
    moda = Mode(dane),
    Q1 = unname(quantile(dane,prob=0.25)),
    Q3 = unname(quantile(dane,prob=0.75)),
    odchylenie_przecietne = mean(abs(dane - srednia)),
    mediana = mediana,
    odchylenie_od_mediany = mean(abs(dane - mediana)),
    odchylenie_cwiartkowe = IQR(dane)/2,
    wsp_zmiennosci = sd(dane)/srednia * 100,
    pozycyjny_wsp_zmiennosci = IQR(dane)/(2*mediana) * 100,
    skosnosc = skosnosc(dane),
    kurtoza = kurtoza(dane),
    eksces = kurtoza(dane) - 3
  )

  return(wyniki)
}

rysuj_histogram <- function(market, num) {
  bb <- seq(min(market), max(market), length.out =ceiling(sqrt(length(market))))
  hist(market, main = paste("Histogram rozkładu empirycznego\nSzereg szczegółowy. Market", num),xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))
  #breaks_market = seq(20,45,by=5)
  freq_market = table(cut(market,breaks = breaks_market, right=FALSE))
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
  wariancja_gwiazdka <- sum(liczebnosci_w_przedzialach * (hh$mids - srednia)^2) / sum(liczebnosci_w_przedzialach)
  wariancja <- sum(liczebnosci_w_przedzialach * (hh$mids - srednia)^2) / (sum(liczebnosci_w_przedzialach) - 1)
  #odchylenia
  odchylenie_std_gwiazdka = sqrt(wariancja_gwiazdka)
  odchylenie_std = sqrt(wariancja)
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
    srednia = srednia,
    mediana = mediana,
    moda = moda,
    Q1 = Q1,
    Q3 = Q3,
    wariancja_gwiazdka = wariancja_gwiazdka,
    wariancja = wariancja,
    odchylenie_std_gwiazdka = odchylenie_std_gwiazdka,
    odchylenie_std= odchylenie_std,
    odchylenie_przecietne = odchylenie_przecietne,
    odchylenie_przecietne_od_mediany = odchylenie_przecietne_od_mediany,
    odchylenie_cwiartkowe = odchylenie_cwiartkowe,
    wspolczynnik_zmiennosci = wspolczynnik_zmiennosci,
    pozycyjny_wspolczynnik_zmiennosci = pozycyjny_wspolczynnik_zmiennosci,
    skosnosc = skosnosc,
    kurtoza = kurtoza,
    eksces = eksces
    )
  return(wyniki)
  
}