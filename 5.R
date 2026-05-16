Zadanie5 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  srednia = mean(market)
  wariancja = var(market)
  X = (n - 1) * wariancja / (h0 ^ 2)
  
  cat("Hipoteza zerowa - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest równe", h0, "zł.\n")
  if (rodzaj == "a") {
    decyzja = zad5_dwustronna(X, alfa, n, h0)
  } else if (rodzaj == "b") {
    decyzja = zad5_prawostronna(X, alfa, n, h0)
  } else if (rodzaj == "c") {
    decyzja = zad5_lewostronna(X, alfa, n, h0)
  } else {
    if (h0 > srednia) {
      decyzja = zad5_prawostronna(X, alfa, n, h0)
    } else {
      decyzja = zad5_lewostronna(X, alfa, n, h0)
    }
  }
  cat("Wartość statystyki testowej X:",X,"\n")
  if (decyzja) {
    cat("Statystyka testowa X należy do obszaru krytycznego. Odrzucamy hipotezę zerową H0, na rzecz hipotezy alternatywnej H1.\n")
  } else {
    cat("Statystyka testowa X nie należy do obszaru krytycznego. Brak podstaw do odrzucenia hipotezy zerowej H0.\n")
  }
}

zad5_dwustronna <- function(X, alfa, n, h0) {
  cat("Hipoteza alternatywna - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu nie jest równe", h0, "zł.\n")
  K_dwustronny_lewy = qchisq(alfa/2, n - 1)
  K_dwustronny_prawy = qchisq(1 - alfa/2, n - 1)
  decyzja = (X <= K_dwustronny_lewy) || (X >= K_dwustronny_prawy)
  cat("Obszar krytyczny dwustronny:", "(0;",K_dwustronny_lewy,"> ∪ <", K_dwustronny_prawy, ";∞)\n")
  return(decyzja)
}

zad5_lewostronna <- function(X, alfa, n, h0) {
  cat("Hipoteza alternatywna - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest mniejsze niż", h0, "zł.\n")
  K_lewostronny = qchisq(alfa, n - 1)
  decyzja = (X <= K_lewostronny)
  cat("Obszar krytyczny lewostronny:","(0;", K_lewostronny, ">\n")
  return(decyzja)
}

zad5_prawostronna <- function(X, alfa, n, h0) {
  cat("Hipoteza alternatywna - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest większe niż", h0, "zł.\n")
  K_prawostronny = qchisq(1 - alfa, n - 1)
  decyzja = (X >= K_prawostronny)
  cat("Obszar krytyczny prawostronny:", "<",K_prawostronny,";∞)\n")
  return(decyzja)
}