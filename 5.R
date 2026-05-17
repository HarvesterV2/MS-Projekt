Zadanie5 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  srednia = mean(market)
  wariancja = var(market)
  statystyka_X = (n - 1) * wariancja / (h0 ^ 2)
  
  cat("Hipoteza zerowa - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest równe", h0, "zł.\n")
  if (rodzaj == "a") {
    decyzja = zad5_dwustronna(statystyka_X, alfa, n, h0)
  } else if (rodzaj == "b") {
    decyzja = zad5_prawostronna(statystyka_X, alfa, n, h0)
  } else if (rodzaj == "c") {
    decyzja = zad5_lewostronna(statystyka_X, alfa, n, h0)
  } else {
    if (h0 > srednia) {
      decyzja = zad5_prawostronna(statystyka_X, alfa, n, h0)
    } else {
      decyzja = zad5_lewostronna(statystyka_X, alfa, n, h0)
    }
  }
  cat("Wartość statystyki testowej X:",statystyka_X,"\n")
  if (decyzja) {
    cat("Statystyka testowa X należy do obszaru krytycznego. Odrzucamy hipotezę zerową H0, na rzecz hipotezy alternatywnej H1.\n")
  } else {
    cat("Statystyka testowa X nie należy do obszaru krytycznego. Brak podstaw do odrzucenia hipotezy zerowej H0.\n")
  }
}

zad5_dwustronna <- function(statystyka_X, alfa, n, h0) {
  cat("Hipoteza alternatywna - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu nie jest równe", h0, "zł.\n")
  K_dwustronny_lewy = qchisq(alfa/2, n - 1)
  K_dwustronny_prawy = qchisq(1 - alfa/2, n - 1)
  decyzja = (statystyka_X <= K_dwustronny_lewy) || (statystyka_X >= K_dwustronny_prawy)
  cat("Obszar krytyczny dwustronny:", "(0;",K_dwustronny_lewy,"> ∪ <", K_dwustronny_prawy, ";∞)\n")
  #pvalue
  cat("P-value: ",2 * min(pchisq(statystyka_X, n - 1),1 - pchisq(statystyka_X, n - 1)),"\n")
  return(decyzja)
}

zad5_lewostronna <- function(statystyka_X, alfa, n, h0) {
  cat("Hipoteza alternatywna - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest mniejsze niż", h0, "zł.\n")
  K_lewostronny = qchisq(alfa, n - 1)
  decyzja = (statystyka_X <= K_lewostronny)
  cat("Obszar krytyczny lewostronny:","(0;", K_lewostronny, ">\n")
  #pvalue
  cat("P-value: ",pchisq(statystyka_X, n - 1),"\n")
  return(decyzja)
}

zad5_prawostronna <- function(statystyka_X, alfa, n, h0) {
  cat("Hipoteza alternatywna - odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest większe niż", h0, "zł.\n")
  K_prawostronny = qchisq(1 - alfa, n - 1)
  decyzja = (statystyka_X >= K_prawostronny)
  cat("Obszar krytyczny prawostronny:", "<",K_prawostronny,";∞)\n")
  #pvalue
  cat("P-value: ",1 - pchisq(statystyka_X, n - 1),"\n")
  return(decyzja)
}