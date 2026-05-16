Zadanie4 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  srednia = mean(market)
  odchylenie = sqrt(sum((market - srednia) ^ 2) / n)
  t = (srednia - h0) * sqrt(n - 1) / odchylenie
  
  cat("Hipoteza zerowa - przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu jest równa", h0, "zł.\n")
  if (rodzaj == "a") {
    decyzja = zad4_dwustronna(t, alfa, n, h0)
  } else if (rodzaj == "b") {
    decyzja = zad4_prawostronna(t, alfa, n, h0)
  } else if (rodzaj == "c") {
    decyzja = zad4_lewostronna(t, alfa, n, h0)
  } else {
    if (h0 > srednia) {
      decyzja = zad4_prawostronna(t, alfa, n, h0)
    } else {
      decyzja = zad4_lewsotronna(t, alfa, n, h0)
    }
  }
  cat("Wartość statystyki testowej T:",t, "\n")
  if (decyzja) {
    cat("Statystyka testowa T należy do obszaru krytycznego. Odrzucamy hipotezę zerową H0, na rzecz hipotezy alternatywnej H1.\n")
  } else {
    cat("Statystyka testowa T nie należy do obszaru krytycznego. Brak podstaw do odrzucenia hipotezy zerowej H0.\n")
  }
}

zad4_dwustronna <- function(t, alfa, n, h0) {
  cat("Hipoteza alternatywna - przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu nie jest równa", h0, "zł.\n")
  t_kryt_prawy = qt(1 - alfa / 2, n - 1)
  t_kryt_lewy = -t_kryt_prawy
  decyzja = (t <= t_kryt_lewy) || (t >= t_kryt_prawy)
  cat("Obszar krytyczny dwustronny:", "(-∞;",t_kryt_lewy,"> ∪ <", t_kryt_prawy, ";∞)\n")
  return(decyzja)
}

zad4_lewsotronna <- function(t, alfa, n, h0) {
  cat("Hipoteza alternatywna - przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu jest mniejsza niż", h0, "zł.\n")
  t_kryt = -qt(1 - alfa, n - 1)
  decyzja = (t <= t_kryt)
  cat("Obszar krytyczny lewostronny:","(-∞;", t_kryt, ">\n")
  
  return(decyzja)
}

zad4_prawostronna <- function(t, alfa, n, h0) {
  cat("Hipoteza alternatywna - przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu jest większa niż", h0, "zł.\n")
  t_kryt = qt(1 - alfa, n - 1)
  decyzja = (t >= t_kryt)
  cat("Obszar krytyczny prawostronny:", "<",t_kryt,";∞)\n")
  
  return(decyzja)
}