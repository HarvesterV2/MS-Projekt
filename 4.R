Zadanie4 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  
  srednia = mean(market)
  odchylenie = sqrt(sum((market - srednia) ^ 2) / n)
  
  t = (srednia - h0) * sqrt(n - 1) / odchylenie
  
  t_crit = qt(1 - alfa / 2, n - 1)
  
  cat("T:",t, "\n")
  if (rodzaj == "a") {
    decyzja = zad4_dwustronna(t, alfa, n)
  } else if (rodzaj == "b") {
    decyzja = zad4_prawostronna(t, alfa, n)
  } else if (rodzaj == "c") {
    decyzja = zad4_lewostronna(t, alfa, n)
  } else {
    if (h0 > srednia) {
      decyzja = zad4_prawostronna(t, alfa, n)
    } else {
      decyzja = zad4_lewsotronna(t, alfa, n)
    }
  }
  
  if (decyzja) {
    cat("Odrzucamy H0\n")
  } else {
    cat("Brak podstaw do odrzucenia H0\n")
  }
}

zad4_dwustronna <- function(t, alfa, n) {
  t_kryt_prawy = qt(1 - alfa / 2, n - 1)
  t_kryt_lewy = -t_kryt_prawy
  decyzja = (t <= t_kryt_lewy) || (t >= t_kryt_prawy)
  cat("Obszar krytyczny dwustronny:", "( -inf ;",t_kryt_lewy,"> u <", t_kryt_prawy, "; inf )\n")
  return(decyzja)
}

zad4_lewsotronna <- function(t, alfa, n) {
  t_kryt = -qt(1 - alfa, n - 1)
  decyzja = (t <= t_kryt)
  cat("Obszar krytyczny lewostronny:","( -inf ;", t_kryt, ">\n")
  return(decyzja)
}

zad4_prawostronna <- function(t, alfa, n) {
  t_kryt = qt(1 - alfa, n - 1)
  decyzja = (t >= t_kryt)
  cat("Obszar krytyczny prawostronny:", "<",t_kryt,"; inf )\n")
  return(decyzja)
}