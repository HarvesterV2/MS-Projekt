Zadanie4 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  
  srednia = mean(market)
  odchylenie = sqrt(sum((market - srednia) ^ 2) / n)
  
  t = (srednia - h0) * sqrt(n - 1) / odchylenie
  
  t_crit = qt(1 - alfa / 2, n - 1)
  
  cat("T:",t, "\n")
  if (rodzaj == 0) {
    t_kryt_prawy = qt(1 - alfa / 2, n - 1)
    t_kryt_lewy = -t_kryt_prawy
    decyzja = (t <= t_kryt_lewy) || (t >= t_kryt_prawy)
    cat("Obszar krytyczny dwustronny:", "( -inf ;",t_kryt_lewy,"> u <", t_kryt_prawy, "; inf )\n")
  } else if (rodzaj == 1) {
    t_kryt = qt(1 - alfa, n - 1)
    decyzja = (t >= t_kryt)
    cat("Obszar krytyczny prawostronny:", "<",t_kryt,"; inf )\n")
  } else {
    t_kryt = -qt(1 - alfa, n - 1)
    decyzja = (t <= t_kryt)
    cat("Obszar krytyczny lewostronny:","( -inf ;", t_kryt, ">\n")
  }
  
  if (decyzja) {
    cat("Odrzucamy H0\n")
  } else {
    cat("Brak podstaw do odrzucenia H0\n")
  }
}