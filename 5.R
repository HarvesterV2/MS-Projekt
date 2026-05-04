Zadanie5 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  srednia = mean(market)
  wariancja = var(market)
  X = (n - 1) * wariancja / (h0 ^ 2)
  
  cat("X:",X,"\n")
  if (rodzaj == 0) {
    K_dwustronny_lewy = qchisq(alfa/2, n - 1)
    K_dwustronny_prawy = qchisq(1 - alfa/2, n - 1)
    decyzja = (X <= K_dwustronny_lewy) || (X >= K_dwustronny_prawy)
    cat("Obszar krytyczny dwustronny:", X, "( -inf ;",K_dwustronny_lewy,"> u <", K_dwustronny_prawy, "; inf )\n")
  } else if (rodzaj == 1) {
    K_prawostronny = qchisq(1 - alfa, n - 1)
    decyzja = (X >= K_prawostronny)
    cat("Obszar krytyczny prawostronny:", X, "<",K_prawostronny,"; inf )\n")
  } else {
    K_lewostronny = qchisq(alfa, n - 1)
    decyzja = (X <= K_lewostronny)
    cat("Obszar krytyczny lewostronny:",X,"( 0 ;", K_lewostronny, ">\n")
  }


  if (decyzja) {
    cat("Odrzucamy H0\n")
  } else {
    cat("Brak podstaw do odrzucenia H0\n")
  }
}