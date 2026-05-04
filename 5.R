Zadanie5 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  srednia = mean(market)
  wariancja = var(market)
  X = (n - 1) * wariancja / (h0 ^ 2)
  
  cat("X:",X,"\n")
  if (rodzaj == "a") {
    decyzja = zad5_dwustronna(X, alfa, n)
  } else if (rodzaj == "b") {
    decyzja = zad5_prawostronna(X, alfa, n)
  } else if (rodzaj == "c") {
    decyzja = zad5_lewostronna(X, alfa, n)
  } else {
    if (h0 > srednia) {
      decyzja = zad5_prawostronna(X, alfa, n)
    } else {
      decyzja = zad5_lewostronna(X, alfa, n)
    }
  }
  
  if (decyzja) {
    cat("Odrzucamy H0\n")
  } else {
    cat("Brak podstaw do odrzucenia H0\n")
  }
}

zad5_dwustronna <- function(X, alfa, n) {
  K_dwustronny_lewy = qchisq(alfa/2, n - 1)
  K_dwustronny_prawy = qchisq(1 - alfa/2, n - 1)
  decyzja = (X <= K_dwustronny_lewy) || (X >= K_dwustronny_prawy)
  cat("Obszar krytyczny dwustronny:", "( -inf ;",K_dwustronny_lewy,"> u <", K_dwustronny_prawy, "; inf )\n")
  return(decyzja)
}

zad5_lewostronna <- function(X, alfa, n) {
  K_lewostronny = qchisq(alfa, n - 1)
  decyzja = (X <= K_lewostronny)
  cat("Obszar krytyczny lewostronny:","( 0 ;", K_lewostronny, ">\n")
  return(decyzja)
}

zad5_prawostronna <- function(X, alfa, n) {
  K_prawostronny = qchisq(1 - alfa, n - 1)
  decyzja = (X >= K_prawostronny)
  cat("Obszar krytyczny prawostronny:", "<",K_prawostronny,"; inf )\n")
  return(decyzja)
}