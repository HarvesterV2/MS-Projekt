Zadanie6 <- function(markets, alfa, rodzaj) {
  cat("=== Przeprowadzenie testu Snedecora: ===\n")
  czy_wariancje_rowne = test_Snedecora(markets, alfa)

  cat("=== Przeprowadzenie testu średnich w marketach: ===\n")
  test_srednie_marketow(markets, alfa, czy_wariancje_rowne, rodzaj)
  
}

test_Snedecora <- function(markets, alfa) {
  market1 = markets[[1]]
  market2 = markets[[2]]
  
  n1 = length(market1)
  n2 = length(market2)
  
  wariancja1 = var(market1)
  wariancja2 = var(market2)
  
  F = wariancja1 / wariancja2
  
  cat("Hipoteza zerowa H0 - wariancje miesięcznych wydatków w obu marketach są równe.\n")
  cat("Hipoteza alternatywna H1 - wariancje miesięcznych wydatków w obu marketach nie są równe.\n")
  
  F_kryt_lewy = qf(alfa / 2, n1 - 1, n2 - 1)
  F_kryt_prawy = qf(1 - alfa / 2, n1 - 1, n2 - 1)
  
  decyzja = (F <= F_kryt_lewy) || (F >= F_kryt_prawy)
  
  cat("Wartość statystyki testowej F:", F, "\n")
  
  cat("Obszar krytyczny dwustronny:",
      "(0;", F_kryt_lewy, "> ∪ <", F_kryt_prawy, ";∞)\n")
  #pvalue
  cat("p-value:", 2 * min(pf(F, n1 - 1, n2 - 1),1 - pf(F, n1 - 1, n2 - 1)), "\n")
  
  if (decyzja) {
    cat("Statystyka testowa F należy do obszaru krytycznego. Odrzucamy hipotezę zerową H0.\n")
  } else {
    cat("Statystyka testowa F nie należy do obszaru krytycznego. Brak podstaw do odrzucenia hipotezy zerowej H0.\n")
  }
  
  return(!decyzja)
}

test_srednie_marketow <- function(markets, alfa, czy_wariancje_rowne, rodzaj) {
  
  cat("Hipoteza zerowa H0 - średnie w obu marketach są sobie równe.\n")
  
  if(rodzaj == "d")
  {
    if(mean( markets[[1]]) > mean(markets[[2]])) {
      rodzaj = "b"
    }else {
      rodzaj = "c"
    }
  }
  
  if (rodzaj == "a") { #obu
    cat("Hipoteza alternatywna H1 - średnia w obu marketach nie są sobie równe.\n")
  } else if (rodzaj == "b") { # prawo
    cat("Hipoteza alternatywna H1 - średnia w pierwszym markecie jest większa.\n")
  } else if (rodzaj == "c") { # lewo
    cat("Hipoteza alternatywna H1 - średnia w pierwszym markecie jest mniejsza.\n")
  }
  
  if(czy_wariancje_rowne)
  {
    #Jeśli są jednakowe, to stosujemy model 2
    wynik_modelu = statystyka_t_model2(markets, alfa, rodzaj)
  }
  else
  {
    #Jeśli nie są jednakowe, to stosujemy model 3
    wynik_modelu = statystyka_C_model3(markets, alfa, rodzaj)
  }
  cat("Statystyka testowa:", wynik_modelu$statystyka, "\n")
  cat("Obszar krytyczny:", wynik_modelu$kryt, "\n")
  cat("P-value:", wynik_modelu$pvalue, "\n")
  if (wynik_modelu$decyzja) {
    cat("Odrzucamy hipotezę zerową H0 na rzecz hipotezy alternatywnej H1.\n")
  } else {
    cat("Brak podstaw do odrzucenia hipotezy zerowej H0.\n")
  }
}

statystyka_t_model2 <- function(markets, alfa, rodzaj) {
  
  market1 = markets[[1]]
  market2 = markets[[2]]
  
  n1 = length(market1)
  n2 = length(market2)
  
  x1sr = mean(market1)
  x2sr = mean(market2)
  
  S1 = var(market1)
  S2 = var(market2)
  
  T = (x1sr - x2sr) / sqrt(((n1 * S1 + n2 * S2) / (n1 + n2 - 2)) * ((n1 + n2) / (n1 * n2)))
  
  df = n1 + n2 - 2
  
  if (rodzaj == "a") {
    
    kryt_left = qt(alfa / 2, df)
    kryt_right = qt(1 - alfa / 2, df)
    
    kryt = paste0("(-∞, ", kryt_left, "] ∪ [", kryt_right, ", ∞)")
    
    pvalue = 2 * (1 - pt(abs(T), df))
    decyzja = (T <= kryt_left) || (T >= kryt_right)
    
  } else if (rodzaj == "b") {
    
    kryt = paste0("[", qt(1 - alfa, df), ", ∞)")
    
    pvalue = 1 - pt(T, df)
    decyzja = (T >= qt(1 - alfa, df))
    
  } else if (rodzaj == "c") {
    
    kryt = paste0("(-∞, ", qt(alfa, df), "]")
    
    pvalue = pt(T, df)
    decyzja = (T <= qt(alfa, df))
    
  }
  
  return(list(statystyka = T,kryt = kryt,pvalue = pvalue,decyzja = decyzja))

}

statystyka_C_model3 <- function(markets, alfa, rodzaj) {
  
  market1 = markets[[1]]
  market2 = markets[[2]]
  
  n1 = length(market1)
  n2 = length(market2)
  
  x1sr = mean(market1)
  x2sr = mean(market2)
  
  S1 = var(market1)
  S2 = var(market2)
  
  C = (x1sr - x2sr) / sqrt((S1 / (n1 - 1)) + (S2 / (n2 - 1)))
  
  # kwantyl Cochrana-Coxa
  c_p = ((S1 / (n1 - 1)) * qt(alfa, n1 - 1) +
           (S2 / (n2 - 1)) * qt(alfa, n2 - 1)) /
    ((S1 / (n1 - 1)) + (S2 / (n2 - 1)))
  
  if (rodzaj == "a") {
    
    c_left = ((S1 / (n1 - 1)) * qt(alfa / 2, n1 - 1) +
                (S2 / (n2 - 1)) * qt(alfa / 2, n2 - 1)) /
      ((S1 / (n1 - 1)) + (S2 / (n2 - 1)))
    
    c_right = ((S1 / (n1 - 1)) * qt(1 - alfa / 2, n1 - 1) +
                 (S2 / (n2 - 1)) * qt(1 - alfa / 2, n2 - 1)) /
      ((S1 / (n1 - 1)) + (S2 / (n2 - 1)))
    
    kryt = paste0("(-∞, ", c_left, "] ∪ [", c_right, ", ∞)")
    
    pvalue = 2 * min(
      pt(C, df = n1 - 1),
      1 - pt(C, df = n1 - 1)
    )
    
    decyzja = (C <= c_left) || (C >= c_right)
    
  } else if (rodzaj == "b") {
    
    kryt = paste0("[", c_p, ", ∞)")
    
    pvalue = 1 - pt(C, df = n1 - 1)
    
    decyzja = (C >= c_p)
    
  } else if (rodzaj == "c") {
    
    kryt = paste0("(-∞, ", c_p, "]")
    
    pvalue = pt(C, df = n1 - 1)
    
    decyzja = (C <= c_p)
    
  }
  
  return(list(statystyka = C,kryt = kryt,pvalue = pvalue,decyzja = decyzja))
}