Zadanie6 <- function(markets, alfa) {
  # ponieważ sigma1 i sigma2 nie są znane i nie wiemy czy są jednakowe czy też nie, przeprowadzamy test Snedecora.
  cat("=== Przeprowadzenie testu Snedecora: ===\n")
  decyzja = test_Snedecora(markets, alfa)
  
  cat("=== Przeprowadzenie testu średnich w marketach: ===\n")
  test_srednie_marketow(markets, alfa, decyzja)
  
}

test_Snedecora <- function(markets, alfa) {
  market1 = markets[[1]]
  market2 = markets[[2]]
  
  n1 = length(market1)
  n2 = length(market2)
  
  wariancja1 = var(market1)
  wariancja2 = var(market2)
  
  F = wariancja1 / wariancja2
  
  cat("Hipoteza zerowa - wariancje miesięcznych wydatków w obu marketach są równe.\n")
  cat("Hipoteza alternatywna - wariancje miesięcznych wydatków w obu marketach nie są równe.\n")
  
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

test_srednie_marketow <- function(markets, alfa, czy_wariancje_rowne) {
  
  cat("Hipoteza zerowa - średnie w obu marketach są równe.\n")
  cat("Hipoteza alternatywna - średnia w pierwszym markecie jest mniejsza.\n")
  
  if(czy_wariancje_rowne)
  {
    #Jeśli są jednakowe, to stosujemy model 2
    wynik_modelu = statystyka_t_model2(markets, alfa)
  }
  else
  {
    #Jeśli nie są jednakowe, to stosujemy model 3
    wynik_modelu = statystyka_C_model3(markets, alfa)
  }
  statystyka = wynik_modelu$statystyka
  df = wynik_modelu$df
  
  cat("Statystyka testowa:", statystyka, "\n")
  kryt = qt(alfa, df)
  cat("Obszar krytyczny: (-∞,", kryt, "]\n")
  #pvalue
  cat("P-value:", pt(statystyka, df), "\n")
  decyzja = statystyka <= kryt
  if (decyzja) {
    cat("Odrzucamy H0 - wydatki w 1 markecie są mniejsze.\n")
  } else {
    cat("Brak podstaw do odrzucenia H0.\n")
  }
}

statystyka_t_model2 <- function(markets, alfa) {
  
  x = markets[[1]]
  y = markets[[2]]
  
  n1 = length(x)
  n2 = length(y)
  
  x1 = mean(x)
  x2 = mean(y)
  
  S1 = var(x)
  S2 = var(y)
  
  sp2 = ((n1 * S1 + n2 * S2) / (n1 + n2 - 2)) * ((n1 + n2) / (n1 * n2))
  
  T = (x1 - x2) / sqrt(sp2)
  
  df = n1 + n2 - 2
  
  return(list(statystyka = T, df = df))

}

statystyka_C_model3 <- function(markets, alfa) {
  
  C = (x1 - x2) / sqrt((S1 / (n1 - 1)) + (S2 / (n2 - 1)))
  df = NA
  
  return(list(statystyka = C, df = df))
}