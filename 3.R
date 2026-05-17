Zadanie3 <- function(market1, market2) {
  cat("=== Test zgodności Kołmogorowa-Lillieforsa dla Marketu 1: ===\n")
  test_lillieforsa(market1)
  cat("=== Test zgodności Kołmogorowa-Lillieforsa dla Marketu 2: ===\n")
  test_lillieforsa(market2)
}

test_lillieforsa <- function(dane) {
  
  cat("Hipoteza zerowa - Miesięczne wydatki na jedną osobę, na jarzyny i warzywa mają rozkład normalny.\n")
  cat("Hipoteza alternatywna - Miesięczne wydatki na jedną osobę, na jarzyny i warzywa nie mają rozkładu normalnego.\n")
  dane = sort(dane)
  n = length(dane)
  
  srednia = mean(dane)
  odchylenie = sd(dane)
  
  # dystrybuanta empiryczna
  Fn_plus = (1 : n) / n
  Fn_minus = ((1 : n) - 1) / n
  
  # dystrybuanta normalna
  F0 <- pnorm(dane, srednia, odchylenie)
  
  # statystyka D
  D_plus = abs(Fn_plus - F0)
  D_minus = abs(F0 - Fn_minus)
  D = max(c(D_plus, D_minus))
  
  # przybliżona wartość krytyczna dla alfa = 0.05
  K_crit = 0.886 / sqrt(n)
  
  cat("Wartość statystyki D:", D, "\n")
  cat("Obszar krytyczny: <", K_crit, ",1>\n")
  
  if (D > K_crit) {
    cat("Statystyka D należy do obszaru krytycznego. Odrzucamy hipotezę zerową, że miesięczne wydatki na jedną osobę, na jarzyny i warzywa mają rozkład normalny.\n")
  } else {
    cat("Statystyka D nie należy do obszaru krytycznego. Nie ma podstaw do odrzucenia hipotezy zerowej, że miesięczne wydatki na jedną osobę, na jarzyny i warzywa mają rozkład normalny.\n")
  }
}
