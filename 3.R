Zadanie3 <- function(market1, market2) {
  cat("Market 1:\n")
  test_lillieforsa(market1)
  cat("Market 2:\n")
  test_lillieforsa(market2)
}

test_lillieforsa <- function(dane, alfa) {
  dane <- sort(dane)
  n = length(dane)
  
  srednia = mean(dane)
  odchylenie = sd(dane)
  
  # dystrybuanta empiryczna
  Fn_plus <- (1 : n) / n
  Fn_minus <- ((1 : n) - 1) / n
  
  # dystrybuanta normalna
  F0 <- pnorm(dane, srednia, odchylenie)
  
  # statystyka D
  D_plus = abs(Fn_plus - F0)
  D_minus = abs(F0 - Fn_minus)
  D <- max(c(D_plus, D_minus))
  
  # przybliżona wartość krytyczna dla alfa = 0.05
  K_crit <- 0.886 / sqrt(n)
  
  cat("Statystyka D:", D, "\n")
  cat("Wartość krytyczna:", K_crit, "\n")
  
  if (D > K_crit) {
    cat("D > K_crit -> brak normalności\n")
  } else {
    cat("D <= K_crit -> rozkład normalny\n")
  }
}
