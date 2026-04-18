test_lillieforsa <- function(dane, num, miary) {
  dane <- sort(dane)
  n <- length(dane)
  
  mu <- mean(dane)
  sigma <- sd(dane)
  
  # dystrybuanta empiryczna
  Fn <- (1 : n) / n
  
  # dystrybuanta normalna
  F0 <- pnorm(dane, miary$srednia, miary$odchylenie_std)
  
  # statystyka D
  D <- max(abs(Fn - F0))
  
  # przybliżona wartość krytyczna dla alfa = 0.05
  D_crit <- 0.886 / sqrt(n)
  
  cat("=== Market", num, "===\n")
  cat("Statystyka D =", D, "\n")
  cat("Wartość krytyczna =", D_crit, "\n")
  
  if (D > D_crit) {
    cat("Wniosek: odrzucamy H0 → brak normalności\n")
  } else {
    cat("Wniosek: brak podstaw do odrzucenia H0 → rozkład normalny\n")
  }
}
