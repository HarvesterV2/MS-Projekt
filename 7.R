Zadanie7 <- function(markets, alfa, N = 10000, rodzaj = "c") {
  
  cat("=== Test permutacyjny dla różnicy średnich ===\n")
  
  market1 = markets[[1]]
  market2 = markets[[2]]
  
  n1 = length(market1)
  n2 = length(market2)
  
  cat("Hipoteza zerowa H0 - średnie wydatki w obu marketach są równe.\n")
  
  if (rodzaj == "a") {
    cat("Hipoteza alternatywna H1 - średnie są różne.\n")
  } else if (rodzaj == "b") {
    cat("Hipoteza alternatywna H1 - średnia w 1 markecie większa niż w 2 markecie.\n")
  } else if (rodzaj == "c") {
    cat("Hipoteza alternatywna H1 - średnia w 1 markecie mniejsza niż w 2 markecie.\n") 
  }
  
  W_obs = mean(market1) - mean(market2)
  
  x = c(market1, market2)
  n = n1 + n2
  
  W_perm = numeric(N)
  
  for (i in 1:N) {
    
    perm = sample(x, n, replace = FALSE)
    
    x1_i = perm[1:n1]
    x2_i = perm[(n1 + 1):n]
    
    W_perm[i] = mean(x1_i) - mean(x2_i)
  }
  
  # p-value
  if (rodzaj == "a") {
    pvalue = mean(abs(W_perm) >= abs(W_obs))
  } else if (rodzaj == "b") {
    pvalue = mean(W_perm >= W_obs)
  } else if (rodzaj == "c") {
    pvalue = mean(W_perm <= W_obs)
  }
  
  cat("Liczba permutacji:", N, "\n")
  cat("Statystyka testowa:", W_obs, "\n")
  cat("P-value:", pvalue, "\n")
  
  # decyzja
  decyzja = (pvalue <= alfa)
  
  # CR (opisowy)
  if (decyzja) {
    cat("Odrzucamy hipotezę zerową H0 na rzecz hipotezy alternatywnej H1.\n")
  } else {
    cat("Brak podstaw do odrzucenia H0.\n")
  }
  
  # wykres
  par(mfrow=c(1,1))
  hist(W_perm,
       breaks = "FD",
       main = "Test permutacyjny - rozkład statystyki",
       xlab = "Różnica średnich",
       ylab = "Częstość",
       col = "gray")
  
  abline(v = W_obs, col = "red", lwd = 2)
  
  legend("topright",
         legend = "statystyka obserwowana",
         col = "red",
         lwd = 2)
}