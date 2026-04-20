Zadanie5 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  srednia = mean(market)
  wariancja = sum((market - srednia)^2) / n

  X = n * wariancja / (h0 ^ 2)
  
  qchi_lewy = qchisq(alfa/2, n - 1)
  qchi_prawy = qchisq(1 - alfa/2, n - 1)
  
  cat("X:", X, "(-inf,",qchi_lewy,"> u <", qchi_prawy, ",inf)\n")
  if (X > qchi_lewy && X < qchi_prawy) {
    cat("X nie należy do obszaru krytycznego -> nie ma podstaw do odczucenia hipotezy, że odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest równe 4,0 zł\n")
  } else {
    cat("X należy do obszaru krytycznego -> odrzucamy hipotezę -> odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu nie jest równe 4,0 zł\n")
  }
}