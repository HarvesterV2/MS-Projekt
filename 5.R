Zadanie5 <- function(market, miary) {
  n <- length(market)
  
  m_0 = 4
  X = n * miary$wariancja_proba / (m_0 ^ 2)
  
  alpha = 0.05
  qchi_l = qchisq(alpha/2, n - 1)
  qchi_r = qchisq(1 - alpha/2, n - 1)
  
  cat("X:", X, "(-inf,",qchi_l,"> u <", qchi_r, ",inf)\n")
  if (X > qchi_l && X < qchi_r) {
    cat("X nie należy do obszaru krytycznego -> nie ma podstaw do odczucenia hipotezy, że odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest równe 4,0 zł\n")
  } else {
    cat("X należy do obszaru krytycznego -> odrzucamy hipotezę -> odchylenie standardowe miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu nie jest równe 4,0 zł\n")
  }
}