Zadanie4 <- function(market, miary) {
  n <- length(market)
  
  m_0 = 32
  t = (miary$srednia - m_0) * sqrt(n - 1) / miary$odchylenie_std
  
  alpha = 0.05
  t_crit = qt(1 - alpha / 2, n - 1)
  
  cat("T:", t, "T_kryt:", t_crit, "\n")
  if (t > -t_crit && t < t_crit) {
    cat("T nie należy do obszaru krytycznego -> nie ma podstaw do odrzucenia hipotezy, że przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu jest równa 32 zł.\n")
  } else {
    cat("T należy do obszaru krytycznego -> odrzucamy hipotezę -> przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu nie jest równa 32 zł.\n")
  }
}