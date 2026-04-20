Zadanie4 <- function(market, h0, alfa, rodzaj) {
  n = length(market)
  
  srednia = mean(market)
  odchylenie = sqrt(sum((market - srednia) ^ 2) / n)
  
  t = (srednia - h0) * sqrt(n - 1) / odchylenie
  
  t_crit = qt(1 - alfa / 2, n - 1)
  
  cat("T:", t, "T_kryt:", t_crit, "\n")
  if (t > -t_crit && t < t_crit) {
    cat("T nie należy do obszaru krytycznego -> nie ma podstaw do odrzucenia hipotezy, że przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu jest równa 32 zł.\n")
  } else {
    cat("T należy do obszaru krytycznego -> odrzucamy hipotezę -> przeciętna wartość miesięcznych wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu nie jest równa 32 zł.\n")
  }
}