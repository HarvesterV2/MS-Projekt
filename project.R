setwd(".")
dane <- read.csv2("dane.csv")
market1 <- na.omit(dane$Market1)
market2 <- na.omit(dane$Market2)

globalmin = min(c(market1,market2))
globalmax = max(c(market1,market2))

# Polecenie 1.
# Zaprezentować dane wykorzystując wykresy pudełkowe. medianę i kwartale
source("1.R")
Zadanie1(list(market1, market2))

# Polecenie 2. 
# Dokonać analizy miesięcznych wydatków na jedną osobę, na jarzyny i warzywa klientów wybranych marketów,
# wyznaczając miary przeciętne, zróżnicowania, asymetrii i koncentracji. 
# Opracować histogramy rozkładów empirycznych. Miary wyznaczyć dwoma 
# sposobami: a) na podstawie szeregu szczegółowego, b) na podstawie szeregu rozdzielczego.
source("2.R")
Tabelka_zadanie_2 = Zadanie2(list(market1, market2))

# 3. Sprawdzić, czy miesięczne wydatki na jedną osobę, na jarzyny i warzywa mają rozkład
# normalny (test zgodności Kołmogorowa-Lillieforsa, współczynnik ufności 0,95).
source("3.R")
Zadanie3(market1, market2, policz_miary(market1),  policz_miary(market2))


# 4. Czy na poziomie istotności 0,05 można twierdzić, że przeciętna wartość miesięcznych
# wydatków na jedną osobę, na jarzyny i warzywa dla klientów pierwszego marketu jest
# równa 32 zł?
source("4.R")
Zadanie4(market1, policz_miary(market1))

# 5. Czy na poziomie istotności 0,05 można twierdzić, że odchylenie standardowe miesięcz-
# nych wydatków na jedną osobę, na jarzyny i warzywa dla klientów drugiego marketu jest
# równe 4,0 zł?
source("5.R")
Zadanie5(market2, policz_miary(market2))