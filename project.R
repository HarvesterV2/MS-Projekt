setwd(".")
dane <- read.csv2("dane.csv")
market1 <- na.omit(dane$Market1)
market2 <- na.omit(dane$Market2)

globalmin = min(c(market1,market2))
globalmax = max(c(market1,market2))

# Polecenie 1.
# Zaprezentować dane wykorzystując wykresy pudełkowe. medianę i kwartale
source("1.R")
Zadanie1_wyniki = Zadanie1(list(market1, market2))

# Polecenie 2. 
# Dokonać analizy miesięcznych wydatków na jedną osobę, na jarzyny i warzywa klientów wybranych marketów,
# wyznaczając miary przeciętne, zróżnicowania, asymetrii i koncentracji. 
source("2.R")

miary1 <- policz_miary(market1)
miary2 <- policz_miary(market2)
miary1_szereg_rozdzielczy <- policz_miary_szereg_rozdzielczy(market1)
miary2_szereg_rozdzielczy <- policz_miary_szereg_rozdzielczy(market2)

# Opracować histogramy rozkładów empirycznych. Miary wyznaczyć dwoma 
# sposobami: a) na podstawie szeregu szczegółowego, b) na podstawie szeregu rozdzielczego.
par(mfrow=c(2,2))
rysuj_histogram(market1, 1)
rysuj_histogram(market2, 2)

# 3. Sprawdzić, czy miesięczne wydatki na jedną osobę, na jarzyny i warzywa mają rozkład
# normalny (test zgodności Kołmogorowa-Lillieforsa, współczynnik ufności 0,95).
source("3.R")
test_lillieforsa(market1, 1, miary1)
test_lillieforsa(market2, 2, miary2)