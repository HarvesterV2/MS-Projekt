#install.packages("DescTools")# Install DescTools package
library("DescTools")  
# TO DO - Przerzucić w osobne funkcje każde zadanie



setwd('C:\\Users\\Konrad\\Desktop\\R\\project')
market1 <- scan('market1',sep = ";", dec = ",")
market2 <- scan('market2',sep = ";", dec = ",")

globalmin = min(c(market1,market2))
globalmax = max(c(market1,market2))

#Polecenie 1. Zaprezentować dane wykorzystując wykresy pudełkowe. medianę i kwartale
par(mfrow=c(1,4))
# market 1
market1_srednia = mean(market1)
market1_mediana = median(market1)
market1_moda = Mode(market1)
market1_q1 = quantile(market1,prob=0.25)
market1_q3 = quantile(market1,prob=0.75)
market1_IQR = market1_q3-market1_q1
boxplot(market1, main ="Mediana i Kwartyle\nMarket 1",ylab="Wydatki [zł]",horizontal = FALSE,ylim=c(globalmin,globalmax))
text(0.6,market1_q1,"Q1",col = "blue",cex = 0.7)
text(0.6,market1_mediana,"Mediana",col = "red",cex = 0.7)
text(0.6,market1_q3,"Q3",col = "blue",cex = 0.7)


# market 2
market2_srednia = mean(market2)
market2_mediana = median(market2)
market2_moda = Mode(market2)
market2_q1 = quantile(market2,prob=0.25)
market2_q3 = quantile(market2,prob=0.75)
market2_IQR = market2_q3-market2_q1
boxplot(market2, main ="Mediana i Kwartyle\nMarket 2",ylab="Wydatki [zł]",horizontal = FALSE,ylim=c(globalmin,globalmax))
text(0.6,market2_q1,"Q1",col = "blue",cex = 0.7)
text(0.6,market2_mediana,"Mediana",col = "red",cex = 0.7)
text(0.6,market2_q3,"Q3",col = "blue",cex = 0.7)

# na wykresie w postaci kropek zaznaczone są dane nietypowe

#sum1 = summary(market1)

#Wykres odchylenie standardowe.
# market 1
market1_odchylenie_standardowe = sd(market1)
market1_xx = c(min(market1),mean(market1)-sd(market1),mean(market1),mean(market1)+sd(market1),max(market1))
boxplot(market1_xx, main ="Odchylenie standardowe.\nMarket 1",ylab="Wydatki [zł]",horizontal = FALSE,ylim=c(globalmin,globalmax))

#plot(1,market1_srednia,main ="Odchylenie standardowe.\nMarket 1",ylab = "Wydatki [zł]",xlab = "",xlim= c(0.5,1.5),ylim = c(globalmin,globalmax),xaxt = "n")
#arrows(1,market1_srednia - market1_odchylenie_standardowe,1,market1_srednia + market1_odchylenie_standardowe,code = 3,angle=90)
#text(0.6, market1_srednia, "Średnia",cex = 0.7)

# market 2
market2_odchylenie_standardowe = sd(market2)
market2_xx = c(min(market2),mean(market2)-sd(market2),mean(market2),mean(market2)+sd(market2),max(market2))
boxplot(market2_xx, main ="Odchylenie standardowe.\nMarket 2",ylab="Wydatki [zł]",horizontal = FALSE,ylim=c(globalmin,globalmax))

#plot(1,market2_srednia,main ="Odchylenie standardowe.\nMarket 2",ylab = "Wydatki [zł]",xlab = "",xlim= c(0.5,1.5),ylim = c(globalmin,globalmax),xaxt = "n")
#arrows(1,market2_srednia - market2_odchylenie_standardowe,1,market2_srednia + market2_odchylenie_standardowe,code = 3,angle=90)
#text(0.6, market2_srednia, "Średnia",cex = 0.7)

# Polecenie 2. Dokonać analizy miesięcznych wydatków na jedną osobę, na jarzyny i warzywa klientów wybranych marketów,
# wyznaczając miary przeciętne, zróżnicowania, asymetrii i koncentracji. 

# market 1
wariancja_market1 = var(market1)
odchylenie_std_market1 = sd(market1)
srednia_market1 = mean(market1)
odchylenie_przecietne_market1 = mean(abs(market1-srednia_market1))
mediana_market1 = median(market1)
odchylenie_przecietne_od_mediany_market1 = mean(abs(market1-mediana_market1))
odchylenie_cwiartkowe_market1 = IQR(market1)/2
wspolczynnik_zmiennosci_market1 = odchylenie_std_market1 / srednia_market1 * 100
pozycyjny_wspolczynnik_zmiennosci_market1 = IQR(market1) / (2*mediana_market1) * 100

#install.packages("e1071")
library(e1071)
skosnosc_market1 = skewness(market1) # jest ujemne,asymetria lewostronna
kurtoza_market1 = kurtosis(market1) 
eksces_market1 = kurtoza_market1 - 3

# market 2
wariancja_market2 = var(market2)
odchylenie_std_market2 = sd(market2)
srednia_market2 = mean(market2)
odchylenie_przecietne_market2 = mean(abs(market2-srednia_market2))
mediana_market2 = median(market2)
odchylenie_przecietne_od_mediany_market2 = mean(abs(market2-mediana_market2))
odchylenie_cwiartkowe_market2 = IQR(market2)/2
wspolczynnik_zmiennosci_market2 = odchylenie_std_market2 / srednia_market2 * 100
pozycyjny_wspolczynnik_zmiennosci_market2 = IQR(market2) / (2*mediana_market2) * 100

#install.packages("e1071")
library(e1071)
skosnosc_market2 = skewness(market2) # jest ujemne,asymetria lewostronna
kurtoza_market2 = kurtosis(market2) 
eksces_market2 = kurtoza_market2 - 3

# Opracować histogramy rozkładów empirycznych. Miary wyznaczyć dwoma 
#sposobami: a) na podstawie szeregu szczegółowego, b) na podstawie szeregu rozdzielczego.
par(mfrow=c(2,2))

#market1
#tab_market1 <- table(market1)
#barplot(tab_market1,main = "Histogram liczebności.\nMarket 1",xlab = "Wydatki[zł]",ylab = "Liczność",las = 2,cex.names = 0.7)

hist(market1, main = "Histogram rozkładu empirycznego\nSzereg szczegółowy. Market 1",xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))
breaks_market1 = seq(20,45,by=5)
freq_market1 = table(cut(market1,breaks = breaks_market1, right=FALSE))
hist(market1,breaks = breaks_market1, main = "Histogram rozkładu empirycznego\nSzereg rozdzielczy. Market 1",xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))


#market2
#tab_market2 <- table(market2)
#barplot(tab_market2,main = "Histogram liczebności.\nMarket 2",xlab = "Wydatki[zł]",ylab = "Liczność",las = 2,cex.names = 0.7)
hist(market2, main = "Histogram rozkładu empirycznego\nSzereg szczegółowy. Market 2",xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))
breaks_market2 = seq(20,45,by=5)
freq_market2 = table(cut(market2,breaks = breaks_market2, right=FALSE))
hist(market2,breaks = breaks_market2, main = "Histogram rozkładu empirycznego\nSzereg rozdzielczy. Market 2",xlab="Wydatki[zł]",ylab="Częstość",xlim = c(globalmin-1,globalmax+5))


