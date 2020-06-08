#### INSTALACJA PAKIETU PACMAN ####

if(!require(pacman)) install.packages(pacman)

#### DOŁĄCZANIE PAKIETÓW ####

pacman::p_load(pacman,rio,ggplot2,nortest,lmtest)

#### IMPORTOWANIE DANYCH Z PLIKU CSV DO PROGRAMU ####

mieszkania <- import("mieszkania_dane.csv") # JEŻELI NIE DZIAŁA PODAĆ PEŁNĄ ŚCIEŻKĘ DO PLIKU

typeof(mieszkania)
mieszkania

##### ZBIÓR PAR NA WYKRESIE X-Y ####
ggplot(mieszkania, aes(x = powierzchnia, y = cena, color = pokoje)) +
        geom_point() +
        labs(x = "powierzchnia w metrach kwadratowych", y = "cena za miesiąc wynajmu",
        title = paste("Zaleność ceny mieszkań od powierzchni z uzwględnieniem liczby pokoi"))+
        geom_smooth(method = "lm", se = F, color = "grey")

ggplot(mieszkania, aes(x = powierzchnia, y = cena, color = "red")) +
        geom_point() +
        labs(x = "powierzchnia w metrach kwadratowych", y = "cena za miesiąc wynajmu",
        title = paste("Zaleność ceny mieszkań od powierzchni bez uzwględniania liczby pokoi"))+
        geom_smooth(method = "lm", se = F, color = "grey")

##### TE SAME WYKRESY UTWORZONE ZA POMOCA FUNKCJI PLOT (NIE KORZYSTAJAC Z GGPLOT)
plot(mieszkania$powierzchnia, mieszkania$cena,
     pch = 19,         # PEŁNE KÓŁKO
     cex = 1.0,        # SKALOWANIE
     col = "#cc0000",  # CZEROWNY
     main = "Zależność ceny mieszkania od powierzchni mieszkania", #TYTUŁ
     xlab = "Powierzchnia",
     ylab = "Cena")

plot(mieszkania$powierzchnia, mieszkania$cena,
     pch = 19,         # PEŁNE KÓŁKO
     cex = 1.0,        # SKALOWANIE
     col = mieszkania$pokoje,  # CZEROWNY
     main = "Zależność ceny mieszkania od powierzchni mieszkania", #TYTUŁ
     xlab = "Powierzchnia",
     ylab = "Cena")
     legend("bottomright", title = "liczba pokoii", c("4","3","2","1"),
       col = c("blue", "green", "red", "black"), pch = 19 , cex = 0.8 )


##### MODELE REGRESJI LINIOWEJ ####

reg1 <- lm(cena ~ powierzchnia, data = mieszkania) # BEZ UWZGLĘDNIANIA POKOII
reg2 <- lm(cena ~ powierzchnia + pokoje , data = mieszkania) # Z UWZGLĘDNIEM OKOII
summary(mieszkania)
reg1
summary(reg1)

reg2
summary(reg2)

#####Pelna diagnostyka z uzyciem LMtest####
#Testy jednorodnosci wariancji
gqtest(reg1) #  { p=0.1431 -> p>0,05 wiec wariancje sa homogeniczne 
gqtest(reg2) #    p=0.1271 -> p>0,05 wiec wariancje sa homogeniczne
bptest(reg1) #    p=0.344 -> p>0,05 wiec wariancje sa homogeniczne
bptest(reg2) #    p=0,4348 -> p>0,05 wiec wariancje sa homogeniczne 
hmctest(reg1) #   p=0.12 -> p>0,05 wiec wariancje sa homogeniczne
hmctest(reg2) #   p=0.124 -> p>0,05 wiec wariancje sa homogeniczne } Wariancje nie sa niejednorodne

#Testy niezaleznosci
dwtest(reg1) # DW=1,5975  dl<DW<dg wiec nie ma atokorelacji miedzy resztami (p=0,07068)
dwtest(reg2) # DW=1,5975  dl<DW<dg wiec nie ma atokorelacji miedzy resztami (p=0,07131)
bgtest(reg1) # p=0.2392 -> p>0,05 i df=1, wiec jest autokorelacja w jednej kolejnosci
bgtest(reg2) # p=0.2349 -> p>0,05 i df=1, wiec jest autokorelacja w jednej kolejnosci

#Testy liniowosci
harvtest(reg1) # p=0,005935 -> p<0,05 wiec brak podstaw do odrzucenia hipotezy zerowej, ze nie jest liniowa (czyli moze byc liniowa)
harvtest(reg2) # p=0,01495 -> p<0,05 wiec brak podstaw do odrzucenia hipotezy zerowej, ze nie jest liniowa (czyli moze byc liniowa)
raintest(reg1) # p=0,3591 -> p<0,05 wiec nie odrzuca sie hipotezy zerowej, ze nie jest liniowa
raintest(reg2) # p=0,4909 -> p<0,05 wiec nie odrzuca sie hipotezy zerowej, ze nie jest liniowa
resettest(y=a*x+b,reg1) # p=0,5829 -> p>0,05 wiec chyba odrzucamy hipoteze, ze pasuja, czyli ze jest liniowa
resettest(y=a*x+b,reg2) # p=0,03014 -> p<0,05 wiec nie odrzucamy hipotezy, ze pasuja.
# z testow liniowosci wychodzi, ze regresja 2 moze byc liniowa, a regresja 1 w 2 testach nie odrzucila tej hipotezy
# ale odrzucil ja test 3 (resetttest), bo p>0,05


#### HISTOGRAMY REZYDUÓW ####
par(mfrow = c(2,1)) #WYŚWIETLANIE W DWÓCH WIERSZACH I JEDNEJ KOLUMNIE

hist(residuals(reg1),
     xlim = c(-150,150), #LIMITY OSI X
     ylim = c(0, 0.01), #LIMITY OSI Y
     freq = FALSE, #GĘSTOŚĆ ZAMIAST CZĘSTOTLIWOŚCI
     breaks = 5, #SUGEROWANA LICZBA PROSTOKĄTÓW
     main = "Histogram rezyduów bez uwzględniania ceny pokoii", #TYTUŁ
     xlab = "rezydua", #NAZWANIE OSI X
     col = "orange") #KOLOR HISTOGRAMU

curve(dnorm(x, mean = mean(residuals(reg1)), sd = sd(residuals(reg1))),
     col = "red", # KOLOR LINII
     lwd = 2,     # SZEROKOŚĆ LINII
     add = TRUE)  # DODAJ NA POPRZEDNI WYKRES

hist(residuals(reg2),
     xlim = c(-150,150),
     ylim = c(0, 0.01),
     freq = FALSE,
     breaks = 5,
     main = "Histogram rezyduów z uwzględnieniem ceny pokoii",
     xlab = "rezydua",
     col = "blue")

curve(dnorm(x, mean = mean(residuals(reg2)), sd = sd(residuals(reg2))),
      col = "red",# KOLOR KRZYWEJ
      lwd = 2,    # SZEROKOŚĆ KRZYWEJ
      add = TRUE) # DODAJ NA POPRZEDNI WYKRES

par(mfrow = c(1,1))#POWRÓT DO WYŚWIETLANIA DOMYŚLNEGO, JEDEN WIERSZ, JEDNA KOLUMNA

###ZAD 7
#BEZ UWZGLEDNIENIA LICZBY POKOI
lillie.test(residuals(reg1))

n <- length(residuals(reg1))
residuals1Sorted <- sort.default(residuals(reg1))
fnx <- pnorm(residuals1Sorted, mean(residuals1Sorted), sd(residuals1Sorted)) #DYSTRYBUANTA ROZKLADU NORMALNEGO

dn_plus <- abs(seq(1:n)/n-fnx)
dn_minus <- abs(fnx-(seq(1:n)-1)/n)

dn_prim = max(dn_plus, dn_minus)
print(dn_prim)

#Z UWZGLEDNIENIEM LICZBY POKOI
lillie.test(residuals(reg2))

n <- length(residuals(reg2))
residuals2Sorted <- sort.default(residuals(reg2))
fnx <- pnorm(residuals2Sorted, mean(residuals2Sorted), sd(residuals2Sorted))

dn_plus <- abs(seq(1:n)/n-fnx)
dn_minus <- abs(fnx-(seq(1:n)-1)/n)

dn_prim = max(dn_plus, dn_minus)
print(dn_prim)

#### CZYSZCZENIE ŚRODOWISKA ####

rm(list = ls()) 

#### ODŁĄCZANIE PAKIETÓW ####

p_unload(all)

#### CZYSZCZENIE WYKRESÓW ####

dev.off()

#### CZYSZCZENIE KONSOLI ####

cat("\014")

