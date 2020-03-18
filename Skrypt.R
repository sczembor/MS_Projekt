#### INSTALACJA PAKIETU PACMAN ####

if(!require(pacman)) install.packages(pacman)

#### DOŁĄCZANIE PAKIETÓW ####

pacman::p_load(pacman,rio,ggplot2)

#### IMPORTOWANIE DANYCH Z PLIKU CSV DO PROGRAMU ####

mieszkania <- import("mieszkania_dane.csv") # JEŻELI NIE DZIAŁA PODAĆ PEŁNĄ ŚCIEŻKĘ DO PLIKU

typeof(mieszkania)
mieszkania



ggplot(mieszkania, aes(x = powierzchnia, y = cena, color = pokoje)) +
        geom_point() +
        labs(x = "powierzchnia w metrach kwadratowych", y = "cena za miesiąc wynajmu", title = paste("Zaleność ceny
        mieszkań od powierzchni bez uzwględniania liczby pokoii"))+
        geom_smooth(method = "lm", se = F, color = "grey")

ggplot(mieszkania, aes(x = powierzchnia, y = cena, color = "red")) +
        geom_point() +
        labs(x = "powierzchnia w metrach kwadratowych", y = "cena za miesiąc wynajmu", title = paste("Zaleność ceny
        mieszkań od powierzchni bez uzwględniania liczby pokoii"))+
        geom_smooth(method = "lm", se = F, color = "grey")


##### MODELE REGRESJI LINIOWEJ ####

reg1 <- lm(cena ~ powierzchnia, data = mieszkania) # BEZ UWZGLĘDNIANIA POKOII
reg2 <- lm(cena ~ powierzchnia + pokoje , data = mieszkania) # Z UWZGLĘDNIEM OKOII

reg1
summary(reg1)

reg2
summary(reg2)

#####ZBIÓR PAR NA WYKRESIE X-Y ####

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
legend("bottomright", title = "liczba pokoii", c("4","3","2","1"), col = c("blue", "green", "red", "black"), pch = 19 , cex = 0.8 )

#### HISTOGRAMY REZYDUÓW ####

par(mfrow = c(2,1)) #WYŚWIETLANIE W DWÓCH WIERSZACH I JEDNEJ KOLUMNIE

hist(residuals(reg1),
     xlim = c(-150,150), #LIMITY OSI X
     ylim = c(0, 0.01), #LIMITY OSI Y
     freq = FALSE, #GĘSTOŚĆ ZAMIAST CZĘSTOTLIWOŚCI
     breaks = 5, #SUGEROWANA LICZBA PROSTOKĄTÓW
     main = "Histogram rezyduów bez uwzględniania ceny pokoii", #TYUŁ
     xlab = "rezyduy",
     col = "orange") 

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
     xlab = "rezyduy",
     col = "blue")

curve(dnorm(x, mean = mean(residuals(reg2)), sd = sd(residuals(reg2))),
      col = "red",# KOLOR KRZYWEJ
      lwd = 2,    # SZEROKOŚĆ KRZYWEJ
      add = TRUE) # DODAJ NA POPRZEDNI WYKRES

par(mfrow = c(1,1))#POWRÓT DO WYŚWIETLANIA DOMYŚLNEGO, JEDEN WIERSZ, JEDNA KOLUMNA


#### CZYSZCZENIE ŚRODOWISKA ####

rm(list = ls()) 

#### ODŁĄCZANIE PAKIETÓW ####

p_unload(all)

#### CZYSZCZENIE WYKRESÓW ####

dev.off()

#### CZYSZCZENIE KONSOLI ####

cat("\014")

