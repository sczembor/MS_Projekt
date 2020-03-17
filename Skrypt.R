#### INSTALACJA PAKIETU PACMAN ####

if(!require(pacman)) install.packages(pacman)

#### DOŁĄCZANIE PAKIETÓW ####

pacman::p_load(pacman,rio)

#### IMPORTOWANIE DANYCH Z PLIKU CSV DO PROGRAMU ####

mieszkania <- import("mieszkania_dane.csv") # JEŻELI NIE DZIAŁA PODAĆ PEŁNĄ ŚCIEŻKĘ DO PLIKU

typeof(mieszkania)

View(mieszkania)

#####ZBIÓR PAR NA WYKRESIE X-Y ####

plot(mieszkania$powierzchnia, mieszkania$cena,
     pch = 19,         # PEŁNE KÓŁKO
     cex = 1.0,        # SKALOWANIE
     col = "#cc0000",  # CZEROWNY
     main = "Zależność ceny mieszkania od powierzchni mieszkania", #TYTUŁ
     xlab = "Powierzchnia",
     ylab = "Cena")

#### HISTOGRAMY ####



#### CZYSZCZENIE ŚRODOWISKA ####

rm(list = ls()) 

#### ODŁĄCZANIE PAKIETÓW ####

p_unload(all)

#### CZYSZCZENIE WYKRESÓW ####

dev.off()

#### CZYSZCZENIE KONSOLI ####

cat("\014")
