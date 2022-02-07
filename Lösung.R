# Abschlussbericht Mathematische Methoden
# Variable 1: Durchschnittliches Haushaltseinkommen in € je Einwohner 
# Variable 2: Vorzeitige Sterblichkeit (Todesfälle pro 1000 EW bei unter 70 Jahren) 
# bearbeitet von: Philipp Jens Krüger
# Matrikelnummer: 903252

# Vorbereitung
# packages

library(readr)                                # zum einlesen der Daten
library(readxl)                               # zum einlesen von exceldateien
library(tidyverse)                            # manipulation der Daten (incl. dplyr & ggplot2)
library(sp)                                   # räuml. Daten 
library(sf)                                   # shape-Dateien einlesen
library(raster)                               # Datenaquise
library(tmap)                                 # zum Erstellen von thematischen Karten
library(ggrepel)      


# package zur vermeidung von überlappenden Beschriftunge
# 1. Daten einlesen und als dataframe konvertieren (Aufgabe 1)

# Einlesen der csv-Tabelle

Tabelle <- readr::read_lines("C:/Users/Philipp Jens/Desktop/r_test/Data/903252.csv")

# Bearbeiten der Tabelle

Tabelle <- Tabelle[-2]                                                                            # löschen der 2. Zeile, da unbrauchbar
Tabelle <- gsub( '"', '', Tabelle)                                                                # sucht und entfernt " 

readr::write_lines( x = Tabelle, file = "./Data/903252.csv")                                             # neue CSV-Datei gespeichert

tibble <- read_delim("./Data/903252.csv",                                                                # importiert CSV als tibble
                     delim = ";",                                                                 # Trennzeichen
                     skip = 1,                                                                    # Überspringt 1. Zeile (Kopfzeile)
                     col_names = c("Nr.", "Kreis/Region", "Aggregat", "HE", "vSM"),               # Spaltennamen in genannter Reihenfolge
                     col_types = cols(                                                            # Datentypen
                       'Aggregat' = col_skip(),                                                   # entfernt die Spalte "Aggregat"
                       'HE' = col_double(), 'vSM' = col_double()),                                # double                
                     locale = locale( decimal_mark = ","),                                        # dt. Dezimaltrennzeichen festlegen
                     trim_ws = TRUE) 

# Konvertieren in dataframe

as.data.frame(tibble)                                                                               
is.data.frame(tibble)                                                                             # Überprüfung -> TRUE

df <- tibble
saveRDS( df, file = "903252.rds")

# Überprüfen der Korrelation

Korrelation <- round(cor(df$HE ,df$vSM), digits = 2)                                  # die Variabeln korrelieren, da 0.77 > 0.60

# 2. Diagramme und Boxplots in ggplot2 (Aufgabe 2) 
# Überblick über die Variablen erhalten, um die passenden Diagramme und Skalierung auszuwählen

summary(df$HE)
summary(df$vSM)

# Histogramme:

# Histogramm Vorzeitige Sterblichkeit (Todesfälle pro 1000 EW bei unter 65 Jahren) 2017

Sturges_vSM <- nclass.Sturges(df$vSM)
ggplot2::ggplot(data = df,                                                                                  
                aes(x= vSM)) +                                                                       
  geom_histogram(bins = Sturges_vSM,                                                          
                 color ='black',                                                                            
                 fill = 'antiquewhite') +                                                                     
  stat_bin(aes(label=..count..), vjust= -0.5, 
           geom = 'text', position = 'identity', bins= Sturges_vSM) +                                
  xlab("Vorzeitige Sterblichkeit (Todesfälle pro 1000 EW bei unter 70 Jahren)") +                                                       
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +                       
  ggtitle("Häufigkeitsverteilung der Vorzeitigen Sterblichkeit 2017") + 
  theme_bw() +                                                                                       
  theme(plot.title = element_text(size=11, face = "bold")) +                                         
  ylab("Verteilung in den Landkreisen (absolute Häufigkeit)") + 
  theme(axis.line = element_line(colour = 'black', size = 1, linetype = 'solid'),                    
        plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'))

# Histogramm Vorzeitige Sterblichkeit (Todesfälle pro 1000 EW bei unter 65 Jahren) 2017 mit Kerneldichtefunktion

ggplot(df, aes(x= vSM)) +
  geom_histogram(aes( y= ..density..),                                                              # Y-Achse als Verteilungsdichte
                 bins = Sturges_vSM, colour = 'black', fill = 'antiquewhite' ) +  
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  geom_density( alpha= 0.4,                                                                         # Kernel-Density-Dichte
                fill= 'red') +                                                                              # Farbe
  geom_vline( aes( xintercept= median(vSM), color = 'median'),                                  # Median als vertikale Linie einfÃ¼gen
              linetype= 'dashed', size= 1) +                                                                    # Liniengart, -grÃ¶Ãe
  geom_vline(aes(xintercept = mean(vSM), color = 'mean'), linetype= 'dashed', size = 1) +       # Mitelwert hinzufÃ¼gen
  scale_color_manual(name = "Legende", values = c(median = "red", mean = "blue")) +                 # Legende hinzufÃ¼gen und Farben zuordnen
  xlab("Vorzeitige Sterblichkeit (Todesfälle pro 1000 EW bei unter 70 Jahren)") +
  ggtitle("Häufigkeitsverteilung der Vorzeitigen Sterblichkeit 2017") +  
  theme_bw() + theme(plot.title = element_text(size=10, face = "bold", hjust = 0.5)) +
  ylab("Dichteverteilung") +
  theme(axis.line = element_line(colour = 'black', size = 1, linetype = 'solid'))

# Säulendiagramm Einkommenssteuer in Euro je Einwohner
Sturges_HE <- nclass.Sturges(df$HE)
ggplot2::ggplot(data = df,                                                                         # Datentabelle festlegen
                aes(x= HE)) +                                                              # Daten aus genannter Spalte für x-Achse
  geom_histogram(bins = Sturges_HE,                                                        # Histogramm mit Klassenanzahl nach Sturges
                 color ='black',                                                                   # Farbe der Umrandung
                 fill = 'aquamarine') +                                                            # Füllfarbe
  stat_bin(aes(label=..count..), vjust= -0.5,                                                      # hinzufügen von Häufigkeitswerten
           geom = 'text', position = 'identity', bins= Sturges_HE) +                       # Angabe der Klassenzahl notwendig
  xlab("Anteil Ausländer der Empfänger von Lebensunterhalthilfe pro 1000 E. 2017") +               # x-Achsenbeschriftung
  scale_x_continuous(breaks = c(1250,1500,1750,2000,2250,2500,2750,3000,3250,3500)) +                         # X-Achse Skala festlegen
  ggtitle("Häufigkeitsverteilung des Ausländeranteils von Empfängern von Lebensunterhalthilfe") +  # Diagrammtitel
  theme_bw() +                                                                                     # Hintergrund weiß mit schwarzem Raster
  theme(plot.title = element_text(size=11, face = "bold")) +                                       # Titelschriftgröße und fettgedruckt
  ylab("Vorkommen in den Landkreisen (absolute Häufigkeit)") +  
  scale_y_continuous(labels = scales::percent_format(scale = 1))
  theme(axis.line = element_line(colour = 'black', size = 1, linetype = 'solid'),                  # Achsenlinien; Farbe, Größe und Linientyp
        plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'))                          # Überschrift, Position, Größe, fettgedruckt

# Kreisdiagramm

table(df$HE)
table(df$HE)/length(df$HE)
count.data <- data.frame(class =                                                                   
                           c("1300€", "1400€", "1500€", "1600€", "1700€", "1800€", "€", "9€", "10€", "11€", "14€"),   
                         n = c(33, 114, 93, 71, 36, 25, 18, 4, 4, 2, 1),                           
                         prop = c(8.23, 28.43, 23.19, 17.7, 8.98, 6.23, 4.49, 1, 1, 0.5, 0.25))    

ggplot(count.data, aes(x = "", y = prop, fill = class)) +                                          
  geom_bar(width = 1, stat = "identity", color = "white") +                                        
  geom_text_repel(aes(x = 1.4, y = 0, label = prop),                                        
                  nudge_x = 0.3,                                                                   
                  segment.size = 0.7) +                                                            
  coord_polar('y', start = 0) +                                                                    
  theme_void() +                                                                                   
  labs(fill = "€/qm",                                                                              
       x = NULL, y = NULL,                                                                         
       title = "Relative Häufigkeit der Haushaltseinkommen 2017 in %") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold')) 

# Boxplots

ggplot(df, aes(x = HE))  

#filter HE outliers
outliers_he <- boxplot(df$HE)$out
#drop the rows containing outliers
df_he_no_outliners <- df[-c(which(df$HE %in% outliers_he)),]
#boxplot without outliers
boxplot(df_he_no_outliners$HE, main="Verteilung der Haushaltseinkommen")
#with outliners
#boxplot(df$HE, main="Verteilung der Haushaltseinkommen")


#filter vSM outliers
outliers_vSM <- boxplot(df$vSM)$out
#drop the rows containing outliers
df_vSM_no_outliners <- df[-c(which(df$vSM %in% outliers_vSM)),]
#boxplot without outliers
boxplot(df_vSM_no_outliners$vSM, main="Vorzeitige Sterblichkeit Männer (Todesfälle pro 1000 EW bei unter 70 Jahren")
#with outliners
#boxplot(df$vSM, main="Vorzeitige Sterblichkeit Männer (Todesfälle pro 1000 EW bei unter 70 Jahren)")

# Punktestreuungsdiagramm (4. Aufgabe)

#drop the rows containing outliners vSM from the data which already removed outliners df
df_no_outliners <- df_he_no_outliners[-c(which(df_he_no_outliners$vSM %in% outliers_vSM)),]

ggplot(df_no_outliners) + geom_point(aes(HE, vSM)) +                                      # Erstellen Streudiagramm
  xlab("Haushaltseinkommen 2017 (€)") +theme_bw() +
  theme(axis.line = element_line(colour = 'black', size = 1, linetype = 'solid'),                  # Achsenlinien; Farbe, Größe und Linientyp
        plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'))                          # Überschrift, Position, Größe, fettgedruckt


  ylab("Vorzeitige Sterblichkeit (Todesfälle pro 1000 EW bei unter 70 Jahren)") +
  scale_x_continuous(breaks = c(0,100,200,300,400,500,600,700,800,900,1000)) +
  scale_y_continuous(breaks = c(0,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,3.0)) +
  theme_bw() +
theme(axis.line = element_line(colour = 'black', size = 1, linetype = 'solid'),                  # Achsenlinien; Farbe, Größe und Linientyp
      plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'))                          # Überschrift, Position, Größe, fettgedruckt


#4. Regression----

#Regressionsmodell
Regressionsmodell <- lm(formula = HE ~ vSM, data = df)                       # Regression (unabhängige Variabel ~ abhängige Variabel)

#Diagramm
ggplot( data= df, aes( HE, vSM)) +
  geom_smooth( method= 'lm') +                                                          # linearen Trend hinzufügen
  geom_point() +
  xlab("Haushaltseinkommen in € je Einwohner 2017") +
  ylab("vorzeitige Sterblichkeit Männer 2017") +
  scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14)) +
  scale_y_continuous(breaks = c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25)) +
  ggtitle("Regression beider Variablen") +
  coord_fixed( sd( df$HE)/sd( df$vSM)) +                                                # Verhältnis Standardabweichung beider Variabeln
  theme_bw() + theme(plot.title = element_text(size=11, face = "bold", hjust = 0.5)) 

summary(Regressionsmodell)                                                              # Zur Erklärung der Güte des Regressionsmodells 

#5. Verknüpfung mit Geometriedaten----
# Einlesen der Excel-Datei
kreise_xls <- readxl::read_excel("./Data/Kreisgrenzen/VG250_Kreisgrenzen.xlsx",
                                 skip = 2,                                                                                        
                                 sheet = 1,                                                                                       
                                 col_names = T) 
c( "krs17", "krs17name")



Gemeinde_sf <- sf::st_read("./Data/Kreisgrenzen/VZ250_GEM.shp")

tmap_mode("plot")

tm_shape(Gemeinde_sf) + tm_borders()

View(Gemeinde_sf)

View(st_drop_geometry(Gemeinde_sf))

Kreis_sf <- summarize( group_by( Gemeinde_sf, ARS_K, GEN_K))

Kreis_sf <- Gemeinde_sf %>%                                                 # Eingangsdaten übergeben an
  dplyr::group_by( ARS_K, GEN_K) %>%                              # Gruppierung ... übergeben an
  dplyr::summarize(Gemeinden = n())                               # Zusammenfassung ... übergeben an <-

saveRDS( Kreis_sf, "Kreise_sf.rds")



tmap_mode("view")

tm_shape( Kreis_sf) + tm_borders()                            # Anzeigen der Grenzen

Land_sf <- Gemeinde_sf %>%                                    # Eingangsdaten übergeben an
  dplyr::group_by( ARS_L, GEN_L) %>%                          # Gruppierung ... übergeben an
  dplyr::summarize(Gemeinden = n())                           # Zusammenfassung ... übergeben an <-

tm_shape( Land_sf) + tm_borders()

Kreisdata_sf <- merge( x = Kreis_sf,
                       y = df,
                       by.x = "ARS_K",
                       by.y = "Nr.")

saveRDS( Kreisdata_sf, "Kreisdata_sf.rds")

head( Kreisdata_sf)

tm_shape( Kreisdata_sf) 
