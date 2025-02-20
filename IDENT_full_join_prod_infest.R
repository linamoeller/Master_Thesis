# ====================================================
# Author: Lina Möller
# Date: 25.10.2024
# Description:  Hier habe ich einen Full join mit infestation und Prod gemacht und somit 14.000 zeilen
#               musste dabei aber große datenmengen imputieren... unsicher wie gut das gelaufen ist
# Ongoing Todos:log Regression einfügen
#               Einschränkungen auf h1_mm noch mal prüfen
#               Kendalls tau anwenden auf mehrere Zusammenhänge
# ====================================================


# ====================================================
# 1. Pakete laden
# ====================================================

#hier möchte ich in der reihenfolge, wie die gebraucht werden die pakete einfügen

# ====================================================
# 2. Daten laden
# ====================================================

# Koordinaten
coordinates <- read.csv("/Users/linamoller/Documents/Uni/Master/Masterarbeit/R Skripte/Flexible Neighborhood Analysis/InputDaten/ident_Plot_coordinates.csv")
head(coordinates)
load("/Users/linamoller/Documents/Uni/Master/Masterarbeit/R Skripte/Flexible Neighborhood Analysis/InputDaten/IDENT-FR_trees_array.Rdata")

# Produktivität
inventory <- read_excel("/Users/linamoller/Documents/Uni/Master/Masterarbeit/R Skripte/Flexible Neighborhood Analysis/InputDaten/IDENT_inventory 2019-20_JF.xlsx", 
                        col_types = c("numeric", "numeric", "numeric", 
                                      "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "text")) %>% 
  separate(pos., c("nrow", "ncol")) %>%       # teilt spalte mit position in jeweil 2 spalten mit koordinaten für x und y achse auf
  mutate(ncol = as.numeric(ncol)) %>%         # ändert die beiden neu entstandenen Spalten in Datentype= numeric um
  mutate(nrow = as.numeric(nrow)) %>% 
  clean_names() %>%                           # säubert die Spaltennamen (einheitlicher Stil: kleinbuchstabig, keine Sonderzeichen oder Leerzeichen)
  drop_na(d1_mm) %>%                          # entfernt alle zeilen, in denen d1_mm leer ist
  select(1:7, 10)                                 # wählt Zeilen aus (Block, Plot, pos. (2x), specien, mixture, d1_mm)



# kurzer check von inventory
head(inventory)

# bark beetle infestation
infest <- as_tibble(read.csv("/Users/linamoller/Documents/Uni/Master/Masterarbeit/R Skripte/Flexible Neighborhood Analysis/InputDaten/InfestationInventory_Winter2020_FS_corrected.csv",
                             na.strings=c("","na"))) %>%
  # formatiere Datensatz in tibble
  # na.strings bestimmt, welche zeichenfolgen als NA gewertet werden sollen
  select(-Date.) %>%                          # entferne die Date-Spalte
  rename("nrow"=X.Pos) %>%                    # Umbenennung: X.Pos in nrow
  rename("ncol"=Y.Pos) %>% 
  clean_names()                               # Bereinigung von Spaltennamen

head(infest)





# ====================================================
# Hilfsfunktion
# ====================================================
# funktion array to dataframe  konvertiert ein 3D array in einen Dataframe, 
# jede Dimension wird in Spaltenaufgebrochen
# können dadurch Koordinaten in tabellarische Form bringen
array_to_dataframe <- function(data){
  ##create output dataframe
  result <- data.frame("dim" = rep(1:dim(data)[3],
                                   each = dim(data)[1] * dim(data)[2]),
                       "ncol" = rep(1:dim(data)[2],
                                    dim(data)[1] * dim(data)[3]),
                       "nrow" = rep(1:dim(data)[1], each = dim(data)[2],
                                    dim(data)[3]),
                       "value" = numeric(prod(dim(data))))
  datalength <- dim(data)[1] * dim(data)[2]
  ##fill the dataframe with values from the array
  for(i in seq_len(dim(data)[3])){
    result$value[(i - 1) * datalength + 1:datalength] <-
      pivot_longer(as.data.frame(data[, ,i]),
                   cols = seq_len(NCOL(data[,,i])))$value
    
  }
  return(result)
}



# ====================================================
# 3. Räumliche Datenvorbereitung
# ====================================================
head(coordinates)
# Brechnung von Koordinaten in Metern
# Gitterpositionen der Baumstandorte innerhalb eines Plots
# Erklärung der genauen Zahlen in Paper Flex. Neigh. ab Ende Seite 2
# Multiplikatoren: Schrittweite zwischen den Bäumen in Metern 
#     (6 * 0.45) beschreibt die Breite (oder Länge) der 7 Bäume pro Zeile oder Spalte innerhalb eines Plots 
#     (6 Abstände zwischen 7 Bäumen à 45 cm), 
#     Plotabstand von 1.8 m
coordinates$x_m <- (6 * 0.45 + 1.8) * (coordinates$Plot_coord_X - 1)   
coordinates$y_m <- (6 * 0.45 + 1.8) * (coordinates$Plot_coord_Y - 1)

species <- array_to_dataframe(trees_array)

species_pos  <- cbind(species,
                      x_m = rep(coordinates$x_m,each = 49) +
                        0.45 * 0:6,  
                      y_m = rep(coordinates$y_m, each = 49)+
                        rep(0.45 * 0:6, each = 7)) %>% 
  # species wird durch 2 weitere spalten ergänzt x_m und y_m
  # x_m enthält räumliche X-Koordinaten in Metern
  # Wiederholt die x&y-Koordinate eines Plots für jeden der 49 Bäume innerhalb des Plots (each = 49)
  # Positionierung berücksichtigt die Anordnung der Bäume in einem 7x7-Gitter pro Plot
  rename("species"= value) %>% 
  rename("plot" = dim) 

# Hinzufügen der Spalte "block" zu species_pos
species_pos <- species_pos %>%
  mutate(block = rep(coordinates$Block, each = 49))  # Block für jeden der 49 Bäume wiederholen

head(species_pos)     #kurzer check
print(species_pos)    #langer check :D

# ====================================================
# 4a. join data (prod und infest)
# ====================================================

# Liste mit Baumarten nach Abkürzungen
species_list <- c("ACPL","LADE","ACSA","LALA","BEPE","PIAB",
                  "BEPA","PIGL","QURO","PISY","QURU","PIST")

# infestation
df_infest <- infest %>% 
  mutate(infestation = case_when(
    infestation_age %in% c("f", "a") ~ "1",   # Wenn infestation_age "f" oder "a" ist, dann 1
    infestation_age == "n" ~ "0",             # Wenn infestation_age "n" ist, dann 0
    infestation_age == "na" ~ "na")) %>% 
  filter(species %in% species_list)

# productivity 
df_prod <- inventory %>% 
  filter(species %in% species_list) %>% 
  filter(d1_mm < 200) #fehleinträge vorbeugen

# Einblick in beide Datensätze
head(df_infest) 
nrow(df_infest)
head(df_infest)

# erst beide tabellen zusammenfügen über block, plot, nrow, ncol
# dann genaue koordinaten mit einfügen je nach obigen variablen

# Zusammenführen der beiden Datensätze
# hier zusammenführen von allen zeilen, auch wenn es keine übereinstimmung in der jew. anderen tabelle gibt
df_all <- full_join(df_prod, df_infest, by = c("block", "plot", "nrow", "ncol"))
# Wenn Werte in einer der Tabellen fehlen, wird NA eingefügt.
head(df_all)
print(df_all)
nrow(df_all)




# ===============================================================================
# 4b. Zusammenführen von Koordinaten und df_all
# ===============================================================================

df_all$block <- as.numeric(df_all$block)
# block 3 ist aufgeteilt in "3" und "3b". ich werte beides als block 3 aus
# "b" aus den Werten in der Spalte block entfernen
species_pos$block <- gsub("b", "", species_pos$block)
species_pos$block <- as.numeric(species_pos$block)
unique(species_pos$block)
# Zusammenführen von df_all und species_pos anhand von plot, ncol, nrow 
df_all <- inner_join(df_all, species_pos, by = c("plot", "ncol", "nrow", "block"))

# Einblick in die resultierende Tabelle
head(df_all)

# Anzahl der Zeilen in der zusammengeführten Tabelle
cat("Anzahl der Zeilen in der finalen df_all:", nrow(df_all), "\n")




# ===============================================================================
# 5. Einträge von Mixture verfollständigen 
# ===============================================================================


# Erstelle die Spalte "Zusammensetzung"
df_all <- df_all %>%
  group_by(plot) %>%  # Gruppieren nach Plot
  mutate(Zusammensetzung = paste(sort(unique(species.x)), collapse = "+")) %>%
  ungroup()  # Gruppierung wieder aufheben

# Ergebnis anzeigen
df_all <- as.data.frame(df_all)
print(df_all)
# spalte zusammensetzung stimmt nicht in allen zeilen mit urspünglicher mixture überein. alledings sollte meine berechnung genau sein


#spalte mixture raus und "Zusammensetzung" umbenennen zu mixture :D
df_all <- subset(df_all, select = -mixture)           # haben wir neu definiert als "Zusammensetzung"
df_all <- subset(df_all, select = -species.y)         # wir bekommen species später durch die tabelle species_pos
df_all <- subset(df_all, select = -species.x)
df_all <- subset(df_all, select = -comments)          # brauchen wir nicht
df_all <- subset(df_all, select = -infestation_age)   # haben wir in infestation abgedeckt
head(df_all)
colnames(df_all) <- c("block", "plot", "nrow", "ncol", "d1_mm", "h1_mm", "dead", "infestation", "species", "x", "y", "Zusammensetzung")
print(df_all)

# ===============================================================================
# 6. jetzt mit mice versuchen die fehlenden daten zu importieren
# ===============================================================================


install.packages("mice")
library(mice)

# Übersicht: wie viele NA habe ich je spalte
missing_summary <- colSums(is.na(df_all))
print(missing_summary)

# =======================
# datentypen anpassen für MICE
# =======================

str(df_all)

# Variablen in factor umwandeln
df_all$dead <- as.factor(df_all$dead)
df_all$species <- as.factor(df_all$species)
df_all$Zusammensetzung <- as.factor(df_all$Zusammensetzung)
df_all$infestation <- as.numeric(df_all$infestation)

# Prüfen, ob die Umwandlung erfolgreich war
str(df_all)

# Levels der Faktoren - passt soweit, ich muss nichts unnötiges entfernen
levels(df_all$dead)
levels(df_all$species)
levels(df_all$Zusammensetzung)
unique(df_all$infestation)

# ===================
# Imputation starten
# ===================
# methoden: pmm für num, polyreg für
imputed_data <- mice(df_all, 
                     method = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "logreg", "pmm", "polyreg", "pmm", "pmm", "polyreg"), 
                     m = 5,       # Anzahl der Iterationen
                     maxit = 5,  # 5 Iterationen
                     seed = 456)  # Für Reproduzierbarkeit

# Zusammenfassung des Ergebnisses
summary(imputed_data)
print(imputed_data)
completed_data <- complete(imputed_data)
str(completed_data)
print(completed_data)
head(completed_data)

#check: wie viele NA habe ich noch in jeder spalte?
missing_summary_2 <- colSums(is.na(completed_data))
print(missing_summary_2)



# =========================================================
# ALTER GEIL! ICH HABE EINFACH JETZT EINE TABELLE MIT 14000 zeilen voll ausgefüllt!
# =========================================================


# ==============
# Check: wie gut war die Imputation?
# ==============

# Visualisierung der Imputationen
library(ggplot2)

# Vergleich der Verteilung der originalen und imputierten Werte im Histogramm von d1_mm
ggplot(completed_data, aes(x = d1_mm)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  geom_histogram(data = df_all, aes(x = d1_mm), binwidth = 1, fill = "red", alpha = 0.5) +
  labs(title = "Vergleich der Verteilungen von originalen und imputierten Werten", x = "d1_mm", y = "Häufigkeit")
# wir sehen, dass Daten sehr ähnlich verteilt sind -> "gute" Wahl der imputierten Werte

# Vergleich der Häufigkeiten der Kategorien in originalen und imputierten Daten von Parameter "dead"
# tjaaa da sehe ich nciht so viel, bzw. kann nicht so gut abschätzen, ob es gut verteilt wurde
ggplot() +
  geom_bar(data = df_all, aes(x = dead, fill = "Original"), position = "dodge", alpha = 0.5) +
  geom_bar(data = completed_data, aes(x = dead, fill = "Imputiert"), position = "dodge", alpha = 0.5) +
  labs(title = "Vergleich der Häufigkeiten von 'dead' zwischen originalen und imputierten Daten",
       x = "Dead", y = "Häufigkeit") +
  scale_fill_manual(name = "Datensatz", values = c("Original" = "red", "Imputiert" = "blue"))

# Vergleich der Verteilung der originalen und imputierten Werte im Histogramm von infestation
ggplot(completed_data, aes(x = infestation)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  geom_histogram(data = df_all, aes(x = infestation), binwidth = 1, fill = "red", alpha = 0.5) +
  labs(title = "Vergleich der Verteilungen von originalen und imputierten Werten", x = "infestation", y = "Häufigkeit")
# joa... wir sehen, dass Daten ähnlich verteilt sind -> "gute" Wahl der imputierten Werte


# =======================
# dataset conifers
# =======================
list_conifers <- c("PIST", "PISY", "PIGL", "PIAB", "LALA", "LADE")
# only conifers
df_conifers <- df_all[df_all$species %in% list_conifers, ]
head(df_conifers)
#sehe hier, dass ich noch 10 level habe bei species
# zuerst gewundert. kommt aber anscheinend daher, dass level bei filter erhalten bleiben und einträge zwar gefiltert werden aber in levels noch mit angezeigt
str(df_conifers)
unique(df_conifers$species)
# das ergibt jetzt wiederum sinn, dass nur noch 3906 zeilen übrig geblieben sind!
nrow(df_conifers)

# =======================
# dataset cores
# =======================
# betrachte hier nur die cores von den plots
df_cores <- df_all %>% filter(nrow > 1, nrow < 7, ncol > 1, ncol < 7)
head(df_cores)
nrow(df_cores)

# =======================
# dataset only cores + only conifers
# =======================
df_cores_conifers <- df_all %>% filter(nrow > 1, nrow < 7, ncol > 1, ncol < 7) %>% filter(species %in% list_conifers)
nrow(df_cores_conifers)
# =========================================================
# FRAGE: Ergibt es Sinn, nun noch zu versuchen, alle Einträge zu befüllen damit alle Koordinaten Inhalten zugeordnet werden können??
# denke aber: Datengrundlage ist viel zu dünn -> also erst mal nicht gemacht
# =========================================================

# 7. jetzt möchte ich verschiedene Diversitätätsindizees implementieren und regressieren

# ==============================================================================
# 7a. normaler Shannon Index Implemtierung
# ==============================================================================

# Funktion zur Berechnung des Shannon-Index
calculate_shannon <- function(data) {
  data %>%
    group_by(block) %>%                              # Gruppierung nach block
    summarise(
      Shannon_Index = -sum(prop.table(table(species)) * 
                             log(prop.table(table(species))), na.rm = TRUE)
    )
}

# Berechnung des Shannon-Index
shannon_result <- calculate_shannon(completed_data)

# Ausgabe des Ergebnisses
print(shannon_result)

# ===============
# 7b. Funktion zur Berechnung des Shannon-Index über alle Daten hinweg
# ===============

# Funktion zur Berechnung des Shannon-Index
calculate_shannon_overall <- function(data, species_col) {
  # Berechnung der absoluten Häufigkeiten jeder Art
  species_counts <- data %>%
    group_by(across(all_of(species_col))) %>% 
    summarise(count = n(), .groups = "drop")
  
  # Gesamtsumme der Individuen berechnen
  total_count <- sum(species_counts$count)
  
  # Relative Häufigkeiten (p_i) jeder Art berechnen
  species_counts <- species_counts %>%
    mutate(p = count / total_count)
  
  # Shannon-Index berechnen
  shannon_index <- -sum(species_counts$p * log(species_counts$p))
  
  return(shannon_index)
}

# Berechnung des Shannon-Index für completed_data
shannon_overall <- calculate_shannon_overall(
  data = completed_data,   
  species_col = "species"  # Spalte mit den Baumarten
)

# hier kann auch shannon index für andere datas berechnet werden

# Ausgabe des Shannon-Index
cat("Globaler Shannon-Index für den gesamten Datensatz:", shannon_overall, "\n")
print(shannon_overall)



# ===============
# weighted shannon index
# ===============
# berechnet lokale Diversität für jeden Baum mit Gewichtung der Entfernung
# habe patricks funktion etwas umgeschrieben: jetzt...
# -> etwas strukturierter, da mehr von dplyr genutzt
# -> dadurch besser zu erweitern
weighted_shannon_index <- function(data, x_col, y_col, species_col, incline = 1, maxdist = Inf) {
  # Benennung der Spalten
  data <- data %>%
    rename(x = !!sym(x_col), 
           y = !!sym(y_col), 
           species = !!sym(species_col)) %>%
    #erstelle 2 neue spalten
    mutate(Shannon = as.numeric(0),            #shannon index für jeden baum
           t_density = as.numeric(0))          #gewichtete anzahl der nachbarbäume
  
  # Hauptschleife geht durch jeden baum in data
  for (i in seq_len(NROW(data))) {
    # berechnung der distanz zu allen anderen Bäumen
    xref <- data$x - data$x[i]
    yref <- data$y - data$y[i]
    dist_ref <- sqrt(xref^2 + yref^2) # Entfernung durch eukl. distanz
    
    # teilmenge erstellen: nur Bäume innerhalb der maxdist zu baum i
    temp_data <- data %>%
      mutate(dist_ref = dist_ref) %>%
      filter(dist_ref <= maxdist)
    
    # Gberechnung der gewichtung (exponentielle abnahme)
    tree_weight <- temp_data %>%
      group_by(species) %>%                                                    #gruppierung nach spezies
      summarise(weight = sum(exp(-dist_ref * incline)), .groups = "drop")     #Gesamtgewichtung für jede Baumart wird summiert
    
    # Berechnung relative Häufigkeit für jede spezies
    percent <- tree_weight$weight / sum(tree_weight$weight)
    
    # Shannon-Index und Dichte berechnen
    data$t_density[i] <- sum(tree_weight$weight)                               #Summe der Gewichtungen aller Nachbarbäume
    data$Shannon[i] <- exp(-sum(percent * log(percent)))                       #shannon index
  }
  
  return(data)
}


# Funktion anwenden für completed_data
completed_data_with_shannon <- weighted_shannon_index(
  data = completed_data,
  x_col = "x",              # Spalte mit X-Koordinaten
  y_col = "y",              # Spalte mit Y-Koordinaten
  species_col = "species",  # Spalte mit Baumarten
  incline = 1,              # Steigung
  maxdist = 10              # Maximale Distanz in Metern
)

# Resultat prüfen
head(completed_data_with_shannon)

df_cores_with_shannon <- weighted_shannon_index(
  data = df_cores,
  x_col = "x",
  y_col = "y",
  species_col = "species",
  incline = 1,
  maxdist = 10
)

head(df_cores_with_shannon)
#hiermit jetzt shannon und prod. data vergleichen

# =======================================
# lineare regression shannon ~ parameter
# =======
# Ergebnisse:
# shannon ~ d1_mm : wenn man alle spezien betrachtet: keine abhängigkeit zwischen den parametern
# shannon ~ infestation: je höher shannon index, desto geringer die w-keit auf infestation (von 70 zu 50%)
# =======================================




# lineare regression: Modell erstellen für infestation
lm_model <- lm(infestation ~ Shannon, data = completed_data_with_shannon)
summary(lm_model)

#hier zum vergleich noch mal log regression für infestation
log_model <- glm(infestation ~ Shannon, data = completed_data_with_shannon, family = binomial)
summary(log_model)
# shannon koeffizient ist negativ -> je größer shannon index destor kleiner risiko für infestation

# =====================
# Visualisierung
# =====================
library(ggplot2)

# plot mit Regressionslinie
# log regression machen ?``
ggplot(completed_data_with_shannon, aes(x = Shannon, y = infestation)) +
  geom_point(alpha = 0.5, color = "blue") +  # Punkte plotten
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regressionslinie mit Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Shannon-Index und infestation ()",
    x = "Shannon-Index",
    y = "infestation ()"
  ) +
  theme_minimal()


#------- hier visualisierung mit log regression 
# hier erklärung https://www.geeksforgeeks.org/how-to-plot-a-logistic-regression-curve-in-r/
# mit funktion stat_smooth()
# zuerst streudiagramm der ursprünglichen datenpunkte, dann überlagern mit regressionskurve (mithilfe von statt_smooth())
ggplot(completed_data_with_shannon, aes(x = Shannon, y = infestation)) +
  geom_point(alpha = 0.5, color = "blue") +  # Punkte plotten
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +  
  labs(
    title = "Logistische Regression: Shannon-Index vs. Infestation",
    x = "Shannon-Index",
    y = "Wahrscheinlichkeit für Infestation"
  ) +
  theme_minimal()
#-------
help("stat_smooth")

#ich sehe keinen Zusammenhang bei Durchmesser und Shannon
ggplot(completed_data_with_shannon, aes(x = Shannon, y = d1_mm)) +
  geom_point(alpha = 0.5, color = "blue") +  # Punkte plotten
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regressionslinie mit Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Shannon-Index und Durchmesser ()",
    x = "Shannon-Index",
    y = "Durchmesser ()"
  ) +
  theme_minimal()

# Zusammenhang Durchmesser und Shannon Index für jede Species alleine
# bei manchen Spezien sieht man größeren Einfluss des Index auf den Durchmesser als bei anderen
ggplot(completed_data_with_shannon, aes(x = Shannon, y = d1_mm)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~species, scales = "free") +  # Erstellt separate Plots für jede Art
  labs(
    title = "Zusammenhang zwischen Shannon-Index und Durchmesser nach Baumart",
    x = "Shannon-Index",
    y = "Durchmesser (mm)"
  ) +
  theme_minimal()

#untersuche Korrelation von d1 und shannon mit kendalls tau
cor(completed_data_with_shannon$Shannon, completed_data_with_shannon$d1_mm, method="kendall")
# recht kleiner aber negativer wert
# wenn also shannon steigt, dann fällt durchmesser...?
# Berechne Kendall's Tau für jede Spezies
cor_results <- completed_data_with_shannon %>%
  group_by(species) %>%  # Nach species gruppieren
  summarise(correlation = cor(Shannon, d1_mm, method = "kendall", use = "pairwise.complete.obs"))
print(cor_results)

# auch kein wirklicher Zusammenhang bei Höhe und Shannon
ggplot(completed_data_with_shannon, aes(x = Shannon, y = h1_mm)) +
  geom_point(alpha = 0.5, color = "blue") +  # Punkte plotten
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regressionslinie mit Konfidenzintervall
  labs(
    title = "Zusammenhang zwischen Shannon-Index und Höhe ()",
    x = "Shannon-Index",
    y = "Höhe ()"
  ) +
  theme_minimal()

# Zusammenhang Höhe und Shannon Index für jede Species alleine
# bei manchen Spezien sieht man größeren Einfluss des Index auf die Höhe als bei anderen
ggplot(completed_data_with_shannon, aes(x = Shannon, y = h1_mm)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~species, scales = "free") +  # Erstellt separate Plots für jede Art
  labs(
    title = "Zusammenhang zwischen Shannon-Index und Höhe nach Baumart",
    x = "Shannon-Index",
    y = "Höhe (mm)"
  ) +
  theme_minimal()


# räumliche visualisierung des weighted shannon index im koordinatensystem -> heatmap
help(ggplot)
ggplot(completed_data_with_shannon, aes(x = x, y = y, color = Shannon)) +
  geom_point(size = 3, alpha = 0.8) +  # Punkte plotten mit Farbskala
  scale_color_viridis_c(option = "plasma") +  # Farbskala für Shannon-Index
  labs(title = "Shannon-Index Heatmap der Baumpositionen",
       x = "X-Koordinate (m)", y = "Y-Koordinate (m)", color = "Shannon-Index") +
  theme_minimal()


# =================================================================================
# 8. PCA auf completed_data anwenden
# =================================================================================

# ========================
# 8a. Datentypen anpassen
# ========================
#!!! muss daten noch standardisieren

# für pca brauche ich numerische daten
# -> verwende one-hot-encoding methode für "species" und "dead"
# -> "Zusammensetzung" weglassen
# könnte auch label encoding nutzen,a ber denke, das ist unpassend, weil meine daten ja nciht gerankt sind oder so

onehot_species <- model.matrix(~ species - 1, data = completed_data) # model.matrix erzeugt die sog. dummi veriablen, -1 für fehlenden intercept
onehot_dead <- model.matrix(~ dead - 1, data = completed_data)
# extrahiere alle numerischen spalten (dadurch "zusammensetzung automatsich entfenrt)
numerical_data <- completed_data %>% select(where(is.numeric))
pca_data <- cbind(numerical_data, onehot_species, onehot_dead)

# ========================
# 8b. PCA durchführen und auswerten
# ========================
pca_result <- prcomp(pca_data, scale. = TRUE)
summary(pca_result)
# beachte, dass ersten PC nur einen geringen teil der gesamtvarianz beinhalten
# deutet auf komplexe struktur hin
# 12 PCs erklären 77% der Varianz
# 18 PCs erklären 98% der Varianz

install.packages("factoextra")
library(factoextra)
fviz_eig(pca_result, addlabels = TRUE)

biplot(pca_result, main = "PCA-Biplot")
# ist das nen neuer picasso oder was...?!

# analysiere correlation
cor_matrix <- cor(pca_data)
heatmap(cor_matrix)

# could analyse how many PCs I optimally need




