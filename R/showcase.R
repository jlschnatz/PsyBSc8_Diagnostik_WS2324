# ——————————————————————————————————————————————————————————————————————————————————————————————————
# Kurs: PsyBSc8 Grundlagen der Diagnostik
# Skript: Itemanalyse
# Datum: 15.11.2023
# Autor: Luca Schnatz
# ——————————————————————————————————————————————————————————————————————————————————————————————————

# Pakete & Daten laden ——————————————————————————————————————————————————————————————————————————————
library(dplyr) # Datenmanipulation
library(sjPlot) # Itemanalyse in HTML Tabellen
library(sjmisc) # Rekodierung inverser Items
library(skimr) # Überblick über Dataframes
library(janitor) # Deskriptive Statistik zu kategoriellen Daten
library(psych) # Berechnung der Reliabilität (Omega) und deskriptive Statistik

# Übersicht über Datensatz ——————————————————————————————————————————————————————————————————————————

# Rekodierung inverser Items ————————————————————————————————————————————————————————————————————————

# Deskriptive Analyse  —————————————————————————————————————————————————————————————————————————————— 

# Itemanalyse  ——————————————————————————————————————————————————————————————————————————————————————

## 1. Schwierigkeit
## 2. Trennschärfe
## 3. Varianz & Alternative Berechnung der Reliabilität

# Session Info  —————————————————————————————————————————————————————————————————————————————————————

sessioninfo::session_info(pkgs = "attached")
