# ——————————————————————————————————————————————————————————————————————————————————————————————————
# Kurs: PsyBSc8 Grundlagen der Diagnostik — Testtheorie & Testkonstruktion
# Skript: Itemanalyse
# Datum: 15.11.2023
# Autor: Luca Schnatz
# ——————————————————————————————————————————————————————————————————————————————————————————————————

# 1. Pakete & Daten laden ——————————————————————————————————————————————————————————————————————————————

library(dplyr) # Datenmanipulation
library(here) # Bei Nutzung von R-Projects zur Nutzung von relativen Pfade
library(sjPlot) # Itemanalyse in HTML Tabellen
library(sjmisc) # Rekodierung inverser Items
library(skimr) # Überblick über Dataframes
library(janitor) # Deskriptive Statistik zu kategoriellen Daten
library(psych) # Berechnung der Reliabilität (Omega) und deskriptive Statistik

data_raw <- readRDS(here("data/raw/data_ub_raw.rds"))
data_item <- select(data_raw, starts_with("ub_"))

# 2. Übersicht über Datensatz ——————————————————————————————————————————————————————————————————————————


skimr::skim(data_raw)
str(data_raw)
glimpse(data_raw)

# 3. Rekodierung inverser Items ————————————————————————————————————————————————————————————————————————

inverse_items <- paste0("ub_", c(12, 14, 17, 22, 23, 26, 27, 29))

data_item_rec <- data_item %>%
  mutate(across(
    .cols = all_of(inverse_items),
    .fns = ~sjmisc::rec(.x, rec = "rev"),
    .names = "{.col}_r"
  )) %>%
  select(-all_of(inverse_items))

col_order <- sort(colnames(data_item_rec))
data_item_rec <- select(data_item_rec, all_of(col_order))

# Fehlende Werte

anyNA(data_item_rec)
sum(is.na(data_item_rec))

data_item_rec <- na.omit(data_item_rec)

# 4. Deskriptive Analyse  —————————————————————————————————————————————————————————————————————————————— 

## Ganzer Dataframe

descr_item <- psych::describe(data_item_rec)

## Eine kontinuierliche Variable

descr_age <- psych::describe(data_raw$dd_alter)

## Kategorielle Variables

tabyl(data_raw$dd_sex)

## Kombination kategorielle & kontinuierliche Variablen

descr_age_sex <- describeBy(data_raw$dd_alter, droplevels(data_raw$dd_sex))

tab_dfs(descr_age_sex)

# 5. Itemanalyse  ——————————————————————————————————————————————————————————————————————————————————————

item_analysis_1 <- tab_itemscale(data_item_rec)

extract_itemtable <- function(.data) {
  tab <- sjPlot::tab_itemscale(.data)$df.list[[1]] 
  # clean_names ändert die Spaltennamen in lowercase und tauscht Leerzeichen mit _
  out <- janitor::clean_names(cbind(id_item = rownames(tab), tab))
  return(out)
}

## 5.1 Schwierigkeit

step1_kick_diff <- extract_itemtable(data_item_rec) %>%
  filter(item_difficulty < .2 | item_difficulty > .8) %>%
  pull(id_item)

data_item_s1 <- select(data_item_rec, -all_of(step1_kick_diff))

tab_itemscale(data_item_s1)

## 5.2 Trennschärfe

step2_kick_disc <- extract_itemtable(data_item_s1) %>%
  dplyr::filter(item_discrimination < .3) %>%
  pull(id_item)

data_item_s2 <- select(data_item_s1, -all_of(step2_kick_disc))

tab_itemscale(data_item_s2)

## 5.3 Varianz & Alternative Berechnung der Reliabilität

diag(var(data_item_rec))

rel_omega <- psych::omega(data_item_s2, plot = FALSE)

rel_omega$omega.tot


write.csv(data_item_s2, file = "data/processed/data_item_itemanalysis.csv")

# 6. Session Info  —————————————————————————————————————————————————————————————————————————————————————

sessioninfo::session_info(pkgs = "attached")
