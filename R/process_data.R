# 1. Load Packages & Custom Functions ----

library(tidyverse)
library(sjlabelled)
library(sjPlot)

add_labels_to_columns <- function(.data, .label_data) {
  for (i in 1:nrow(.label_data)) {
    column_name <- .label_data$id_item[i]
    label <- .label_data$id_name[i]
    .data <- sjlabelled::var_labels(.data, !!sym(column_name) := !!label)
  }
  return(.data)
}

extract_itemtable <- function(.data) {
  out <- .data %>%
    sjPlot::tab_itemscale() %>%
    purrr::pluck("df.list", 1) %>% # extract df.list
    tibble::rownames_to_column(var = "id_name") %>% # add item name as seperate column
    janitor::clean_names() %>% # clean up names (lower case)
    dplyr::inner_join(data_names, ., join_by(id_name))
  return(out)
}

# 2. Process Raw Data ----

inverse_items <- paste0("ub_", c(12, 14, 17, 22, 23, 26, 27, 29))
  
data_raw <- readr::read_csv2("data/raw/data_ub_raw.csv") %>%
  # selection
  select(c(CASE, DD01_01:U29)) %>%
  # renaming colnames
  rename(
    id_case = CASE,
    dd_alter = DD01_01,
    dd_sex = DD02,
    dd_wohn1 = DD03,
    dd_wohn2 = DD04,
    dd_bildung = DD05
  ) %>%
  rename_with(
    .cols = matches(regex("U[0-9]{1}")),
    .fn = ~ str_replace(.x, "U", "ub_") %>% str_replace("ub_([0-9]{1})$", "ub_0\\1")
  ) %>%
  # adequate vector type
  mutate(id_case = as_factor(id_case)) %>%
  mutate(dd_alter = as.integer(dd_alter)) %>%
  # factorize demographic variables
  mutate(dd_sex = factor(
    x = dd_sex, 
    levels = 1:3, 
    labels = c("Männlich", "Weiblich", "Divers"))
    ) %>%
  mutate(dd_wohn1 = factor(
    x = dd_wohn1, 
    levels = 1:4, 
    labels = c("Allein", "In einer WG", "Mit meinen Eltern", "Mit dem/der Partner:in zusammen"))
    ) %>%
  mutate(dd_wohn2 = factor(
    x = dd_wohn2,
    levels = 1:4,
    labels = c("Ländlich", "Eher Ländlich", "Eher Städtisch", "Städtisch"))
    ) %>%
  mutate(dd_bildung = factor(
    x = dd_bildung,
    levels = 1:5,
    labels = c("Hauptschulabschluss", "Realschulabschluss", "Abitur", "Berufsausbildung", "Hochschulabschluss")
  )) %>%
  # re-reverse inverse items (after this step they are again inverse)
  mutate(across(all_of(inverse_items), ~sjmisc::rec(.x, rec = "rev"))) %>%
  mutate(across(starts_with("ub_"), ~.x - 1))

data_names <- read_csv("data/raw/data_ub_names.csv")

data_raw_names <- add_labels_to_columns(data_raw, data_names) 


write_rds(data_raw_names, "data/raw/data_ub_raw.rds")


# Item Data ----


data_item <- data_raw %>% select(starts_with("ub_"))

tab_itemscale(data_item, factor.groups.titles = "Itemanalyse 1")


step1_kick_diff <- extract_itemtable(data_item) %>%
  filter(item_difficulty < .2 | item_difficulty > .8) %>%
  pull(id_item)


data_item_s1 <- data_item_labelled %>%
  select(-all_of(step1_kick_diff))

step2_kick_disc <- data_item_s1 %>%
  extract_itemtable() %>%
  filter(item_discrimination < .3) %>%
  pull(id_item)

data_item_s2 <- data_item_s1 %>%
  select(-all_of(step2_kick_disc))

fa1_results <- data_item_s2 %>%
  sjPlot::tab_fa(
    rotation = "oblimin",
    fctr.load.tlrn = .2,
    nmbr.fctr = 2,
    method = "pa",
    digits = 2,
    wrap.labels = 200
  )


step3_kick_fa <- colnames(data_item_s2[, fa1_results$removed.items])

data_item_s3 <- data_item_s2 %>%
  select(-all_of(step3_kick_fa))

test <- data_item_s3 %>%
  tab_fa(
    nmbr.fctr = 2,
    rotation = "oblimin",
    fctr.load.tlrn = .2,
  )

sjt.itemanalysis(
  df = data_item_s3,
  factor.groups = index,
  factor.groups.titles = c("Faktor 1", "Faktor 2")
)
