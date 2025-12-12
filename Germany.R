library(tidyverse)

# # participants (218 Dozierende und 1.846 Geförderte) in Studienstiftung academies in 2024
# # source: Jahresbericht der Studienstiftung 2024
# participants <- 2064

# # share of participants following average German diet
# # source: own estimation
# conversion_factor <- 0.5

# # mean duration of Studienstiftung academies in days
# # source: own estimation
# duration <- 10

days_average_German <- 13120

days_plant_based <- 16799.5

days_planetary_health <- 3679.5

# life-cycle greenhouse gas emissions embedded in German food consumption (tons of CO2 equivalent)
# source: Eberle & Mumm (2024)
emissions <- 216*(10^6) 

# carbon footprint of diets relative to status quo in Germany
# plant-based versus planetary health
# source: Eberle & Mumm (2024)
planetary_health_decarbonization_factor <- 0.53
plant_based_decarbonization_factor <- 0.52

# German population at the end of 2024
# source: Statistisches Bundesamt
population <- 83.6*(10^6)

# average weight of finfish caught in wild, 2000-2019 (kg)
# source: own calculation based on Mood & Brooke (2024)
fish_weight <- 0.0545

# average consumption by meat type in Germany 2024
# source: Bundesinformationszentrum Landwirtschaft: Versorungsbilanz Fleisch, https://www.ble.de/DE/BZL/Daten-Berichte/Fleisch/fleisch_node.html
# source: Bundesministerium für Landwirtschaft, Ernährung und Heimat: Versorgunsbilanz Fisch, https://www.bmel-statistik.de/ernaehrung/versorgungsbilanzen/fisch
Konsum <- tibble(
  Tierart = c(
    "Schwein",
    "Rind+Kalb",
    "Huhn",
    "Truthuhn",
    "Gans",
    "Ente",
    "Schaf+Ziege",
    "Fisch"
  ),
  Konsum = c(
    28.4,
    9.3,
    10.2,
    2.9,
    0.1,
    0.3,
    0.5,
    12
  )
)

# total weight and total number of slaughtered mammals in Germany 2024 by type
# Statistisches Bundesamt: Schlachtungs- und Schlachtgewichtsstatistik
# https://www-genesis.destatis.de/datenbank/online/statistic/41331/details

slaughter_statistics <- read_delim(
  "Germany-slaughter/41331-0001_de_flat.csv",
  delim = ";"
  )

# total weight and total number of slaughtered poultry in Germany 2024 by type
# Statistisches Bundesamt: Geflügelschlachtereien, Geschlachtete Tiere, Schlachtmenge
# https://www-genesis.destatis.de/genesis/online?sequenz=tabelleErgebnis&&selectionname=41322-0001
slaughter_statistics_poultry <- read_delim(
  "Germany-slaughter/41322-0001_de_flat.csv",
  delim = ";"
)


Rind_Kalb <- c(
  "Bullen",
  "Jungrinder",
  "Ochsen",
  "Rinder",
  "Kälber",
  "Weibliche Rinder"
)

Schaf_Ziege <- c(
  "Schafe",
  "Ziegen",
  "Lämmer"
)

Huhn <- c(
  "Suppenhühner",
  "Jungmasthühner"
)

Säugetier <- c(
  "Rind+Kalb",
  "Schaf+Ziege",
  "Schwein"
)

slaughter_statistics_poultry <- slaughter_statistics_poultry %>% 
  filter(value_variable_label != "Geflügelschlachtereien")

slaughter_statistics <- slaughter_statistics %>% 
  full_join(slaughter_statistics_poultry) %>% 
  rename(
    Jahr = time,
    Tierart = `2_variable_attribute_label`,
    Schlachtungsort = `3_variable_attribute_label`
    ) %>%
  pivot_wider(
    id_cols = c(Jahr, Tierart, Schlachtungsort),
    names_from = value_variable_label,
    values_from = value
  ) %>% 
  mutate(
    Schlachtmenge = na_if(Schlachtmenge, "-"),
    Schlachtmenge = na_if(Schlachtmenge, "x"),
    Schlachtmenge = as.numeric(Schlachtmenge),
    `Geschlachtete Tiere` = na_if(`Geschlachtete Tiere`, "-"),
    `Geschlachtete Tiere` = na_if(`Geschlachtete Tiere`, "x"),
    `Geschlachtete Tiere` = as.numeric(`Geschlachtete Tiere`)
    ) %>% 
  filter(
    Jahr == 2024 & !is.na(Schlachtmenge) & !is.na(`Geschlachtete Tiere`)
    ) %>% 
  mutate(
    Tierart = case_when(
      Tierart %in% Rind_Kalb ~ "Rind+Kalb",
      Tierart %in% Schaf_Ziege ~ "Schaf+Ziege",
      Tierart == "Schweine" ~ "Schwein",
      Tierart == "Truthühner" ~ "Truthuhn",
      Tierart %in% Huhn ~ "Huhn",
      Tierart == "Gänse" ~ "Gans",
      Tierart == "Enten" ~ "Ente"
    )
  ) %>%
  filter(!is.na(Tierart)) %>% 
  group_by(Tierart) %>% 
  summarize(across(c(Schlachtmenge, `Geschlachtete Tiere`), sum)) %>% 
  mutate(
    Gewicht = Schlachtmenge/`Geschlachtete Tiere`,
    Gewicht = if_else(
      Tierart %in% Säugetier, 
      Gewicht * 1000,
      Gewicht
    )
    )

data <- slaughter_statistics %>% 
  full_join(Konsum) %>% 
  mutate(
    Gewicht = if_else(Tierart == "Fisch", fish_weight, Gewicht),
    Anzahl = (Konsum/Gewicht)*(days_average_German/365)
    ) %>% 
  arrange(desc(Anzahl))

co2_equivalent <-
  (days_average_German/365)*(emissions/population)*plant_based_decarbonization_factor
