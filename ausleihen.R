#-------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------

library(DT)
library(gameofthrones)
library(gt)
library(highcharter)
library(htmlwidgets)
library(janitor)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(readxl)
library(scales)
library(tidyverse)


#-------------------------------------------------------------------
# Load excel files (title master reports)
#-------------------------------------------------------------------


ausleihen_2021 <- read_excel("T:/Statistik/ALMA_ART/Rohdaten_Ausleihen/Ausleihen_nach_Campus_Bibliothek_2021.xlsx",
                                                    col_types = c("text", "numeric", "numeric",
                                                                  "text", "numeric"))

#-------------------------------------------------------------------
# Transform data
#-------------------------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

ausleihen_2021_tidy <- ausleihen_2021 %>%
  mutate(Bibliothek = substrRight(Bibliothek, 3)) %>%
  mutate(Bibliothek = factor(Bibliothek, levels = c("CVK", "CCM", "ZMK"))) %>%
  filter(Monat != "Mai") %>%
  group_by(Bibliothek) %>%
  arrange(Jahr, Monat_Zahl)


#-------------------------------------------------------------------
# Visualize data
#-------------------------------------------------------------------

hc <- ausleihen_2021_tidy %>%
  hchart(
    type = "spline",
    hcaes(x = Monat, y = Ausleihen, group = Bibliothek)
  ) %>%
  hc_title(text = "Ausleihen 01.11.2020 bis 30.04.2021") %>%
  hc_tooltip(shared = TRUE)

#-------------------------------------------------------------------
# Save graph
#-------------------------------------------------------------------

saveWidget(hc, file = "T:/Statistik/ALMA_ART/Auswertung_R/usage-stats2/ausleihen_11_2020_04_2021.html", selfcontained = TRUE)


#-------------------------------------------------------------------
# OA-Dashboard
#-------------------------------------------------------------------


OA_data <- read_excel("T:/OA-Dashboard/Endfassung.xlsx",
                         sheet = "Worksheet")
