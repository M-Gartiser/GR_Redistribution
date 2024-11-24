## GR-document variables

# load packages

library(here)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(timetk)


# load document variable xlsx

docvars <- readxl::read_xlsx(here("data/document_variables-2024-11-24.xlsx")) %>% 
  dplyr::rename_with( ~ tolower(gsub(" ", "_", .x))) %>% 
  dplyr::rename(c("datum" = "datum_(veröffentlichung)",
                  "structure_affirming" = "structure_related_ii:_affirming",
                  "structure_failure" = "structure_related_i:_failures",
                  "inequality_and_power" = "inequality_and-or_asymmetry_of_power",
                  "principles_of_democracy" = "priniples_of_democracy")) %>% 
  dplyr::mutate(erstellt_am = dmy_hms(erstellt_am),
                datum = dmy_hms(datum))


# plot media coverage over time

# weeks
media_cov <- docvars %>% 
  dplyr::group_by(category, datum) %>% 
  timetk::summarise_by_time(.date_var = datum,
                            .by = "week",
                            .week_start = 1,
                            value = n()) %>%
  dplyr::filter(category == "Media" & is.na(datum) == F & datum >= as_date("2023-12-31")) %>% 
  plot_time_series(datum, value, .smooth = F, .title = "Medienbeiträge über den Guten Rat (nach Wochen)")
  

# months
media_cov_m <- docvars %>% 
  dplyr::group_by(category, datum) %>% 
  timetk::summarise_by_time(.date_var = datum,
                            .by = "month",
                            value = n()) %>%
  dplyr::filter(category == "Media" & is.na(datum) == F & datum >= as_date("2023-12-31")) %>% 
  plot_time_series(datum, value, .smooth = F, .title = "Medienbeiträge über den Guten Rat (nach Monaten)")
  
# themes

coverage_th <- docvars %>% 
  dplyr::select(category, datum, structure_affirming, structure_failure, understructure, inequality_and_power,
                efficiency_concerns, principles_of_democracy, reporting_style, gr_as_example)

media_themes_time <- docvars %>%
  dplyr::group_by(category) %>% 
  timetk::summarise_by_time(.date_var = datum,
                            .by = "month",
                            structure_affirming = n(structure_affirming),
                            structure_failure = n(structure_failure)) %>%
  dplyr::filter(category == "Media" & is.na(datum) == F & datum >= as_date("2023-12-31")) #%>% 
  plot_time_series(datum, value, .smooth = F, .title = "Themenabdeckung in Medienberichterstattung über den Guten Rat (nach Monaten)")
  
  
  

