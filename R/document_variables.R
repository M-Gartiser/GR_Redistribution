## GR-document variables

# load packages

library(here)
library(readxl)
library(tidyverse)



# load document variable xlsx

docvars <- readxl::read_xlsx(here("data/document_variables-2024-11-24.xlsx")) %>% 
  dplyr::rename_with( ~ tolower(gsub(" ", "_", .x))) %>% 
  dplyr::rename(c("datum" = "datum_(veröffentlichung)",
                  "struture_affirming" = "structure_related_ii:_affirming",
                  "structure_failure" = "structure_related_i:_failures",
                  "inequality_and_power" = "inequality_and-or_asymmetry_of_power")) %>% 
  dplyr::mutate(erstellt_am = dmy_hms(erstellt_am),
                datum = dmy_hms(datum))
  

