## GR-document variables

# load packages

library(here)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(timetk)
library(paletteer)

# load document variable xlsx

docvars <- readxl::read_xlsx(here("data/coded_segments_2024-11-28.xlsx"),
                             col_types = c(rep("guess", times = 19), 
                                           rep("text", times = 7),
                                           rep("guess", times = 8))) %>% 
  dplyr::rename_with( ~ tolower(gsub(" ", "_", .x))) %>% 
  dplyr::select(!c(farbe, segment, dokumentname...12) & !starts_with("dokumentgruppe")) %>% 
  dplyr::rename(c("datum" = "datum_(veröffentlichung)",
                  "structure_affirming" = "structure_related_ii:_affirming",
                  "structure_failure" = "structure_related_i:_failures",
                  "inequality_and_power" = "inequality_and-or_asymmetry_of_power",
                  "principles_of_democracy" = "priniples_of_democracy",
                  "critique_of_gr" = "efficiency_concerns",
                  "dokumentname" = "dokumentname...3")) %>% 
  dplyr::mutate(erstellt_am = parse_date_time(erstellt_am, orders = c("YmdHMS", "dmYHM")),
                datum = dmy(datum))

sub_docvars <- docvars %>% 
  dplyr::filter(str_detect(code, ">") == T)

themes_docvars <- docvars %>% 
  dplyr::filter(str_detect(code, ">") == F)

gr_events <- tibble::tibble(
  datum = lubridate::ymd(
    c("2024-01-09", "2024-03-16", "2024-04-06", "2024-04-20", "2024-05-04", "2024-05-25", "2024-06-08", "2024-06-18", "2024-09-14")),
  event = c("Projektlaunch", "WE1", "WE2", "WE3", "WE4", "WE5", "WE6", "Ergebnisverkündung", "Abschluss"),
  y.end = rep(0, times = 9),
  y = c(37, 13, 9, 7, 8, 9, 29, 44, 6)
)



# (1) plot media coverage over time ---------------------------------------

# weeks
media_cov <- themes_docvars %>%
  dplyr::select(dokumentname, datum, category) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(category, datum) %>%
  timetk::summarise_by_time(.date_var = datum,
                            .by = "week",
                            .week_start = 1,
                            value = n()) %>%
  dplyr::filter(is.na(datum) == F & datum >= as_date("2023-12-31")) %>% 
  ggplot( mapping = aes(x = datum, y = value, colour = category)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1", 
                     labels = c("Medien", "GR")) +
  scale_x_date(date_breaks = "month", date_labels = "%b") +  
  geom_segment(data = gr_events, 
               mapping = aes(x = datum, y = y, yend = y.end),linewidth = 0.35, inherit.aes = F) +
  geom_label(data = gr_events, aes(x = datum, y = y + 0.5, label = event), 
             inherit.aes = F) + 
  labs(title = "Beiträge über den Guten Rat",
       subtitle = "Medien- und Eigenbeiträge nach Kalenderwochen 2024",
       colour = "Quelle",
       x = "Datum",
       y = "Anzahl") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("coverage_gr_comp_2024-11-28.svg",
       device = "svg",
       width = 14,
       height = 6,
       path = here("output/plots")
)
  

# months
# media_cov_m <- docvars %>% 
#   dplyr::group_by(category, datum) %>% 
#   timetk::summarise_by_time(.date_var = datum,
#                             .by = "month",
#                             value = n()) %>%
#   dplyr::filter(is.na(datum) == F & datum >= as_date("2023-12-31")) %>% 
#   plot_time_series(datum, value, .smooth = F, .title = "Medienbeiträge über den Guten Rat (nach Monaten)")


# (2) reporting style -----------------------------------------------------

repstyle <- docvars %>% 
  dplyr::filter(stringr::str_detect(code, "Reporting|xample") == T) %>% 
  dplyr::mutate(code = code %>% stringr::str_replace_all("Reporting Style > ", "")) %>% 
  dplyr::group_by(code) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::add_row(tibble(code = "Gesamt", n = 172)) %>% 
  dplyr::mutate(prozent = round((n*100)/172, digits = 1),
                code = code %>% 
                  stringr::str_replace_all(c(
                    "cler" = "st",
                    "cal" = "sch",
                    "GR as example" = "Beispiel",
                    " Journalism" = ""
                  ))
                ) %>% 
  dplyr::arrange(desc(n)) %>% 
  ggplot2::ggplot( mapping = aes(x = reorder(code, n, decreasing = T), y = n)) +
  geom_bar( aes(fill = reorder(code, n, decreasing = T)), stat = "identity") +
  geom_text( aes(y = n+5, label = stringr::str_c(n, " (", prozent, "%)")))+
  theme_classic() +
  scale_fill_paletteer_d("LaCroixColoR::Apricot", direction = -1) +
  labs(title = "Art der Reportage über den Guten Rat",
       subtitle = "Anzahl der Artikel nach Reportagestil") +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
        )

ggsave("reportingstyle_2024-11-29.svg",
       device = "svg",
       width = 7,
       height = 5,
       path = here("output/plots")
)


# (3) Themes coded (& weighted) -------------------------------------------

coverage_th <- themes_docvars %>% 
  dplyr::select(dokumentname, category, code, datum, gewicht) %>%
  dplyr::filter(is.na(datum) == F & datum >= as_date("2023-12-31") & stringr::str_detect(code, "example") == F)

# media_themes_month <- coverage_th %>%
#   dplyr::filter(!theme == "reporting_style") %>% 
#   dplyr::group_by(theme) %>% 
#   timetk::summarise_by_time(.date_var = datum,
#                             .by = "month",
#                             themecount = sum(themecount)
#                             ) %>%
#   plot_time_series(datum, themecount, 
#                    .facet_ncol = 2,
#                    .facet_scales = "fixed",
#                    .facet_collapse = T,
#                    .interactive = F, 
#                    .smooth = F,
#                    .title = "Themenabdeckung in Medienberichterstattung über den Guten Rat (nach Monaten)")

media_themes_week <- coverage_th %>%
  dplyr::group_by(code, datum) %>%
  timetk::summarise_by_time(.date_var = datum,
                            .by = "week",
                            .week_start = 1,
                            themecount = sum(themecount)
  ) %>%
  plot_time_series(datum, themecount, 
                   .facet_ncol = 2,
                   .facet_scales = "fixed",
                   .facet_collapse = T,
                   .interactive = F, 
                   .smooth = F,
                   .title = "Themenabdeckung in Medienberichterstattung über den Guten Rat (nach Wochen)")
  
# themes occurring in articles (non-aggregated)

nonaggr_th <- coverage_th %>% 
  dplyr::mutate(themecount = dplyr::case_when(themecount >= 1 ~ 1,
                                              .default = themecount))


nonaggr_themes_month <- nonaggr_th %>%
  dplyr::filter(!theme == "reporting_style") %>% 
  dplyr::group_by(theme) %>% 
  timetk::summarise_by_time(.date_var = datum,
                            .by = "month",
                            themecount = sum(themecount)
  ) %>%
  plot_time_series(datum, themecount, 
                   .facet_ncol = 2,
                   .facet_scales = "fixed",
                   .facet_collapse = T,
                   .interactive = F, 
                   .smooth = F,
                   .title = "Themenvorkommen in Medienbeiträgen über den Guten Rat (nach Artikeln und Monaten)")

nonaggr_themes_week <- nonaggr_th %>%
  dplyr::filter(!theme == "reporting_style") %>% 
  dplyr::group_by(theme) %>% 
  timetk::summarise_by_time(.date_var = datum,
                            .by = "week",
                            .week_start = 1,
                            themecount = sum(themecount)
  ) %>%
  plot_time_series(datum, themecount, 
                   .facet_ncol = 2,
                   .facet_scales = "fixed",
                   .facet_collapse = T,
                   .interactive = F, 
                   .smooth = F,
                   .title = "Themenvorkommen in Medienbeiträgen über den Guten Rat (nach Artikeln und Wochen)")

