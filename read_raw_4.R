file_path <- commandArgs()[4]
file_path <- strsplit(file_path, "=")[[1]][2]
file_path <- dirname(file_path)

message(getwd())
setwd(file_path)

message(getwd())

options(stringsAsFactors=FALSE)

library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

RKI_COVID19 <- vroom::vroom("raw_data/RKI_COVID19.csv")

df.nation_wide <- RKI_COVID19 %>%
    mutate(Meldedatum = lubridate::parse_date_time(Meldedatum, "%Y/%m/%d %H:%M:%S")) %>% 
    mutate(Meldedatum_date = lubridate::date(Meldedatum)) %>% 
    filter(NeuerFall >= 0) %>% 
    dplyr::group_by(Meldedatum_date) %>% 
    summarise(
        cases = sum(AnzahlFall),
        deaths = sum(AnzahlTodesfall),
        recovered = sum(AnzahlGenesen),
        .groups = "drop"
    ) %>% 
    select(
        Meldedatum_date,
        cases,
        deaths,
        recovered
    )

df.nation_wide <- left_join(
    data.frame(
        Meldedatum_date = seq(
            min(df.nation_wide$Meldedatum_date), 
            max(df.nation_wide$Meldedatum_date), 
            by="days")) %>% 
        arrange(Meldedatum_date),
    df.nation_wide, 
    by = c("Meldedatum_date")) %>% 
    mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>% 
    mutate(population = 83166711) %>% 
    arrange(Meldedatum_date) %>% 
    mutate(i = zoo::rollsum(cases, k = 7, align = "right", fill = 0)) %>% 
    mutate(i = (i/population)*100000) %>% 
    mutate(across(c(cases, deaths, recovered), cumsum, .names = "{col}_kum")) %>% 
    arrange(desc(Meldedatum_date)) %>% 
    rename(sieben_tage_inzidenz = i)

df.city_wide <- RKI_COVID19 %>%
    filter(IdLandkreis == "05515") %>% 
    mutate(Meldedatum = lubridate::parse_date_time(Meldedatum, "%Y/%m/%d %H:%M:%S")) %>% 
    mutate(Meldedatum_date = lubridate::date(Meldedatum)) %>% 
    filter(NeuerFall >= 0) %>% 
    dplyr::group_by(Meldedatum_date) %>% 
    summarise(
        cases = sum(AnzahlFall),
        deaths = sum(AnzahlTodesfall),
        recovered = sum(AnzahlGenesen),
        .groups = "drop"
    ) %>% 
    select(
        Meldedatum_date,
        cases,
        deaths,
        recovered
    )

df.city_wide <- left_join(
    data.frame(
        Meldedatum_date = seq(
            min(df.city_wide$Meldedatum_date), 
            max(df.city_wide$Meldedatum_date), 
            by="days")) %>% 
        arrange(Meldedatum_date),
    df.city_wide, 
    by = c("Meldedatum_date")) %>% 
    mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>% 
    mutate(population = 316403) %>% 
    arrange(Meldedatum_date) %>% 
    mutate(i = zoo::rollsum(cases, k = 7, align = "right", fill = 0)) %>% 
    mutate(i = (i/population)*100000) %>% 
    mutate(across(c(cases, deaths, recovered), cumsum, .names = "{col}_kum")) %>% 
    arrange(desc(Meldedatum_date)) %>% 
    rename(sieben_tage_inzidenz = i)

df.inzidenz_widget <- df.nation_wide %>% 
    select(Meldedatum_date, sieben_tage_inzidenz) %>% 
    left_join(
        df.city_wide %>% select(Meldedatum_date, sieben_tage_inzidenz), 
        by = "Meldedatum_date",
        suffix = c("_nation", "_local")
    )

fig.inzidenz_widget <- df.inzidenz_widget %>% 
    rename(Datum = Meldedatum_date) %>%
    arrange(desc(Datum)) %>% 
    slice_head(n = 21) %>% 
    mutate(id = 1:n()) %>% 
    mutate(text_local = round(sieben_tage_inzidenz_local, digits = 0)) %>% 
    mutate(text_nation = round(sieben_tage_inzidenz_nation, digits = 0)) %>% 
    mutate(text_local = ifelse(id == min(which(!is.na(.$text_local))), text_local, NA)) %>% 
    mutate(text_nation = ifelse(id == min(which(!is.na(.$text_nation))), text_nation, NA)) %>% 
    ggplot(aes(x = Datum)) +
    geom_line(aes(y = sieben_tage_inzidenz_nation), color = "darkblue", size = 1.5) +
    geom_line(aes(y = sieben_tage_inzidenz_local), color = "blue", size = 1.5) +
    geom_text(
        aes(y = sieben_tage_inzidenz_nation+20, label = text_nation),
        hjust = 1,
        color = "white",
        size = 3
    ) +
    geom_text(
        aes(y = sieben_tage_inzidenz_local+20, label = text_local),
        hjust = 1,
        color = "white",
        size = 3
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%e. %b") +
	theme(
        plot.background = element_rect(fill = "#222222", color = "#222222"),
        panel.background = element_rect(fill = "#222222", color = "#222222"),
        strip.background = element_rect(fill = "#222222", color = "#222222"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "white"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5)
    )

ggsave("data/widget.png", fig.inzidenz_widget, scale = 1.2, height = 30, width = 30, units = "mm")
