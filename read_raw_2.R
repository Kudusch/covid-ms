file_path <- commandArgs()[4]
file_path <- strsplit(file_path, "=")[[1]][2]
file_path <- dirname(file_path)

message(getwd())
setwd(file_path)
message(getwd())

options(stringsAsFactors=FALSE)
# Read and transform data ----
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(stringr)
library(lubridate)
library(openxlsx)

RKI_COVID19 <- vroom::vroom("raw_data/RKI_COVID19.csv")

list.bundeslaender <- list(
    "Schleswig-Holstein" = 2903773,
    "Hamburg" = 1847253,
    "Niedersachsen" = 7993608,
    "Bremen" = 681202,
    "Nordrhein-Westfalen" = 17947221,
    "Hessen" = 6288080,
    "Rheinland-Pfalz" = 4093903,
    "Baden-Württemberg" = 11100394,
    "Bayern" = 13124737,
    "Saarland" = 986887,
    "Berlin" = 3669491,          
    "Brandenburg" = 2521893,
    "Mecklenburg-Vorpommern" = 1608138,
    "Sachsen" = 4071971,       
    "Sachsen-Anhalt" = 2194782,
    "Thüringen" = 2133378
)

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

df.states <- RKI_COVID19 %>% 
    mutate(Meldedatum = lubridate::parse_date_time(Meldedatum, "%Y/%m/%d %H:%M:%S")) %>% 
    mutate(Meldedatum_date = lubridate::date(Meldedatum)) %>% 
    filter(NeuerFall >= 0) %>% 
    dplyr::group_by(Meldedatum_date, Bundesland) %>% 
    summarise(
        cases = sum(AnzahlFall),
        deaths = sum(AnzahlTodesfall),
        recovered = sum(AnzahlGenesen),
        .groups = "drop"
    ) %>% 
    select(
        Bundesland,
        Meldedatum_date,
        cases,
        deaths,
        recovered
    )

df.states <- left_join(
    data.frame(
        Meldedatum_date = seq(
            min(df.states$Meldedatum_date), 
            max(df.states$Meldedatum_date), 
            by="days")) %>% 
        dplyr::slice(rep(1:n(), length(list.bundeslaender))) %>% 
        arrange(Meldedatum_date) %>% 
        mutate(Bundesland = rep(
            names(list.bundeslaender), 
            n()/length(list.bundeslaender))),
    df.states, 
    by = c("Meldedatum_date", "Bundesland")) %>%
    mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>% 
    mutate(population = unlist(lapply(Bundesland, function(x) {list.bundeslaender[[x]]}))) %>% 
    group_by(Bundesland) %>% 
    arrange(Meldedatum_date) %>% 
    mutate(i = zoo::rollsum(cases, k = 7, align = "right", fill = 0)) %>% 
    mutate(i = (i/population)*100000) %>% 
    mutate(across(c(cases, deaths, recovered), cumsum, .names = "{col}_kum")) %>% 
    arrange(desc(Meldedatum_date)) %>% 
    ungroup() %>% 
    rename(sieben_tage_inzidenz = i)

rm(RKI_COVID19)

fig.inzidenz_nation_wide <- rbind(
    df.states %>% select(Meldedatum_date, sieben_tage_inzidenz, Bundesland),
    df.nation_wide %>% select(Meldedatum_date, sieben_tage_inzidenz) %>% mutate(Bundesland = "Gesamt")
) %>% 
    rename(Datum = Meldedatum_date) %>%
    mutate(Bundesland = factor(Bundesland, levels = c("Gesamt", sort(names(list.bundeslaender))))) %>% 
    rename(Gruppe = Bundesland) %>% 
    ggplot(aes(
        x = Datum, 
        y = sieben_tage_inzidenz, 
        color = Gruppe, 
        group = Gruppe, 
        text = paste(Gruppe, format(round(sieben_tage_inzidenz, 2), nsmall = 2), sep = ": ")
    )) +
    scale_color_manual(values = c(
        "#000000",
        "#1B9E77",
        "#D95F02",
        "#7570B3",
        "#E7298A",
        "#66A61E",
        "#E6AB02",
        "#A6761D",
        "#666666",
        "#66C2A5",
        "#FC8D62",
        "#8DA0CB",
        "#E78AC3",
        "#A6D854",
        "#FFD92F",
        "#E5C494",
        "#B3B3B3"
    )) +
    geom_hline(yintercept = 100, color = "#bf0000", size = 0.25) +
    geom_hline(yintercept = 50, color = "#bf0000", size = 0.25) +
    geom_hline(yintercept = 35, color = "#bf0000", size = 0.25) +
    geom_line() +
    labs(
        title = "7-Tage-Inzidenz über Zeit",
        y = "7-Tage-Inzidenz",
        x = "Datum",
        color = ""
    ) +
    theme_minimal()

saveRDS(fig.inzidenz_nation_wide, "data/fig.inzidenz_nation_wide.RDS")

fig.deaths <- df.nation_wide %>% 
    rename(Datum = Meldedatum_date, Todesfälle = deaths_kum) %>%
    select(Datum, Todesfälle) %>% 
    ggplot(aes(
        x = Datum, 
        y = Todesfälle
    )) +
    geom_area() +
    scale_y_continuous(labels = number) +
    labs(
        title = "Kummulierte Todesfälle über Zeit",
        y = "Kummulierte Todesfälle",
        x = "Datum"
    ) +
    theme_minimal()

saveRDS(fig.deaths, "data/fig.deaths.RDS")

fig.cases_kum_over_time <- df.states %>% 
    rename(Datum = Meldedatum_date) %>%
    select(Datum, Bundesland, cases_kum) %>% 
    ggplot(aes(x = Datum, y = cases_kum, fill = Bundesland)) +
    scale_y_continuous(labels = number) +
    geom_area(position = position_stack(), color = "black") +
    labs(
        title = "Kummulierte Fälle über Zeit",
        y = "Kummulierte Infektionen",
        x = "Datum",
        fill = "Bundesland"
    ) +
    theme_minimal()

fig.cases_kum_over_time <- df.states %>% 
    rename(Datum = Meldedatum_date) %>%
    select(Datum, Bundesland, cases_kum) %>% 
    plot_ly(
        ., 
        x = ~Datum, 
        y = ~cases_kum, 
        type = 'scatter', 
        mode = 'none', 
        stackgroup = 'one', 
        split = ~Bundesland
    ) %>% 
    plotly::config(displayModeBar = FALSE) %>% 
    layout(
        title = "Kummulierte Fälle über Zeit",
        xaxis = list(title = "Datum"), 
        yaxis = list(title = "Kummulierte Infektionen"), 
        legend = list(title=list(text="Bundesland"))
    )

saveRDS(fig.cases_kum_over_time, "data/fig.cases_kum_over_time.RDS")