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

list.age_means <- list(
    "A00-A04"=mean(0:4),
    "A05-A14"=mean(5:14),
    "A15-A34"=mean(15:34),
    "A35-A59"=mean(35:59),
    "A60-A79"=mean(60:79),
    "A80+"=mean(80:99)
)

county_list <- stringr::str_split(unlist(lapply(stringr::str_split(list.files("raw_data"), "\\."), function(x) {if (x[2] == "json") {x[1]}})), "_")
county_list <- unique(unlist(lapply(county_list, `[`, 1)))

county_id_list <- c()
population_list <- c()
cases7_per_100k_list <- c()
county_name_list <- c()
last_updated_list <- c()
for (county in county_list) {
    if (county != "RKI") {
        meta_json <- jsonlite::read_json(paste0("raw_data/", county, "_meta.json"))
        population <- meta_json$features[[1]]$attributes$EWZ
        cases7_per_100k <- meta_json$features[[1]]$attributes$cases7_per_100k
        county_name <- meta_json$features[[1]]$attributes$county
        last_updated <- meta_json$features[[1]]$attributes$last_update
        last_updated <- substr(last_updated, 1, 10)
        
        county_id_list <- c(county_id_list, county)
        population_list <- c(population_list, population)
        cases7_per_100k_list <- c(cases7_per_100k_list, cases7_per_100k)
        county_name_list <- c(county_name_list, county_name)
        last_updated_list <- c(last_updated_list, last_updated)
    }
}
df.meta <- data.frame(
    "county_id" = county_id_list,
    "county_name" = county_name_list,
    "population" = population_list,
    "cases7_per_100k" = cases7_per_100k_list,
    "last_updated" = last_updated_list
)

RKI_COVID19 <- vroom::vroom("raw_data/RKI_COVID19.csv")

df <- RKI_COVID19 %>% 
    filter(IdLandkreis %in% df.meta$county_id) %>% 
    mutate(Meldedatum = lubridate::parse_date_time(Meldedatum, "%Y/%m/%d %H:%M:%S")) %>% 
    mutate(Meldedatum_date = lubridate::date(Meldedatum)) %>% 
    filter(NeuerFall >= 0) %>% 
    dplyr::group_by(Meldedatum_date, Landkreis) %>% 
    summarise(
        cases = sum(AnzahlFall),
        deaths = sum(AnzahlTodesfall),
        recovered = sum(AnzahlGenesen),
        .groups = "drop"
    ) %>% 
    rename(county_name = Landkreis) %>% 
    select(
        county_name,
        Meldedatum_date,
        cases,
        deaths,
        recovered
    )

fig.mean_age <- RKI_COVID19 %>% 
    mutate(Meldedatum = lubridate::parse_date_time(Meldedatum, "%Y/%m/%d %H:%M:%S")) %>% 
    mutate(Meldedatum_month = paste(
        lubridate::year(Meldedatum), 
        stringr::str_pad(lubridate::month(Meldedatum), 2, "0", side = "left"),
        sep = "_")) %>% 
    mutate(Meldedatum_month = as.POSIXct(as.Date(lubridate::parse_date_time(Meldedatum_month, "%Y_%m")))) %>%
    filter(NeuerFall >= 0) %>% 
    select(Meldedatum, Altersgruppe, AnzahlFall) %>% 
    filter(Altersgruppe != "unbekannt") %>% 
    slice(rep(1:n(), .$AnzahlFall)) %>%
    mutate(Altersgruppe_num = unlist(lapply(Altersgruppe, function(x) {list.age_means[[x]]}))) %>% 
    select(Meldedatum, Altersgruppe_num) %>% 
    group_by(Meldedatum) %>% 
    summarise(Alter = mean(Altersgruppe_num), n = n()) %>% 
    ungroup() %>% 
    filter(n > 100) %>% 
    ggplot(aes(x = Meldedatum, y = Alter, group = 1)) +
    geom_line() +
    scale_x_datetime() +
    labs(
        title = "Geschätzes Durchschnittsalter der Infizierten über Zeit",
        y = "Alter",
        x = "Datum",
        fill = ""
    ) +
    theme_minimal()

fig.gender <- RKI_COVID19 %>% 
    mutate(Meldedatum = lubridate::parse_date_time(Meldedatum, "%Y/%m/%d %H:%M:%S")) %>% 
    mutate(Meldedatum_week = paste(
        lubridate::year(Meldedatum), 
        stringr::str_pad(lubridate::week(Meldedatum)-1, 2, "0", side = "left"),
        "0",
        sep = "_")) %>% 
    mutate(Meldedatum_week = as.POSIXct(lubridate::parse_date_time(Meldedatum_week, "%Y_%U_%w"))) %>%
    filter(NeuerFall >= 0) %>% 
    select(Meldedatum_week, Geschlecht, AnzahlFall) %>% 
    filter(Geschlecht != "unbekannt") %>% 
    group_by(Meldedatum_week, Geschlecht) %>% 
    summarise(n = sum(AnzahlFall)) %>% 
    ungroup() %>% 
    group_by(Meldedatum_week) %>% 
    mutate(Anteil = n/sum(n), total = sum(n)) %>% 
    ungroup() %>% 
    filter(n > 100) %>% 
    ggplot(aes(x = Meldedatum_week, y = Anteil, color = Geschlecht)) +
    geom_line() +
    scale_x_datetime() +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c("#66a61e", "#7570b3")) +
    labs(
        title = "Verteilung des Geschlechts der Infizierten über Zeit",
        y = "%",
        x = "Datum (Woche)",
        color = ""
    ) +
    theme_minimal()

rm(RKI_COVID19)

df <- left_join(
    data.frame(
        Meldedatum_date = seq(
            min(df$Meldedatum_date), 
            max(df$Meldedatum_date), 
            by="days")) %>% 
        dplyr::slice(rep(1:n(), length(df.meta$county_id))) %>% 
        arrange(Meldedatum_date) %>% 
        mutate(county_name = rep(
            df.meta$county_name, 
            n()/length(df.meta$county_id))), 
    df, 
    by = c("Meldedatum_date", "county_name")) %>% 
    mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>% 
    left_join(df.meta, by = c("county_name")) %>% 
    mutate(population = as.numeric(population)) %>% 
    group_by(county_name) %>% 
    arrange(Meldedatum_date) %>% 
    mutate(i = zoo::rollsum(cases, k = 7, align = "right", fill = 0)) %>% 
    mutate(i = (i/population)*100000) %>% 
    mutate(across(c(cases, deaths, recovered), cumsum, .names = "{col}_kum")) %>% 
    arrange(desc(Meldedatum_date)) %>% 
    ungroup() %>% 
    rename(sieben_tage_inzidenz = i)


df.meta <- df.meta %>% 
    left_join(
        df %>% 
            group_by(county_name) %>% 
            summarise(
                total_cases = sum(cases), 
                total_deceased = sum(deaths), 
                total_recovered = sum(recovered)
            ),
        by = "county_name"
    ) %>% 
    mutate(
        current_cases = total_cases - (total_deceased + total_recovered),
        case_rate_percent = (current_cases/population)*100,
        death_rate_percent = (total_deceased/total_cases)*100
    )

df.gemeldet <- openxlsx::read.xlsx(
    "raw_data/Fallzahlen_Kum_Tab.xlsx",
    #sheetName = "LK_7-Tage-Inzidenz",
    sheet = 7,
    detectDates = FALSE
)
df.gemeldet <- df.gemeldet[2:length(df.gemeldet)]
list.dates <- strptime(df.gemeldet[1,3:length(df.gemeldet)], "%d.%m.%Y")
list.dates <- c(
    as.numeric(list.dates[!is.na(list.dates)]),
    as.numeric(as.POSIXct(as.numeric(df.gemeldet[1,3:length(df.gemeldet)][is.na(list.dates)]) * (60*60*24), origin="1899-12-30", tz="GMT"))
)
list.dates <- c("county_name", "county_id", list.dates)
df.gemeldet <- df.gemeldet[2:nrow(df.gemeldet),]
names(df.gemeldet) <- as.character(list.dates)
df.gemeldet <- df.gemeldet %>% 
    mutate(across(-c(county_name, county_id), as.numeric)) %>% 
    tidyr::pivot_longer(-c(county_name, county_id), names_to = "Meldedatum_date", values_to = "gemeldet") %>% 
    filter(county_id %in% as.character(as.numeric(county_list))) %>% 
    mutate(Meldedatum_date = lubridate::as_date(as.POSIXlt(as.numeric(Meldedatum_date)-(60*60*24), origin="1970-01-01", tz="Europe/Berlin"))) %>% 
    select(-county_id) %>% 
    full_join(
        select(df, county_name, Meldedatum_date, sieben_tage_inzidenz),
        by = c("Meldedatum_date", "county_name")
    ) %>% 
    rename(
        i_calculated = sieben_tage_inzidenz,
        i_reported = gemeldet
    )

f.check_notbremse <- function(x) { 
    if (str_detect(x[2], "1{3}(?!0{5,}|1)")) {
        return(c(x[1], TRUE))
    } else {
        return(c(x[1], FALSE))
    }
}

df.notbremse <- df.gemeldet %>% 
    filter(Meldedatum_date >= "2021-04-21") %>% 
    group_by(county_name) %>% 
    arrange(Meldedatum_date) %>% 
    group_map(~ c(unique(.x$county_name), paste(as.numeric(.x$i_reported > 100), collapse = "")), .keep = T) %>% 
    lapply(., f.check_notbremse) %>% 
    do.call(rbind, .) %>% 
    as.data.frame(.) %>%
    rename(county_name = V1, notbremse = V2) %>%  
    mutate(county_name = stringr::str_replace(county_name, regex(".* "), "")) %>% 
    mutate(notbremse = ifelse(notbremse, "gilt", "gilt nicht"))

df.notbremse <- left_join(
    df.gemeldet %>% 
        arrange(county_name, desc(Meldedatum_date)) %>% 
        mutate(is_over_100 = i_reported > 100) %>% 
        group_by(county_name) %>% 
        group_map(~ c(unique(.x$county_name), match(FALSE, .x$is_over_100)), .keep = TRUE) %>% 
        do.call(rbind, .) %>% 
        as.data.frame(.) %>% 
        rename(reported_over = V2) ,
    df.gemeldet %>% 
        arrange(county_name, desc(Meldedatum_date)) %>% 
        mutate(is_over_100 = i_calculated > 100) %>% 
        group_by(county_name) %>% 
        group_map(~ c(unique(.x$county_name), match(FALSE, .x$is_over_100)), .keep = TRUE) %>% 
        do.call(rbind, .) %>% 
        as.data.frame(.) %>% 
        rename(calculated_over = V2),
    by = "V1"
) %>% left_join(
    (left_join(
        df.gemeldet %>% 
            arrange(county_name, desc(Meldedatum_date)) %>% 
            mutate(is_over_100 = i_reported < 100) %>% 
            group_by(county_name) %>% 
            group_map(~ c(unique(.x$county_name), match(FALSE, .x$is_over_100)), .keep = TRUE) %>% 
            do.call(rbind, .) %>% 
            as.data.frame(.) %>% 
            rename(reported_under = V2),
        df.gemeldet %>% 
            arrange(county_name, desc(Meldedatum_date)) %>% 
            mutate(is_over_100 = i_calculated < 100) %>% 
            group_by(county_name) %>% 
            group_map(~ c(unique(.x$county_name), match(FALSE, .x$is_over_100)), .keep = TRUE) %>% 
            do.call(rbind, .) %>% 
            as.data.frame(.) %>% 
            rename(calculated_under = V2),
        by = "V1")
    ),
    by = "V1") %>% 
    rename(county_name = V1) %>% 
    mutate(across(-county_name, ~ as.numeric(.x)-1)) %>% 
    mutate(county_name = stringr::str_replace(county_name, regex(".* "), "")) %>% 
    left_join(df.notbremse, by = "county_name")

# Figuers ----
fig.inzidenz <- df %>% 
    rename(Datum = Meldedatum_date) %>%
    mutate(county_name = stringr::str_replace(county_name, regex(".* "), "")) %>% 
    ggplot(aes(x = Datum, y = sieben_tage_inzidenz, color = county_name)) +
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

fig.cases <- df %>% 
    filter(county_name == "SK Münster") %>% 
    pivot_longer(ends_with("_kum")) %>% 
    mutate(name = factor(
        name, 
        levels = c("cases_kum", "recovered_kum", "deaths_kum"),
        labels = c("Fälle", "Genesen", "Verstorben")
    )) %>% 
    rename(Datum = Meldedatum_date) %>%
    mutate(county_name = stringr::str_replace(county_name, regex(".* "), "")) %>% 
    ggplot(aes(x = Datum, y = value, fill = name)) +
    geom_area(position = position_dodge(width = 1)) +
    scale_fill_manual(values = c("#e41a1c", "#4daf4a", "black")) +
    labs(
        title = "Kumulierte Fälle in Münster über Zeit",
        y = "Kumulierte Fälle",
        x = "Datum",
        fill = ""
    ) +
    theme_minimal()

fig.notbremse <- df.gemeldet %>% 
    filter(Meldedatum_date >= (as_date(now())-15)) %>% 
    filter(county_name == "SK Münster") %>% 
    #filter(!is.na(i_calculated)) %>% 
    mutate(county_name = stringr::str_replace(county_name, regex(".* "), "")) %>% 
    rename(berechnet = i_calculated, gemeldet = i_reported) %>%
    ggplot() +
    geom_bar(
        aes(fill = gemeldet > 100, y = gemeldet, x = Meldedatum_date),
        stat = "identity"
    ) +
    scale_fill_manual(values = c("#a55757", "#8e0404")) +
    geom_point(
        aes(
            x = Meldedatum_date, 
            y = berechnet,
            fill = berechnet > 100,
            color = berechnet > 100),
        shape = 23
    ) +
    geom_hline(yintercept = 100, color = "black", size = 0.25) +
    scale_color_manual(values = c("#a58484", "#330101")) +
    #facet_grid(vars(county_name)) +
    labs(
        title = "Gemeldete und errechnete 7-Tage-Inzidenz für Münster",
        y = "7-Tage-Inzidenz",
        x = "Datum (Woche)",
        color = "",
        fill = ""
    ) +
    theme_minimal()


saveRDS(fig.notbremse, "data/fig.notbremse.RDS")
saveRDS(fig.gender, "data/fig.gender.RDS")
saveRDS(fig.mean_age, "data/fig.mean_age.RDS")
saveRDS(fig.cases, "data/fig.cases.RDS")
saveRDS(fig.inzidenz, "data/fig.inzidenz.RDS")
saveRDS(df.notbremse, "data/df.notbremse.RDS")
saveRDS(df.meta, "data/df.meta.RDS")
saveRDS(df.gemeldet, "data/df.gemeldet.RDS")

