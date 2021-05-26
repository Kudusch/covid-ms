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

df.holidays <- read_csv("holidays.csv") %>% 
    mutate(Datum = as_date(strptime(Datum, format = "%Y-%m-%d")))

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
) %>% mutate(county_name = stringr::str_replace(county_name, regex(".* "), ""))

df <- vroom::vroom("raw_data/RKI_COVID19.csv") %>% 
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
    ) %>% mutate(county_name = stringr::str_replace(county_name, regex(".* "), ""))

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

df.gemeldet <- openxlsx::read.xlsx(
    "raw_data/Fallzahlen_Kum_Tab.xlsx",
    #sheetName = "LK_7-Tage-Inzidenz",
    sheet = 6,
    detectDates = FALSE
)
df.gemeldet <- df.gemeldet[2:nrow(df.gemeldet), 2:length(df.gemeldet)]
list.dates <- strptime(df.gemeldet[1,3:length(df.gemeldet)], "%d.%m.%Y")
list.dates <- c(
    as.numeric(list.dates[!is.na(list.dates)]),
    as.numeric(as.POSIXct(as.numeric(df.gemeldet[1,3:length(df.gemeldet)][is.na(list.dates)]) * (60*60*24), origin="1899-12-30", tz="GMT"))
)
list.dates <- c("county_name", "county_id", format(list.dates, scientific = FALSE))
df.gemeldet <- df.gemeldet[2:nrow(df.gemeldet),]
names(df.gemeldet) <- list.dates
df.gemeldet <- df.gemeldet %>% 
    mutate(across(-c(county_name, county_id), as.numeric)) %>% 
    tidyr::pivot_longer(-c(county_name, county_id), names_to = "Meldedatum_date", values_to = "gemeldet") %>% 
    filter(county_id %in% as.character(as.numeric(county_list))) %>% 
    mutate(Meldedatum_date = as_date(as_datetime(as.numeric(Meldedatum_date), tz = "Europe/Berlin"))) %>% 
    mutate(Meldedatum_date = Meldedatum_date-1) %>% 
    mutate(county_name = stringr::str_replace(county_name, regex(".* "), "")) %>% 
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
    z <- str_locate_all(x[2], regex("1{3}(?!1)"))
    y <- substr(x[2], tail(z[[1]], 1)[2]+1, str_length(x[2]))
    y <- str_split(y, "")[[1]]
    
    if (length(z[[1]]) == 0) {
        z <- str_locate(x[2], regex("0*$"))
        y <- substr(x[2], z[1], str_length(x[2]))
        y <- str_split(y, "")[[1]]
        y <- y[(str_split(x[3], "")[[1]][z[1]:str_length(x[2])] == 1)]
        return(c(x[1], FALSE, NA, length(y)))
    }
    y <- y[(str_split(x[3], "")[[1]][(tail(z[[1]], 1)[2]+1):str_length(x[3])] == 1)]
    y <- paste0(y, collapse = "")
    if (str_detect(y, regex("0{5}"))) {
        return(c(x[1], FALSE, NA, str_length(str_extract(y, regex("0*$")))))
    } else {
        return(c(x[1], TRUE, str_locate(x[2], regex("1{3,}"))[1], str_length(str_extract(y, regex("0*$")))))
    }
}

df.notbremse <- df.gemeldet %>% 
    filter(Meldedatum_date >= "2021-04-20") %>% 
    mutate(is_workday = case_when(
        Meldedatum_date %in% df.holidays$Datum ~ FALSE,
        strftime(Meldedatum_date, "%w") == 0 ~ FALSE,
        TRUE ~ TRUE
    )) %>% 
    group_by(county_name) %>% 
    arrange(Meldedatum_date) %>% 
    group_map(~ c(
        unique(.x$county_name), 
        paste(as.numeric(.x$i_reported > 100), collapse = ""),
        paste(as.numeric(.x$is_workday), collapse = "")
        ), .keep = T) %>% 
    lapply(., f.check_notbremse) %>% 
    do.call(rbind, .) %>% 
    as.data.frame(.) %>%
    rename(county_name = V1, notbremse = V2, first_day = V3, days_since = V4) %>%  
    mutate(county_name = stringr::str_replace(county_name, regex(".* "), "")) %>% 
    mutate(first_day = as_date("2021-04-20") + (as.numeric(first_day) + 2)) %>% 
    mutate(days_since = as.numeric(days_since)) %>% 
    mutate(notbremse = ifelse(notbremse, "gilt", "gilt nicht"))
    
fig.notbremse <- df.gemeldet %>% 
    filter(Meldedatum_date >= (as_date(now())-15)) %>% 
    filter(county_name == "Münster") %>% 
    #filter(!is.na(i_calculated)) %>% 
    rename(berechnet = i_calculated, gemeldet = i_reported) %>%
    mutate(day_of_week = strftime(Meldedatum_date, "%A")) %>% 
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
    geom_hline(yintercept = 50, color = "black", size = 0.25) +
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
saveRDS(df.gemeldet, "data/df.gemeldet.RDS")
saveRDS(df.notbremse, "data/df.notbremse.RDS")
