library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(jsonlite)
library(httr)

return_json <- content(
        httr::GET(
            "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_Landkreisdaten/FeatureServer/0/query?where=AGS%20%3D%20'05515'&outFields=death_rate,cases,deaths,cases_per_100k,cases_per_population,county,last_update,cases7_per_100k,recovered,BL,EWZ&returnGeometry=false&outSR=4326&f=json",
        accept_json()
    ),
    type="application/json"
)
population <- return_json$features[[1]]$attributes$EWZ
cases7_per_100k <- return_json$features[[1]]$attributes$cases7_per_100k
last_updated <- return_json$features[[1]]$attributes$last_update
last_updated <- substr(last_updated, 1, 10)

return_json <- httr::GET(
    "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=IdLandkreis%20%3D%20'05515'&outFields=Landkreis,AnzahlFall,AnzahlTodesfall,IdLandkreis,NeuerFall,NeuerTodesfall,Refdatum,NeuGenesen,AnzahlGenesen,Meldedatum,Datenstand&returnGeometry=false&outSR=4326&f=json",
    accept_json()
    )
return_json <- content(return_json, type="application/json")

df <- lapply(return_json$features, function(x) {
    return(c(x$attributes$AnzahlFall, x$attributes$Meldedatum))
    })
df <- do.call(rbind, df)
df <- as.data.frame(df)
names(df) <- c("n", "date")
df$date <- lubridate::as_datetime(as.numeric(df$date/1000))

df <- df %>% 
    group_by(date) %>% 
    summarise(n = sum(n)) %>% 
    arrange(date) %>% 
    mutate(i = zoo::rollsum(n, k = 7, align = "right", fill = NA)) %>% 
    mutate(i = (i/population)*100000)

fig <- df %>% 
    filter(!is.na(i)) %>% 
    ggplot(aes(x = date, y = i)) +
    geom_hline(yintercept = 50, color = "#bf0000", size = 0.25) +
    geom_hline(yintercept = 35, color = "#e80000", size = 0.25) +
    geom_line() +
    labs(
        title = paste0("7-Tage-Inzidenz für Münster (Stand am ", last_updated, ": ", round(cases7_per_100k, 1), ")"),
        y = "7-Tage-Inzidenz",
        x = "Datum"
    ) +
    theme_minimal()

htmlwidgets::saveWidget(
    ggplotly(fig, tooltip = "y", dynamicTicks = TRUE), 
    file = "/var/www/html/projects/covid-ms/plot.html",
    selfcontained = FALSE
    )
