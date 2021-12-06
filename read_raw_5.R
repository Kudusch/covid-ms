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

RKI_Hosp_adj <- vroom::vroom("raw_data/RKI_Hosp_adj.csv")

fig.hosp <- RKI_Hosp_adj %>%
	rename(
    	Hospitalisierungsrate_fix = "fixierte_7T_Hospitalisierung_Inzidenz",
    	Hospitalisierungsrate_akt = "aktualisierte_7T_Hospitalisierung_Inzidenz",
    	Korrigierte_Hospitalisierungsrate = PS_adjustierte_7T_Hospitalisierung_Inzidenz,
    	upper = OG_PI_adjustierte_7T_Hospitalisierung_Inzidenz,
    	lower = UG_PI_adjustierte_7T_Hospitalisierung_Inzidenz
  	) %>% 
  	select(Datum, Bundesland, Hospitalisierungsrate_fix, Hospitalisierungsrate_akt, Korrigierte_Hospitalisierungsrate, upper, lower) %>% 
    filter(Bundesland == "Bundesgebiet") %>% 
  	ggplot(aes(x = Datum)) +
  	geom_ribbon(
    	aes(ymin = lower, ymax = upper),
        fill = "lightblue",
        color = "lightblue"
    ) +
  	geom_line(aes(y = Korrigierte_Hospitalisierungsrate), color = "blue", linetype = "dotted") +
    #geom_line(aes(y = Hospitalisierungsrate_fix), color = "black") +
  	geom_line(aes(y = Hospitalisierungsrate_akt), color = "black") +
	labs(
		title = "7-Tage-Inzidenz der hospitalisierten COVID-19-Fälle",
		subtitle = "Punktschätzer und 95%-Prädiktionsintervall der adjustierten 7-Tage-Hospitalisierungsinzidenz in blau",
		y = "Hospitalisierungsrate",
		x = "Datum"	
	) +
	theme_minimal()

saveRDS(fig.hosp, "data/fig.hosp.RDS")
