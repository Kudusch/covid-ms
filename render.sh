#!/bin/bash

curl -L https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data > /var/www/html/projects/covid-ms/meta_data/RKI_COVID19.csv
python3 /var/www/html/projects/covid-ms/get_data.py '14713'
python3 /var/www/html/projects/covid-ms/get_data.py '05515'
python3 /var/www/html/projects/covid-ms/get_data.py '05913'

Rscript /var/www/html/projects/covid-ms/render.R
