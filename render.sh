#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

mkdir -p $DIR/meta_data

curl -L https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data > $DIR/meta_data/RKI_COVID19.csv

curl https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile > $DIR/meta_data/Fallzahlen_Kum_Tab.xlsx

python3 $DIR/get_data.py '14713'
python3 $DIR/get_data.py '05515'
python3 $DIR/get_data.py '05913'
python3 $DIR/get_data.py '03403'
python3 $DIR/get_data.py '05334'
python3 $DIR/get_data.py '05978'
python3 $DIR/get_data.py '05513'

Rscript $DIR/render.R
