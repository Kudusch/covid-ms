#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

curl -L https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data > $DIR/meta_data/RKI_COVID19.csv

python3 $DIR/get_data.py '14713'
python3 $DIR/get_data.py '05515'
python3 $DIR/get_data.py '05913'
python3 $DIR/get_data.py '03403'
python3 $DIR/get_data.py '05334'
python3 $DIR/get_data.py '05978'


Rscript $DIR/render.R
