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

#curl https://www.corona-kvwl.de/praxisinformationen/corona-schutzimpfung/impfberichte | grep -io 'fileadmin.*\.pdf' > files.txt 

CUR_DATE=$(date +%d_%m_%Y)

curl https://www.corona-kvwl.de/fileadmin/user_upload/pdf/praxisinfos/corona_impfungen/COVID-19_Impfbericht_Praxen_$CUR_DATE.pdf > $DIR/meta_data/Praxen.pdf

curl https://www.corona-kvwl.de/fileadmin/user_upload/pdf/praxisinfos/corona_impfungen/COVID-19_Impfbericht_Impfzentren_$CUR_DATE.pdf > $DIR/meta_data/Impfzentren.pdf

curl https://www.corona-kvwl.de/fileadmin/user_upload/pdf/praxisinfos/corona_impfungen/COVID-19_Impfbericht_mobile_Teams_$CUR_DATE.pdf > $DIR/meta_data/Teams.pdf

convert -density 150 -trim $DIR/meta_data/Praxen.pdf $DIR/meta_data/page%d.png
tesseract -l deu $DIR/meta_data/page2.png $DIR/meta_data/Praxen

convert -density 150 -trim $DIR/meta_data/Impfzentren.pdf $DIR/meta_data/page%d.png
tesseract -l deu $DIR/meta_data/page2.png $DIR/meta_data/Impfzentren

convert -density 150 -trim $DIR/meta_data/Teams.pdf $DIR/meta_data/page%d.png
tesseract -l deu $DIR/meta_data/page0.png $DIR/meta_data/Teams

grep -i "Münster" $DIR/meta_data/*.txt > $DIR/meta_data/vacc_numbers.csv

rm $DIR/meta_data/*.pdf $DIR/meta_data/*.png $DIR/meta_data/*.txt

Rscript $DIR/render.R
