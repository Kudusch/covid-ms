#!/bin/zsh

# Set working directory
DIR=${BASH_SOURCE:-$0}
DIR="$( cd "$( dirname "$DIR" )" &> /dev/null && pwd )"

mkdir -p $DIR/meta_data

# Download RKI-Data
curl -L https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data > $DIR/meta_data/RKI_COVID19.csv

# Download daily reported data
curl "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile" > $DIR/meta_data/Fallzahlen_Kum_Tab.xlsx

# Download metadata
python3 $DIR/get_data.py '14713'
python3 $DIR/get_data.py '05515'
python3 $DIR/get_data.py '05913'
python3 $DIR/get_data.py '03403'
python3 $DIR/get_data.py '05334'
python3 $DIR/get_data.py '05978'
python3 $DIR/get_data.py '05513'

# Download and extract vaccination numbers
curl https://www.corona-kvwl.de/praxisinformationen/corona-schutzimpfung/impfberichte | grep -io 'fileadmin.*\.pdf' > $DIR/meta_data/files.txt
while read L; do; echo $L; wget -q -P $DIR/meta_data/ "https://www.corona-kvwl.de/"$L;  done < $DIR/meta_data/files.txt 
i=1
for f in $DIR/meta_data/*.pdf; do; echo "$DIR/meta_data/$i_%04d.png"; convert -density 150 -trim $f $DIR/meta_data/%04d_$i.png; i=$((i+1)); done
for f in $DIR/meta_data/*.png; do; echo $f; tesseract -l deu $f $f ; done 
grep -i "Münster" $DIR/meta_data/*.txt > $DIR/meta_data/vacc_numbers.csv

# Remove tmp data
rm $DIR/meta_data/*.pdf $DIR/meta_data/*.png $DIR/meta_data/*.txt

# Render report
Rscript $DIR/render.R

