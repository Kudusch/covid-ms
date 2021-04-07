#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import urllib.request
import json
import sys
import os

ags = sys.argv[1]

api_url = "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_Landkreisdaten/FeatureServer/0/query?where=AGS%20%3D%20'{}'&outFields=death_rate,cases,deaths,cases_per_100k,cases_per_population,county,last_update,cases7_per_100k,recovered,BL,EWZ&returnGeometry=false&outSR=4326&f=json".format(ags)
r = urllib.request.urlopen(api_url).read()
dir_name = os.path.dirname(os.path.realpath(__file__))
with open("{}/meta_data/{}_meta.json".format(dir_name, ags), "w") as f:
    json.dump(json.loads(r), f)
