#!/usr/bin/env bash 

mkdir 01_data/temp
/Library/Frameworks/R.framework/Resources/bin/Rscript 00_code/02_subset_light_data_windows.R

python 00_code/03_dinard_light_6_plots.py

rm -rf 01_data/temp

rm 02_visuals/Dinard_daylight_spectrum_3D_part*.pdf