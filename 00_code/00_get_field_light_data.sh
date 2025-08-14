#!/usr/bin/env bash 

file=$1
rm 01_data/$file

wget -P 01_data/ https://edmond.mpg.de/api/access/datafile/$file