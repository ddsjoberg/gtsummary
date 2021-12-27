#!/bin/bash

# download and extract taxi data
mkdir ~/data/
cd ~/data

wget -O ~/data/trip_fare.7z https://archive.org/download/nycTaxiTripData2013/trip_fare.7z && \
  sudo apt install p7zip-full && \
  7z x trip_fare.7z &> data.out

# fix trailing space in header for every file
ls *trip_fare*.csv | xargs -P 16 sed -i '1 s/, /,/g'

# download the US census data
wget -O ~/data/all_California.zip https://www2.census.gov/census_2000/datasets/PUMS/FivePercent/California/all_California.zip && \
  sudo apt install unzip && \
  unzip all_California.zip
