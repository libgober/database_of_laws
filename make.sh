
# Build

CWD=$(pwd)

# make the heinonline files
cd $CWD/build/heinonline/
python main.py
mv hein_data_raw.csv ..

# gather the mods files
cd $CWD/build
Rscript gather_mods.R

# combine the files

Rscript combine_sources.R

# build the popualr names data

python pop_names_scrape2.py

# merge prior collections

Rscript merge_popular_names.R
Rscript merge_sig_leg.R

# evaluate merge quality/validation

# compile latex


