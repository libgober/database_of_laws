
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


cd $CWD
python build/pop_names_scrape.py
Rscript build/prep_popular_names.R

# merge prior collections

Rscript build/merge_popular_names.R

# make figures

Rscript analysis/scriptforggplot1.R

# compile latex


