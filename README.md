# Preparing a satellite-operator-year panel from UCS and space-track data

This repository contains the data and R code needed to reproduce the dataset `UCS-JSpOC-soy-panel-22.csv`. This dataset combines and cleans data from the Union of Concerned Scientists and Space-Track.org to create a panel of satellites, operators, and years. 

This dataset is used in the paper "Oligopoly competition between satellite constellations will reduce economic welfare from orbit use". The final dataset can also be downloaded from the replication files for that paper:

A "living" version of this repository can be found at: 

# Repository structure

* `/UCS data` contains Excel and CSV data files from the Union of Concerned Scientists, as well as output files generated from data cleaning. You can find the UCS Satellite Database here: https://www.ucsusa.org/resources/satellite-database . Historical data was obtained from Dr. Teri Grimwood.

* `/Space-Track data` contains JSON data from Space-Track.org, files to help identify operator names for harmonization in `UCS_text_cleaner.R`, and output generated from cleaning and merging data.

	* API queries to generate the JSON files can be found in json_cleaned_script.R. They are restated below for convenience. These queries were run on January 1, 2023 to produce the data used in "Oligopoly competition between satellite constellations will reduce economic welfare from orbit use".

		* < 800: https://www.space-track.org/basicspacedata/query/class/satcat/OBJECT_TYPE/PAYLOAD/APOGEE/%3C800/orderby/INTLDES%20asc/emptyresult/show
		*  800-1999: https://www.space-track.org/basicspacedata/query/class/satcat/OBJECT_TYPE/PAYLOAD/APOGEE/800,800--2000/orderby/INTLDES%20asc/emptyresult/show
		* 2000-33999: https://www.space-track.org/basicspacedata/query/class/satcat/OBJECT_TYPE/PAYLOAD/APOGEE/2000,2000--34000/orderby/INTLDES%20asc/emptyresult/show
		* \> 33999: https://www.space-track.org/basicspacedata/query/class/satcat/APOGEE/>33999/OBJECT_TYPE/PAYLOAD/orderby/INTLDES%20asc/emptyresult/show

* `/Current R scripts` contains R scripts to process the data.
	
	* `combined_scripts.R` loads and cleans UCS data. It takes the raw CSV files from `/UCS data` as input and produces `UCS_Combined_Data.csv` as output.

	* `UCS_text_cleaner.R` harmonizes various text fields in the UCS data, including operator and owner names. Best efforts were made to ensure correctness and completeness, but some gaps may remain.

	* `json_cleaned_script.R` loads and cleans Space-Track data, and merges it with the cleaned and combined UCS data.

	* `panel_builder.R` uses the cleaned and merged files to construct the satellite-operator-year panel dataset with annual satellite histories and operator information. The logic behind the dataset construction approach is described in this blog post: https://akhilrao.github.io/blog//data/2020/08/20/build_stencil_cut/

* `/Output_figures` contains figures produced by the scripts. Some are diagnostic, some are just interesting.

* `/Output_data` contains the final data outputs.

* `/data-cleaning-notes` contains Excel and CSV files used to assist in harmonizing text fields in `UCS_text_cleaner.R`. They are included here for completeness.

# Creating the dataset

To reproduce the `UCS-JSpOC-soy-panel-22.csv` dataset:

1. Ensure `R` is installed along with the required packages

2. Run the scripts in `/Current R scripts` in the following order:

	* `combined_scripts.R` (this will call `UCS_text_cleaner.R`)

	* `json_cleaned_script.R`

	* `panel_builder.R`

3. The output file `UCS-JSpOC-soy-panel-22.csv`, along with several intermediate files used to create it, will be generated in `/Output data`
