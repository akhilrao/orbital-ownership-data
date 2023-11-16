# Tracking orbital-use patterns

This repository contains data and code to track orbital-use patterns, including concentration measures, over time. It currently combines and cleans data from the Union of Concerned Scientists and Space-Track.org to create a satellite-operator-year panel dataset.

# What is this for?

There are many sources that track some measures of orbit use, e.g. Space-Track.org, DISCOSweb, UCS, Gunther's Space Report. Most if not all of these are focused on knowing where physical objects have been sent. While useful, this type of data is missing important fields necessary for economic analysis of orbit use---things like who owns what, where, what the debris environment looks like, and how it all changes over time. This project is an effort to track that data using only public domain sources.

At the moment the dataset only has active payloads. Future updates will incorporate the evolution of the debris environment, estimates of launch prices, and model-derived estimates of location-specific payoffs.

Gordon Lewis and [Ethan Berner](https://github.com/ethanberner) contributed immensely to the construction of this dataset. Please reach out if you are interested in contributing.

**Update 2023-15-11:** The JSR satcat has a lot of detail on satellite Owner/Operators along with details on debris. I'm still investigating the extent to which this might be a suitable replacement for the current data flow, which is a bit effort-intensive. I think JSR uses the UCS data as well, which would help minimize drift from switching. For now the JSR launch log and satellite catalog have been added to a folder labeled "JSR data".

Citation: McDowell, Jonathan C., 2020. General Catalog of Artificial Space Objects,
  Release 1.5.4 , https://planet4589.org/space/gcat

## Relation to other replication materials

A version of this dataset was used in the paper "Oligopoly competition between satellite constellations will reduce economic welfare from orbit use". The final dataset can also be downloaded from the replication files for that paper: https://doi.org/10.57968/Middlebury.23816994.v1  . Repeatable code and data associated with Figure 1 of that paper can be found in the Middlebury Institutional Repository : https://doi.org/10.57968/Middlebury.23982468.v1 . The MIR code and datasets will recreate the same dataset used in that paper. This is a "living" version of the project to track orbital-use patterns and will continue to be updated.

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

* `/JSR data` contains TSV files from [Jonathan McDowell's catalog](https://planet4589.org/space/gcat/index.html) on object launches and satellite ownership. The folder also contains some simple plots and an R script to produce them.

# Creating the dataset

To reproduce the `UCS-JSpOC-soy-panel-22.csv` dataset:

1. Ensure `R` is installed along with the required packages

2. Run the scripts in `/Current R scripts` in the following order:

	* `combined_scripts.R` (this will call `UCS_text_cleaner.R`)

	* `json_cleaned_script.R`

	* `panel_builder.R`

3. The output file `UCS-JSpOC-soy-panel-22.csv`, along with several intermediate files used to create it, will be generated in `/Output data`
