### This script cleans up the names for the UCS data variables Entity_Name and Entity_Type ###

### This script still does not get rid of the replacement character, which accounts for ~20 unique names. However, when
### searching for these names in the data (i.e. typing the replacement character into the Filter bar) these names do not
### appear in the actual UCS data. This leads us to believe that there are some names that are hidden within the original 
### code which we cannot physically change within R. Currently (as of 04/2020 UCS release) there are 816 unique names.

### Block 1: Clears the global environment, loads the relevant packages, sets working directory
#####

### Loads the relevant packages
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
library(data.table)
library(gsubfn)
library(stringi)
library(purrr)

#getwd()
#setwd("/Users/ethan/Dropbox/Orbit-use economics data assembly/Current R scripts")
#setwd("/Users/ethan/Dropbox/Orbit-use economics data assembly/UCS data/Output")
#combined_csv <- read.csv("UCS_Combined_Data.csv")
#combined_csv <- read.csv ("/Users/ethan/Dropbox/Orbit-use economics data assembly/UCS data/Output")
#combined_csv <- read.csv("../UCS\ data/Output/UCS_Combined_Data.csv")

### Block 2: Cleaning the strings
#####

### Creates a vector that lists all the unique entity names, then exports that vector as a .csv to be referenced later
names_to_clean <- unique(levels(as.factor(combined_csv$Entity_Name)))
#is_vector(names_to_clean)
#write.csv(matrix(names_to_clean, nrow=length(names_to_clean)), file ="names_to_clean.csv", row.names=FALSE)
#View(names_to_clean)

########## Large chunk that basically just gets each typo/misspelling and standardizes them 

### Corrects the spelling of the Entity_Type variable names
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Governmnet")] <- "Government" # clearly a typo
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Commecial"|combined_csv$Entity_Type=="Commerical")] <- "Commercial" # clearly typos
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Commercial/Government/Government")] <- "Commercial/Government" # likely another typo
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Commercial/Gov/Mil"|combined_csv$Entity_Type=="Commercial/Gov/Mil."|combined_csv$Entity_Type=="Comm/Gov/Mil."|combined_csv$Entity_Type=="Gov/Mil/Comm")] <- "Commercial/Government/Military" # many ways of saying the same thing
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Japan")] <- "Government" # All launched by JAXA, so "Government" seems reasonable
### Alphabetical simplificaiton of Entity_Type cooperation entries 
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Military/Commercial")] <- "Commercial/Military"
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Military/Government")] <- "Government/Military"
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Military/Civil")] <- "Civil/Military"
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Commercial/Civil")] <- "Civil/Commercial"
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Government/Civil")] <- "Civil/Government"
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Government/Commercial")] <- "Commercial/Government"
combined_csv$Entity_Type[which(combined_csv$Entity_Type=="Government/Commercial/Military")] <- "Commercial/Government/Military"
combined_csv$Entity_Type[which(combined_csv$Entity_Type == "Civilian Research")] <- "Civil"

#Check if commands are working
unique_types <- unique(levels(as.factor(combined_csv$Entity_Type)))
#View(unique_types)

### Corrects the spelling of the Class_of_Orbit variable names
combined_csv$Class_of_Orbit[which(combined_csv$Class_of_Orbit=="LEO, Sun sync."|combined_csv$Class_of_Orbit=="LEO, Sun-sync"|combined_csv$Class_of_Orbit=="LEO, Sun-sync."|combined_csv$Class_of_Orbit=="LEO,Sun-sync."|combined_csv$Class_of_Orbit=="LEO, Syn-sync."|combined_csv$Class_of_Orbit=="LEO, Sun-Sync."|combined_csv$Class_of_Orbit=="LEO, Sun.sync.")] <- "LEO, Sun sync" # many ways of saying the same thing
combined_csv$Class_of_Orbit[grep("GEO*",combined_csv$Class_of_Orbit)] <- "GEO" # all that GEO noise... The degrees are worth keeping for a different analysis, but we'll strip them out for now
combined_csv$Class_of_Orbit[grep("Elliptical, Molniya",combined_csv$Class_of_Orbit)] <- "Molniya" # A Molniya orbit is a kind of elliptical orbit, no need for redundancy
combined_csv$Class_of_Orbit[which(combined_csv$Class_of_Orbit=="Sun-Sychronous"|combined_csv$Class_of_Orbit=="Sun-Synchronous")] <- "Sun sync" # More typos...3

### Corrects the spelling of the Entity_Name variable names, gets rid of special characters, other name changes
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Air Force Research Laboratory (AFRL)" | combined_csv$Entity_Name == "Air Force Research Laboratory (ARFL)")] <- "Air Force Research Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Al Yah Satellite Communications Co. (YAHSAT)")] <- "Al Yah Satellite Communications Company (YAHSAT)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Algerian Space Agency")] <- "Algerian Space Agency (ASAL)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Amsat-NA")] <- "AMSAT-NA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Aprize Satellites Argentina")] <- "Aprize Satellite Argentina"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Asia Broadcasting Satellite" | combined_csv$Entity_Name == "Asia Broadcast Satellite Ltd.")] <- "Asia Broadcast Satellite"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Astronautic Technology Sdn. Bhd (ATSB)" | combined_csv$Entity_Name == "Astronautic Technology Sdn Bhd")] <- "Astronautic Technology Sdn Bhd (ATSB)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Centre National d'Etudes Spatiales (CNES)/European Space Agency")] <- "Centre National d'Etudes Spatiales (CNES)/European Space Agency (ESA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "China Aerospace Science and Technology Corp." | combined_csv$Entity_Name == "China Aerospace Science and Technology Corp. (CASC)" | combined_csv$Entity_Name == "China Aerospace Science and Technology Corporation" | combined_csv$Entity_Name == "China Aerospace Science and Technology Corporation (CASTC)")] <- "China Aerospace Science and Technology Corporation (CASC)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "China Direct Broadcast Satellite co.")] <- "China Direct Broadcast Satellite Co."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "China‚Äôs Ministry of Land and Resources, Ministry of Environmental Protection, and Ministry of Agriculture" | combined_csv$Entity_Name == "China’s Ministry of Land and Resources, Ministry of Environmental Protection, and Ministry of Agriculture" | combined_csv$Entity_Name == "Chinaâ€™s Ministry of Land and Resources, Ministry of Environmental Protection, and Ministry of Agriculture" | combined_csv$Entity_Name ==  "China's Ministry of Land and Resources, Ministry of Environmental Protection, and Ministry of Agriculture")] <- "China Ministry of Land and Resources, Ministry of Environmental Protection, and Ministry of Agriculture"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Chinese Academy of Science" | combined_csv$Entity_Name == "Chinese Academy of Sciences")] <- "Chinese Academy of Sciences (CAS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Chinese Academy of Space Technology")] <- "Chinese Academy of Space Technology (CAST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "DFH Satellite")] <- "DFH Satellite Co. Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "DFH Satellite/AMSAT-China")] <- "DFH Satellite Co. Ltd./AMSAT-China"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Digital Globe" | combined_csv$Entity_Name == "DigitalGlobal Corporation" | combined_csv$Entity_Name == "DigitalGlobe Corporation")] <- "DigitalGlobe"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "√âcole de Mines" | combined_csv$Entity_Name == "Ã‰cole de Mines" | combined_csv$Entity_Name == "Ècole des Mines")] <- "Ecole des Mines"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Ecole Polytechnique FÈdÈrale de Lausanne (EPFL)" | combined_csv$Entity_Name == "Ecole Polytechnique F√©d√©rale de Lausanne (EPFL)" | combined_csv$Entity_Name == 	"Ecole Polytechnique F�d�rale de Lausanne (EPFL)" | combined_csv$Entity_Name == "Ecole Polytechnique F<e9>d<e9>rale de Lausanne (EPFL)" | combined_csv$Entity_Name == "Ecole Polytechnique F\xe9d\xe9rale de Lausanne (EPFL)")] <- "Ecole Polytechnique Federale de Lausanne (EPFL)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "European Space Agency")] <- "European Space Agency (ESA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "European Space Agency/NASA")] <- "European Space Agency (ESA)/NASA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "European Space Operations Centre")] <- "European Space Operations Centre (ESOC)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "European Telecommunications Satellite Cosorrtium")] <- "European Telecommunications Satellite Consortium (EUTELSAT)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "German Aerospace Center")] <- "German Aerospace Center (DLR)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Goddard Space Flight Center" | combined_csv$Entity_Name == "Goddard Space Flight Center (NASA)")] <- "NASA Goddard Space Flight Center"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Goddard Space Flight Center/EOS Data and Operations System")] <- "NASA Goddard Space Flight Center/EOS Data and Operations System"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Goddard Space Flight Center/Penn State University")] <- "NASA Goddard Space Flight Center/Penn State University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Indian National Satellite (INSAT)" | combined_csv$Entity_Name == "Indian National Satellite System")] <- "Indian National Satellite System (INSAT)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Indian Space Reseaerch Organization (ISRO)" | combined_csv$Entity_Name == "Indian Space Research Organization" | combined_csv$Entity_Name == "Indian Space Research Organization (ISRO)")] <- "Indian Space Research Organisation (ISRO)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "INMARSAT, Ltd./European Space Agency")] <- "INMARSAT, Ltd./European Space Agency (ESA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Institute of Space & Astronautical Science")] <- "Institute of Space and Astronautical Science"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Korean Advanced Institute of Science and Technology (KAIST)")] <- "Korea Advanced Institute of Science and Technology (KAIST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Kyushu Institute of Technology")] <- "Kyushu Institute of Technology (KIT)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Los Alamos National Laboratory" | combined_csv$Entity_Name == "Los Alamos National Labs (LANL)")] <- "Los Alamos National Laboratory (LANL)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Los Alamos National Labs/DOE")] <- "Los Alamos National Laboratory (LANL)/DOE"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Microspace Rapid Pvt Ltd.")] <- "Microspace Rapid Pte Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Moscow State University")] <- "Moscow State University (MSU)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Nagora University and Daido University" | combined_csv$Entity_Name == "Nagoya University and Daido University")] <- "Nagoya University/Daido University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA Goddard Space Flight Center, Jet Propulsion Laboratory")] <- "NASA Goddard Space Flight Center/Jet Propulsion Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA - Earth Science Enterprise/Japan Meteorological Agency/Brazillian Space Agency")] <- "NASA Earth Science Enterprise/Japan Meteorological Agency/Brazillian Space Agency"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA-Ames Research Center/Stanford University")] <- "NASA Ames Research Center/Stanford University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/Ames Research Center" | combined_csv$Entity_Name == "Ames Research Center, NASA")] <- "NASA Ames Research Center"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/Goddard Space Flight Center" | combined_csv$Entity_Name == "NASA/GSFC")] <- "NASA Goddard Space Flight Center"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/Applied Physics Laboratory, Johns Hopkins")] <- "NASA/Johns Hopkins University Applied Physics Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics  and Space Administration (NASA)" | combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)")] <- "NASA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA) - Earth Science Enterprise/Japan Meteorological Agency/Brazillian Space Agency")] <- "NASA Earth Science Enterprise/Japan Meteorological Agency/Brazillian Space Agency"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA) Earth Science Office/Laboratory for Atmospheric and Space Physics, Univ. of Colorado")] <- "NASA Earth Science Office/University of Colorado Laboratory for Atmospheric and Space Physics"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA) Goddard Space Flight Center" | combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/Goddard Space Flight Center" | combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/GSFC")] <- "NASA Goddard Space Flight Center"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA) Jet Propulsion Laboratory")] <- "NASA Jet Propulsion Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)-Ames Research Center/Stanford University")] <-"NASA Ames Research Center/Stanford University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/Applied Physics Laboratory, Johns Hopkins" | combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/Johns Hopkins University Applied Physics Laboratory")] <- "NASA/Johns Hopkins University Applied Physics Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/Centre National d'Etudes Spatiales (SNES)/NOAA/EUMSETSAT")] <- "NASA/Centre National d'Etudes Spatiales (SNES)/NOAA/EUMSETSAT"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/Colorado State University")] <- "NASA/Colorado State University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/JAXA")] <- "NASA/JAXA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/Multinational")] <- "NASA/Multinational"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/University of California, Berkeley (in partnership with Germany, France, Austria, Canada)")] <- "NASA/University of California, Berkeley (in partnership with Germany, France, Austria, Canada)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/US Geological Survey")] <- "NASA/US Geological Survey"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Authority for Remote Sensing and Space Science (NARSSS)")] <- "National Authority for Remote Sensing and Space Science (NARSS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Institute of Information and Communications Technology (JPN)")] <- "National Institute of Information and Communications Technology (NICT)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Reconaissance Office" | combined_csv$Entity_Name == "National Reconaissance Office (NRO)" | combined_csv$Entity_Name == "National Reconnaissance Office")] <- "National Reconnaissance Office (NRO)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Reconnaissance Office (NRO)/USAF")] <- "National Reconnaissance Office (NRO)/US Air Force"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Reconnaissance Office/US Navy")] <- "National Reconnaissance Office (NRO)/US Navy"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Space Development Agency")] <- "National Space Development Agency (NASDA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Space Development Agency (NASA)/Japan Aerospace Exploration Agency (JAXA)")] <- "National Space Development Agency (NASDA)/JAXA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Space of Activites Commission - Argentina/NASA")] <- "National Space Activities Commission (CONAE)/NASA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NigComSat")] <- "NigComSat Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Peruvian SpaceAgency")] <- "Peruvian Space Agency"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Politecnico di Torino" | combined_csv$Entity_Name == "Polytechnic of Turin")] <- "Polytechnic University of Turin"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Russian Federal Space Agency" | combined_csv$Entity_Name == "Russian Federal Space Agency (Roskosmos)" | combined_csv$Entity_Name == "Roscosmos State Corporation")] <- "Russian Federal Space Agency (ROSCOSMOS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Russian Satelite Communications Company (Intersputnik)" | combined_csv$Entity_Name =="Russian Satellite Communications Company (intersputnik)"| combined_csv$Entity_Name =="Russian Satellite Communications Company (Intersputnik)")] <- "Russian Satellite Communications Company (RSCC)/Intersputnik"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Russian Satellite Communications Company" | combined_csv$Entity_Name == "Russian Satellite Communications Co.")] <- "Russian Satellite Communications Company (RSCC)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Russian Satellite Communications Company (Intersputnik)/EUTELSAT")] <- "Russian Satellite Communications Company (RSCC)/Intersputnik/EUTELSAT"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Samara State Aerospace University"| combined_csv$Entity_Name == "Samara State Aerospace University, SGAU")] <- "Samara State Aerospace University (SSAU)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Shanghai Academy of Space Technology (CAST)" | combined_csv$Entity_Name == "Shanghai Academy of Space Technology (SAST")] <- "Shanghai Academy of Spaceflight Technology (SAST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Shanghai Engineering Center for Microsatellites" | combined_csv$Entity_Name == "Shanghai Micro Satellite Engineering Center")] <- "Shanghai Engineering Center for Microsatellites (SECM)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Space-Communication Ltd" | combined_csv$Entity_Name == "Space-Communication Ltd.")] <- "Space Communications Ltd. (Spacecom)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Sputnix Ltd.")] <- "SPUTNIX Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Surrey Satellite Technology Ltd." | combined_csv$Entity_Name == "Surrey Satellite Technologies Ltd. ()" | combined_csv$Entity_Name == "Surrey Satellite Technology Limited (SSTL)" | combined_csv$Entity_Name == "Surrey Satellite Technology Ltd." | combined_csv$Entity_Name == "Surrey Satellite Technologies Ltd.")] <- "Surrey Satellite Technology Ltd. (SSTL)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Technical  University Berlin"  | combined_csv$Entity_Name == "Technical University Berlin" | combined_csv$Entity_Name == "Technical University, Berlin")] <- "Technical University of Berlin"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Technical University, Delft")] <- "Technical University of Delft"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Telesatd Canada Ltd. (BCE, Inc.)")] <- "Telesat Canada Ltd. (BCE, Inc.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Tokyo Institute of Technology, Tokyo University of Science and JAXA")] <- "Tokyo Institute of Technology/Tokyo University of Science/JAXA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "U.S.Army Space and Missile Defense Command" | combined_csv$Entity_Name == "U.S.Army Space ad Missile Defense Command")] <- "U.S. Army Space and Missile Defense Command"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "University of Sydney, University of New South, Austrialian National University")] <- "University of Sydney/University of New South Wales/Australian National University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "University of Tokyo and NESTRA")] <- "University of Tokyo/NESTRA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "University of Toronto, Institute for Aerospace Studies")] <- "University of Toronto Institute for Aerospace Studies"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force"  | combined_csv$Entity_Name == "Air Force" )] <- "U.S. Air Force"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force Academy")] <- "U.S. Air Force Academy"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force Institute of Technology")] <- "U.S. Air Force Institute of Technology"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force Rapid Capabilities Office" )] <- "U.S. Air Force Rapid Capabilities Office"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force Space Command (AFSPC)")] <- "U.S. Air Force Space Command (AFSPC)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force Space Test Program" | combined_csv$Entity_Name == "USAF Space Test Office")] <- "U.S. Air Force Space Test Program"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force/ DoD" | combined_csv$Entity_Name == "U.S. Air Force/ DoD")] <- "U.S. Air Force/DoD"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force/ US Navy")] <- "U.S. Air Force/US Navy"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force/ US Navy/NASA")] <- "U.S. Air Force/US Navy/NASA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Air Force/NRL")] <- "U.S. Air Force/NRL"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Army")] <- "U.S. Army"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Army Space and Missile Defense Command"  | combined_csv$Entity_Name == "U.S. Army Space and Missile Defence Command")] <- "U.S. Army Space and Missile Defense Command"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Department of Energy/Office of Nonproliferation and National Security")] <- "U.S. Department of Energy/Office of Nonproliferation and National Security"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Naval Academy")] <- "U.S. Naval Academy"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Naval Research Laboratory")] <- "U.S. Naval Research Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Navy")] <- "U.S. Navy"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Southern Command")] <- "U.S. Southern Command"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "USAF /ORSO (Operationally Responsive Space Office)")] <- "U.S. Air Force/Operationally Responsive Space Office (ORSO)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "USAF Space Test Program")] <- "U.S. Air Force Space Test Program"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "USArmy Space and Missile Defense Command")] <- "U.S. Army Space and Missile Defense Command"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Air Force Research Laboratory")] <- "U.S. Air Force Research Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Air Force Satellite Control Network")] <- "U.S. Satellite Control Network"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Air Force Space and Missile Systems Center")] <- "U.S. Air Force Space and Missile Systems Center"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Air Force Space Command")] <- "U.S. Air Force Space Command"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Gaxprom Space Systems")] <- "Gazprom Space Systems"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Horizons 2 Satellite, LLC (Intelsat, JSAT Corporation")] <- "Horizons 2 Satellite, LLC (Intelsat/JSAT Corporation)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Horizons 2 Satellite, LLC (Intelsat, Sky Perfect JSAT Corporation")] <- "Horizons 2 Satellite, LLC (Intelsat/Sky Perfect JSAT Corporation)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Indonesian National Aeronautics and  Space Agency (Lembaga Penergangan dan Antariksa Nasional - LAPAN")] <- "Indonesian National Aeronautics and Space Agency (LAPAN)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Lockheed Martin Intersputnik")] <- "Lockheed Martin/Intersputnik"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Max Valler school Bolzano, Italy, Oskar von Miller school, Merano, Italy" | combined_csv$Entity_Name == "Max Valier school Bolzano, Italy, Oskar von Miller school, Merano, Italy")] <- "Max Valler School Bolzano/Oskar von Miller School Merano"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Military Satellite Communications - US Air Force")] <- "U.S. Air Force Military Satellite Communications"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Missile Defense Agency")] <- "Missile Defense Agency (MDA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Technical University")] <- "National Technical University of Ukraine"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Technical University of Ukraine & Shenyang Aerospace University")] <- "National Technical University of Ukraine/Shenyang Aerospace University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National University of Defense")] <- "National University of Defense Technology"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Norwegian Space Center")] <- "Norwegian Space Centre"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Norwegian Space Centre\\Norwegian Defence Research Establishment (FFI)")] <- "Norwegian Space Center/Norwegian Defence Research Establishment (FFI)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Centre National d'Etudes Spatiales (CNES)/Délégation Générale de l'Armement (DGA)" | combined_csv$Entity_Name == "Centre National d'Etudes Spatiales (CNES)/D\xe9l\xe9gation G\xe9n\xe9rale de l'Armement (DGA)" | combined_csv$Entity_Name == "Centre National d'Etudes Spatiales (CNES)/DÃ©lÃ©gation GÃ©nÃ©rale de l'Armement (DGA)" | combined_csv$Entity_Name == "Centre National d'Etudes Spatiales (CNES)/D\xe9l\xe9gation G\xe9n\xe9rale de l'Armement (DGA)")] <- "Centre National d'Etudes Spatiales (CNES)/Delegation Generale de l'Armement (DGA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "CTRS Morocco/Institut für Luft-und Raumfahrttechnik (Berlin)" | combined_csv$Entity_Name == "CTRS Morocco/Institut f\xfcr Luft-und Raumfahrttechnik (Berlin)" | combined_csv$Entity_Name == "CTRS Morocco/Institut fÃ¼r Luft-und Raumfahrttechnik (Berlin)" | combined_csv$Entity_Name == "CTRS Morocco/Institut f�r Luft-und Raumfahrttechnik (Berlin)" | combined_csv$Entity_Name == "CTRS Morocco/Institut f\xfcr Luft-und Raumfahrttechnik (Berlin)")] <- "CTRS Morocco/Institut fur Luft- und Raumfahrttechnik (Berlin)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Deutsches Zentrum für Luft- und Raumfahrt" | combined_csv$Entity_Name == "Deutsches Zentrum f\xfcr Luft- und Raumfahrt" | combined_csv$Entity_Name == "Deutsches Zentrum fÃ¼r Luft- und Raumfahrt" | combined_csv$Entity_Name == "Deutsches Zentrum f\xfcr Luft- und Raumfahrt")] <- "Deutsches Zentrum fur Luft- und Raumfahrt"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Ecole Polytechnique Fédérale de Lausanne (EPFL)" | combined_csv$Entity_Name == "Ecole Polytechnique F\xe9d\xe9rale de Lausanne (EPFL)" | combined_csv$Entity_Name == "Ecole Polytechnique FÃ©dÃ©rale de Lausanne (EPFL)")] <- "Ecole Polytechnique Federale de Lausanne (EPFL)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Facultad de Ingeniería de la Universidad de la República (FING), ANTEL" | combined_csv$Entity_Name == "Facultad de Ingenier\xeda de la Universidad de la Rep\xfablica (FING), ANTEL" | combined_csv$Entity_Name == "Facultad de Ingenier\xeda de la Universidad de la Rep\xfablica (FING), ANTEL")] <- "Facultad de Ingenieria de la Universidad de la Republica (FING)/ANTEL"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Institut für Luft- und Raumfahrt" | combined_csv$Entity_Name == "Institut f\xfcr Luft- und Raumfahrt" | combined_csv$Entity_Name == "Institut f\xfcr Luft- und Raumfahrt")] <- "Institut fur Luft- und Raumfahrt"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Instituto Nacional de Técnia Aeroespacial (INTA)" | combined_csv$Entity_Name == "Instituto Nacional de T\xe9cnia Aeroespacial (INTA)" | combined_csv$Entity_Name == "Instituto Nacional de T\xe9cnia Aeroespacial (INTA)")] <- "Instituto Nacional de Tecnica Aeroespacial"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Instituto Tecnológico de Costa Rica" | combined_csv$Entity_Name == "Instituto TecnolÃ³gico de Costa Rica")] <- "Instituto Tecnologico de Costa Rica"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Institute for Radio Astronomy at Pontificia Universidad Cat\xf3lica del Per\xfa (PUCP)" | combined_csv$Entity_Name == "Institute for Radio Astronomy at Pontificia Universidad Cat\xf3lica del Per\xfa (PUCP)")] <- "Institute for Radio Astronomy at Pontificia Universidad Catolica del Peru (PUCP)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Laboratoire d’Etudes Spatiales et d’Instrumentation en Astrophysique")] <- "Laboratoire d'Etudes Spatiales et d'Intstrumentation en Astrophysique"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Nanjing\xa0University\xa0of\xa0Aeronautics\xa0and\xa0Astronautics" | combined_csv$Entity_Name == "Nanjing\xa0University\xa0of\xa0Aeronautics\xa0and\xa0Astronautics")] <- "Nanjing University of Aeronautics and Astronautics"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Universitat Politècnica de Catalunya")] <- "Universitat Politecnica de Catalunya (UPC)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "University of Würzburg" | combined_csv$Entity_Name == "University of W\xfcrzburg" | combined_csv$Entity_Name == "University of Wuerzburg" | combined_csv$Entity_Name == "University of W\xfcrzburg")] <- "University of Wurzburg"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Russian Space Agency")] <- "Russian Space Agency (RKA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Alascom Inc. (AT&T) shared with GE Americom")] <- "Alascom Inc. (AT&T)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "China National Space Administration (China)/National Institute for Space Research (Brazil)")] <- "China National Space Administration (CNSA)/National Institute for Space Research (Brazil)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "CNES (Centre national d'Ã©tudes spatiales)" | combined_csv$Entity_Name == "CNES (Centre national d'études spatiales)")] <- "Centre National d'Etudes Spatiales (CNES)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Department of Astronautics, University of Tokyo")] <- "University of Tokyo Department of Astronautics"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Echostar Corporation (entire payload leased from Telesat Canada Ltd.)")] <- "Echostar Corporation"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Echostar Technologies, LLC - leased to Bermuda, operated by SES Satellites" | combined_csv$Entity_Name == "Echostar Technologies, LLC (leased to Ceil Satellite Group)")] <- "Echostar Technologies, LLC"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Esâ€™hailSat")] <- "Es'hailSat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "European Space Agency (and 250 international scientific investigators)" | combined_csv$Entity_Name == "European Space Agency (ESA) (and 250 international scientific investigators)" | combined_csv$Entity_Name == "European Space Agency (ESA)(and 250 international scientific investigators)")] <- "European Space Agency (ESA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "European Telecommunications Satellite Consoritum (EUTELSAT)" | combined_csv$Entity_Name == "European Telecommunications Satellite Consortium (EUTELSAT)" | combined_csv$Entity_Name == "European Telecommunications Satellite Consortium (EUTELSAT) - leased from France Telecom" | combined_csv$Entity_Name == "European Telecommunications Satellite Consortium (EUTELSAT)/transponders leased to Asia Broadcast Satellite" | combined_csv$Entity_Name == "European Telecommunications Satellite Consotium (EUTELSAT) - leased from France Telecom" | combined_csv$Entity_Name == "European Telecommunications Satellite Cosorrtium (EUTELSAT)" | combined_csv$Entity_Name == "Eutelsat -- leased from Loral Skynet Satellite Services (Loral Global Alliance)" | combined_csv$Entity_Name == "Eutelsat (leased from Chinasat)" | combined_csv$Entity_Name == "EUTELSAT S.A." | combined_csv$Entity_Name == "EUTELSAT S.A. -- leased from Loral Skynet Satellite Services (Loral Global Alliance)")] <- "Eutelsat S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "EUTELSAT (leases Ku-band capacity) - Russian Satellite Communications Company (Intersputnik)")] <- "Eutelsat S.A./Russian Satellite Communications Company (RSCC)/Intersputnik"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "EUTELSAT Americas")] <- "Eutelsat Americas"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "EUTELSAT S.A./Es'hailSat" | combined_csv$Entity_Name =="European Telecommunications Satellite Consortium (EUTELSAT)/Es'hailSat")] <- "Eutelsat S.A./Es'hailSat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "EUTELSAT S.A./Nilesat"|combined_csv$Entity_Name =="European Telecommunications Satellite Consoritum (EUTELSAT)/Nilesat")] <- "Eutelsat S.A./Nilesat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "École de Mines")] <- "Ecole de Mines"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Herzliya Science Centre (HSC)")] <- "Herzliya Science Center (HSC)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Hispamar (subsidiary of Hispasat - Spain)")] <- "Hispamar"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "GasTianta and GZH-HNJ BDS AGR Co., Ltd.")] <- "GasTianta/GZH-HNJ BDS AGR Co., Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "HughesNet leased from Echostar Technologies, LLC" | combined_csv$Entity_Name == "Hughes Network Systems")] <- "HughesNet"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "ICEYE" | combined_csv$Entity_Name == "ICEYE Ltd.")] <- "ICEYE Oy"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./Telenor (leased capacity)")] <- "Intelsat, Ltd./Telenor"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat/Paradigm Secure Communications (wholly owned by EADS Astrium)")] <- "Intelsat/Paradigm Secure Communications"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Ministry of Defence/Paradigm Secure Communications (wholly owned by EADS Astrium)" | combined_csv$Entity_Name == "Ministry of Defense/Paradigm Secure Communications (wholly owned by EADS Astrium)")] <- "Ministry of Defence/Paradigm Secure Communications"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Ministry of Defense/Centre National d'Etudes Spatiales (CNES) - cooperation with Austria, Belgium, Spain, Sweden")] <- "Ministry of Defense/Centre National d'Etudes Spatiales (CNES)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/University of California, Berkeley (in partnership with Germany, France, Austria, Canada)")] <- "NASA/University of California, Berkeley"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Authority for Remote Sensing and Space Sciences (NARSS)")] <- "National Authority for Remote Sensing and Space Science (NARSS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Commission of Space Activities (CONAE) (with NASA, Denmark, Italy, Spain, France, Brazil)")] <- "National Commission of Space Activities (CONAE)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Oceanographic and Atmospheric Administration (NOAA) (part of international program)")] <- "National Oceanographic and Atmospheric Administration (NOAA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Northwest Nazarene University (Idaho)")] <- "Northwest Nazarene University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Optus Communications (Parent: Signapore Telecom)")] <- "Optus Communications (Parent: Singapore Telecom)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Optus Communications (Parent: Singapore Telecom)/Australian Ministry of Defence")] <- "Optus Communications (Parent: Singapore Telecom)/Australian Ministry of Defense"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "ORBCOMM Inc. (subsidiary of Orbital Sciences Corp.)")] <- "ORBCOMM Inc."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "QuetzSat, S. de R.L. de C.V. (leased from Echostar Technologies, LLC)")] <- "QuetzSat, S. de R.L. de C.V."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Roshydromet - Planeta")] <- "Russian Federal Service For Hydrometeorology and Environmental Monitoring (ROSHYDROMET)/Planeta"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Royal Australian Navy/Panamsat (owner)")] <- "Royal Australian Navy/Panamsat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Joint Stock Company-Information Satellite Systems")] <- "JSC Academician M.F. Reshetnev Information Satellite Systems"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Kyushu Institute of Technology (Kyutech)/Nanyang Technological University (NTU)")] <- "Kyushu Institute of Technology (KIT)/Nanyang Technological University (NTU)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Leased to APT Satellite Holdings Ltd./Loral Skynet")] <- "APT Satellite Holdings Ltd./Loral Skynet"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA) - Earth Science Enterprise/Japan Meteorological Agency/Brazilian Space Agency" | combined_csv$Entity_Name == "NASA - Earth Science Enterprise/Japan Meteorological Agency/Brazilian Space Agency")] <- "NASA Earth Science Enterprise/Japan Meteorological Agency/Brazilian Space Agency"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/Centre National d'Etudes Spatiales (CNES)/NOAA/EUMETSAT")] <- "NASA/Centre National d'Etudes Spatiales (CNES)/NOAA/EUMETSAT"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Space Development Agency(NASA)/Japan Aerospace Exploration Agency (JAXA)")] <- "National Space Development Agency (NASDA)/Japan Aerospace Exploration Agency (JAXA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NATO (North Atlantic Treaty Organization) (controlled by UK Ministry of Defence)")] <- "North Atlantic Treaty Organization (NATO)/Ministry of Defence (UK)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NBN Co (operated by Optus)")] <- "NBN Co"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Ohio State University/NASA")] <- "The Ohio State University/NASA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Optus Communications (Parent: Singapore Telecom)")] <- "Optus Communications"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Optus Communications (Parent: Singapore Telecom)/Australian Ministry of Defense")] <- "Optus Communications/Australian Ministry of Defense"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "People's Liberation Army (C41)")] <- "People's Liberation Army"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Scientific Production Corporation (joint stock creation of Russian Space Agency)")] <- "Scientific Production Corporation (ROSCOSMOS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "School of Aerospace Engineering - University of Rome")] <- "University of Roem School of Aerospace Engineering"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SpaceQuest")] <- "SpaceQuest, Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Spire")] <- "Spire Global Inc."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Telenor Satellite Broadcasting - leased to SES")] <- "Telenor Satellite Broadcasting"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Telesat Canada Ltd. (leased from DirecTV, Inc.)")] <- "Telesat Canada Ltd. (BCE, Inc.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "von Karman Institute")] <- "Von Karman Institute (VKI)"

combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Indonesian National Aeronautics and Space Agency (Lembaga Penerbangan dan Antariksa Nasional - LAPAN)")] <- "Indonesian National Aeronautics and  Space Agency (Lembaga Penerbangan dan Antariksa Nasional - LAPAN)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Earthi/Surrey Satellite Technology Ltd.")] <- "Earth-i/Surrey Satellite Technology Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "MIT/Lincoln Laboratory-University of Massachusetts  Amherst")] <- "MIT/Lincoln Laboratory-University of Massachusetts Amherst"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Authority for Remote Sensing and Space Sciences")] <- "National Authority for Remote Sensing and Space Science (NARSS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Spacety")] <- "Spacety Aerospace Co."


### Dealing with SES S.A. which is a particularly frustrating company to fix. There are several "short lived" shoot-off companies that get re-absorbed after a year or two
### so instead of keeping all of the names we are uniting them under the parent company SES S.A.

combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/SES Americom (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)])")] <- "NASA/SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Asia Satellite Telecommunications Co. Ltd. (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)])" | combined_csv$Entity_Name == "Asia Satellite Telecommunications Co. Ltd. (SES)" | combined_csv$Entity_Name == "Asia Satellite Telecommunications Co. Ltd. (SES [Société Européenne des Satellites (SES)])" | combined_csv$Entity_Name == "Asia Satellite Telecommunications Co. Ltd. (SES [Societe Europienne des Satellites])" |combined_csv$Entity_Name == "Asia Satellite Telecommunications Co. Ltd. (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)])")] <- "Asia Satellite Telecommunications Co. Ltd./SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Asia Satellite Telecommunications Co. Ltd. (SES Global)")] <- "Asia Satellite Telecommunications Co. Ltd./SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V. [SES Global]" | combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V. [SES S.A.]")] <- "Intelsat, Ltd./New Skies Satellites N.V. (SES S.A.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/SES Americom (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)])" | combined_csv$Entity_Name == "NASA/SES Americom (SES [Societe Europienne des Satellites])")] <- "NASA/SES Americom (SES S.A.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Aeronautics and Space Administration (NASA)/SES Americom (SES [Société Européenne des Satellites (SES)])")] <- "NASA/SES Americom (SES S.A.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES (Société Européenne des Satellites (SES))" | combined_csv$Entity_Name == "SES (Societe Europienne des Satellites)" | combined_csv$Entity_Name == "SES (Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES))" | combined_csv$Entity_Name == "SES S.A" | combined_csv$Entity_Name == "SES (Soci�t� Europ�enne des Satellites (SES))" | combined_csv$Entity_Name == "SES (Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES))")] <- "SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES (Societe Europienne des Satellites)/Intelsat (shared capacity)" | combined_csv$Entity_Name == "SES (Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES))/Intelsat (shared capacity)"  | combined_csv$Entity_Name == "SES (Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES))/Intelsat (shared capacity)")] <- "SES S.A./Intelsat (shared capacity)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES Americom (SES [Société Européenne des Satellites (SES)])" | combined_csv$Entity_Name == "SES Americom (SES [Societe Europienne des Satellites])" | combined_csv$Entity_Name == "SES Americom (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)])")] <- "SES Americom (SES S.A.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES World Skies (SES [Société Européenne des Satellites (SES)])" | combined_csv$Entity_Name == "SES World Skies" | combined_csv$Entity_Name == "SES World Skies (SES [Societe Europienne des Satellites])" | combined_csv$Entity_Name == "SES World Skies (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)])")] <- "SES World Skies (SES S.A.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES World Skies (SES [Société Européenne des Satellites (SES)]) -- total capacity leased to subsidiary of EchoStar Corp." | combined_csv$Entity_Name == "SES World Skies (SES [Societe Europienne des Satellites]) -- total capacity leased to subsidiary of EchoStar Corp." | combined_csv$Entity_Name == "SES World Skies (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)]) -- total capacity leased to subsidiary of EchoStar Corp.")] <- "SES World Skies (SES S.A.) (total capacity leased to subsidiary of EchoStar Corp.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES World Skies (SES [Societe Europienne des Satellites])/Intelsat (shared capacity)" | combined_csv$Entity_Name == "SES World Skies/Intelsat (SES [Societe Europienne des Satellites]) (shared capacity)" | combined_csv$Entity_Name == "SES World Skies/Intelsat [SES] (shared capacity)" )] <- "SES World Skies (SES S.A.)/Intelsat (shared capacity)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES/Intelsat (SES [Société Européenne des Satellites (SES)]) (shared capacity)" | combined_csv$Entity_Name == "SES/Intelsat (SES [Societe Europienne des Satellites]) (shared capacity)" | combined_csv$Entity_Name == "SES/Intelsat (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)]) (shared capacity)")] <- "SES S.A./Intelsat (shared capacity)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Star One (SES [Societe Europienne des Satellites])" | combined_csv$Entity_Name == "Star One (SES [Soci\xe9t\xe9 Europ\xe9enne des Satellites (SES)])" | combined_csv$Entity_Name == "Star One (SES Global)")] <- "Star One/SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V. (shared capacity)")] <- "Intelsat, Ltd./New Skies Satellites N.V."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V. [SES Global] (shared capacity)" | combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V. [SES Global]")] <- "Intelsat/SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V. [SES] (shared capacity)" | combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V. [SES S.A.]")] <- "Intelsat, Ltd./SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/SES Americom (SES Global)" | combined_csv$Entity_Name == "NASA/SES Americom (SES S.A.)")] <- "NASA/SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "New Skies Satellites N.V." | combined_csv$Entity_Name == "New Skies Satellites N.V. (SES [Societe Europienne des Satellites])")] <- "SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "New Skies Satellites N.V. (SES [Societe Europienne des Satellites])/Intelsat (shared capacity)" | combined_csv$Entity_Name == "New Skies Satellites N.V. [SES Global]/Intelsat (shared capacity)" | combined_csv$Entity_Name == "New Skies Satellites N.V./Intelsat (SES [Societe Europienne des Satellites])(shared capacity)" | combined_csv$Entity_Name == "New Skies Satellites N.V./Intelsat (shared capacity)" | combined_csv$Entity_Name == "New Skies Satellites N.V./Intelsat [SES Global] (shared capacity)" | combined_csv$Entity_Name == "New Skies Satellites N.V./Intelsat [SES Global](shared capacity)" | combined_csv$Entity_Name == "New Skies Satellites N.V./Intelsat [SES] (shared capacity)")] <- "Intelsat, Ltd./SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "New Skies Satellites N.V. (SES Global)")] <- "SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "O3b Networks Ltd." | combined_csv$Entity_Name == "Worldsat (SES Americom; SES Global)")] <- "SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SEA Astra (part of SES [Societe Europienne des Satellites])" | combined_csv$Entity_Name == "SES Astra (part of SES [Societe Europienne des Satellites])" | combined_csv$Entity_Name == "SES Astra (part of SES Global) (Societe Europienne des Satellites [SES])")] <- "SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES Americom (SES Global)" | combined_csv$Entity_Name == "SES Americom (SES S.A.)")] <- "SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES S.A. -- total capacity leased to subsidiary of EchoStar Corp." | combined_csv$Entity_Name == "SES.S.A.")] <- "SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES S.A./Intelsat (shared capacity)" | combined_csv$Entity_Name == "SES World Skies (SES S.A.)/Intelsat (shared capacity)")] <- "Intelsat/SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES World Skies (SES S.A.)" | combined_csv$Entity_Name == "SES World Skies (SES S.A.) (total capacity leased to subsidiary of EchoStar Corp.)")] <- "Alascom Inc. (AT&T)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES World Skies/Global Eagle Entertainment")] <- "SES S.A./Global Eagle Entertainment"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "SES World Skies/Gogo")] <- "SES S.A./Gogo"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Worldsat (SES Americom; SES Global)/Space Communications Corp.")] <- "SES S.A./Space Communications Corp."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "DoD/US Air Force")] <- "U.S. Air Force/DoD"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "U.S. Air Force/US Navy")] <- "U.S. Air Force/U.S. Navy"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "DoD/US Navy")] <- "U.S. Navy/DoD"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Chinese Academy of Sciences (CAS)/China Academy of Space Technology")] <- "Chinese Academy of Sciences (CAS)/China Academy of Space Technology (CAST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Operational Responsive Space (ORS) Office")] <- "Operational Responsive Space Office (ORSO)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Peacesat (PanPacific Education and Communications Experiment by Satellite) (loaned by NOAA)")] <- "Peacesat (PanPacific Education and Communications Experiment by Satellite)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Philippines’ Department of Science and Technology, University of the Philippines Diliman, and Japan’s Hokkaido University and Tohoku University")] <- "Philippines’ Department of Science and Technology/University of the Philippines Diliman/Hokkaido University/Tohoku University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Planetary Exploration Research Center (PERC) of the Chiba Institute of Technology, Tohoku University")] <- "Planetary Exploration Research Center (PERC), Chiba Institute of Technology/Tohoku University"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Sistema Espacial para Realiza\xe7\xe3o de Pesquisa e Experimentos com Nanossat\xe9lites")] <- "Sistema Espacial para Realizacao de Pesquisas e Experimentos com Nanossatelites (SERPENS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Space Technology Institute-Vietnam Academy of Science and Technology (STI-VAST)")] <- "Space Technology Institute, Vietnam Academy of Science and Technology (STI-VAST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "TeamStudSat - Nitte Meenakshi Institute of Technology (ground station)")] <- "Team STUDSAT, Nitte Mennakshi Institute of Technology (NMIT)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Telemetry Tracking and Command Network (ISTRAC) Centre")] <- "ISRO Telemetry Tracking and Command Network (ISTRAC)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Coast Guard")] <- "U.S. Coast Guard"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "ORBCOMM Inc./US Coast Guard")] <- "ORBCOMM Inc./U.S. Coast Guard"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US DoD/Royal Australian Navy/Panamsat (owner)")] <- "DoD/Royal Australian Navy/Panamsat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "US Naval Academy Satellite Laboratory")] <- "U.S. Naval Academy Satellite Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Applied Physics Laboratory/NASA")] <- "NASA/Johns Hopkins University Applied Physics Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "AprizeSat")] <- "Aprize Satellite"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "China Academy of Space Technology (CAST)")] <- "Chinese Academy of Space Technology (CAST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Hellas-Sat Consortium Ltd./INMARSAT")] <- "Hellas-Sat Consortium Ltd./INMARSAT, Ltd."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Jet Propulsion Laboratory (NASA)/California Institute of Technology")] <- "NASA Jet Propulsion Laboratory/California Institute of Technology"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA Goddard Space Flight Center/Jet Propulsion Laboratory")] <- "NASA Goddard Space Flight Center/NASA Jet Propulsion Laboratory"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/Yonsei University/KARI")] <- "NASA/Yonsei University/Korea Aerospace Research Institute (KARI)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "PanAmSat (Intelsat, S.A.)")] <- "PanAmSat (Intelsat S.A.)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "National Oceanographic and Atmospheric Administration (NOAA)")] <- "NOAA (National Oceanographic and Atmospheric Administration)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "University of California, Berkeley/Korea Advanced Institute of Science and Technology")] <- "University of California, Berkeley/Korea Advanced Institute of Science and Technology (KAIST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "University of Vigo/Alén Space")] <- "University of Vigo/Alen Space"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Shanghai Academy of Space Technology (SAST)" | combined_csv$Entity_Name == "Shanghai Academy of Spaceflight Technology")] <- "Shanghai Academy of Spaceflight Technology (SAST)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "JSAT Corporation")] <- "Sky Perfect JSAT Corporation"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "DARPA (Defense Advanced Research Projects Agency)")] <- "Defense Advanced Research Projects Agency (DARPA)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "DARPA/Tethers Unlimited")] <- "Defense Advanced Research Projects Agency (DARPA)/Tethers Unlimited"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Defence Research and Development Organization")] <- "Defence Research and Development Organization (DRDC"


#Cleaning up cooperations
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA/Centre National d'Etudes Spatiales (CNES)")] <- "Centre National d'Etudes Spatiales (CNES)/NASA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Japan Aerospace Exploration Agency (JAXA)/NASA")] <- "NASA/JAXA"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Institute of Space and Astronautical Science (ISAS)/Japan Aerospace Exploration Agency (JAXA)")] <- "Japan Aerospace Exploration Agency (JAXA)/Institute of Space and Astronautical Science (ISAS)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Nanyang Technological University/Kyushu Institute of Technology")] <- "Kyushu Institute of Technology (KIT)/Nanyang Technological University (NTU)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "NASA Earth Science Office/University of Colorado Laboratory for Atmospheric and Space Physics")] <- "NASA Earth Science Office/Laboratory for Atmospheric and Space Physics, Univ. of Colorado"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Space Sciences Laboratory, UC Berkeley/NASA")] <- "NASA/Space Sciences Laboratory, UC Berkeley"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Russian Space Agency (RKA)/Defense Ministry")] <- "Russian Space Agency (RKA)/Ministry of Defense"



#Replacement characters - staying strong
#Caveat of this method: In future datasets, newly added operator names that start with the same phrase are changed (unlikely to occur)
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Asia Satellite Telecommunications Co. Ltd. (SES [Soci") == TRUE)] <- "Asia Satellite Telecommunications Co. Ltd./SES S.A."
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Centre National d'Etudes Spatiales (CNES)/D") == TRUE)] <- "Centre National d'Etudes Spatiales (CNES)/Delegation Generale de l'Armement (DGA)"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "CTRS Morocco/Institut f") == TRUE)] <- "CTRS Morocco/Institut fur Luft- und Raumfahrttechnik (Berlin)"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Deutsches Zentrum f") == TRUE)] <- "Deutsches Zentrum fur Luft- und Raumfahrt"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Ecole Polytechnique F") == TRUE)] <- "Ecole Polytechnique Federale de Lausanne (EPFL)"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Facultad de Ingenier") == TRUE)] <- "Facultad de Ingenieria de la Universidad de la Republica (FING)/ANTEL"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Institut f") == TRUE)] <- "Institut fur Luft- und Raumfahrt"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Instituto Nacional de T") == TRUE)] <- "Instituto Nacional de Tecnica Aeroespacial"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "NASA/SES Americom (SES ") == TRUE)] <- "NASA/SES S.A."
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "SES Americom (SES ") == TRUE)] <- "SES S.A."
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "SES World Skies (SES ") == TRUE)] <- "SES World Skies (SES S.A.)"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "SES/Intelsat (SES [Soci") == TRUE)] <- "SES S.A./Intelsat (shared capacity)"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Sistema Espacial para Realiza") == TRUE)] <- "Sistema Espacial para Realizacao de Pesquisa e Experimentos com Nanossatelites"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Star One (SES [Soc") == TRUE)] <- "Star One/SES S.A."
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Institute for Radio Astronomy at Pontificia Universidad Cat") == TRUE)] <- "Institute for Radio Astronomy at Pontificia Universidad Catolica del Peru (PUCP)"

#More special cases ...
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "Nanjing") & endsWith(combined_csv$Entity_Name, "Astronautics"))] <- "Nanjing University of Aeronautics and Astronautics"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "SES (Soci") & endsWith(combined_csv$Entity_Name, "enne des Satellites (SES))"))] <- "SES S.A."
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "SES (Soci") & endsWith(combined_csv$Entity_Name, "enne des Satellites (SES))/Intelsat (shared capacity)"))] <- "SES S.A./Intelsat (shared capacity)"
combined_csv$Entity_Name[which(startsWith(combined_csv$Entity_Name, "University of W") & endsWith(combined_csv$Entity_Name, "burg"))] <- "University of Wurzburg"

#Intelsat, PanAmSat
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat S.A." | combined_csv$Entity_Name == "Intelsat, Ltd." | combined_csv$Entity_Name == "Intelsat, S.A." | combined_csv$Entity_Name == "PanAmSat (Intelsat S.A.)" | combined_csv$Entity_Name == "PanAmSat (Intelsat, Ltd.)")] <- "Intelsat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./Sky Perfect JSAT Corp." | combined_csv$Entity_Name == "Intelsat, S.A./Sky Perfect JSAT Corp." | combined_csv$Entity_Name == "PanAmSat (Intelsat S.A.)/Sky Perfect JSAT Corp." | combined_csv$Entity_Name == "PanAmSat (Intelsat, Ltd.)/Sky Perfect JSAT Corp." | combined_csv$Entity_Name == "PanAmSat (Intelsat, Ltd.)/JSAT Corp." | combined_csv$Entity_Name == "Horizons 2 Satellite, LLC (Intelsat, JSAT Corporation)" | combined_csv$Entity_Name == "Intelsat S.A./Sky Perfect JSAT Corp." | combined_csv$Entity_Name == "PanAmSat/JSAT Corp.")] <- "Intelsat/Sky Perfect JSAT Corporation."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "PanAmSat (Intelsat, Ltd.)/DirecTV L.A." | combined_csv$Entity_Name == "PanAmSat (Intelsat S.A.)/DirecTV, Inc.")] <- "Intelsat/DirecTV Inc."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Horizons 2 Satellite, LLC (Intelsat, JSAT Corporation)")] <- "Horizons 2 Satellite, LLC (Intelsat, JSAT Corporation)"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./SES S.A." | combined_csv$Entity_Name == "SES S.A./Intelsat (shared capacity)")] <- "Intelsat/SES S.A."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./New Skies Satellites N.V.")] <- "Intelsat./New Skies Satellites N.V."
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Intelsat, Ltd./Telenor")] <- "Intelsat/Telenor"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "DoD/Royal Australian Navy/Panamsat")] <- "DoD/Royal Australian Navy/Intelsat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "PanAmSat")] <- "Intelsat"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "PanAmSat/Telesat Canada")] <- "Intelsat/Telesat Canada"
combined_csv$Entity_Name[which(combined_csv$Entity_Name == "Royal Australian Navy/Panamsat")] <- "Royal Australian Navy/Intelsat"


#### Taking a closer look at the data
#unnamed <- which(is.na(combined_csv$Entity_Name) == TRUE)

#names_to_clean <- unique(levels(as.factor(combined_csv$Entity_Name)))
#View(names_to_clean)


### Lists the unique entries of specific columns
#unique(levels(as.factor(combined_csv$Class_of_Orbit))) # Check that the number of levels can't be easily reduced further
#unique(levels(as.factor(combined_csv$Entity_Name))) # Check that the number of levels can't be easily reduced further


#record linkage approach to find similar entries and manually check them
#names_to_clean[agrep("Ecole Polytechnique", names_to_clean)]

#for all entity names, list the entity and all similar entries (if applicable)
#for (i in 2:length(names_to_clean))
#{
#  print(names_to_clean[agrep(names_to_clean[i], names_to_clean)])
#}

