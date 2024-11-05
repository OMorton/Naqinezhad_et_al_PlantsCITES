# Naqinezhad, Morton and Edwards (2024).

Scripts and data for Naqinezhad, Morton and Edwards (2024 in press) **"Increasing timber and declining live plant diversity and volumes in global trade from 2000 to 2020"** *Communications Earth and Environment*, DOI: (To be added).

*Note 1 - files/folders ending with E refer to scripts related to processing exporter reported data
and files/folders without this refer to scripts processing the importer reported data.The approach
used in both sets of scripts is identical they just use differently reported data.*

*Note 2 - This repo contains processed data used in the analyses (within the Outputs folder). But all orginal raw data is openly available and frequently updated. The CITES Trade Database (https://trade.cites.org/) note this analysis used Version 2022 v1 subsequent versions have now been published, The IUCN Red List (https://www.iucnredlist.org/) and the "rredlist" API (https://cran.r-project.org/web/packages/rredlist/index.html), the record of current and past CITES Listings (https://www.speciesplus.net/species) and the TRY Plant Trait Database (https://www.try-db.org/TryWeb/Home.php).*

*Note 3 - Supplementary Table 2 can be found in the outputs folder.*

## Structure

### Data_Preparation
These scripts process the raw CITES bulk download trade database and process this to reconcile units across woody species traded as timber, live traded species and plant derived products. This also deals with species presence and absence in the CITES listings through time, unit conversion, taxonomy and appending Red List statuses.

### Modelling
These scripts demonstrate the modelling approach used, using the data resulting from Data_Preparation and provided in Outputs.

### Interpretation
This contains code to reproduce the key figures and maps. Note minor aesthetic differences may be present between this version and the formatted published figures. Contact om403@cam.ac.uk for any queries.