#Climate Match Scores function

#steps
## 1. read in csv and make provenances and planting sites into spatial point data (spat_vector)
## 2. buffer all points by 4km
## 3. read in western US digital elevation model (DEM), crop and mask it to buffered points
## 4. use ClimateNAr() function to download climate data in masked footprint for desired time periods/scenarios and climate variables
## 5. extract climate data from all planting sites and provenances [ add cols ref mcmt, ref shm, target mcmt, target shm]
## 6. calculate climate match scores with climate_match() function that I wrote [add cols delta and match score]
## 7. put data in long format (optional)

#inputs
## dataframe with provenance and planting site lat/longs -- make dataframe with all transfers in the analysis across all studies
      #columns: prov name, plant name, prov loc, plant loc
## planting site names 
## western US DEM
## reference time period
## target time period(s)
## climate variables for calculating match

#output
##dataframe with columns: prov name, plant name, prov loc, plant loc, ref mcmt, ref shm, target mcmt, targt shm, 
##                        delta_mcmt, delta_shm, match score