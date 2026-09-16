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


## 1. make provenances and planting sites into spatial point data (spat_vector)


#need to have these before running function:
# dem_west & us already in environment 
# data
# varList -- a list (c()) of climate vars that you want to use to calculate match; e.g., c("MCMT","SHM")
# refperiod -- the reference climate period (e.g., "Normal_1961_1990.nrm")
# targetperiod -- the target periods you want to calculate match for. Must be a list (e.g., c(('Normal_1961_1990.nrm','8GCMs_ensemble_ssp245_2011-2040.gcm')))
# 

## 1. make provenances and planting sites into spatial point data (spat_vector)

getmatchscores<- function(data, varList, refperiod, targetperiod, mcmt_TL){
all_vect <- bind_rows(
  data %>% 
    select(c(planting_site_name, planting_site_lat, planting_site_lon)) %>% 
    unique() %>% 
    rename(lat = planting_site_lat, lon = planting_site_lon, name=planting_site_name) %>% 
    mutate(site_type = "planting site")
  ,
  data %>% 
    select(c(provenance_name, provenance_lat, provenance_lon)) %>% 
    unique() %>% 
    rename(lat = provenance_lat, lon = provenance_lon, name=provenance_name) %>% 
    mutate(site_type="provenance")) %>% 
  filter(!is.na(lat)) %>% 
  vect(geom=c("lon","lat"), crs=crs(us))

#writeVector(pipo_nigro_all_vect, "C:/Users/katherinenigro/Box/01. katherine.nigro Workspace/SST_validation/data/PIPO/nigro/pipo_nigro_all_vect.shp")

plantation_vect <- data %>% 
  select(c(planting_site_name, planting_site_lat, planting_site_lon)) %>% 
  unique() %>% 
  filter(!is.na(planting_site_lat)) %>% 
  vect(geom=c("planting_site_lon","planting_site_lat"), crs=crs(us))

provs_vect <- data %>% 
  select(c(provenance_name, provenance_lat, provenance_lon)) %>% 
  unique() %>% 
  filter(!is.na(provenance_lat)) %>% 
  vect(geom=c("provenance_lon","provenance_lat"), crs=crs(us))


## 2. buffer all points by 4km

all_buffered <- buffer(all_vect, width=4000)

dem_pts_crop <- crop(dem_west, all_buffered)
dem_pts_mask <- mask(dem_pts_crop, all_buffered)

writeRaster(dem_pts_mask, "C:/Users/katherinenigro/Box/01. katherine.nigro Workspace/SST_validation/data/gmted_mea150_crop_bufferedpts.tif", overwrite=TRUE)

##get climatena data
inputFile = "C:/Users/katherinenigro/Box/01. katherine.nigro Workspace/SST_validation/data/gmted_mea150_crop_bufferedpts.tif"
varList= varList
periodList= c(refperiod, targetperiod)
outDir= 'C:/Users/katherinenigro/Box/01. katherine.nigro Workspace/SST_validation/data'
data_climatena <- ClimateNAr::ClimateNAr(inputFile,periodList,varList,outDir) 

##get all TIF files you just pulled

tif_files <- list.files(
  path = "C:/Users/katherinenigro/Box/01. katherine.nigro Workspace/SST_validation/data/gmted_mea150_crop_bufferedpts", 
  pattern = "\\.tif$", 
  recursive = TRUE, 
  full.names = TRUE
)

climate_stack<- rast(tif_files)
names(climate_stack)<- paste(str_match(tif_files, ".*/(.*)/.*$")[, 2],str_match(tif_files, "/([^/]+)\\.tif$")[,2],sep="_")

plantation_clim <- terra::extract(climate_stack, plantation_vect, method="simple") %>% 
  bind_cols(as.data.frame(plantation_vect)) %>% 
  bind_cols(crds(plantation_vect))
provenance_clim <- terra::extract(climate_stack, provs_vect, method="simple") %>% 
  bind_cols(as.data.frame(provs_vect)) %>% 
  bind_cols(crds(provs_vect)) 

##get match scores
transfers <- data %>% 
  select(provenance_name, planting_site_name) %>% 
  unique()

m_output <- data.frame()

for(i in 1:nrow(transfers)){
provclim = provenance_clim %>% 
  filter(provenance_name==transfers[i,1])
plantclim = plantation_clim %>% 
  filter(planting_site_name==transfers[i,2])

m <- climate_match(provclim, plantclim, mcmt_TL = mcmt_TL, shm_TL = plantclim$Normal_1961_1990_SHM, plant_period=targetperiod) %>% 
  mutate(planting_site_name = transfers[i,2])

m_output <- bind_rows(m_output, m)
}

return(m_output)}
