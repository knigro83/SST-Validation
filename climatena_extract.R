library(foreign)

dem <- rast("C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/spatial/GMTED2010N30W120_150/30n120w_20101117_gmted_mea150.tif")

##engelmann spruce elevations
adjusted_coords<- read.csv("C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/PIENproject/PIEN_corrected_coords.csv")
coords_vect<- vect(adjusted_coords, geom=c("long_corrected","lat_corrected"), crs=crs(dem))
adjusted_coords_ele<- terra::extract(dem, coords_vect, method="simple")


plot(dem)

us <- geodata::gadm(country = "USA",  level = 1, resolution = 2,
                    path = "C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/PIENproject")
include_states<- c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","South Dakota","North Dakota","Nebraska","Utah","Wyoming","California","Oregon","Washington","Kansas","Oklahoma","Texas")
westernus <- us[match(toupper(include_states),toupper(us$NAME_1)),]

canada <- geodata::gadm(country = "Canada",  level = 1, resolution = 2,
                        path = "C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/PIENproject")
include_provinces<- c("British Columbia","Alberta","Saskatchewan")
westernca <- canada[match(toupper(include_provinces), toupper(canada$NAME_1))]

western_na <- rbind(westernca, westernus)
plot(western_na)

dem_crop <- mask(dem, western_na)

plot(dem_crop)

table1 <- as.data.frame(dem_crop) %>% 
  bind_cols(crds(dem_crop))

head(table1)

colnames(table1)<-c("el","long","lat")

table1<- table1 %>% 
  dplyr::select("lat","long","el")

write.csv(table1, "C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/SST_validation/table1.csv", row.names=FALSE, quote=FALSE)

###convert DEM to ASCII file
library(terra)
tif <- rast("C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/spatial/GMTED2010N30W120_150/30n120w_20101117_gmted_mea150.tif")

NAflag(tif)<- -9999
NAflag(tif)

writeRaster(tif, 'C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/ClimateNA_v6a/ClimateNA_v6.40a (updated 2021-09-15)/30n120w_20101117_gmted_mea150.asc',overwrite=TRUE, NAflag=-9999)  

dem_asc<- rast("C:/Users/KatherineNigro/Box/01. katherine.nigro Workspace/ClimateNA_v6a/ClimateNA_v6.40a (updated 2021-09-15)/30n120w_20101117_gmted_mea150.asc")
NAflag(dem_asc) <- -9999
NAflag(dem_asc)
dem.asc
