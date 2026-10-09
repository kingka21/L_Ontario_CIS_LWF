
#### raster manipulation 

#### load libraries #### 
library(sf)
library(dplyr)
library(raster)
library(PerformanceAnalytics) #for looking at correlation matrix 


#### Ontario spatial extent #### 
ont_shp<-sf::st_read("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/Ontario_shape", layer = "ontario_shp")
crs(ont_shp)
#ont_shp<-st_transform(ont_shp, prjnew)
ont_box<-st_bbox(ont_shp) 
ext<-extent(ont_box)
#plot(ont_shp)


#### rasters #### 

#1 historical substrate from TNC 
substrate_raw<-raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/hist_substrate.tif")
substrate_raw #100m  
substrate_raw<-projectRaster(substrate_raw, crs=crs(shoreline_ont), method="ngb") #substrate must be projected to shared extent and method nearest neighbor (ngb) used for categorical variables 
unique(substrate_raw) #check that it is still whole numbers 
substrate_ont <- crop(substrate_raw, ext) #then cropped
substrate_ont
plot(substrate_ont)


#2 ont bathy from GLAHF 
bathy_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/bathy_ontario.tif"), ext)
bathy_ont #30 m 
plot(bathy_ont)

#3 fetch - read in all of the fetch layers from Mason et al and take the mean 
fetch_list <- list.files("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/Mason2018_Ontario_fetch", pattern = ".tif$", full.names = T)  # 
fetch_raw <- raster::stack(fetch_list)
fetch_raw #30 m 
fetch_mean <- crop(calc(fetch_raw, fun = mean, na.rm = T), ext)
fetch_mean <- projectRaster(fetch_mean, crs=crs(shoreline_ont))
plot(fetch_mean)

#4 tributary influence from GLAHF
trib_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/tribinfl.tif"), ext)
trib_ont #30 m 
plot(trib_ont)

#5 reef predictions 
reefs_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/lake_ontario_reef_predictions/lake_ontario_reef_predictions.tif"), ext)
reefs_ont <- projectRaster(reefs_ont, crs=crs(shoreline_ont))
reefs_ont #30 m 
plot(reefs_ont) 

#6ice duration coefficient of variation from 5 years 
ice_dur_cv_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/ice_cv_ontario.tif"), ext)
ice_dur_mean_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/ice_dur_mean_ontario.tif"), ext)
ice_dur_cv_ont #1800 m 
ice_dur_cv_ont <- projectRaster(ice_dur_cv_ont, crs=crs(shoreline_ont))
ice_dur_mean_ont <- projectRaster(ice_dur_mean_ont, crs=crs(shoreline_ont))
plot(ice_dur_cv_ont)
plot(ice_dur_mean_ont)

#7 distance to wetlands: delta, protected, and open water
wetland_delta_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/wetdelta_dist.tif"), ext)
wetland_delta_ont <- projectRaster(wetland_delta_ont, crs=crs(shoreline_ont))
wetland_prot_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/wetprot_dist.tif"), ext)
wetland_prot_ont <- projectRaster(wetland_prot_ont, crs=crs(shoreline_ont))
wetland_open_ont<-crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/wetopen_dist.tif"), ext)
wetland_open_ont <- projectRaster(wetland_open_ont, crs=crs(shoreline_ont))

wetland_delta_ont #90m 
plot(wetland_delta_ont)
plot(wetland_prot_ont)
plot(wetland_open_ont)

#8 circulation data 
circ_list <- list.files("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/circulation_ras/ontario", pattern = ".tiff", full.names = T)  # 
circ_raw <- raster::stack(circ_list)
circ_raw #1800 m 
circ_raw_mean <- crop(calc(circ_raw, fun = mean, na.rm = T), ext)
circ_raw_mean <- projectRaster(circ_raw_mean, crs=crs(shoreline_ont))
plot(circ_raw_mean)

#9 bottom slope 
slope <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/MI_ON_combined/ont_mi_slope.tif"), ext)  # 
slope <- projectRaster(slope, crs=crs(shoreline_ont))
plot(slope)

#contemporary ice 
ice_contemp <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/ice_dur_2000_2014.tif"), ext)  # 
ice_contemp <- projectRaster(ice_contemp, crs=crs(shoreline_ont))
plot(ice_contemp)

#10river dist
river_dist <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/rasters/distance_to_rivermouth_strahler5.tiff"), ext)  # 
river_dist <- projectRaster(river_dist, crs=crs(shoreline_ont))
plot(river_dist)

#11 upwelling
upwelling <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/ontario_mean_upwelling.tif"), ext)  # 
upwelling <- projectRaster(upwelling, crs=crs(shoreline_ont))
plot(upwelling)

#12 extrapolated reef 
reef_extrap <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/ontario_reef_extrap.tif"), ext)  # 
reef_extrap <- projectRaster(reef_extrap, crs=crs(shoreline_ont))
plot(reef_extrap)

#13 extrapolated ice mean duration historical 
hist_ice_extrap <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/hist_ice_extrap.tif"), ext)  # 
hist_ice_extrap <- projectRaster(hist_ice_extrap, crs=crs(shoreline_ont))
plot(hist_ice_extrap)

#14 extrapolated ice mean duration contemporary  
contemp_ice_extrap <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/ont_contemp_ice_dur_extrap.tif"), ext)  # 
hist_ice_extrap <- projectRaster(contemp_ice_extrap, crs=crs(shoreline_ont))
plot(contemp_ice_extrap)

#15 extrapolated bathymetry 
bathy_extrap <- crop(raster("/Users/katelynking/Desktop/UofM/Coregonine/cor_data/Ontario/ont_bathymetry_extend.tif"), ext)  # 
bathy_extrap <- projectRaster(bathy_extrap, crs=crs(shoreline_ont))
plot(bathy_extrap)

#resample variables 
ice_dur_cv_ont <- terra::resample(ice_dur_cv_ont, shoreline_ont) 
ice_dur_mean_ont <- terra::resample(ice_dur_mean_ont, shoreline_ont)
substrate_ont <- terra::resample(substrate_ont, shoreline_ont, method="ngb") 
wetland_delta_ont <- terra::resample(wetland_delta_ont, shoreline_ont) 
wetland_prot_ont <- terra::resample(wetland_prot_ont, shoreline_ont)
wetland_open_ont <- terra::resample(wetland_open_ont, shoreline_ont)
reefs_ont<- terra::resample(reefs_ont, shoreline_ont)
trib_ont<- terra::resample(trib_ont, shoreline_ont)
fetch_mean<- terra::resample(fetch_mean, shoreline_ont)
bathy_ont<- terra::resample(bathy_ont, shoreline_ont)
circ_raw_mean<-terra::resample(circ_raw_mean, shoreline_ont)
slope<-terra::resample(slope, shoreline_ont)
ice_contemp<-terra::resample(ice_contemp, shoreline_ont)
river_dist<-terra::resample(river_dist, shoreline_ont)
upwelling<-terra::resample(upwelling, shoreline_ont)
reef_extrap<-terra::resample(reef_extrap, shoreline_ont)
hist_ice_extrap<-terra::resample(hist_ice_extrap, shoreline_ont)
contemp_ice_extrap<-terra::resample(contemp_ice_extrap, shoreline_ont)
bathy_extrap<-terra::resample(bathy_extrap, shoreline_ont)

plot(bathy_extrap)
#* write rasters ####
writeRaster(bathy_extrap,
            filename="data/ont_rasters/bathy_extrap.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)


writeRaster(contemp_ice_extrap,
            filename="data/ont_rasters/contemp_ice_extrap.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(hist_ice_extrap,
            filename="data/ont_rasters/hist_ice_extrap.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(reef_extrap,
            filename="data/ont_rasters/reef_extrap.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(upwelling,
            filename="data/ont_rasters/upwelling.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)


writeRaster(river_dist,
            filename="data/ont_rasters/river_dist.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(ice_contemp,
            filename="data/ont_rasters/ice_contemp.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(slope,
            filename="data/ont_rasters/slope.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(wetland_prot_ont,
            filename="data/ont_rasters/wetland_prot.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(wetland_delta_ont,
            filename="data/ont_rasters/wetland_delta.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(wetland_open_ont,
            filename="data/ont_rasters/wetland_open.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(substrate_ont,
            filename="data/ont_rasters/substrate_hist.asc", 
            format="ascii" ## the output format
)
writeRaster(ice_dur_cv_ont,
            filename="data/ont_rasters/ice_dur_cv.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(ice_dur_mean_ont,
            filename="data/ont_rasters/ice_dur_mean.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(reefs_ont,
            filename="data/ont_rasters/reef.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(fetch_mean,
            filename="data/ont_rasters/fetch.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(bathy_ont,
            filename="data/ont_rasters/bathy.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(trib_ont,
            filename="data/ont_rasters/trib.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

writeRaster(circ_raw_mean,
            filename="data/ont_rasters/circ_raw_mean.asc", 
            overwrite=TRUE,
            format="ascii" ## the output format
)

#### correlation plots #### 
raster_stack<-raster::stack(ice_dur_cv_ont, ice_dur_mean_ont, substrate_ont, wetland_delta_ont, wetland_prot_ont, wetland_open_ont, reefs_ont, trib_ont, fetch_mean, bathy_ont,  shoreline_ont, circ_raw_mean) 

ras_value<-extract(raster_stack, ont_bloat_aea)
point_values<-as.data.frame(cbind(ont_bloat_aea,ras_value)) %>% 
  rename(reefs=mean, fetch=layer.1, circ=layer.2) %>% 
  mutate(hist_substrate = as.factor(hist_substrate)) 

# look at correlation among driver variables
my_data <- point_values[, c(15:16, 18:24, 26)] #leave out substrate because it is a factor
PerformanceAnalytics::chart.Correlation(my_data, histogram=TRUE, pch=19)
summary(my_data)

# background data 
set.seed(1) 
bg <- sampleRandom(x=raster_stack,
                   size=10000, #total cells 30011685 (30 MIL) in study area 
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 
a <- extract(raster_stack, bg, df=TRUE)%>% 
  dplyr::select(-c(ID))%>% 
  rename(reefs=mean, fetch=layer.1, circ=layer.2) %>% 
  mutate(hist_substrate = as.factor(hist_substrate))

bg_data <- a[, c(1:4, 7, 9,11,12,15:18)] #leave out substrate because it is a factor
PerformanceAnalytics::chart.Correlation(bg_data, histogram=TRUE, pch=19)
