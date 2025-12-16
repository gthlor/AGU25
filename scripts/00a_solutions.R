setwd("C:/Users/loren/Desktop/AGU25/SampleData")

library(terra)

# For plotting operations
library(tidyterra) 
library(tmap)
library(ggplot2)
library(mapview) 


# 1.  Load the SM (SMAP_L3_USA.nc) & NDVI (NDVI_resamp_yyyy_mm_dd.tif) raster datasets.

sm <- terra::rast("./SMAP_L3_USA.nc")
plot(sm)

ndvi.files.list <- list.files("./raster_files/NDVI/", pattern='*.tif', full.names=TRUE)
rast.list  <- lapply(ndvi.files.list, rast)
rast.stack <- rast(rast.list)
plot(rast.stack)

# 2. Get CONUS shapefile from spData() library.
# state_shapefile = read_sf("./USA_states/cb_2018_us_state_5m.shp")
us_states <- spData::us_states

conus <- us_states[!(us_states$NAME %in% c("Alaska","Hawaii","American Samoa",
                                                      "United States Virgin Islands","Guam", "Puerto Rico",
                                                      "Commonwealth of the Northern Mariana Islands")),] 
plot(conus)

# 3. Crop both SM & NDVI rasters for the following states: Washington, Texas & Florid
AOI <- us_states[(us_states$NAME %in% c("Washington","Texas", "Florida")),]

states_trim = crop(rast.stack, ext(AOI))                # Crop raster
ndvi_mask_states = terra::mask(states_trim, vect(AOI))  # Mask

mypal2 = cetcolor::cet_pal(20, name = "r2")  
plot(ndvi_mask_states[[1]], 
     col=mypal2, 
     fun=function(){plot(vect(conus), add=TRUE)} # Add background states
)

states_trim = crop(sm, ext(AOI))
sm_mask_states = terra::mask(states_trim, vect(AOI)) 
plot(sm_mask_states[[3]], 
     col=mypal2, 
     fun=function(){plot(vect(conus), add=TRUE)} # Add background states
)


# 4. Plot the statewise (cropped) raster for mean SM and mean NDVI using tmap.
ndvi_mean_states <- app(ndvi_mask_states, mean, na.rm=TRUE)
tm_shape(ndvi_mean_states) +
  tm_raster(palette = "Greens", title = "NDVI") +
  tm_shape(vect(AOI)) +
  tm_borders() +
  tm_layout(title = "Mean NDVI for Selected States")

sm_mean_states <- app(sm_mask_states, mean, na.rm=TRUE)
tm_shape(sm_mean_states) +
  tm_raster(palette = "Blues", title = "Soil Moisture (m3/m3)") +
  tm_shape(vect(AOI)) +
  tm_borders() +
  tm_layout(title = "Mean Soil Moisture for Selected States")

# 5. Resample the raster datasets to make sure the spatial and temporal resolution is consistent.

sm_resamp_states <- terra::resample(sm_mask_states, ndvi_mask_states, method='bilinear')

nm <- names(sm_resamp_states)
time_num <- as.numeric(sub(".*Time=", "", nm))
dates <- as.Date(time_num, origin = "1970-01-01")
names(sm_resamp_states) <- dates

names(ndvi_mask_states) <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})$", "\\1", names(ndvi_mask_states)))

idx_2016 <- format(as.Date(names(sm_resamp_states)), "%Y") == "2016"
sm_2016 <- sm_resamp_states[[idx_2016]]

# groups
sm_month <- as.integer(format(as.Date(names(sm_2016)), "%m"))
ndvi_month <- as.integer(format(as.Date(names(ndvi_mask_states)), "%m"))

# resemp by month
sm_monthly <- terra::tapp(sm_2016, sm_month, mean, na.rm=TRUE)
ndvi_monthly <- terra::tapp(ndvi_mask_states, ndvi_month, max, na.rm=TRUE)

# 6. Extract mean values and merge them in one dataframe.

# 7. Write a custom function to find seasonal (Pearson/Linear) correlation between SM & NDVI.
# 8. Find Soil Moisture and NDVI anomaly.
# 9. Plot the anomaly time series.
# 10. Is the seasonal anomaly correlation similar to seasonal absolute correlation? Why or why not?
  