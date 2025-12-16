# link: https://vinit-sehgal.github.io/lgar/
  


## GET SAMPLE DATA ----
download.file(url = "https://github.com/Vinit-Sehgal/SampleData/archive/master.zip",
              destfile = "SampleData-master.zip")    # Download ".Zip"

# Unzip the downloaded .zip file
unzip(zipfile = "SampleData-master.zip") ### Make sure to unzip!!

## INSTALL LIBS and setup workdir ----
lib_names=c("terra", "tidyterra", "cetcolor", "scico", "tmap",    
            "gifski", "lubridate","Rcpp",
            "raster","ggplot2","unikn","mapview",
            "gridExtra","rgdal","fields",
            "RColorBrewer","ncdf4","rasterVis",
            "rcartocolor","pacman","purrr","moments","tictoc", 
            "sf", "sp", "exactextractr","readxl", 
            "snow","future.apply","parallel")

invisible(suppressMessages
          (suppressWarnings
            (lapply
              (lib_names, require, character.only = T))))


setwd("C:/Users/loren/Desktop/AGU25/SampleData")

## Chapter 03 ----
# For raster operations
library(terra)

# For plotting operations
library(tidyterra) 
library(tmap)
library(ggplot2)
library(mapview)  

# For Perceptually Uniform Colour palettes
library(cetcolor)
library(scico)

# Import SMAP soil moisture raster from the downloaded folder
sm=terra::rast("./raster_files/SMAP_SM.tif")
print(sm)

# Try:
# dim(sm)   # Dimension (nrow, ncol, nlyr) of the raster
# terra::res(sm)   # X-Y resolution of the raster
# terra::ext(sm)   # Spatial extent of the raster
# terra::crs(sm)   # Coordinate reference system

# Basic Raster plot
terra::plot(sm, main = "Soil Moisture")

### 3.1 scientific color palettes ----
# Make custom color palette
library(unikn) 

#~~ A) User defined color palette using scico library
mypal1 = scico(20, alpha = 1.0, direction =  -1, palette = "vik")
unikn::seecol(mypal1)

#Check: scico_palette_names() for available palettes!
# Try combinations of alpha=0.5, direction =1, and various different color palette

#~~ B) User defined color palette using cetcolor library
mypal2 = cetcolor::cet_pal(20, name = "r2")  
unikn::seecol(mypal2)

# Or reverse color pal
mypal2 = rev(cetcolor::cet_pal(20, name = "r2") ) 
unikn::seecol(mypal2)

terra::plot(sm,
            main = "Scientific Plot of Raster",
            
            #Color options
            col = mypal1,                    # User Defined Color Palette
            breaks = seq(0, 1, by=0.1),      # Sequence from 0-1 with 0.1 increment
            colNA = "lightgray",             # Color of cells with NA values
            
            # Axis options      
            axes=TRUE,                       # Plot axes: TRUE/ FALSE
            xlim=c(-180, 180),               # X-axis limit
            ylim=c(-90, 90),                 # Y-axis limit
            xlab="Longitude",                # X-axis label
            ylab="Latitue",                  # Y-axis label
            
            # Legend options      
            legend=TRUE,                     # Plot legend: TRUE/ FALSE
            
            # Miscellaneous
            mar = c(3.1, 3.1, 2.1, 7.1),     #Margins
            grid = FALSE                     #Add grid lines
)


#Plotting through tmap:
tmap_mode("plot")  #Setting tmap mode: Static plots by "plot", Interactive plots by"view"

tmap_SM = tm_shape(sm) +
  tm_grid(alpha = 0.2) +
  tm_raster(alpha = 0.7, palette = mypal2, 
            style = "pretty", title = "Volumetric Soil Moisture") +
  tm_layout(legend.position = c("left", "bottom")) +
  tm_xlab("Longitude") + 
  tm_ylab("Latitude")

tmap_SM

# Interactive plot
library(mapview)
library(raster)

mapview(brick(sm),            # Convert SpatRaster to RasterBrick
        col.regions = mypal2, # Color palette 
        at=seq(0, 0.8, 0.1)   # Breaks
)

### 3.3 Using ggplot for plotting ----
library(tidyterra)
library(ggplot2)

ggplot() +
  geom_spatraster(data = sm) +
  scale_fill_gradientn(colors=mypal2,                               # Use user-defined colormap
                       name = "SM",                                 # Name of the colorbar
                       na.value = "transparent",                    # transparent NA cells
                       labels=(c("0", "0.2", "0.4", "0.6", "0.8")), # Labels of colorbar
                       breaks=seq(0,0.8,by=0.2),                    # Set breaks of colorbar
                       limits=c(0,0.8))+
  theme_void()  # Try different themes: theme_bw(), theme_gray(), theme_minimal()

# let's specify an
sm_conus= ggplot() +
  geom_spatraster(data = sm) +
  scale_fill_gradientn(colors=mypal2,                               # Use user-defined colormap
                       name = "SM",                                 # Name of the colorbar
                       na.value = "transparent",                    # transparent NA cells
                       labels=(c("0", "0.2", "0.4", "0.6", "0.8")), # Labels of colorbar
                       breaks=seq(0,0.8,by=0.2),                    # Set breaks of colorbar
                       limits=c(0,0.8)) +
  coord_sf(xlim = c(-125,-67),                                      # Add extent for CONUS
           ylim = c(24,50))+               
  theme_bw()                                                        # Try black-and-white theme. 

print(sm_conus)


#We will manipulate the existing tmap_SM plot by adding style parameter:
tmap_SM = tm_shape(sm)+
  tm_grid(alpha = 0.2)+
  tm_raster(alpha = 0.7, style = "pretty", 
            palette = mypal2,  title = "Volumetric Soil Moisture") +
  tm_layout(legend.position = c("left","bottom"), inner.margins = 0)+           #Adjust the legend position
  tm_xlab("Longitude")+ tm_ylab("Latitude")  

tmap_SM

#Using breaks would give more control on scale discretization
tm_shape(sm)+
  tm_grid(alpha = 0.2)+
  tm_raster(alpha = 0.7, breaks = c(0.00, 0.25, 0.50, 0.75, 1.00), 
            palette = mypal2,  title = "Volumetric Soil Moisture") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = c("right","top"),
            inner.margins = 0)+
  tm_xlab("Longitude")+ tm_ylab("Latitude")

# Saving the plot to disk:
tmap_save(tmap_SM,                    # tmap object
          filename = "../out/sm_world.png",  # filename including extension
          dpi = 300,                  # dots per inch
          height = 5,
          width = 5,
          units = "in")               #units for width and height

### 3.4 plotting VECTOR ----
#~~~  Importing and visualizing shapefiles
library(sf)  

# Import the shapefile of global IPCC climate reference regions (only for land) 
IPCC_shp = read_sf("./CMIP_land/CMIP_land.shp")

# View attribute table of the shapefile
IPCC_shp # Notice the attributes look like a data frame

# Load global coastline shapefile 
coastlines = read_sf("./ne_10m_coastline/ne_10m_coastline.shp")

# Alternatively, download global coastlines from the web 
# NOTE: May not work if the online server is down
# download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_coastline.zip?version=4.0.1",destfile = 'ne_110m_coastline.zip')
# # Unzip the downloaded file
# unzip(zipfile = "ne_110m_coastline.zip",exdir = 'ne-coastlines-110m')

# Plot both sf objects using tmap:
tm_shape(IPCC_shp)+
  tm_borders()+            # Add IPCC land regions in blue color
  tm_shape(coastlines)+
  tm_sf()                  # Add global coastline

#Subset shapefile for Eastern North-America
terra::plot(IPCC_shp[4,][1], main="Polygon for Eastern North-America")

# Combine terra plots with overlaying shapefiles
tmap_SM + 
  tm_shape(IPCC_shp)+
  tm_borders()+            
  tm_shape(coastlines)+
  tm_sf()


#~~~ Add spatial point to shapefile/ raster

#~~ Create a spatial point for College Station, Texas
college_station = st_sfc(
  st_point(x = c(-96.33, 30.62), dim = "XY"), # Lat-long as spatial points 
  crs = "EPSG:4326")  # Coordinate system: More details in the next part

#~~ Create map by adding all the layers
tm_shape(IPCC_shp[c(3,4,6,7),])+                       # Selected regions from 'IPCC_shp'
  tm_borders(col = "black",lwd = 1, lty = "solid")+    # Border color
  tm_fill(col = "lightgrey")+                          # Fill color
  tm_shape(coastlines)+                                # Add coastline
  tm_sf(col = "maroon")+                               # Change color of coastline
  tm_shape(college_station)+                           # Add spatial point to the map
  tm_dots(size = 2, col = "blue")                      # Customize point


### 3.5 Projection ----
#~~ Projection 1: NAD83 (EPSG: 4269)

sm_proj1 = terra::project(sm, "epsg:4269")

terra::plot(sm_proj1, 
            main = "NAD83",    # Title of the plot
            col = mypal2,      # Colormap for the plot
            axes = FALSE,      # Disable axes
            box = FALSE,       # Disable box around the plots
            asp = NA,          # No fixed aspect ratio; asp=NA fills plot to window
            legend=FALSE)      # Disable legend
#~~ Projection 2: World Robinson projection (ESRI:54030)

sm_proj2 = terra::project(sm, "ESRI:54030")

terra::plot(sm_proj2, 
            main = "Robinson", # Title of the plot
            col = mypal2,      # Colormap for the plot
            axes = FALSE,      # Disable axes
            box = FALSE,       # Disable box around the plots
            asp = NA,          # No fixed aspect ratio; asp=NA fills plot to window
            legend=FALSE)      # Disable legend

#~~~Projection 2: Robinson projection

WorldSHP=terra::vect(spData::world)

RobinsonPlot <- ggplot() +
  geom_spatraster(data = sm)+                   # Plot SpatRaster layer               
  geom_spatvector(data = WorldSHP, 
                  fill = "transparent") +       # Add world political map
  ggtitle("Robinson Projection") +              # Add title
  scale_fill_gradientn(colors=mypal2,           # Use user-defined colormap
                       name = "Soil Moisture",  # Name of the colorbar
                       na.value = "transparent",# Set color for NA values
                       lim=c(0,0.8))+           # Z axis limit
  theme_minimal()+                              # Select theme. Try 'theme_void'
  theme(plot.title = element_text(hjust =0.5),  # Place title in the middle of the plot
        text = element_text(size = 12))+        # Adjust plot text size for visibility
  coord_sf(crs = "ESRI:54030",                  # Reproject to World Robinson
           xlim = c(-152,152)*100000,    
           ylim = c(-55,90)*100000)

print(RobinsonPlot)


WorldSHP = st_as_sf(WorldSHP)           # Convert 'WorldSHP' to simple feature

tm_shape(WorldSHP,                      # Initiate shapefile       
         projection = 'ESRI:54030',     # Set projection: World Robinson
         ylim = c(-65, 90)*100000,      # Set y-limit
         xlim = c(-152,152)*100000,     # Set x-limit
         raster.warp = TRUE)+              
  tm_sf()+                              # Plot shapefile 
  tm_shape(sm,                          # Add raster file
           projection = 'ESRI:54030',   # Set projection 
           raster.warp = FALSE) +
  tm_raster( palette = mypal2,          # Set color map for raster
             title = "Soil Moisture")+  # Add plot title
  tm_layout(main.title = "Surface Soil Moisture",
            main.title.fontfamily = "Times",               # Set text font
            legend.show = T,                               # Show legend= T/F
            legend.outside = T,                            
            legend.outside.position = c("right", "top"),   # Legend position
            frame = FALSE,                                 # Add plot frame
            earth.boundary.color = "grey",                 # Boundary color
            earth.boundary.lwd = 2,                        # Boundary linewidth
            fontfamily = "Times")+                         # Text font
  tm_graticules(alpha = 0.2,                               # Add lat-long graticules
                labels.inside.frame = FALSE,  
                col = "lightgrey", n.x = 4, n.y = 3)

## Chapter 04 ----

### 4.1 raster resampling ----
# Importing SMAP soil moisture data
sm=rast("./raster_files/SMAP_SM.tif") 

# Original resoluton of raster for reference
res(sm)

#~~ Aggregate raster to coarser resolution
SMcoarse = terra::aggregate(sm,           # Soil moisture raster
                            fact = 10,    # Aggregate by x 10
                            fun = mean)   # Function used to aggregate values
res(SMcoarse)

#~~ Disaggregate raster to finer resolution
SMfine = terra::disagg(sm, 
                       fact=3, 
                       method='bilinear')
res(SMfine)

#~~ Raster resampling
# Import global aridity raster
aridity=rast("./raster_files/aridity_36km.tif") 

# Plot aridity map
terra::plot(aridity, col=mypal2, main= "Aridity Spatraster")

# Resample aridity raster to coarse resolution  
aridityResamp=terra::resample(aridity,      # Original raster
                              SMcoarse,     # Target resolution raster
                              method='near') # bilinear or ngb (nearest neighbor) 

# Plot resampled aridity map
terra::plot(aridityResamp, col=mypal2, main= "Aridity at Coarser Resolution")

### 4.2 Summary stats ----
# Simple arithmetic operations
sm2=sm*2
print(sm2) # Try sm2=sm*10, or sm2=sm^2 and see the difference in sm2 values

# Summary statistics
global(sm, mean, na.rm = T)
global(sm, sd, na.rm = T)
global(sm, quantile, probs = c(0.25, 0.75), na.rm = T)

# User-defined statistics by defining own function
quant_fun = function(x, na.rm=TRUE){ # Remember to add "na.rm" option
  quantile(x, probs = c(0.25, 0.75), na.rm=TRUE)
} 
global(sm, quant_fun)   # 25th, and 75th percentile of each layer

### 4.3 Summary stats with shapefiles ----

# Using shapefile to summarize a raster
sm_IPCC_df=terra::extract(sm,        # Spatraster to be summarized
                          vect(IPCC_shp),  # Shapefile/ polygon to summarize the raster
                          fun=mean,  # Desired statistic: mean, sum, min and max 
                          na.rm=TRUE)# Ignore NA values? TRUE=yes! 

head(sm_IPCC_df)


# Extract cell values for each region 
sm_IPCC_list=terra::extract(sm,       # Raster to be summarized
                            vect(IPCC_shp),   # Shapefile/ polygon to summarize the raster
                            df=FALSE,   # Returns a list
                            fun=NULL,   # fun=NULL will output cell values within each region
                            na.rm=TRUE) # Ignore NA values? yes! 

# Apply function on cell values for each region
library(tidyverse)

sm_IPCC_list %>% 
  as_tibble() %>% 
  group_by(ID) %>% 
  summarise(mean_SM = mean(SMAP_SM, na.rm =T))


#~~ Try user defined function
myfun=function (y){return(mean(y, na.rm=TRUE))}    # User defined function for calculating means

sm_IPCC_list %>% 
  as_tibble() %>% 
  group_by(ID) %>% 
  summarise(mean_SM = myfun(SMAP_SM))  


#~~~ Convert a raster to a shapefile
aridity=rast("./raster_files/aridity_36km.tif") #Global aridity

# Convert raster to shapefile
arid_poly=st_as_sf(as.polygons(aridity))   # Convert SpatRaster to polygon and then to sf

# Plot aridity polygon
terra::plot(arid_poly, 
            col=arid_poly$aridity_36km)  # Colors based on aridity values (i.e. 1,2,3,4,5)

sm_arid_df=terra::extract(sm,        # Raster to be summarized
                          vect(arid_poly), # Shapefile/ polygon to summarize the raster
                          fun=mean,  # Desired statistic: mean, sum, min and max 
                          na.rm=TRUE)# Ignore NA values? yes! 

# Lets plot the climate-wise mean of surface soil moisture
{plot(sm_arid_df,     
      xaxt = "n",              # Disable x-tick labels
      xlab="Aridity",          # X axis label
      ylab="Soil moisture",    # Y axis label
      type="b",                # line type
      col="blue",              # Line color
      main="Climate-wise mean of surface soil moisture")
  axis(1, at=1:5, labels=c("Hyper-arid", "Arid", "Semi-Arid","Sub-humid","Humid"))}


## Chapter 05 ----
### 5.1 ----
#~~~ Create and plot NDVI SpatRaster
library(terra)

# Location of the NDVI raster files
ndvi_path="./raster_files/NDVI/" #Specify location of NDVI rasters

# List of all NDVI rasters
ras_path=list.files(ndvi_path,pattern='*.tif',full.names=TRUE)
head(ras_path)

# Method 1: Use lapply to create raster layer list from the raster location
# this is the prefered method.
ras_list = lapply(paste(ras_path, sep = ""), rast)
#This a list of 23 raster objects stored as individual elements.

#Convert raster layer lists to data cube/Spatraster 
ras_stack = rast(ras_list)     # Stacking all rasters as a data cube!!! 
# This a multi-layer (23 layers in this case) SpatRaster Object.

#inMemory() reports whether the raster data is stored in memory or on disk.
inMemory(ras_stack[[1]]) 


# Method 2: Using pipes to create raster layers from the raster location
library(tidyr) # For piping
ras_list = ras_path %>%  purrr::map(~ rast(.x))  # Import the raster
ras_stack = rast(ras_list)  # Convert RasterStack to RasterBrick

# Method 3: Using for loop to create raster layers from the raster location
ras_stack=rast()
for (nRun in 1:length(ras_path)){
  ras_stack=c(ras_stack,rast(ras_path[[nRun]]))
}

# Check dimension of data cube
dim(ras_stack) #[x: y: z]- 23 raster layers with 456 x 964 cells


# Subset raster stack/brick (notice the double [[]] bracket and similarity to lists)
sub_ras_stack=ras_stack[[c(1,3,5,10,12)]] #Select 1st, 3rd, 5th, 10th and 12th layers

#Try subsetting with dates:
Season<-str_subset(string = names(ras_stack), pattern = c("20..-0[3/4/5]")) 
ras_stack[[Season]]

#~~ Plot first 4 elements of NDVI SpatRaster
# Function to add shapefile to a raster plot
addCoastlines=function(){
  plot(vect(coastlines), add=TRUE)   #convert 'coastlines' to vector format
} 

terra::plot(sub_ras_stack[[1:4]],  # Select raster layers to plot
            col=mypal2,                   # Specify colormap
            asp=NA,                       # Aspect ratio= NA
            fun=addCoastlines)            # Add coastline to each layer


### 5.2 geospatial operation on rasters ----
#~~~~ Method 1.1: Extract values for a single location

# User defined lat and long
Long=-96.33; Lat=30.62 
# Creating a spatial vector object from the location coords
college_station = vect(SpatialPoints(cbind(Long, Lat)))


# Extract time series for the location
ndvi_val=terra::extract(ras_stack,
                        college_station,    # lat-long as spatial locations
                        method='bilinear')  # or "simple" for nearest neighbor

# Create a dataframe using the dates (derived from raster layer names) and extracted values
ndvi_ts=data.frame(Time=c(1:nlyr(ras_stack)), # Sequence of retrieval time
                   NDVI=as.numeric(ndvi_val[,-1]))     #NDVI values
# Try changing Time to as.Date(substr(colnames(ndvi_val),13,22), format = "%Y.%m.%d")

# Plot NDVI time series extracted from raster brick/stack
plot(ndvi_ts, type="l", col="maroon", ylim=c(0.2,0.8))


#~~~~ Method 1.2:  Extract values for multiple locations
# Import sample locations from contrasting hydroclimate
library(readxl)
loc= read_excel("./location_points.xlsx")
print(loc)

# Extract time series using "raster::extract"
loc_ndvi=terra::extract(ras_stack,
                        #2-column matrix or data.frame with lat-long
                        loc[,3:4],   
                        # Use bilinear interpolation (or simple) option
                        method='bilinear')


# Create a new data frame with dates of retrieval and NDVI for different hydroclimates
library(lubridate)
ndvi_locs = data.frame(Date=ymd(substr(colnames(ndvi_val[,-1]),13,22)),
                       Humid = as.numeric(loc_ndvi[1,-1]),    #Location 1
                       Arid = as.numeric(loc_ndvi[2,-1]),     #Location 2
                       SemiArid = as.numeric(loc_ndvi[3,-1])) #Location 3

# Convert data frame in "long" format for plotting using ggplot
library(tidyr)
df_long=ndvi_locs %>% gather(Climate,NDVI,-Date)
#"-" sign indicates decreasing order
head(df_long,n=4)

# Plot NDVI for different locations (hydroclimates). Do we see any pattern?? 
library(ggplot2)
l = ggplot(df_long,               # Dataframe with input Dataset 
           aes(x=Date,             # X-variable
               y=NDVI,             # Y-variable
               group=Climate)) +   # Group by climate 
  geom_line(aes(color=Climate))+  # Line color based on climate variable
  geom_point(aes(color=Climate))+ # Point color based on climate variable 
  theme_classic() 
print(l)


#~~~~ Method 2: Retrieve values using cell row and column number
row = rowFromY(ras_stack[[1]], Lat)    # Gives row number for the selected Lat
col = colFromX(ras_stack[[1]], Long)   # Gives column number for the selected Long
ras_stack[row,col][1:5]                # First five elements of data cube for selected x-y

##### 5.2.1  Spatial extraction/summary from SpatRaster ----
#~~~~ Method 1: Extract values based on spatial polygons
IPCC_shp = read_sf("./CMIP_land/CMIP_land.shp")

# Calculates the 'mean' of all cells within each feature of 'IPCC_shp' for each layers
ndvi_sp = terra::extract(ras_stack,   # Data cube
                         vect(IPCC_shp),    # Shapefile for feature reference 
                         fun=mean, 
                         na.rm=T, 
                         #df=TRUE, 
                         method='bilinear')

head(ndvi_sp,n=3)[1:5]

#### 5.2.2 Crop and mask SpatRaster ----

# Import polygons for polygons for USA at level 1 i.e. state from disk

state_shapefile = read_sf("./USA_states/cb_2018_us_state_5m.shp")

# Alternatively, use dataset from 'spData' package 'spData::us_states'

# Print variable names
names(state_shapefile)

# Exclude states outside of CONUS 
conus = state_shapefile[!(state_shapefile$NAME %in% c("Alaska","Hawaii","American Samoa",
                                                      "United States Virgin Islands","Guam", "Puerto Rico",
                                                      "Commonwealth of the Northern Mariana Islands")),] 

# plot CONUS as a vector using terra package
terra::plot(vect(conus)) 

# Crop SpatRaster using USA polygon
usa_crop = crop(ras_stack, ext(conus))       # Crop raster to CONUS extent

# Plot cropped raster
plot(usa_crop[[1:4]], col=mypal2)

# Mask SpatRaster using USA polygon
ndvi_mask_usa = terra::mask(usa_crop, vect(conus))    # Mask raster to CONUS 

# Mask SpatRaster using USA polygon
plot(ndvi_mask_usa[[1:4]], 
     col=mypal2, 
     fun=function(){plot(vect(conus), add=TRUE)} # Add background states
)


# Mask raster for selected states
states = c('Colorado','Texas','New Mexico')

# Subset the selected states from CONUS shapefile
state_plot = state_shapefile[(state_shapefile$NAME %in% states),] 

# Raster operation
states_trim = crop(ras_stack, ext(state_plot))                # Crop raster
ndvi_mask_states = terra::mask(states_trim, vect(state_plot)) # Mask

# Plot raster and shapefile
plot(ndvi_mask_states[[1]], 
     col=mypal2, 
     fun=function(){plot(vect(conus), add=TRUE)} # Add background states
)

### 5.3 Layer-wise operations on SpatRaster ----

#~~~ Layer-wise cell-statistics 
# Layer-wise Mean
global(sub_ras_stack, mean, na.rm= T) # Mean of each raster layer. Try modal, median, min etc. 

# Layer-wise quantiles
global(sub_ras_stack, quantile, probs=c(.25,.75), na.rm= T)

# Custom function for mean, variance and skewness
my_fun = function(x){ 
  meanVal=mean(x, na.rm=TRUE)              # Mean 
  varVal=var(x, na.rm=TRUE)                # Variance
  skewVal=moments::skewness(x, na.rm=TRUE) # Skewness
  output=c(meanVal,varVal,skewVal)         # Combine all statistics
  names(output)=c("Mean", "Var","Skew")    # Rename output variables
  return(output)                           # Return output
} 

global(sub_ras_stack, my_fun) # Mean, Variance and skewness of each layer

#You can also use summary() to retrieve common descriptive statistics
summary(sub_ras_stack)

#~~~ Global cell-statistics

# By vectorizing the SpatRaster and finding statistics
min_val=min(as.vector(sub_ras_stack), na.rm=T) 
max_val=max(as.vector(sub_ras_stack), na.rm=T)
print(c(min_val,max_val))

# Another example of statistics of vectorized SpatRaster
quant=quantile(as.vector(sub_ras_stack), c(0.01,0.99), na.rm=TRUE) #1st and 99th percentiles
print(quant)

#~~~ Layer-wise operations on SpatRaster

# Arithmetic operations on SpatRaster are same as lists
add = sub_ras_stack+10                  # Add a number to raster layers
mult = sub_ras_stack*5                  # Multiply a number to raster layers
subset_mult = sub_ras_stack[[1:3]]*10   # Multiply a number to a subset of raster layers

# Data filtering based on cell-value
filter_stack = sub_ras_stack            # Create a RasterBrick to operate on
filter_stack[filter_stack<0.5] = NA     # Assign NA to any value less than 0.5

# Let's plot the filtered rasters
plot(filter_stack, 
     col=mypal2,  # Color pal  
     asp=NA,      # Aspect ratio: NA, fill to plot space
     nc=2,        # Number of columns to arrange plots
     fun=function(){plot(vect(coastlines), add=TRUE)} # Add background map
)                           

# Layer-wise Log-transformation
log_ras_stack=log(sub_ras_stack) 

# Normalize raster layers to [0,1] based on min and max of raster brick/stack
norm_stk=(sub_ras_stack-min_val)/(max_val-min_val) # Notice that the values are between [0,1]

# Plot in Robinson projection
mypal3 = cetcolor::cet_pal(20, name = "l5")
unikn::seecol(mypal3)

WorldSHP = st_as_sf(spData::world)

norm_NDVI= tm_shape(WorldSHP, projection = 'ESRI:54030', 
                    ylim = c(-65, 90)*100000, 
                    xlim = c(-152,152)*100000, 
                    raster.warp = T)+
  tm_sf()+
  tm_shape(norm_stk[[2:5]], projection = 'ESRI:54030', raster.warp = FALSE) +
  tm_raster(palette = rev(mypal3), title = "Normalized NDVI", style = "cont")+
  tm_layout(main.title = "NDVI",
            main.title.fontfamily = "Times",
            legend.show = T,
            legend.outside = T,
            legend.outside.position = c("right", "top"),
            frame = FALSE, 
            #earth.boundary = c(-160, -65, 160, 88),
            earth.boundary.color = "grey",
            earth.boundary.lwd = 2,
            fontfamily = "Times")+
  tm_graticules(alpha = 0.2,                               # Add lat-long graticules
                labels.inside.frame = FALSE,  
                col = "lightgrey", n.x = 4, n.y = 3)+
  tm_facets(ncol = 2)

print(norm_NDVI)

### 5.4 Cell-wise operations with app, lapp, tapp ----
#### 5.4.1 app: Cell-wise operation on all layers ----

#Let's look at the help section for app()
?terra::app

# Calculate mean of each grid cell across all layers
mean_ras = app(sub_ras_stack, fun=mean, na.rm = T)

# Calculate sum of each grid cell across all layers
sum_ras = app(sub_ras_stack, fun=sum, na.rm = T)

#~~ A user-defined function for mean, variance and skewness
my_fun = function(x){ 
  meanVal=mean(x, na.rm=TRUE)              # Mean 
  varVal=var(x, na.rm=TRUE)                # Variance
  skewVal=moments::skewness(x, na.rm=TRUE) # Skewness
  output=c(meanVal,varVal,skewVal)         # Combine all statistics
  names(output)=c("Mean", "Var","Skew")    # Rename output variables
  return(output)                           # Return output
} 

# Apply user function to each cell across all layers
stat_ras = app(sub_ras_stack, fun=my_fun)

# Plot statistics
plot(stat_ras, 
     col=mypal2,  # Color pal  
     asp=NA,      # Aspect ratio: NA, fill to plot space
     nc=2,        # Number of columns to arrange plots
     fun=function(){plot(vect(coastlines), add=TRUE)} # Add background map
)

#### 5.4.2 tapp: Cell-wise operation on layer groups ----
#Let's look at the help section for app()
?terra::tapp

#The layers are combined based on indexing.
stat_ras = terra::tapp(sub_ras_stack,
                       index=c(1,1,2,2,2),
                       fun= mean)

# Try other functions: "sum", "mean", "median", "modal", "which", "which.min", "which.max", "min", "max", "prod", "any", "all", "sd", "first".

names(stat_ras) = c("Mean_of_rasters_1_to_2", "Mean_of_rasters_3_to_5")
# Two layers are formed, one for each group of indices

# Lets plot the two output rasters
plot(stat_ras, 
     col=mypal2,  # Color pal  
     asp=NA,      # Aspect ratio: NA, fill to plot space
     nc=2,        # Number of columns to arrange plots
     fun=function(){plot(vect(coastlines), add=TRUE)} # Add background map
)


#### 5.4.3 lapp: layers as function arguments ----
#Let's look at the help section for app()
?terra::lapp

#User defined function for finding difference
diff_fun = function(a, b){ return(a-b) }

diff_rast = lapp(sub_ras_stack[[c(4, 2)]], fun = diff_fun)

#Plot NDVI difference
plot(diff_rast, 
     col=mypal2,  # Color pal  
     asp=NA,      # Aspect ratio: NA, fill to plot space
     nc=2,        # Number of columns to arrange plots
     fun=function(){plot(vect(coastlines), add=TRUE)} # Add background map
)

### Chapter 06 ----
library(parallel)

SMAPBrk=rast("./SMAP_L3_USA.nc")

plot(mean(SMAPBrk, na.rm=TRUE), asp=NA)

#~~ We will make some changes in the custom function for mean, variance and skewness
minSamp = 50                           # Minimum assured samples for statistics

my_fun = function(x, minSamp, na.rm=TRUE){    
  smTS=as.numeric(as.vector(x))     # Convert dataset to numeric array
  smTS=as.numeric(na.omit(smTS))    # Omit NA values 
  
  # Implement function with trycatch for catching exception 
  tryCatch(if(length(smTS)>minSamp) {      # Apply minimum sample filter
    
    ######## OPERATION BEGINS #############    
    meanVal=mean(smTS, na.rm=TRUE)              # Mean 
    varVal=var(smTS, na.rm=TRUE)                # Variance
    skewVal=moments::skewness(smTS, na.rm=TRUE) # Skewness
    output=c(meanVal,varVal,skewVal)            # Combine all statistics
    return(output)                              # Return output
    ######## OPERATION ENDS #############    
    
  } else {
    return(rep(NA,3))                         # If conditions !=TRUE, return array with NA
  },error =function(e){return(rep(NA, 3))})   # If Error== TRUE, return array with NA
}

# Load the package
library(tictoc)
# Apply function to all grids in parallel
tic()
stat_brk = app(SMAPBrk, 
               my_fun, 
               minSamp = 50,                           # Minimum assured samples for statistics
               cores =parallel::detectCores() - 1)     # Leave one core for housekeeping

names(stat_brk)=c("Mean", "Variance", "Skewness")      # Add layer names
toc()
plot(stat_brk, col=mypal2)


library(terra)
e <- ext( c(-110,-108, 35,37) )   # Sample 2X2 degree domain
p <- as.polygons(e)
crs(p) <- "EPSG:4326"

# Use this polygon to crop and mask the larger SpatRaster

#### 6.2 Layerwise implimentation of functions ----
# Convert Spatraster to a list of rasters
rasList=as.list(SMAPBrk[[1:10]])           #What will happen if we pass rast(rasList)?
length(rasList)

my_fun = function(x){                
  x=na.omit(as.numeric(as.vector(x)))      # Create vector of numeric values of SpatRaster
  meanVal=base::mean(x, na.rm=TRUE)        # Mean 
  varVal=stats::var(x, na.rm=TRUE)         # Variance
  skewVal=moments::skewness(x, na.rm=TRUE) # Skewness
  output=c(meanVal,varVal,skewVal)         # Combine all statistics
  return(output)                           # Return output
} 

# Test the function for one raster
my_fun(rasList[[2]])

# Apply function in parallel for all layers
library(parallel) 
library(future.apply) 

# A multicore future: employ max core-1 for processing
plan(multicore, workers = detectCores() - 1)

# Deploy function in parallel 
tic()
outStat= future_lapply(rasList, my_fun)
toc()

##### 6.2.2 Blockwise summary of feature extracted data ----
#~ Extract feature data as data frame
library(exactextractr)
library(sf)
library(sp)

featureData=exact_extract(SMAPBrk,   # Raster brick 
                          st_as_sf(conus),     # Convert shapefile to sf (simple feature)
                          force_df = FALSE,    # Output as a data.frame?
                          include_xy = FALSE,  # Include cell lat-long in output?
                          fun = NULL,          # Specify the function to apply for each feature extracted data
                          progress = TRUE)     # Progressbar

length(featureData) # Same as feature count in CONUS? i.e. nrow(conus) 
# Lets try out data for Texas
which(conus$NAME=="Texas")  # Find feature number for Texas
# View(featureData[[5]])    # View the extracted data frame
nrow(featureData[[5]])      # No. pixels within selected feature
# Extract SM time series for first pixel by removing percentage fraction
cellTS=as.numeric(featureData[[5]][1,1:nlyr(SMAPBrk)])

# Plot time time series for the selected feature
plot(cellTS, type="l", xlab="Time", ylab="Soil moisture")

#~~ We will make another small change in the custom function for mean, variance and skewness
minSamp=50   # Minimum assured samples for statistics

my_fun = function(x, na.rm=TRUE){    
  xDF=data.frame(x)                  # Convert list to data frame
  xDF=xDF[ , !(names(xDF) %in% 'coverage_fraction')] # Remove coverage_fraction column
  xData=as.vector(as.matrix(xDF))    # Convert data.frame to 1-D matrix
  smTS=as.numeric(na.omit(xData))    # Omit NA values                   
  
  # Implement function with trycatch for catching exception 
  tryCatch(if(length(smTS)>minSamp) {      # Apply minimum sample filter
    
    ######## OPERATION BEGINS #############    
    meanVal=mean(smTS, na.rm=TRUE)              # Mean 
    varVal=var(smTS, na.rm=TRUE)                # Variance
    skewVal=moments::skewness(smTS, na.rm=TRUE) # Skewness
    output=c(meanVal,varVal,skewVal)         # Combine all statistics
    return(output)                           # Return output
    ######## OPERATION ENDS #############    
    
  } else {
    return(rep(NA,3))   # If conditions !=TRUE, return array with NA
  },error =function(e){return(rep(NA, 3))}) # If Error== TRUE, return array with NA
}

# Test the function for one block
my_fun(featureData[[5]])

# Apply function in parallel for all layers
library(parallel) 
library(snow)
library(future.apply) 
library(future)

# Cross-platform safe option (works everywhere)
plan(multisession, workers = 4)

outStat <- future_lapply(featureData, my_fun)

# Test output for one feature
outStat[[5]]  # Is this the same as before?


# Extract each summary stats for all features from the output list  
FeatureMean=sapply(outStat,"[[",1)  # Extract mean for all features
FeatureVar=sapply(outStat,"[[",2)   # Extract variance for all features
FeatureSkew=sapply(outStat,"[[",3)  # Extract skewness for all features

# Let's place mean statistics as an attribute to the shapefile
conus$meanSM=FeatureMean


# Plot mean soil moisture map for CONUS 
library(rcartocolor)
library(ggplot2)
library(sf)
library(sp)

mean_map=ggplot() + 
  geom_sf(data = st_as_sf(conus), # CONUS shp as sf object (simple feature)
          aes(fill = meanSM)) +   # Plot fill color= mean soil moisture
  scale_fill_carto_c(palette = "BluYl",     # Using carto color palette
                     name = "Mean SM",      # Legend name
                     na.value = "#e9e9e9",  # Fill values for NA 
                     direction = 1)+        # To invert color, use -1
  coord_sf(crs = 2163)+   # Reprojecting polygon 4326 or 3083 
  theme_void() +          # Plot theme. Try: theme_bw
  theme(legend.position = c(0.2, 0.1),  
        legend.direction = "horizontal",
        legend.key.width = unit(5, "mm"),
        legend.key.height = unit(4, "mm"))
mean_map
