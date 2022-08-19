## This is a script for processing the
## ASTER Global Digital Elevation Map, version 2.
## The script loads 12 ASTER GDEM tiles, 
## mosaics them into a single raster,
## reprojects the new raster to NAD83 UTMs, 
## crops it to an extent given in UTM coordinates,
## and resamples at a pre-set resolution.

## Raw ASTER GDEM tiles should be put in a folder
## whose path is set below by the user.

## GDEM tiles may be downloaded at 
## http://asterweb.jpl.nasa.gov/gdem.asp

## Author: R. Kyle Bocinsky
## Date: 04/17/2013

library(sp)
library(raster)

########## PARAMETERS ##########
## The user should change these values to correspond to their area of interest

# Set the path of the raw DEMs
setwd('/Users/bocinsky/Desktop/Bocinsky_2013_NWCoast/Input/DEM')

# Set the latitude range of the tiles
lats <- 48:50

# Set the longitude range of the tiles
lons <- 122:125

# Set the UTM zone for the area of interest
UTM_Zone <- 10

# Set the intended resolution, in meters, of the area of interest.
# The ASTER GDEM has a roughly 30-meter resolution, so intended 
# resolutions of less than 30 meters will degrade accuracy.
scale <- 30

# Set the north, south, east, and west boundaries of the area of interest
UTM_North <- 5480020
UTM_South <- 5350000
UTM_East <- 640020
UTM_West <- 435000

########## END PARAMETERS ##########


########## SOURCE ##########
# Create matrix of coordinates
datainUTM<-matrix(c(UTM_East, UTM_West, UTM_West, UTM_East, UTM_East, UTM_North, UTM_North, UTM_South, UTM_South, UTM_North),nrow=5)

# Set universal projection
master.proj <- CRS(paste("+proj=utm +datum=NAD83 +zone=",UTM_Zone,sep=''))

# Create SpatialPolygon of simulation area
sim.poly <- Polygons(list(Polygon(datainUTM, hole=FALSE)),ID='A')
sim.poly <- SpatialPolygons(list(sim.poly), proj4string=master.proj)

# Create a raster template of the simulation area
rows <- abs(UTM_North-UTM_South)/scale
columns <- abs(UTM_East-UTM_West)/scale
sim.raster <- raster(ext=extent(sim.poly),nrows=rows,ncols=columns,crs=master.proj@projargs)

# Create mosaic from all DEMs
num <- length(dir())
dems <- list()
k <- 1
for(i in lats){
  for(j in lons){
    if(file.exists(paste('ASTGTM2_N',i,'W',j,'/ASTGTM2_N',i,'W',j,'_dem.tif',sep=''))){
      dems[k] <- raster(paste('ASTGTM2_N',i,'W',j,'/ASTGTM2_N',i,'W',j,'_dem.tif',sep=''))
      k <- k+1
    }
  }
}

dems$fun <- mean
mosaic.all <- do.call(mosaic, dems)

# Rough-crop to area of interest
extent.small <- extent(c(-124,-121,48.25,49.5))
mosaic.all.small <- crop(mosaic.all,extent.small)

# Reproject mosaic to NAD83 UTMs
UTM.mosaic.all.small <- projectRaster(mosaic.all.small,crs=master.proj@projargs)

# Crop full-resolution mosaic to simulation extent
UTM.mosaic.all.small.crop <- crop(UTM.mosaic.all.small,sim.poly)

# Resample at 30-meter resolution
UTM.mosaic.all.small.crop.resample <- resample(UTM.mosaic.all.small.crop,sim.raster)

# Write the final raster
writeRaster(UTM.mosaic.all.small.crop.resample,'FINAL_DEM',format='raster', overwrite=TRUE)

########## END SOURCE ##########