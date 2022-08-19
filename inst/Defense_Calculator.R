## This is a script for calculating the
## extrinsic defensibility for any position
## on a 30-meter DEM.

## Extrinsic defensibility is defined in Bocinsky 2013:
## Bocinsky, R. Kyle. 2013. Extrinsic site defensibility and landscape-based archaeological
## inference: An example from the Northwest Coast. Journal of Anthropological Archaeology nn(nn):nnn--nnn.

## This defensibility index is a rasterization of one defined
## by Martindale and Supernant (2009):
## Martindale, Andrew and Kisha Supernant. 2009. Quantifying the defensiveness of defended
## sites on the northwest coast of North America. Journal of Anthropological Archaeology 28(2):191--204.

## The input DEM should be put in a folder
## whose path is set below by the user.

## This script outputs several intermediate files and three final rasters:
## elevation, visibility, and defensibility,
## cooresponding to the elevation and visibility components and the final defensibility.

## Author: R. Kyle Bocinsky
## Date: 04/17/2013

library(sp)
library(raster)

########## PARAMETERS ##########

# The working directory
setwd('/Users/bocinsky/IMPORTANT/WSU/RESEARCH/MyPublications/Bocinsky_2013_NWCoast')

# Set the path of the input DEM
dem.path <- 'Input/DEM/FINAL_DEM'

########## END PARAMETERS ##########

########## FUNCTIONS ##########
elev.difference.angular <- function(obs_elev){
  k <- 0
  elev.diffs <- array(list(),c(7,7))
  for(dx in -3:3){
    for(dy in -3:3){
      temp <- dem.m
      k <- k+1

      if(!(3-dx==0)){
        temp.col.left <- matrix(nrow=nrow(dem.m),ncol=3-dx)
        temp.col.left[,1:(3-dx)] <- rep(0,nrow(dem.m))
        temp <- cbind(temp.col.left,temp)
      }
      if(!(3+dx==0)){
        temp.col.right <- matrix(nrow=nrow(dem.m),ncol=3+dx)
        temp.col.right[,1:(3+dx)] <- rep(0,nrow(dem.m))
        temp <- cbind(temp,temp.col.right)
      }

      if(!(3-dy==0)){
        temp.row.top <- matrix(nrow=3-dy,ncol=ncol(dem.m)+6)
        temp.row.top[1:(3-dy),] <- rep(0,ncol(dem.m)+6)
        temp <- rbind(temp.row.top,temp)
      }
      if(!(3+dy==0)){
        temp.row.bottom <- matrix(nrow=3+dy,ncol=ncol(dem.m)+6)
        temp.row.bottom[1:(3+dy),] <- rep(0,ncol(dem.m)+6)
        temp <- rbind(temp,temp.row.bottom)
      }

      elev.diffs[4-dx,4-dy] <- list(atan(((dem.c+obs_elev)-temp)/(sqrt(((dx*30)^2)+((dy*30)^2))))*(180/pi))
    }
  }
  return(elev.diffs)
}
########## END FUNCTIONS ##########



########## SOURCE ##########
# Load the DEM
dem <- raster(dem.path)

# Convert the DEM into a matrix for manipulation
dem.m <- as.matrix(dem)

# Set all NA values to 0 elevation
dem.m[is.na(dem.m)] <- 0

# Add three buffer columns of zeros all around the matrix
# This is a necessary step, though will cause slight edge effects.
temp.col <- matrix(nrow=nrow(dem.m),ncol=3)
temp.col[,1:3] <- rep(0,nrow(dem.m))
temp.row <- matrix(nrow=3,ncol=ncol(dem.m)+6)
temp.row[1:3,] <- rep(0,ncol(dem.m)+6)
dem.c <- cbind(temp.col,dem.m)
dem.c <- cbind(dem.c,temp.col)
dem.c <- rbind(temp.row,dem.c)
dem.c <- rbind(dem.c,temp.row)

# Calculate the angular difference in elevation between each cell and
# all of its Moore neighbors within a radius of three cells.
# Store these in a list of matrices called `elev.diffs'.
# `elev.diffs' is needed for the calculation of the visibility
# index.
# The elevation indices are calculated and stored in a
# matrix called `e.m'
elev.diffs <- elev.difference.angular(0)

# Calculate the Elevation Index for each cell.
e.m <- matrix(nrow=nrow(dem.c),ncol=ncol(dem.c))
e.m[,] <- 0
for(dx in -3:3){
  for(dy in -3:3){
    elev.diffs[4-dx,4-dy][[1]][is.na(elev.diffs[4-dx,4-dy][[1]])] <- 0
    if(!(abs(dx)==3 | abs(dy)==3)) next
    e.m <- e.m+elev.diffs[4-dx,4-dy][[1]]
  }
}
e.m <- ((e.m/24)+90)/180

rm(elev.diffs)
gc()

save(e.m,file='Output/R_data/e_m.data')



# Calculate the visibility between the focal cell and
# all of its Moore neighbors at a radius of three cells.
# The visibility indices are calculated and stored in a
# matrix called `v.m'
# This algorithm goes cell by cell and checks the visibility
# to all r3 cells, via matrix algebra.

# Recalculate angular elevation given the observer height.
elev.diffs <- elev.difference.angular(0)

# This section check for visiblity of the r3 cells.
vis.frames <- array(list(),c(7,7))
# The corners
vis.frames[7,7][[1]] <- ifelse(elev.diffs[7,7][[1]]>elev.diffs[6,6][[1]] | elev.diffs[7,7][[1]]>elev.diffs[5,5][[1]],0,1)
vis.frames[1,1][[1]] <- ifelse(elev.diffs[1,1][[1]]>elev.diffs[2,2][[1]] | elev.diffs[1,1][[1]]>elev.diffs[3,3][[1]],0,1)
vis.frames[7,1][[1]] <- ifelse(elev.diffs[7,1][[1]]>elev.diffs[6,2][[1]] | elev.diffs[7,1][[1]]>elev.diffs[5,3][[1]],0,1)
vis.frames[1,7][[1]] <- ifelse(elev.diffs[1,7][[1]]>elev.diffs[2,6][[1]] | elev.diffs[1,7][[1]]>elev.diffs[3,5][[1]],0,1)

# Edge centers
vis.frames[7,4][[1]] <- ifelse(elev.diffs[7,4][[1]]>elev.diffs[6,4][[1]] | elev.diffs[7,4][[1]]>elev.diffs[5,4][[1]],0,1)
vis.frames[4,7][[1]] <- ifelse(elev.diffs[4,7][[1]]>elev.diffs[4,6][[1]] | elev.diffs[4,7][[1]]>elev.diffs[4,5][[1]],0,1)
vis.frames[1,4][[1]] <- ifelse(elev.diffs[1,4][[1]]>elev.diffs[2,4][[1]] | elev.diffs[1,4][[1]]>elev.diffs[3,4][[1]],0,1)
vis.frames[4,1][[1]] <- ifelse(elev.diffs[4,1][[1]]>elev.diffs[4,2][[1]] | elev.diffs[4,1][[1]]>elev.diffs[4,3][[1]],0,1)

# One in from the corners
vis.frames[7,6][[1]] <- ifelse(elev.diffs[7,6][[1]]>elev.diffs[6,5][[1]] | elev.diffs[7,6][[1]]>elev.diffs[5,5][[1]] | elev.diffs[7,6][[1]]>elev.diffs[5,4][[1]] | elev.diffs[7,6][[1]]>elev.diffs[6,6][[1]],0,1)
vis.frames[7,2][[1]] <- ifelse(elev.diffs[7,2][[1]]>elev.diffs[4,3][[1]] | elev.diffs[7,2][[1]]>elev.diffs[5,3][[1]] | elev.diffs[7,2][[1]]>elev.diffs[5,2][[1]] | elev.diffs[7,2][[1]]>elev.diffs[6,2][[1]],0,1)
vis.frames[6,7][[1]] <- ifelse(elev.diffs[6,7][[1]]>elev.diffs[4,5][[1]] | elev.diffs[6,7][[1]]>elev.diffs[5,5][[1]] | elev.diffs[6,7][[1]]>elev.diffs[5,6][[1]] | elev.diffs[6,7][[1]]>elev.diffs[6,6][[1]],0,1)
vis.frames[6,1][[1]] <- ifelse(elev.diffs[6,1][[1]]>elev.diffs[4,3][[1]] | elev.diffs[6,1][[1]]>elev.diffs[5,3][[1]] | elev.diffs[6,1][[1]]>elev.diffs[5,2][[1]] | elev.diffs[6,1][[1]]>elev.diffs[6,2][[1]],0,1)
vis.frames[2,7][[1]] <- ifelse(elev.diffs[2,7][[1]]>elev.diffs[3,4][[1]] | elev.diffs[2,7][[1]]>elev.diffs[3,5][[1]] | elev.diffs[2,7][[1]]>elev.diffs[2,5][[1]] | elev.diffs[2,7][[1]]>elev.diffs[2,6][[1]],0,1)
vis.frames[2,1][[1]] <- ifelse(elev.diffs[2,1][[1]]>elev.diffs[4,3][[1]] | elev.diffs[2,1][[1]]>elev.diffs[3,3][[1]] | elev.diffs[2,1][[1]]>elev.diffs[3,2][[1]] | elev.diffs[2,1][[1]]>elev.diffs[2,2][[1]],0,1)
vis.frames[1,6][[1]] <- ifelse(elev.diffs[1,6][[1]]>elev.diffs[3,4][[1]] | elev.diffs[1,6][[1]]>elev.diffs[3,5][[1]] | elev.diffs[1,6][[1]]>elev.diffs[2,5][[1]] | elev.diffs[1,6][[1]]>elev.diffs[2,6][[1]],0,1)
vis.frames[1,2][[1]] <- ifelse(elev.diffs[1,2][[1]]>elev.diffs[2,3][[1]] | elev.diffs[1,2][[1]]>elev.diffs[3,3][[1]] | elev.diffs[1,2][[1]]>elev.diffs[3,4][[1]] | elev.diffs[1,2][[1]]>elev.diffs[2,2][[1]],0,1)

# Two in from the corners
vis.frames[7,5][[1]] <- ifelse(elev.diffs[7,5][[1]]>elev.diffs[6,5][[1]] | elev.diffs[7,5][[1]]>elev.diffs[5,4][[1]],0,1)
vis.frames[7,3][[1]] <- ifelse(elev.diffs[7,3][[1]]>elev.diffs[6,3][[1]] | elev.diffs[7,3][[1]]>elev.diffs[5,4][[1]],0,1)
vis.frames[5,7][[1]] <- ifelse(elev.diffs[5,7][[1]]>elev.diffs[4,5][[1]] | elev.diffs[5,7][[1]]>elev.diffs[5,6][[1]],0,1)
vis.frames[5,1][[1]] <- ifelse(elev.diffs[5,1][[1]]>elev.diffs[4,3][[1]] | elev.diffs[5,1][[1]]>elev.diffs[5,2][[1]],0,1)
vis.frames[3,7][[1]] <- ifelse(elev.diffs[3,7][[1]]>elev.diffs[3,6][[1]] | elev.diffs[3,7][[1]]>elev.diffs[4,5][[1]],0,1)
vis.frames[3,1][[1]] <- ifelse(elev.diffs[3,1][[1]]>elev.diffs[3,2][[1]] | elev.diffs[3,1][[1]]>elev.diffs[4,3][[1]],0,1)
vis.frames[1,5][[1]] <- ifelse(elev.diffs[1,5][[1]]>elev.diffs[2,5][[1]] | elev.diffs[1,5][[1]]>elev.diffs[3,4][[1]],0,1)
vis.frames[1,3][[1]] <- ifelse(elev.diffs[1,3][[1]]>elev.diffs[2,3][[1]] | elev.diffs[1,3][[1]]>elev.diffs[3,4][[1]],0,1)

rm(elev.diffs)
gc()

# Calculate the Visibility Index
v.m <- matrix(nrow=nrow(dem.c),ncol=ncol(dem.c))
v.m[,] <- 0
for(dx in -3:3){
  for(dy in -3:3){

    if(!(abs(dx)==3 | abs(dy)==3)) next
    v.m <- v.m+vis.frames[4-dx,4-dy][[1]]

  }
}
v.m <- (v.m/24)

rm(vis.frames)
gc()

save(v.m,file='Output/R_data/v_m.data')

# Reload the visibility and elevation datasets
load('Output/R_data/v_m.data')
load('Output/R_data/e_m.data')

# Convert the matrices back into geographic rasters.
# Also, drop the "buffer" of the outer three rows.
elev <- raster(e.m[4:(nrow(e.m)-3),4:(ncol(e.m)-3)],crs=CRS(projection(dem)))
vis <- raster(v.m[4:(nrow(v.m)-3),4:(ncol(v.m)-3)],crs=CRS(projection(dem)))

# Set the extents of the new elevation and visibility rasters
extent(vis) <- extent(elev) <- extent(dem)

# Defensibility is the mean of the
# Elevation and Visibility indices
defense <- (elev+vis)/2

# Apply a 3x3 mean filter to each of the index rasters
# This removes noise from the rasters, and helps diminish the
# impact of randomly placed site stakes
defense <- focal(defense,w=matrix(1/9,ncol=3,nrow=3))
vis <- focal(vis,w=matrix(1/9,ncol=3,nrow=3))
elev <- focal(elev,w=matrix(1/9,ncol=3,nrow=3))

# Output the visibility, elevation, and defensibility rasters.
writeRaster(vis,'Output/rasters/visibility',format='raster', overwrite=TRUE)
writeRaster(elev,'Output/rasters/elevation',format='raster', overwrite=TRUE)
writeRaster(defense,'Output/rasters/defensibility',format='raster', overwrite=TRUE)

