di_buffer_dem <- function(dem){
  # Convert the DEM into a matrix for manipulation
  dem.m <- raster::as.matrix(dem)

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

  return(dem.c)
}
