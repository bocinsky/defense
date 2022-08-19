## This is a script for performing the analyses reported in Bocinsky 2014: 
## Bocinsky, R. Kyle. 2014. Extrinsic site defensibility and landscape-based archaeological
## inference: An example from the Northwest Coast. Journal of Anthropological Archaeology nn(nn):nnn--nnn.

## The defensibility of a landscape has already been calculated for an input DEM.
## At the most basic level, this script reads in a file containing site locations and 
## extracts the defensibility for those locations. This allows the researcher to 
## compare the defensibility of archaeological sites to the raw defensibility of the landscape.

## This script also handles all of the statistics and graphs presented in Bocinsky 2013.
## Required inputs include the DEM (created in DEM.R, above), the Visiblity, Elevation, and 
## Defensibility rasters (generated in Defense_ORIG.R, above), a file of site locations, 
## the GSHHS dataset (Global Self-consistent, Hierarchical, High-resolution Geography Database, 
## available at http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html), the waterbodies dataset 
## (created in CanVec_Prepper.R, above), and, for this report, the list of sites analyzed in 
## Martindale and Supernant 2009.

## Several dozen graphs and tables are output.

## Author: R. Kyle Bocinsky
## Date: 04/25/2013

library(sp)
library(gstat)
library(maptools)
library(maps)
library(RColorBrewer)
library(rgeos)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(plotrix)
library(rgdal)
library(reshape)
library(raster)
library(psych)
library(xtable)

########## PARAMETERS ##########
# Set the path to the DEM
demPath <- '../Input/DEM/FINAL_DEM'

# Set the path to the sites data
sitesPath <- '../Input/BC_Sites.csv'

# Set the path to the GSHHS dataset
gshhsDir <- '../Input/gshhs/'

# Set the path of the FUNCTIONS.R script
functionsPath <- './FUNCTIONS.R'

# Set the path to the Elevation raster
elevationPath <- '../Output/rasters/elevation.grd'

# Set the path to the Visibility raster
visibilityPath <- '../Output/rasters/visibility.grd'

# Set the path to the Defensibility raster
defensibilityPath <- '../Output/rasters/defensibility.grd'

# Set the path to the directory containing the waterbodies dataset
waterbodiesDir <- '../Input/CanVEC/'

# Set the path to the sites list from Martindale and Supernant 2009.
martindaleSitesPath <- '../Input/MartindaleSites.txt'

# Set the output path
output <- '../Output/'

########## END PARAMETERS ##########

########## SOURCE ##########
# Load the plotting functions
source(functionsPath)

# Load the DEM
dem <- raster(demPath)

# Set the north, south, east, and west boundaries of the simulation area
# This is done from the DEM
UTM_North <- ymax(dem)
UTM_South <- ymin(dem)
UTM_East <- xmax(dem)
UTM_West <- xmin(dem)

# Create matrix of coordinates
datainUTM<-matrix(c(UTM_East, UTM_West, UTM_West, UTM_East,UTM_East, UTM_North,UTM_North,UTM_South,UTM_South,UTM_North),nrow=5)

# Set universal projection from the DEM
master.proj <- CRS(projection(dem))

# Create SpatialPolygon of simulation area
sim.poly <- Polygons(list(Polygon(datainUTM, hole=FALSE)),ID='A')
sim.poly <- SpatialPolygons(list(sim.poly), proj4string=master.proj)

# Create a raster template of the simulation area
rows <- abs(UTM_North-UTM_South)/res(dem)[1]
columns <- abs(UTM_East-UTM_West)/res(dem)[1]
sim.raster <- raster(ext=extent(sim.poly),nrows=rows,ncols=columns,crs=master.proj@projargs)


### PREPARE THE SITE DATA ###
## This takes raw output from the RAAD database and queries it for residential sites.
# Read the original site data
sites <- read.csv(file=sitesPath)

# Remove modern sites
sites <- subset(sites, (regexpr("precontact", sites$Typology,ignore.case = TRUE) > 0) | (regexpr("postcontact", sites$Typology,ignore.case = TRUE) > 0))

# Keep only residential sites
sites <- subset(sites, regexpr("house", sites$Typology,ignore.case = TRUE) > 0 | regexpr("pit", sites$Typology,ignore.case = TRUE) > 0 | regexpr("habitation", sites$Typology,ignore.case = TRUE) > 0 | regexpr("shelter", sites$Typology,ignore.case = TRUE) > 0 | regexpr("midden", sites$Typology,ignore.case = TRUE) > 0 | regexpr("mound", sites$Typology,ignore.case = TRUE) > 0 | regexpr("hearth", sites$Typology,ignore.case = TRUE) > 0 | regexpr("embankment", sites$Typology,ignore.case = TRUE) > 0)

# Convert the sites data into spatial data
sp_point <- matrix(NA, nrow=nrow(sites),ncol=2)
sp_point[,1] <- sites$Easting
sp_point[,2] <- sites$Northing
colnames(sp_point) <- c("EASTING","NORTHING")
data.sp <- SpatialPointsDataFrame(coords=sp_point,sites,proj4string=master.proj)

# Crop sites data to study area
sites <- as.vector(gIntersects(data.sp,sim.poly,byid=TRUE))
sites <- subset(data.sp,sites)


### PREPARE THE LANDSCAPE DATA ###
# Rough area of the study area, in latlon
WEx <- c(230,240)
WEy <- c(46,50)

# Read in the GSHHS coastline data
america <- getRgshhsMap(paste(gshhsDir,'gshhs_f.b',sep=''), xlim=WEx, ylim=WEy, checkPolygons=TRUE, level=1)
americas <- spTransform(america, master.proj)
americas <- gIntersection(americas,sim.poly)

# Read in the GSHHS political borders data
borders <- Rgshhs(paste(gshhsDir,'wdb_borders_f.b',sep=''),xlim=WEx, ylim=WEy, level = 4)$SP
borders <- spTransform(borders, master.proj)
borders <- gIntersection(borders,sim.poly)

# Create a polygon of the border area
# This is for masking out the US
border <- as.data.frame(borders@lines[[1]]@Lines[[1]]@coords)
border <- rbind(border,c(UTM_East,UTM_South))
border <- rbind(c(UTM_East,UTM_South), border)
border <- Polygons(list(Polygon(border, hole=FALSE)),ID='A')
border <- SpatialPolygons(list(border), proj4string=master.proj)

# Create the dividing line between the US and Canada
borders.points <- coordinates(borders)[[1]][[1]]
borders.points[,1] <- borders.points[,1]-2000
borders2 <- SpatialLines(list(Lines(Line(borders.points),1)),master.proj)
borders.points <- coordinates(borders)[[1]][[1]]
borders.points[,1] <- borders.points[,1]-1000
borders3 <- SpatialLines(list(Lines(Line(borders.points),1)),master.proj)

# Read in the CanVec warerbodies data
waterbodies <- readOGR(waterbodiesDir, layer=ogrListLayers(paste(waterbodiesDir,"waterbodies.shp",sep='')))


### PREPARE THE DEFENSIBILITY DATA ###
# Read in the elevation, visibility, and defensibility rasters
elev <- raster(elevationPath)
vis <- raster(visibilityPath)
defense <- raster(defensibilityPath)

# We only care about defensiveness and the landscape in Canada (mainly because of the site data we have).
# So, set the parts of the rasters in the USA to 'NA'
dem.mask <- mask(dem,border, inverse=TRUE)
elev.mask <- mask(elev,border, inverse=TRUE)
vis.mask <- mask(vis,border, inverse=TRUE)
defense.mask <- mask(defense,border, inverse=TRUE)

# We also only care about the defensibility on land (open water has a visibility of 1 and elevation advantage of 0.5)
# So, set the parts of the rasters in open water to 'NA'
dem.mask <- mask(dem.mask,waterbodies,inverse=TRUE)
elev.mask <- mask(elev.mask,waterbodies, inverse=TRUE)
vis.mask <- mask(vis.mask,waterbodies, inverse=TRUE)
defense.mask <- mask(defense.mask,waterbodies, inverse=TRUE)


### ANALYSIS ###
# Generate matrix versions of each raster (for histograms)
elev.m <- as.matrix(elev.mask)
vis.m <- as.matrix(vis.mask)
defense.m <- as.matrix(defense.mask)
dem.m <- as.matrix(dem.mask)

# Generate a histogram of the elevation of the landscape
dem.h <- hist(dem.m[dem.m<2500], breaks=seq(0,2500,25),plot=F)
dem.h$density <- dem.h$counts/sum(dem.h$counts)

# Generate a histogram of the Elevation index of the landscape
elev.h <- hist(elev.m, breaks=seq(0,1,.01),plot=F)
elev.h$density <- elev.h$counts/sum(elev.h$counts)

# Generate a histogram of the Visibility index of the landscape
vis.h <- hist(vis.m, breaks=seq(0,1,.01),plot=F)
vis.h$density <- vis.h$counts/sum(vis.h$counts)

# Generate a histogram of the Defensibility index of the landscape
defense.h <- hist(defense.m, breaks=seq(0,1,.01),plot=F)
defense.h$density <- defense.h$counts/sum(defense.h$counts)

# Extract site location data from rasters
sites$elev <- extract(elev,sites)
sites$vis <- extract(vis,sites)
sites$defense <- extract(defense,sites)
sites$elevation <- extract(dem,sites)

# Create a vector of site elevations
site.elevs <- sites$elevation[!is.na(sites$elevation)]
site.elevs <- rev(site.elevs[order(site.elevs)])

# Generate a histogram of site elevations
sites.elevation.h <- hist(sites$elevation[sites$elevation<500], seq(0,500,5),plot=F)
sites.elevation.h$density <- sites.elevation.h$counts/sum(sites.elevation.h$counts)

# Generate a histogram of the Elevation index of sites
sites.elev.h <- hist(sites$elev, breaks=seq(0,1,.02),plot=F)
sites.elev.h$density <- sites.elev.h$counts/sum(sites.elev.h$counts)

# Generate a histogram of the Visibility index of sites
sites.vis.h <- hist(sites$vis, breaks=seq(0,1,.02),plot=F)
sites.vis.h$density <- sites.vis.h$counts/sum(sites.vis.h$counts)

# Generate a histogram of the Defensibility index of sites
sites.defense.h <- hist(sites$defense, breaks=seq(0,1,.02),plot=F)
sites.defense.h$density <- sites.defense.h$counts/sum(sites.defense.h$counts)


### MARTINDALE AND SUPERNANT SITE ANALYSIS ###
# Read in the site data from Martindale and Supernant 2009
site.small <- read.csv(martindaleSitesPath)

# Only keep Martindale and Supernant sites included in our study area
site.small <- site.small[site.small$Borden.Number %in% sites$Borden.Number,]

# Re-calculate the Martindale and Supernant indices
site.small$MS.E <- (site.small$MS.E+1)/2
site.small$MS.D <- (site.small$MS.V+site.small$MS.E)/2

# Get the Defensibility indicies for these sites, and order by Borden Number
sites.test <- sites[sites$Borden.Number %in% site.small$Borden.Number,]
sites.test <- sites.test[order(sites.test$Borden.Number),]

# Order by Borden Number
site.small <- site.small[order(site.small$Borden.Number),]

# Transfer over the rasterized Defensibility indices
site.small$vis <- sites.test$vis
site.small$elev <- sites.test$elev
site.small$defense <- (site.small$vis+site.small$elev)/2

# Drop NA sites
site.small <- site.small[!is.na(site.small$vis),]

# Re-order by rasterized defensibility index values
site.small <- site.small[order(site.small$defense),]


### STATISTICAL ANALYSIS ###

# Perform a one-sample Kolmogorov-Smirnov test to compare the defensibility index distribution of sites versus the landscape
defense.ks <- ks.test(sites$defense,ecdf(as.vector(defense.m)))

# Perform a one-sample Kolmogorov-Smirnov test to compare the elevation index distribution of sites versus the landscape
elev.ks <- ks.test(sites$elev,ecdf(as.vector(elev.m)))

# Perform a one-sample Kolmogorov-Smirnov test to compare the visibility index distribution of sites versus the landscape
vis.ks <- ks.test(sites$vis,ecdf(as.vector(vis.m)))


### TRENCH EMBANKMENT ANALYSIS ###
# Extract trench embankment sites
sites.trench <- subset(sites, regexpr("embankment", sites$Typology,ignore.case = TRUE) > 0)
sites.trench.highly <- sites.trench[sites.trench$defense>=0.63,]

# Extract "highly defensible" sites
sites.highly <- sites[!is.na(sites$defense),]
sites.highly <- sites.highly[sites.highly$defense>=0.63,]

# Calculate the proportion of "highly defensible" places on the landscape
denom <- length(defense.m[!is.na(defense.m)])
defense.m.highly <- defense.m[defense.m>=0.63]
num <- length(defense.m.highly[!is.na(defense.m.highly)])


### MAPS ###
# An add-on function to plot sites
plot.sites <- function(){
	points(sites,pch=19,cex=1,col='black')
}

# Plot the DEM with archaeological sites
raster.png(dem.mask,brewer.pal(11,'Spectral'),'DEM_with_sites','Digital Elevation Model','Elevation (meters)','plot.sites')

# Plot the defensibility index rasters
index.raster.png(defense.mask,rev(brewer.pal(11,'RdYlBu')),'landscape_defense','Landscape Defensibility','Defensiveness Index',NA)
index.raster.png(vis.mask,rev(brewer.pal(11,'RdYlBu')),'landscape_vis','Landscape Visibility Advantage','Visibility Index',NA)
index.raster.png(elev.mask,rev(brewer.pal(11,'RdYlBu')),'landscape_elev','Landscape Elevation Advantage','Elevation Index',NA)

## A zoomed-in version around Dionisio Point
dionisio.extent <- c(455250,458760,5426500,5430010)
sites@data$pch <- 17
sites@data[sites@data$Borden.Number=='DgRv-3',]$pch <- 8
plot.sites.dionisio <- function(){
	points(sites,pch=sites@data$pch,cex=10,col='black')
	text(456950,5426800,"Galiano Island", cex=8)
	text(455600,5429100,"Valdes Island", cex=8)
#	text(sites,sites$Borden.Number, cex=10, pos=2)
}
raster.small.png(dem, dionisio.extent,brewer.pal(11,'Spectral'),'DEM_with_sites_dionisio','Digital Elevation Model - Dionisio Point','Elevation (meters)','plot.sites.dionisio')
index.raster.small.png(vis, dionisio.extent,rev(brewer.pal(11,'RdYlBu')),'landscape_visibility_dionisio','Landscape Visibility - Dionisio Point','Visibility Index','plot.sites.dionisio')
index.raster.small.png(elev, dionisio.extent,rev(brewer.pal(11,'RdYlBu')),'landscape_elevation_dionisio','Landscape Elevation - Dionisio Point','Elevation Index','plot.sites.dionisio')
index.raster.small.png(defense, dionisio.extent,rev(brewer.pal(11,'RdYlBu')),'landscape_defensibility_dionisio','Landscape Defensibility - Dionisio Point','Defensibility Index','plot.sites.dionisio')


### FIGURES ###
# Compare the continuous and rasterized versions of defensibility
index.compare.pdf(site.small$MS.V,site.small$vis,"Martindale-Supernant Visibility Index","Rasterized Visibility Index","Martindale_vis",NA)
index.compare.pdf(site.small$MS.E,site.small$elev,"Martindale-Supernant Elevation Index","Rasterized Elevation Index","Martindale_elev",NA)
index.compare.pdf(site.small$MS.D,site.small$defense,"Martindale-Supernant Defensibility Index","Rasterized Defensibility Index","Martindale_defense",NA)

# Plot the empirical cumulative density functions of the defensibility index for the landscape (population) and sites (sample)
# Also plot the difference between the distribution (the K-S statistic) and the point of the 95th percentile of the landscape (the "highly defensible" part)
ecdf.compare.pdf(sites$defense,as.vector(defense.m),'Archaeological Sites','Landscape',"Defensibility Index","defensibility_compare_CDF",NA)

# Plot the empirical cumulative density functions of the elevation index for the landscape (population) and sites (sample)
# Also plot the difference between the distribution (the K-S statistic) and the point of the 95th percentile of the landscape (the "highly defensible" part)
ecdf.compare.pdf(sites$elev,as.vector(elev.m),'Archaeological Sites','Landscape',"Elevation Index","elevation_compare_CDF",NA)

# Plot the empirical cumulative density functions of the visibility index for the landscape (population) and sites (sample)
# Also plot the difference between the distribution (the K-S statistic) and the point of the 95th percentile of the landscape (the "highly defensible" part)
ecdf.compare.pdf(sites$vis,as.vector(vis.m),'Archaeological Sites','Landscape',"Visibility Index","visibility_compare_CDF",NA)


#### TABLES ####
martindale <- site.small
sink(paste(output,'TABLES/martindale.tex',sep=''))
cat("\\begin{sidewaystable}[ht]\n")
cat("\\centering\n")
cat("\\caption{Defensibility, visibility, and elevation indices of Gulf of Georgia archaeological sites, from \\citet{Martindale2009} and this study.}\n")
cat("\\label{tab:martindale}\n")
cat("\\scalebox{0.8}{\n")
cat("\\begin{tabular}{lllcccccc}\n")
cat("\\toprule\n")
cat("& & & \\multicolumn{3}{c}{Martindale and Supernant 2009} & \\multicolumn{3}{c}{This study}\\\\\n")
cat("\\cmidrule(rl){4-6}\n")
cat("\\cmidrule(rl){7-9}\n")
cat("Site Name & Site Type & Borden Number & Visibility & Elevation & Defensibility & Visibility & Elevation & Defensibility \\\\ \n")
cat("\\midrule\n")

print(
  xtable(
    martindale,
    caption = "Defensibility, visibility, and elevation indices of Gulf of Georgia archaeological sites, from \\citet{Martindale2009} and this study.",
    label = "tab:martindale",
    digits=2),
  only.contents=TRUE,
  floating.environment='sidewaystable',
  include.rownames=FALSE,
  include.colnames=FALSE,
  hline.after=NULL
)

cat("\\bottomrule\n")
cat("\\end{tabular}}\n")
cat("\\end{sidewaystable}")
sink()







trench <- as.data.frame(sites.trench)[c("Borden.Number","vis","elev","defense")]
names(trench) <- c('Borden Number','Visibility','Elevation','Defensibility')
trench <- trench[order(-trench$Defensibility),]
print(
  xtable(
    trench,
    caption = "Defensibility, visibility, and elevation indices of trench embankment sites in the Gulf of Georgia region",
    label = "tab:trench",
    digits=2,
    align="llccc"),
  booktabs=TRUE,
  sanitize.text.function = function(x){x},
  table.placement = "ht",
  caption.placement = "top",
  include.rownames=FALSE,
  scalebox=0.8,
  file=paste(output,'TABLES/trench.tex',sep='')
)