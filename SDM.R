####Species distribution modelling
# set working directory
setwd("~/Documents/Speciale2.0")


#Load libraries
library(biomod2)
library(tidyr)
library(terra)
library(dismo)
library(dplyr)
library(viridis)
library(raster)
library(rgdal)
library(rJava)
library(jsonlite)
library(data.table)
library(magick)
library(png)
library(ggplot2)
library(ggthemes)
library(sp)
library(sf)
sf::sf_use_s2(FALSE)
library(maptools)
library(readxl)

#set up the prepPara function from https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix2_prepPara.R as i cannot figure out how to load it from source

prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                     responsecurves=TRUE,
                     jackknife=TRUE,      
                     outputformat="logistic",
                     outputfiletype="asc", 
                     projectionlayers=NULL,
                     randomseed=FALSE,
                     removeduplicates=TRUE,
                     betamultiplier=NULL,
                     biasfile=NULL,
                     testsamplesfile=NULL,
                     replicates=1,
                     replicatetype="crossvalidate",
                     writeplotdata=TRUE,
                     extrapolate=TRUE,
                     doclamp=TRUE,
                     beta_threshold=NULL,
                     beta_categorical=NULL,
                     beta_lqp=NULL,
                     beta_hinge=NULL,
                     applythresholdrule=NULL
){
  #20 & 29-33 features, default is autofeature
  if(is.null(userfeatures)){
    args_out <- c("autofeature")
  } else {
    args_out <- c("noautofeature")
    if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
    if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
    if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
    if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
    if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
  }
  
  
  #1 
  if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
  #2
  #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
  #3
  if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
  #4
  args_out <- c(args_out,paste0("outputformat=",outputformat))
  #5
  args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
  #7
  if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
  #10
  if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
  #16
  if(removeduplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
  #20 & 53-56
  # check if negative
  betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
  if(! is.null(betas) ){
    for(i in 1:length(betas)){
      if(betas[i] <0) stop("betamultiplier has to be positive")
    }
  }
  if (  !is.null(betamultiplier)  ){
    args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
  } else {
    if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
    if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
    if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
    if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
  }
  #22
  if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
  #23
  if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
  #24&25
  replicates <- as.integer(replicates)
  if(replicates>1 ){
    args_out <- c(args_out,
                  paste0("replicates=",replicates),
                  paste0("replicatetype=",replicatetype) )
  }
  #37
  if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
  #39
  if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
  #42
  if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
  #60
  if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
  
  return(args_out)
}  

occ_data<-read_xlsx("Data/Occurence_data/Occ_data_new.xlsx")

# Determine geographic extent of our study area
max_lat <- ceiling(max(occ_data$lat))
min_lat <- floor(min(occ_data$lat))
max_lon <- ceiling(max(occ_data$long))
min_lon <- floor(min(occ_data$long))
geographic_extent <- extent(x = c(min_lon, max_lon, min_lat, max_lat))

###load and reproject environmental data
#Current Climate 
clim_list <- list.files("~/Documents/Uni/Speciale/SDM Madagascar/SDM_madagscar/Bioclim", pattern = ".bil$", 
                        full.names = T)  # '..' leads to the path above the folder where the .rmd file is located
clim <- raster::stack(clim_list)

# crop study area to a manageable extent (rectangle shaped)
Madagascar_area <- crop(clim,extent(geographic_extent)) 


#Slope
slope<-raster("~/Documents/Speciale2.0/Data/Environmental_data/Slope.tif")
slope <- crop(slope,extent(geographic_extent)) 
#Elevation
elevation<-raster("~/Documents/Speciale2.0/Data/Environmental_data/Elevation.tif")
elevation <- crop(elevation,extent(geographic_extent)) 
#Soil
soil<- raster("~/Documents/Speciale2.0/Data/Environmental_data/Soil.tif")
soil <- crop(soil,extent(geographic_extent)) 
#Landcover
landcover<-raster("~/Documents/Speciale2.0/Data/Environmental_data/Landcover.tif")
landcover <- crop(landcover,extent(geographic_extent)) 
#Spatial filters
Lat<-raster("~/Documents/Speciale2.0/Data/Environmental_data/Latitude.tif")
Lat <- crop(Lat,extent(geographic_extent)) 
Long<-raster("~/Documents/Speciale2.0/Data/Environmental_data/Longitude.tif")
Long <- crop(Long,extent(geographic_extent)) 
Spatialfilter <- raster::stack(Lat,Long)

###Stack environmentl data
Environmental <- raster::stack(Madagascar_area,slope,elevation,soil,landcover,Spatialfilter)
plot(stack (elevation, soil, Madagascar_area[[1]], landcover,Long, Lat))


#writeRaster(Environmental,
            # a series of names for output files
           # filename=paste0("~/Documents/Speciale2.0/Resultater/Environment_variabels",names(Environmental),".asc"), 
           # format="ascii", ## the output format
           # bylayer=TRUE, ## this will save a series of layers
           # overwrite=T)

Environmental<-list.files("~/Documents/Speciale2.0/Resultater/Environment_variabels", pattern = ".asc$", 
                          full.names = T)
Env<-raster::stack(Environmental)

#Extract unique species (131 species)
unique_species <- unique(occ_data$Species)

###### Run in a loop from here ######
#Make species subset
for (Species in unique_species) {
  
  filename <- paste0(Species)
  
  species_data <- occ_data[occ_data$Species == Species, ]
  # make occ spatial
  coordinates(species_data) <- ~long + lat
  
  # thin occ data (keep one occurrence point per cell)
  cells <- cellFromXY(Env[[1]], species_data)
  dups <- duplicated(cells)
  species_final <- species_data[!dups, ]
  cat(nrow(species_data) - nrow(species_final), "records are removed")
  
  # select background points from this buffered area; when the number provided 
  set.seed(1) 
  bg <- sampleRandom(x=Env,
                     size=10000,
                     na.rm=T, #removes the 'Not Applicable' points  
                     sp=T) # return spatial points 
  plot(Env[[1]])
  # add the background points to the plotted raster
  plot(bg,add=T,col= "black", pch=4, cex=0.5) 
  # add the occurrence data to the plotted raster
  plot(species_final,add=T,col="red",pch=20)
  
  
  # extracting env conditions for training occ from the raster
  # stack; a data frame is returned (i.e multiple columns)
  p <- extract(Env, species_final)
  # extracting env conditions for background
  a <- extract(Env, bg)
  
  # repeat the number 1 as many numbers as the number of rows
  # in p, and repeat 0 as the rows of background points
  pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))
  
  
  # (rep(1,nrow(p)) creating the number of rows as the p data
  # set to have the number '1' as the indicator for presence;
  # rep(0,nrow(a)) creating the number of rows as the a data
  # set to have the number '0' as the indicator for absence;
  # the c combines these ones and zeros into a new vector that
  # can be added to the Maxent table data frame with the
  # environmental attributes of the presence and absence
  # locations
  pder <- as.data.frame(rbind(p, a))
  
  # train Maxent with tabular data
  mod <- maxent(x=pder, ## env conditions
                p=pa,   ## 1:presence or 0:absence
                
                path=paste0("Resultater/SDM_model/",file=filename), ## folder for maxent output; 
                # if we do not specify a folder R will put the results in a temp file, 
                # and it gets messy to read those. . .
                args=c("responsecurves") ## parameter specification
  )
  
  
  
  mod1 <- maxent(x=pder,
                 p=pa, 
                 path=paste0("Resultater/SDM_maps/",file=filename),
                 args=prepPara(userfeatures="LQ",
                               betamultiplier=1,
                               projectionlayers="Resultater/Environment_variabels/") )
  
}
mod@results
mod1@results



#Extract unique species
folder <- unique(occ_data$Species)

folder_list <- as.data.frame(folder)

# Specify the name of the file you want to extract
file_to_extract <- "species_layer.4.dat"

###### Run in a loop from here ######


for ( folder in folder_list$folder) {
  # Create the path to the current folder
  main_folder_path <- file.path("Resultater/SDM_maps",folder)
  
  # Create the path to the second-level folder within the main folder
  second_level_folder_path <- file.path(main_folder_path, "plots")
  
  #Construct the full file path
  full_file_path <- file.path(second_level_folder_path, file_to_extract)
  
  # Read the file
  Bio1 <- read.table(full_file_path, header = TRUE)
  Bio1 <- separate(Bio1,variable.x.y, into = c("Bio","Temp",folder),sep = ",")
  Bio1 <-Bio1[,c("Temp",folder)]
  indices_to_extract <- seq(from = 1, to = nrow(Bio1), by = 5)
  df <- Bio1[indices_to_extract, ]
  print(df)
  filename <- paste0(folder)
  path=paste0("Resultater/Responscurves/Landcover/",file=filename)
  write.csv(df, path, row.names = FALSE)
  
}




ped <- raster(paste0("Resultater/SDM_maps/Voanioala gerardii/species_Environment_variabels.asc"))
plot(ped)
pdf("Resultater/Maps/Voanioala gerardii.pdf", width = 4, height = 5)  # Adjust width and height as needed
plot(ped)
title("Voanioala gerardii")
dev.off()  # Close the PDF device



