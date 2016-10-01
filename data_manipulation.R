#libraries used in this file.
library(dplyr)

#import data from csv files.
######
#point counts

#sightings
pointcount.data<-
#metadata
pointcount.metadata<-read.csv(file="pointcount_metadata.csv")
pointcount.metadata$Longitude<-ifelse(sign(pointcount.metadata$Longitude)>0, 
       pointcount.metadata$Longitude*-1, 
       pointcount.metadata$Longitude*1)


#merge the files so every row has all metadata attached.
pointcount.complete<-left_join(pointcount.data,
                               pointcount.metadata,
                               by)


#transects
  
#sightings
transect.data<-read.csv()
#metadata
transect.metadata<-read.csv()
#merge the files so every row has all metadata attached.
transect.complete<-left_join(transect.data,
                             transect.metadata,
                             by)
######

#import gpx files of where transects and point counts should be
library(tools)
library(sp) # R's base package for vector data types
library(raster) # better printing of spatial objects, export shapefiles
library(rgdal) # for reading different spatial file formats
library(rgeos) # for spatial distance and topology operations
library(dplyr) # data manipulation

# Data Import
# gpx files can store a combination of waypoints, routes, and tracks.
# shapefiles, and R's native spatial formats, can only store one type of 
# geometry at a time, e.g. points OR lines OR polygons.
# I don't know a priori which type of data each gpx file has, so I try both 
# waypoints and tracks for all (no routes exist in this dataset), then combine
# into 2 files, one for all waypoints and one for all tracks.

# list all .gpx files in data/ subdirectory
gpx <- list.files(".", pattern = "\\.gpx", full.names = TRUE)
waypoints <- list()
tracks <- list()
for (f in gpx) {
  # label for different types of features, taken from filename without extension
  nm <- file_path_sans_ext(basename(f))
  
  # process waypoints
  wp <- suppressWarnings(
    tryCatch(
      readOGR(dsn=f, layer="waypoints", stringsAsFactors = FALSE),
      error = function(x) NULL
    )
  )
  if (!is.null(wp)) {
    wp$type <- nm
    wp$id <- wp$name
    # only keep useful fields
    wp <- subset(wp, select = c(type, id, time, ele, cmt))
    # change feature IDs to be unique
    row.names(wp) <- paste0(nm, 1:length(wp)) 
    waypoints[nm] <- wp
  }
  
  # process tracks
  tr <- suppressWarnings(
    tryCatch(
      readOGR(dsn=f, layer="tracks", stringsAsFactors = FALSE),
      error = function(x) NULL
    )
  )
  if (!is.null(tr)) {
    tr$type <- nm
    tr$id <- tr$name
    # only keep useful fields
    tr <- subset(tr, select = c(type, id, cmt))
    # change feature IDs to be unique
    row.names(tr) <- paste0(nm, 1:length(tr)) 
    tracks[nm] <- tr
  }
  rm(wp, tr)
}
waypoints <- do.call(rbind, waypoints)
row.names(waypoints) <- as.character(1:length(waypoints))
unique(waypoints$type)

tracks <- do.call(rbind, tracks)
row.names(tracks) <- as.character(1:length(tracks))
unique(tracks$type)

# seperate out point counts, transect start/ends, and any transect tracks
point_counts <- subset(waypoints, !type %in% c("Transect Start_End"))
transect_startend <-  subset(waypoints, type %in% c("Transect Start_End"))

#plotting gpx original points along with actual data points.
#
plot(y=pointcount.metadata$Latitude,
     x=pointcount.metadata$Longitude)
plot(point_counts, add=TRUE)

#First create buffer around each original point (windmill) to be new polygon.
buf <- gBuffer(point_counts, 
               width = 0.003,
               byid=TRUE,
               quadsegs = 8)
plot(buf, col="red", add=TRUE)
# plotting the map with those points and buffers on it for doublechecking


#make point counts into spatial data
pointcount.metadata.clean<-dplyr::filter(pointcount.metadata,
                                         !is.na(Latitude),
                                         !is.na(Longitude))
coordinates(pointcount.metadata.clean) <- c("Longitude", "Latitude")
proj4string(pointcount.metadata.clean)<-proj4string(point_counts)
#use over() to take actual points (ptsrand) and join with vegmap/buffers
pointcount.join <- sp::over(pointcount.metadata.clean, buf)
# Integrate this vegetation data into the SpatialPoints object

pointcount.metadata.clean$newnames<-pointcount.join$id
pointcount.metadata.newnames<-merge(x=pointcount.metadata,
                                        y=as.data.frame(pointcount.metadata.clean),
                                    all.x=TRUE,
                                    all.y=FALSE,
                                        by=c("Date",
                                             "Observer",
                                             "Location",
                                             "Point",
                                             "StandardizedLocationName",
                                             "LocalSurveyName",
                                             "Start.Time..24h.",
                                             "Latitude",
                                             "Longitude",
                                             "Cloud",
                                             "Wind",
                                             "Notes",
                                             "Species.before",
                                             "Species.After"))
                                                    
#Then the actual points should have the nearby names on them.

#Export file, and correct any dubious ones manually in spreadsheet.
