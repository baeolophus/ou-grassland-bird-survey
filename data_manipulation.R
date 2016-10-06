#libraries used in this file.
library(tools)
library(sp) # R's base package for vector data types
library(raster) # better printing of spatial objects, export shapefiles
library(rgdal) # for reading different spatial file formats
library(rgeos) # for spatial distance and topology operations
library(dplyr) # data manipulation

#import data from csv files.
######
#point counts
#metadata
pointcount.metadata<-read.csv(file="pointcount_metadata.csv")
pointcount.metadata$Longitude<-ifelse(sign(pointcount.metadata$Longitude)>0, 
       pointcount.metadata$Longitude*-1, 
       pointcount.metadata$Longitude*1)
pointcount.metadata$index<-rownames(pointcount.metadata)

#####

#import gpx files of where transects and point counts should be

##this section of code/comments for importing gpx given by a friend of colleague's (do not know name)
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
##end borrowed


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

#add column of new names back into original clean file.
pointcount.metadata.clean$newspotnames<-pointcount.join$id

#then use index (rownames) to join with original file.
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
                                             "Species.After",
                                             "index"))
                                                    
#Then the actual points should have the nearby names on them.

#Export file, and correct any dubious ones manually in spreadsheet.
write.csv(pointcount.metadata.newnames,
          file="20161003_pointcount_metadata_newnames.csv")

#Export dubious points to .gpx file.
#requires "name" field, use "index" as this.
pointcount.metadata.clean@data$name <- pointcount.metadata.clean@data$index  
#get dubious points only.
fix.pointcounts<-pointcount.metadata.clean[is.na(pointcount.metadata.clean$newspotnames),]

writeOGR(fix.pointcounts["name"], driver="GPX", layer="waypoints", 
         dsn="2016104_unnamed_points.gpx")
#use this to compare original basecamp point list to where point counts conducted

##sightings
pointcount.data<-read.csv(file="pointcount_data.csv")
#bring in the manually corrected file
pointcount.metadata.manually.corrected<-read.csv(file="20161003_pointcount_metadata_newnames_manual_corrections.csv")
#merge the files so every row has all metadata attached.
pointcounts.complete<-left_join(pointcount.data,
                                pointcount.metadata.manually.corrected,
                                    by=c("Date",
                                         "Observer",
                                         "Location",
                                         "Point"))
#Compare pointcounnts.complete row count vs pointcount.data.  
#Pointcount.data is the standard, number should be same.
#Need to check which site/date/obs combos are identical but should not be.

count.combos<-distinct(pointcount.metadata.manually.corrected,
                       Date,
                       Observer,
                       Location,
                       Point,
                       newspotnames) #number of unique.
#Should match number of rows in pointcount.metadata.manually.corrected
#Use which.have.duplicates to see which have duplicate primary key columns.
which.have.duplicates<-group_by(pointcount.metadata.manually.corrected,
                                Date,
                                Observer,
                                Location,
                                Point,
                                newspotnames)%>%
  summarize(duplicates=n())
which.have.duplicates[which.have.duplicates$duplicates>1,]
#after corrections for bad matches in primary key columns and duplicates, now only 6 rows apart.

old.count.combos<-distinct(pointcount.metadata.manually.corrected,
         Date,
         Observer,
         Location,
         Point)
#This one is one short.  So there is one lumped one left??
#Check without the new unique sitenames (newspotnames).
which.have.duplicates.old<-group_by(pointcount.metadata.manually.corrected,
                                Date,
                                Observer,
                                Location,
                                Point)%>%
  summarize(duplicates=n())
which.have.duplicates.old[which.have.duplicates.old$duplicates>1,]
#One sighting (05/08/2014 Roy WXRD point 7 is duplicates).  Found mistaken naming in metadata file.

#Now all numbers match!  number of distinct key columns/sites/observer/date and 
#number of pointcount.metedata.manually.corrected rows are all same.
#pointcount.data and pointcounts.complete also match because all primary keys unique.


#transects

#sightings
transect.data<-read.csv()
#metadata
transect.metadata<-read.csv()
#merge the files so every row has all metadata attached.
transect.complete<-left_join(transect.data,
                             transect.metadata,
                             by)
