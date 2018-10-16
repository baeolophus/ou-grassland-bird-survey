#Figure of data distribution
library(ggplot2)
library(rgdal)
library(sp)
state<-readOGR(dsn="/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")

#transform so same projection as map
state<-spTransform(x = state,
                   CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))




#bring in sightings data
complete.dataset.for.sdm <- read.csv(file = "manuscript_files/oklahomadatasetforsdm_naomit_utm.csv")
complete.dataset.for.sdm.sampling.events <- distinct(complete.dataset.for.sdm,
                                                     SAMPLING_EVENT_ID, 
                                                     Longitude,
                                                     Latitude,
                                                     ebird.time) %>%
  dplyr::filter(!is.na(ebird.time))

#make it spatial and set coordinate system appropriately.
coordinates(complete.dataset.for.sdm.sampling.events)<-c("Longitude", "Latitude")
#make it spatial
proj4string(complete.dataset.for.sdm.sampling.events)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
#transform to lat/long for mapping
complete.dataset.for.sdm.sampling.events<-spTransform(x = complete.dataset.for.sdm.sampling.events,
                                                      CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

#check it worked
proj4string(complete.dataset.for.sdm.sampling.events)
plot(complete.dataset.for.sdm.sampling.events,
    xlab = "UTM x",
    ylab = "UTM y")
plot(state, add = TRUE)

statedf <- fortify(state) #turns it into a data frame

svg("manuscript_files/figures/data_distribution_figure.svg",
    width = 5, 
    height = 5)
ggplot() +
  geom_point(data = as.data.frame(complete.dataset.for.sdm.sampling.events),
             aes(x = Longitude,
                 y = Latitude), 
             size = 0.7) + 
  geom_path(data = statedf,
            aes(x = long,
                y = lat))+
  coord_fixed() + 
  labs(x = "Longitude",
       y = "Latitude")+
  theme_bw()
dev.off()