library(raster)


thresholds <- function(SPECIES,
                       rasterfilenamesansspecies,
                       threshold) {
filetothreshold <- raster(paste0(
    SPECIES,
    rasterfilenamesansspecies))

#thresholds from http://neon-workwithdata.github.io/neon-data-institute-2016/R/mask-raster-threshold-R/
thresholded <- filetothreshold
thresholded[thresholded<threshold] <- NA
plot(thresholded,
     main=paste0("probability > ", threshold))

#from http://r-sig-geo.2731867.n2.nabble.com/Calculating-area-of-region-with-a-raster-td7561970.html
f <- freq(thresholded,
          useNA = 'no',
          merge = TRUE,
          digits = 1,
          progress = 'text') 

#from cellcounts to areas: 
area <- sum(f[,2]) * prod(res(thresholded)) 
kmsq <- area/(1000*1000)
return(kmsq)
}


