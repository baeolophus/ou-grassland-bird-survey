create_support_set <- function (numberofpoints,
                                radius,
                                sizename)

{
  source("source_ensemble_function_support_set_generation.R")
  source("source_ensemble_function_support_set_subsetting_trees.R")
  source("source_ensemble_function_support_set_ensemble_mosaic.R")

  #make sure other functions can access variables from main function per this: http://stackoverflow.com/a/14401168/7226183
  environment(spatial.support.set) <- environment()
  environment(ensemble.function) <- environment()
  
polys <- random.stratified.support.sets(numberofpoints = numberofpoints,
                                              radius)
polys.p <- unlist(polys[[1]])
polys.df <- unlist(polys[[2]])

saveRDS(polys,
        file = paste(SPECIES,
                         sizename,
                         "intermediatefile",
                         "polys",
                          sep = "_"))
#create temporary raster files in a folder that can be deleted as the intermediate ones won't be needed later
rasterOptions()$tmpdir
rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp/",
                            SPECIES,
                            "/",
                            sizename))
send.mail(from = sender,
          to = recipients,
          subject = paste0(sizename,
                           " ensemble is starting for ",
                           SPECIES),
          body = "Save the before and after in case microbenchmark crashes.",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "curryclairem.mail@gmail.com",            
                      passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

microbenchmark1 <- microbenchmark(
  support.list <- lapply(1,#:length(polys.df),
                               FUN = spatial.support.set,
                               spatialdataset = latlong.predictors.SPECIES.spatial,
                               predictor_stack = predictors_stack_with_all_variables,
                               polys.df = polys.df,
                               ntree = ntree,
                               importance = importance),
  times = 1)

microbenchmark1$model <- paste0(sizename,
                               "1")
saveRDS(microbenchmark1,
        file = paste(
          SPECIES,
          sizename,
          "backup_microbenchmark1",
          sep = "_")) 

saveRDS(support.list,
        file = paste(
          SPECIES,
          sizename,
          "intermediates_support_list",
          sep = "_"))

gc() 

rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp/",
                            SPECIES,
                            "/",
                            sizename,
                            "/mosaic"))

microbenchmark2 <- microbenchmark(support.ensemble <- ensemble.function(support.list),
                                        times = 1)
#report microbenchmark values
microbenchmark2$model <- paste0(sizename, "2")
saveRDS(microbenchmark2,
        file = paste(SPECIES,
                      sizename,
                      "backup_microbenchmark2",
                      sep = "_"))

send.mail(from = sender,
          to = recipients,
          subject = paste0(sizename,
                           " small ensemble is complete for ",
                           SPECIES),
          body = "Go download files!  Onward!",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "curryclairem.mail@gmail.com",            
                      passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

rm(support.list)
gc()

unlink(file.path(getwd(),
                 "rastertemp",
                 SPECIES,
                 "/",
                 sizename),
       recursive = TRUE)

return(support.ensemble)
}