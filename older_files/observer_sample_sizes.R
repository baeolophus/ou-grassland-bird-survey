complete.dataset.for.sdm <- read.csv(file = "oklahomadatasetforsdm_naomit_utm.csv")
ours.only <- complete.dataset.for.sdm[complete.dataset.for.sdm$datasource!="EBIRD",]

unique.observers <- distinct(ours.only,
                             Observer, 
                             year)

fix <- filter(ours.only,
       Observer == "MDF ")




pointcount.data$Observer <- gsub("MDF ", "MDF",
                                 pointcount.data$Observer, 
                                 fixed = TRUE)
pointcount.metadata.manually.corrected$Observer <- gsub("MDF ", "MDF",
                                                        pointcount.metadata.manually.corrected$Observer, 
                                                        fixed = TRUE)
complete.dataset.for.sdm$Observer <- gsub("MDF ", "MDF",
                                          complete.dataset.for.sdm$Observer, 
                                                        fixed = TRUE)

filter(pointcount.data,
       Observer == "MDF ")
filter(pointcount.metadata.manually.corrected,
       Observer == "MDF ")

write.csv(pointcount.data,
          file="pointcount_data.csv")

write.csv(pointcount.metadata.manually.corrected,
          file="pointcount_metadata.csv")

write.csv(complete.dataset.for.sdm,
          file = "oklahomadatasetforsdm_naomit_utm.csv")

#count point counts.
distinct(pointcounts.complete, newspotnames) %>% arrange(.)
#339 plus an NA
#range of number of visits (1-4)
summary(distinct(pointcounts.complete, newspotnames, Date, Observer) %>% arrange (.) %>% group_by(newspotnames)%>%
          summarize(n()))

transect.complete$START.newspotnames.short <- gsub("\\(.*","",
                                                   transect.complete$START.newspotnames)
transect.complete$START.newspotnames.short <- gsub(" start",
                                                   "",
                                                   transect.complete$START.newspotnames.short,
                                                   fixed = TRUE)
transect.complete$START.newspotnames.short <- gsub(" end",
                                                   "",
                                                   transect.complete$START.newspotnames.short,
                                                   fixed = TRUE)
transect.complete$START.newspotnames.short <- substr(transect.complete$START.newspotnames.short, 1, 6)

#how many transects = 87
distinct(transect.complete, START.newspotnames.short) %>% arrange (.) 
#range of number of visits (1-4)
summary(distinct(transect.complete, START.newspotnames.short, Date, Observer) %>% arrange (.) %>% group_by(START.newspotnames.short)%>%
  summarize(n()))
