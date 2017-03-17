#Import data
oklahomadatasetforsdm_naomit_utm <- read.csv("oklahomadatasetforsdm_naomit_utm.csv")

#Only look at our surveys (targeted for grassland birds).
oklahomadatasetforsdm_naomit_utm <- oklahomadatasetforsdm_naomit_utm %>%
  filter(datasource != "EBIRD")
#count how many checklists there are
totalnumberofchecklists <- oklahomadatasetforsdm_naomit_utm %>% 
  distinct(SAMPLING_EVENT_ID) %>% 
  summarize(n()) %>%
  .[[1]]

#Summarize by species in order of abundance.
summary_by_abundance <- oklahomadatasetforsdm_naomit_utm %>%
  filter(quantity.numeric > 0) %>%
  group_by(COMMONNAME) %>%
  summarize(numberofchecklists = n(),
            totalseen = sum(quantity.numeric),
            proportionofchecklists = round(numberofchecklists/totalnumberofchecklists,
                                           2)) %>%
  arrange(desc(numberofchecklists))
