source("2_map setup.R")

# count number of receivers available in Hornsby and Rupnik studies
lake_ontario <- great_lakes %>% filter(name == "Lake Ontario")

lake_receivers %>%
  mutate(deploy_date = date(deploy_date_time),
         recover_date = date(recover_date_time)) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>% select(recover_date) %>% summarize
  filter(recover_date %in% seq(date("2015-06-01"), 
                               date("2015-06-01")+730, by = 1)) %>%
  st_intersection(lake_ontario) 
  
