# where did the SMB go in the main lake?

require("gdistance");
require("sp"); 
require("gganimate")
library(ggpubr)
library(mapview)
#library(export)
library(readxl)
library(lubridate)
library(raster)
library(glatos)
library(sf)
library(tidyverse)

windowsFonts(Times=windowsFont("Times New Roman"))
release_date = date("2018-09-23")
#### Data - read in and format detections from GLATOS  ########################

release_site = st_as_sf(data.frame(Name = "Shelby St Boat Ramp", "Latitude" = 41.454647, "Longitude" = -82.723714),
                        coords = c("Longitude", "Latitude"), crs = 4326)

fish_and_rec_data <- read_glatos_workbook("data/GLATOSWeb_Submission_Package/GLATOSWeb_Slagle_Data_no_macros.xlsx")

lake_detections <- read_glatos_detections("data/GLATOS Export/SBSMB_detectionsWithLocs_20220622_202614.csv")
lake_receivers <- read_glatos_receivers("data/GLATOS Export/GLATOS_receiverLocations_20220617_131806.csv")

# format data
head(lake_detections)
lake_detections$detection_timestamp_utc = with_tz(lake_detections$detection_timestamp_utc, tz = "US/Eastern")
lake_detections$detection_date = date(lake_detections$detection_timestamp_utc)
bass_id_numbers = sort(fish_and_rec_data$animals$tag_id_code)
lake_detections$station = as.character(lake_detections$station) #chr format for abacus plots


### Remove false detections    ##########################################
#add binary col for false detections; rule of thumb for tf (threshold time) is 30 X delay, so 1800 for short interval of SMB tags
lake_detections <- false_detections(lake_detections, tf = 1800, show_plot = F)

#base function doesn't allow for multiple delays; create two different tf's and filter out false detections
#passed_filter_modified == 1 means it's a legit detection based on 
lake_detections %>%
  mutate(tf = ifelse(detection_timestamp_utc < min(detection_timestamp_utc)+ddays(28), 1800, 7200),
         passed_filter_modified = ifelse(min_lag<tf, 1,0))-> lake_detections
#also, some NA's in min_lag when fish was only detected at that location once. 
lake_detections$passed_filter_modified[is.na(lake_detections$passed_filter_modified)] <- 0

#doublecheck that we did it right
lake_detections %>%
  group_by(Month = month(detection_date),
           Year = year(detection_date))%>%
  summarize(n_std_filter = sum(passed_filter==0),
            n_new_filter = sum(passed_filter_modified==0)) %>%
  arrange(Year, Month)


#save data without false detections (as GLATOS dataframe)
lake_detections_filtered <- filter(lake_detections, passed_filter_modified==1) %>%
  mutate(days_at_large = suppressMessages(seconds_to_period(int_length(interval("2018-09-23", detection_date)))/ days(1))) %>%
  as_tibble
  #filter(!transmitter_id %in% c("24046", "24060", "24054", "24059")) # filter out dead and unknown fates

# simple feature of receivers by study year
# note - this ONLY includes receivers that detected bass!
receivers_by_year <- lake_detections_filtered %>%
  mutate(study_year = case_when(detection_date < release_date+365 ~ 1,
                                detection_date %in% seq(release_date+365, 
                                                        release_date+730, by = 1) ~ 2,
                                detection_date %in% seq(release_date+730, 
                                                        release_date+1095, by = 1) ~ 3,
                                T ~ 4)) %>%
  group_by(station, study_year, deploy_lat, deploy_long) %>%
  summarize(n_detections = n(),
            initial_date = min(detection_date),
            last_date = max(detection_date)) %>%
  arrange(study_year, station) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326)


### Make Simple Features for acoustic receivers   ################################
# make bounding box for erie
erie_bounds = c(xmin = -83.5338, ymin = 41.2569, xmax = -78.6614, ymax = 43.1208)
erie_bounds = st_bbox(erie_bounds) %>% #create new polygon of bounding box for inset map
  st_as_sfc() %>% 
  st_set_crs(4326)

# calc dist of each receiver from boat ramp, also create SF of receivers
lake_receivers_filtered <- lake_receivers %>%
  dplyr::select(station, glatos_array, deploy_lat, deploy_long, deploy_date_time, recover_date_time) %>%
  as_tibble %>%
  mutate(deploy_date = date(deploy_date_time),
         recover_date = date(recover_date_time)) %>%
  dplyr::select(-deploy_date_time, -recover_date_time) %>%
  filter(recover_date > "2018-09-01", !is.na(recover_date)) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  st_intersection(erie_bounds) %>%
  mutate(dist_from_ramp = as.double(round(st_distance(., release_site),digits = 1)))

#same, for receivers still in the lake
lake_receivers_at_large <- lake_receivers %>%
  dplyr::select(station, glatos_array, deploy_lat, deploy_long, deploy_date_time, recover_date_time) %>%
  as_tibble %>%
  mutate(deploy_date = date(deploy_date_time),
         recover_date = date(recover_date_time)) %>%
  dplyr::select(-deploy_date_time, -recover_date_time) %>%
  filter(is.na(recover_date)) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  st_intersection(erie_bounds) %>%
  mutate(dist_from_ramp = as.double(round(st_distance(., release_site), digits = 1)))


# number of receivers during project
lake_receivers %>%
  dplyr::select(station, glatos_array, deploy_lat, deploy_long, deploy_date_time, recover_date_time) %>%
  as_tibble %>%
  mutate(deploy_date = date(deploy_date_time),
         recover_date = date(recover_date_time)) %>%
  dplyr::select(-deploy_date_time, -recover_date_time) %>%
  filter(recover_date > "2018-09-01",
         deploy_date < max(lake_detections_filtered$detection_date)-1) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  st_intersection(erie_bounds) %>%
  mutate(dist_from_ramp = as.double(round(st_distance(., release_site),digits = 1))) 


### Individuals    ###################################

# add recap report as a separate point - called in tag from angler
recap_reports <- tibble(detection_timestamp_utc = as.POSIXct("2019-07-07 12:00:00"),
                        transmitter_id = c("24066"), 	
                        detection_date = mdy("7/7/2019"),
                        days_at_large = as.numeric(detection_date - ymd("2018-09-23")),
                        deploy_lat = 41.678617,
                        deploy_long = -82.852101,
                        recap = TRUE,
                        receiver_sn = "1")

# which fish have been detected recently?
condensed_detections = detection_events(lake_detections_filtered, time_sep = 86400) # 1 hour

# which fish have been detected recently?
condensed_detections %>%
  filter(last_detection > ymd(20210101)) # last DL 12/4/19

#individual detections - closer look
# lake_detections_filtered %>%
#   filter(detection_timestamp_utc > ymd(20200601),
#          receiver_sn %in% c(547931, 483737)) %>%
#   filter(animal_id == "A69-1602-24062") %>% view

lake_detections_filtered %>%
  filter(animal_id == "A69-1602-24068") %>%
  abacus_plot(., location_col = "station")

# saveRDS(lake_receivers_filtered, file = "data/lake_receivers_filered.RDS")
# saveRDS(lake_detections_filtered, file = "data/lake_detections_filtered.RDS")


### Graph detections over season -excludes Sandusky Bay receivers  ##############
lake_detections_filtered %>%
  filter(glatos_array != "SDB") %>%
  mutate(month = month(detection_timestamp_utc, label = TRUE)) %>%
  group_by(month) %>%
  summarize(n_detections = n()) %>%
  ggplot(aes(x = month, y = n_detections))+
    geom_col(fill = "orange")+
    cowplot::theme_cowplot()+
    labs(y = "Detections (n)")+
    scale_y_continuous(expand = c(0,0))+
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 14), 
          axis.text = element_text(size = 14))

ggsave("figures/detections by month.png", width = 9, height = 6, units = "in")


### Create dataframes for animation    #####################################

# get all detections for fish moving around outside Sandusky Bay
main_lake_fish_locs <- lake_detections_filtered %>%
  unique() %>%
  filter(transmitter_id %in% c(24047, 24052, 24055, 24062, 24064, 24065, 24066, 24068), #only fish moving around in main lake
         !is.na(detection_timestamp_utc)) %>% 
  mutate(recap = FALSE) %>% 
  full_join(recap_reports) %>% #join in recaps
  arrange(detection_date)

main_lake_fish_locs %>%
  filter(is.na(detection_timestamp_utc))

# read in erie transition layer 
 erie_trans <- readRDS("data/erie_trans_layer.rds")

# interpolate fish paths - makes daily paths between detection events
# # use transition layer to get fish to avoid land - need to make a proper layer here!
# main_lake_paths = interpolate_path(main_lake_fish_locs #%>%
#                                      #filter(detection_date < release_date+(365*2)), use this to trim off final data
#                                    trans = erie_trans$transition,
#                                    out_class = "tibble",
#                                    lnl_thresh = 1.1, # threshold for linear/nonlinear interp - default = .9, working = 1.1
#                                    int_time_stamp = (86400/2)) #time step for interpolation; 86400 = 1 day
# 
# saveRDS(main_lake_paths, file = "data/main_lake_paths_2_years.RDS")
 # going with 2 years of data
main_lake_paths = readRDS(file = "data/main_lake_paths_2_years.RDS")

# reformat path points 
main_lake_paths <- main_lake_paths %>%
  filter(!is.na(animal_id)) %>%
  mutate(date = date(bin_timestamp),
         time_step = as.integer(date - release_date),
         month = month(date, label = TRUE, abbr = FALSE),
         year = year(date),
         transmitter_id = substr(animal_id, 10, 15))
# 
# # # did interpolate_paths work?
# main_lake_paths %>%
#   filter(transmitter_id == 24062) %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   group_by(transmitter_id) %>%
#   summarize(do_union = F) %>%
#   st_cast("LINESTRING") %>%
#   mapview()+mapview(erie_trans$rast, maxpixels =  10035900)

         