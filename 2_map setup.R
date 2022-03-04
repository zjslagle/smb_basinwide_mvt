
source("1_data format.R") #<- need to source if you haven't already
library(RColorBrewer)
library(leafem)
library(leaflet)
library(magrittr)


map_dir = "shapefiles/"      #local = "C:/Users/zakjs/Documents/Ohio DoW/Maps/"


### read in shapefiles   ###################################
states = st_read(paste0(map_dir, "ne_10m_admin_1_states_provinces.shp"))
us_and_can <- states %>% filter(gu_a3 == "CAN"|gu_a3 == "USA"|gu_a3 == "MEX") #filter out US and CAN only
urban_areas = st_read(paste0(map_dir, "ne_10m_urban_areas.shp"))  #cities
lake_erie = st_read(paste0(map_dir, "Lake_Erie_Shoreline.shp"))
great_lakes <- st_read(paste0(map_dir, "ne_10m_lakes.shp"))

great_lakes <- great_lakes %>% filter(name_alt == "Great Lakes") # Great Lakes only (otherwise, this is all lakes)


#place_names %<>% filter(NAME %in% c("Lorain", "Sandusky"))
place_names <- read_excel(paste0(map_dir, "place names.xlsx")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


### summarize n_detections and unique fish at each receiver   ######################
lake_receivers_filtered %<>%
  left_join(lake_detections_filtered %>% count(station, name = "n_detections")) %>%
  left_join(lake_detections_filtered %>% 
              group_by(station, transmitter_id) %>% 
              tally %>%
              group_by(station) %>%
              summarize(n_fish = length(transmitter_id))) %>%
  mutate(n_detections = ifelse(is.na(n_detections), 0, n_detections))


##### summarize individual fish movements with points and lines between them   ###############################
# combine Sandusky Bay detections so movement distances aren't biased by moves b/t bay receivers
last_bay_detection <- lake_detections_filtered %>%
  filter(glatos_array == "SDB") %>%
  arrange(transmitter_id, detection_timestamp_utc) %>%
  group_by(transmitter_id) %>%
  summarize(last_detected_in_bay = last(detection_timestamp_utc),
            last_detected_date = date(last_detected_in_bay))


# Get detections for fish detected outside of SDB array, keep only one SDB detection
# NOTE - NOT interpolated!
detection_points <- lake_detections_filtered %>%
  left_join(last_bay_detection) %>%
  group_by(transmitter_id) %>%
  mutate(keep = if_else(glatos_array == "SDB", # remove all bay detection except last one
                        if_else(detection_timestamp_utc == last_detected_in_bay, 
                                TRUE, FALSE),
                        TRUE)) %>%
  filter(keep == TRUE) %>%  
  mutate(n_detections = n()) %>% # filter out fish only detected in SDB
  ungroup %>%
  filter(n_detections > 1) %>%
  arrange(animal_id, detection_timestamp_utc) %>%
  dplyr::select(transmitter_id, days_at_large, detection_date, deploy_lat, deploy_long, receiver_sn) %>%
  unique() %>%
  mutate(recap = FALSE) %>% 
  full_join(recap_reports) %>% #join in recaps
  mutate(monthyear = paste(month(detection_date, label = TRUE, abbr = TRUE), year(detection_date))) %>%
  filter(transmitter_id != "24049") %>% #fish barely moved, just happened to be detected on a non SDB receiver
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>% #make sf
  arrange(transmitter_id, detection_date)

# now make lines in between the detections
detection_lines <- detection_points %>%
  group_by(transmitter_id) %>%
  summarize(last_detected = max(detection_date),
            do_union = F) %>% # do_union allows preservation of the order
  filter(!st_is(., "POINT")) %>%  #gets rid of trouble spots (shouldn't actually remove anything)
  st_cast("LINESTRING") %>%
  mutate(total_dist_km = round(st_length(.)/1000, 1))


recap_reports %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) -> recap_points

#finally, condense points into date ranges (when fish were detected at each receiver) 
lake_detections_filtered %>%
  dplyr::select(transmitter_id, days_at_large, detection_date, deploy_lat, deploy_long, receiver_sn) %>%
  unique() %>%
  #filter(transmitter_id %in% c(24047, 24052, 24055, 24062, 24064, 24065, 24066, 24068)) %>% #only fish moving around in main lake
  mutate(recap = FALSE) %>% 
  full_join(recap_reports) %>% #join in recaps
  arrange(detection_date) %>%
  group_by(transmitter_id, receiver_sn) %>%
  summarize(first_detected = min(detection_date),
            last_detected = max(detection_date),
            deploy_lat = mean(deploy_lat),
            deploy_long = mean(deploy_long),
            n_detection = n()) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) -> detection_points_summary

#units are annoying
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x<- as.vector(x)
  x
}

#remove units
detection_lines %<>%  mutate(total_dist_km = round(clean_units(total_dist_km), 1))

### Get larger bounding box for whole lake   ############################################
main_lake_bbox <- main_lake_paths %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_bbox()

main_lake_bbox <- c(main_lake_bbox[c(1,2)]-.25, 
                    main_lake_bbox[c(3,4)]+.25) # add/subtract 0.1 to above bounding box


main_lake_frame <- st_bbox(main_lake_bbox) %>%
  st_as_sfc() %>% 
  st_set_crs(4326)

main_lake_frame <- st_transform(main_lake_frame, crs = 32617)

