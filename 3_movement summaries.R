
source("2_map setup.R")

### Basic individual summaries    ###########################
# View individual fish detection summary
# detection_points_summary %>%
#   filter(transmitter_id == 24068) %>% 
#   arrange(first_detected) %>% view

# raw detection data for given fish and receiver - doublecheck quick movements are real
# NOT FILTERED for false detections!
lake_detections %>%
  dplyr::select(transmitter_id, detection_timestamp_utc, receiver_sn, passed_filter_modified) %>%
  filter(transmitter_id == 24068,
         receiver_sn == 122441)

# Filtered and summaried detection data     
lake_detections_filtered %>%
  dplyr::select(transmitter_id, station, days_at_large, detection_date, deploy_lat, deploy_long) %>%
  unique() %>%
  filter(transmitter_id %in% c(24047, 24051, 24052, 24055, 24062, 24064, 24065, 24066, 24068)) %>%
  filter(transmitter_id == 24064)


### Group summaries    ##############
detection_lines
summary(detection_lines$total_dist_km)

# movement distances by year post-release - 
#  use interpolated points, otherwise we miss distances b/t years
interpolated_points <- main_lake_paths %>%
  left_join(last_bay_detection) %>%
  left_join(lake_receivers %>% select(glatos_array, deploy_lat, deploy_long), 
            by = c("latitude" = "deploy_lat",
                   "longitude" = "deploy_long")) %>%   #need lake_receivers for filtering out Sandusky Bay detections except for last
  group_by(transmitter_id) %>%
  mutate(keep = case_when(date < last_detected_date ~ FALSE, # remove detections before "last detected in bay"
                          transmitter_id == 24066 & latitude < 41.49441 ~ F, #hacky method of removing 3 in-bay detections from interpolated data
                          T ~ T)) %>% 
  filter(keep == TRUE) %>%  
  mutate(n_detections = n()) %>% # filter out fish only detected in SDB
  ungroup %>%
  filter(n_detections > 1) %>%
  mutate(year_post_release = case_when(time_step < 365 ~ 1, # years post-release
                               time_step < 730 ~ 2,
                               time_step < 1095 ~ 3))

# Make lines by year
interpolated_lines <- interpolated_points %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% #make sf
  group_by(transmitter_id, year_post_release) %>%
  summarize(date_left_bay = min(date),
            date_last_detected = max(date),
            censored_days = date_left_bay - release_date,
            do_union = F) %>% # do_union allows preservation of the order
  st_cast("LINESTRING") %>%
  ungroup() %>%
  mutate(total_dist_km = round(st_length(.)/1000, 1))

# write to CSV
interpolated_lines %>%
  st_drop_geometry() %>%
  write_csv("data derived/summary - lakewide mvt by fish and year.csv")

# Make lines by fish (all years combined)
interpolated_lines_total <- interpolated_points %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% #make sf
  group_by(transmitter_id) %>%
  summarize(date_left_bay = min(date),
            date_last_detected = max(date),
            do_union = F) %>% # do_union allows preservation of the order
  #filter(!st_is(., "POINT")) %>%  #gets rid of trouble spots (shouldn't actually remove anything)
  st_cast("LINESTRING") %>%
  ungroup() %>%
  mutate(total_dist_km = round(st_length(.)/1000, 1))

