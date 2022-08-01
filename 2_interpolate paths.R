source("1_data format.R")
### Interpolate paths between detections    #####################################

main_lake_fish_locs %>%
  filter(is.na(detection_timestamp_utc))

# read in erie transition layer
erie_trans <- readRDS("data/erie_trans_layer.rds")


# interpolate fish paths - makes daily paths between detection events
# use transition layer to get fish to avoid land - need to make a proper layer here!
interp_points = interpolate_path(main_lake_fish_locs, 
                                   trans = erie_trans$transition,
                                   out_class = "tibble",
                                   lnl_thresh = 12, # threshold for linear/nonlinear interp - default = .9, 
                                                    # needs to be high to not have them swim over land!
                                   int_time_stamp = (86400/2)) #time step for interpolation; 86400 = 1 day


# reformat path points 
interp_points <- interp_points %>%
  filter(!is.na(animal_id)) %>%
  mutate(date = date(bin_timestamp),
         time_step = as.integer(date - ymd("2018-09-23")),
         month = month(date, label = TRUE, abbr = FALSE),
         year = year(date),
         transmitter_id = substr(animal_id, 10, 15))

# did interpolate_paths work?
interp_points %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mapview()#+mapview(erie_trans$rast, maxpixels =  10035900)

#now save it
saveRDS(interp_points, file = "data/locations_interpolated.RDS")
interp_points <- readRDS(file = "data/locations_interpolated.RDS")
