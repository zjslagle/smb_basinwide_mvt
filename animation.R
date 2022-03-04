require("gdistance"); require("raster"); require("sp"); require("gganimate")

source("movement summaries.R")

### Bounding boxes and frames     #######################
#create bounding box:
west_basin_bounds <- c(xmin = -83.5605, ymin = 41.3373, xmax = -82.2669, ymax = 42.049)
west_basin_bbox <- st_bbox(west_basin_bounds) %>%
  st_as_sfc() %>% 
  st_set_crs(4326)

# create slightly larger frame to clip geometries
west_basin_frame <- c(west_basin_bounds[c(1,2)]-.1, west_basin_bounds[c(3,4)]+.1) # add/subtract 0.1 to above bounding box
west_basin_frame <- st_bbox(west_basin_frame) %>%
  st_as_sfc() %>% 
  st_set_crs(4326)
west_basin_frame <- st_transform(us_and_can, crs = 32617)




#### Clip geometries to make mapping and animations quicker   ####################

land_proj <- st_transform(us_and_can, crs = 32617) %>% 
  st_intersection(main_lake_frame)
lake_proj <- st_transform(lake_erie, crs = 32617) %>% 
  st_intersection(main_lake_frame)
urban_proj <- st_transform(urban_areas, crs = 32617) %>%
  st_buffer(dist = 1) %>%
  st_intersection(main_lake_frame)
receivers_filtered <- st_transform(lake_receivers_filtered, crs = 32617) %>% 
  st_intersection(main_lake_frame)


land_proj <- st_transform(land_proj, crs = 4326)
lake_proj <- st_transform(lake_proj, crs = 4326)
urban_proj <- st_transform(urban_proj, crs = 4326)
receivers_filtered <- st_transform(receivers_filtered, crs = 4326)

#reduce size to avoid goofy BS
main_lake_bbox <- c(main_lake_bbox[c(1,2)]+.25, 
                    main_lake_bbox[c(3,4)]-.25) # add/subtract 0.1 to above bounding box

### Make GGplot map for all fish, setting up animation     ###################################################
#set up animation label
move_map_title = paste("{unique(main_lake_paths$month[which(main_lake_paths$date == as.Date(closest_state))])}", #month
                   " ",
                   "{unique(main_lake_paths$year[which(main_lake_paths$date == as.Date(closest_state))])}",
                   sep = "") #year

ggplot(data = main_lake_paths)+
  scale_x_continuous(limits = c(main_lake_bbox["xmin"], main_lake_bbox["xmax"]))+
  scale_y_continuous(limits = c(main_lake_bbox["ymin"], main_lake_bbox["ymax"]))+
  geom_sf(data = land_proj)+
  geom_sf(data = urban_proj, fill = "wheat3")+
  geom_sf(data = lake_proj, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(data = data.frame(lon = -82.51, lat = 41.67, name = "CANADA\nUNITED STATES"),
            aes(x = lon, y = lat, label = name),
            fontface = "bold", size = 4, color = "slategrey", family = "Times")+
  geom_sf(data = lake_receivers_filtered, shape = 21, fill = "blue", size = 2)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .3, nudge_y = -.15)+
  geom_sf(data = land_proj, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
  geom_point(data = main_lake_paths, aes(x = longitude, y = latitude, group = date),
             pch = 21, color = "black", fill = "darkgreen", size = 5)+
  geom_sf(data = lake_receivers_filtered, pch = 18, lwd = 2, fill = "black")+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), legend.position = "none")+
  labs(title = move_map_title) -> bass_move_map


#bass_move_map

ggsave("figures/basin movement maps/test animation map.png", 
       bass_move_map, width = 11, height = 9, units = "in") #use this to test map elements


### Animation - All fish ###########################################################################
# animation (w/ transition_states) needs number of frames to be similar to cols, otherwise will
#      give error about matching col numbers
main_lake_paths %>% select(date) %>% unique %>% nrow()

main_lake_paths %>%
  group_by(animal_id) %>%
  summarize(n_days = n_distinct(date))

#set up animation:
bass_move_map+
  #transition_time(date)-> animated_bass_setup
  transition_states(date, 
                    transition_length = 1,
                    wrap = FALSE,
                    state_length = 0)+ #default = 1
  enter_fade()+exit_fade()+
  ease_aes('cubic-in-out')-> animated_bass_setup

# nframes is VERY specific - start with X = nrows of unique(date) - above.
# nframes = (X * transition_length) + (X * state_length -1)
# I think, anyway.

#create animation!
animated_bass = animate(animated_bass_setup, 
                        nframes = 990,
                        fps = 22,
                        #start_pause = 20, 
                        #end_pause = 20,
                        width = 10,    # PNG setup
                        height = 6.5, 
                        units = "in",
                        res = 300,
                        device = "png") 


#and save animation
anim_save("figures/basin movement maps/SMB whole lake movement 2021 v4.gif", 
          animation = animated_bass)

### Make GGplot map for single fish, setting up animation    ###################################################

# Make vector of which fish IDs need plotting (kinda hacky)
interesting_fish <- detection_points %>% dplyr::select(transmitter_id)
st_geometry(interesting_fish) <- NULL
interesting_fish <- interesting_fish %>% as_vector() %>% unique() 


boundary_size = 0.3 # use this to adjust how big the map is around the bounding box


#loop over each interesting fish, make a PNG map
for(i in 1:length(interesting_fish)){
  
  # subset data 
  target_fish = interesting_fish[i]
  #filter data for just one fish
  target_fish_line = detection_lines %>% filter(transmitter_id == target_fish)
  target_fish_points = main_lake_paths %>%
    filter(transmitter_id == target_fish)
  #get bounding box and important stats
  bound_box <- detection_lines %>% filter(transmitter_id == target_fish) %>% st_bbox()
  total_dist_km = target_fish_line$total_dist_km
  last_detected = target_fish_line$last_detected

  
  
  #set up animation label
move_map_title = paste("SMB ",
                       interesting_fish,
                       " - ",
                       "{unique(main_lake_paths$month[which(main_lake_paths$date == as.Date(closest_state))])}", #month
                       " ",
                       "{unique(main_lake_paths$year[which(main_lake_paths$date == as.Date(closest_state))])}",
                       sep = "") #year

ggplot(data = target_fish_line)+
  scale_x_continuous(limits = c(bound_box$xmin-boundary_size, bound_box$xmax+boundary_size))+
  scale_y_continuous(limits = c(bound_box$ymin-.1, bound_box$ymax+boundary_size))+
  geom_sf(data = land_proj)+
  geom_sf(data = urban_proj, fill = "wheat3")+
  geom_sf(data = lake_proj, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(data = data.frame(lon = -82.51, 
                              lat = 41.67, 
                              name = "CANADA\nUNITED STATES"),
            aes(x = lon, y = lat, label = name),
            fontface = "bold", size = 4, color = "slategrey", family = "Times")+
  geom_sf(data = lake_receivers_filtered, 
          shape = 21, fill = "blue", size = 2)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .3, nudge_y = -.15)+
  geom_sf(data = land_proj, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
  geom_point(data = target_fish_points, aes(x = longitude, y = latitude, group = date),
             pch = 21, color = "black", fill = "darkgreen", size = 5)+
  geom_sf(data = lake_receivers_filtered, pch = 18, lwd = 2, fill = "black")+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), legend.position = "none")+
  labs(title = move_map_title) -> single_bass_move_map


#bass_move_map

# ggsave("figures/basin movement maps/test single fish animation map.png", 
#        single_bass_move_map, width = 11, height = 9, units = "in") #use this to test map elements

target_fish_points %>%
  dplyr::select(date) %>% unique %>% nrow()

#set up animation:
single_bass_move_map+
  transition_states(date, 
                    transition_length = 0.1,
                    wrap = FALSE)+
  enter_fade()+
  exit_fade()+
  ease_aes('cubic-in-out')+
  shadow_trail(distance = 0.005,
               alpha = .4, size = 2)-> animated_bass_setup

#create animation!
animated_bass = animate(animated_bass_setup, #1619 frames
                        nframes = 350, #animation setup; n rows needs to be something specific, 
                        fps = 30,                                        #see error message when you get it wrong
                        #duration = 60, #60 sec TEST
                        #start_pause = 20, 
                        #end_pause = 20,
                        width = 10,    # PNG setup
                        height = 6.5, 
                        units = "in",
                        res = 300, 
                        device = "png") 


#and save animation
anim_save(paste("figures/basin movement maps/Animation - ", interesting_fish[i], ".gif", sep = ""),
          animation = animated_bass)

}
