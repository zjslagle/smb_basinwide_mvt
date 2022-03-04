source("movement summaries.R")

# Make one map per long movement fish. Also group bay receivers together so we can get 
#   more accurate estimate of distance moved

# Make vector of which fish IDs need plotting (kinda hacky)
interesting_fish <- interpolated_lines_total %>% dplyr::select(transmitter_id)
st_geometry(interesting_fish) <- NULL
interesting_fish <- interesting_fish %>% as_vector() %>% unique() 

### Automated static maps by tag ID    ################################

boundary_size = 0.3 # use this to adjust how big the map is around the bounding box

#loop over each interesting fish, make a PNG map
for(i in 1:length(interesting_fish)){
  
# subset data 
  target_fish = interesting_fish[i]
  #filter data for just one fish
  target_fish_line = interpolated_lines_total %>% filter(transmitter_id == target_fish)
  target_fish_points = detection_points %>% 
    filter(transmitter_id == target_fish) %>%
    dplyr::select(transmitter_id, monthyear) %>% unique()
  #get bounding box and important stats
  bound_box <- detection_lines %>% filter(transmitter_id == target_fish) %>% st_bbox()
  total_dist_km = target_fish_line$total_dist_km
  last_detected = target_fish_line$last_detected
  
#map it!
  ggplot()+
    scale_x_continuous(limits = c(bound_box$xmin-boundary_size, bound_box$xmax+boundary_size))+
    scale_y_continuous(limits = c(bound_box$ymin-.1, bound_box$ymax+boundary_size))+
    geom_sf(data = us_and_can)+
    geom_sf(data = urban_areas, fill = "wheat3")+
    geom_sf(data = lake_erie, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
    geom_text(aes(x = -82.51, y = 41.67, label = "CANADA\nUNITED STATES"),
              fontface = "bold", size = 4, color = "slategrey", family = "Times")+
    geom_sf(data = lake_receivers_filtered, shape = 21, fill = "darkgoldenrod1", size = 2)+
    geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
    geom_sf(data = target_fish_line, 
            lwd = 1, lty = 5, color = "black", show.legend = FALSE)+
    geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                  aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                  nudge_x = -.1, nudge_y = .05)+
    geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                  aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                  nudge_x = .3, nudge_y = -.15)+
    ggrepel::geom_label_repel(data = target_fish_points, 
                     aes(label = monthyear, geometry = geometry), 
                     color = "black", size = 2, stat = "sf_coordinates",
                     label.padding = .1, max.overlaps = 2)+
    ggsn::scalebar(dist = 10, model = "WGS84",location = "topright", dist_unit = "km", 
                   x.min = bound_box$xmin-boundary_size, y.min = bound_box$ymin-.1, 
                   x.max = bound_box$xmax+boundary_size, y.max = bound_box$ymax+boundary_size,
                   st.dist = .03, anchor = c(x = -82.62, y = 41.405), family = "Times", transform = TRUE,
                   border.size = .5)+
    labs(title = paste("SMB ID ", target_fish, 
                       ", Total movement = ", total_dist_km, "km/ ", 
                       round(total_dist_km/1.609, 1), "mi", sep = ""),
         subtitle = paste("Last detected", last_detected))+
    theme(axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank()) -> basinwide_map
  
  ggsave(filename = paste("figures/basin movement maps/basinwide map ",target_fish, ".png", sep = ""),
         plot = basinwide_map,
         width = 7, height = 7, units = "in", dpi = 300)
}




### Maps for PPT presentations   ###################
#same maps as previous, but no titles or labels

for(i in 1:length(interesting_fish)){
  
  # subset data 
  target_fish = interesting_fish[i]
  #filter data for just one fish
  target_fish_line = interpolated_lines_total %>% filter(transmitter_id == target_fish)
  target_fish_points = detection_points %>% 
    filter(transmitter_id == target_fish) %>%
    dplyr::select(transmitter_id, monthyear) %>% unique()
  #get bounding box and important stats
  bound_box <- detection_lines %>% filter(transmitter_id == target_fish) %>% st_bbox()
  total_dist_km = target_fish_line$total_dist_km
  last_detected = target_fish_line$last_detected
  
  #map it!
  ggplot()+
    scale_x_continuous(limits = c(bound_box$xmin-boundary_size, bound_box$xmax+boundary_size))+
    scale_y_continuous(limits = c(bound_box$ymin-.1, bound_box$ymax+boundary_size))+
    geom_sf(data = us_and_can)+
    geom_sf(data = urban_areas, fill = "wheat3")+
    geom_sf(data = lake_erie, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
    geom_text(aes(x = -82.51, y = 41.67, label = "CANADA\nUNITED STATES"),
              fontface = "bold", size = 4, color = "slategrey", family = "Times")+
    geom_sf(data = lake_receivers_filtered, shape = 21, fill = "darkgoldenrod1", size = 2)+
    geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
    geom_sf(data = target_fish_line, 
            lwd = 1, lty = 5, color = "black", show.legend = FALSE)+
    geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                  aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                  nudge_x = -.1, nudge_y = .05)+
    geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                  aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                  nudge_x = .3, nudge_y = -.15)+
    ggsn::scalebar(dist = 10, model = "WGS84",location = "topright", dist_unit = "km", 
                   x.min = bound_box$xmin-boundary_size, y.min = bound_box$ymin-.1, 
                   x.max = bound_box$xmax+boundary_size, y.max = bound_box$ymax+boundary_size,
                   st.dist = .03, anchor = c(x = -82.62, y = 41.43), family = "Times", transform = TRUE,
                   border.size = .5)+
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank()) -> basinwide_map
  
  ggsave(filename = paste("figures/basin movement maps/powerpoint/basinwide map ",target_fish, ".png", sep = ""),
         plot = basinwide_map,
         width = 7, height = 7, units = "in", dpi = 300)
}
