source("movement summaries.R")

### Static Map - showing all fish paths across all years  ##################################################
ggplot()+
  scale_x_continuous(limits = c(main_lake_bbox["xmin"], main_lake_bbox["xmax"]))+ # bbox from whole lake animation
  scale_y_continuous(limits = c(main_lake_bbox["ymin"], main_lake_bbox["ymax"]))+
  geom_sf(data = us_and_can)+
  geom_sf(data = urban_areas, fill = "wheat3")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(aes(x = -82.8, y = 41.415, label = "OHIO"),
            fontface = "bold", size = 10, color = "grey", family = "Times")+
  geom_text(aes(x = -82.95, y = 41.65, label = "LAKE ERIE"),
            fontface = "bold", size = 10, color = "slategrey", family = "Times")+
  geom_text(aes(x = -82.51, y = 41.67, label = "CANADA\nUNITED STATES"),
            fontface = "bold", size = 4, color = "slategrey", family = "Times")+
  geom_sf(data = lake_receivers_filtered, shape = 21, fill = "blue", size = 2)+
  geom_sf(data = interpolated_lines, #%>% filter(transmitter_id == 24066),
          lwd = 1,
          lty = 5, #aes(color = transmitter_id),
          show.legend = FALSE)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .3, nudge_y = -.15)+
  geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
  ggsn::scalebar(dist = 20, model = "WGS84",location = "topleft", dist_unit = "km", 
                 x.min = -83.209735, y.min = 41.353893, x.max = -81.98064, y.max = 42.040895,
                 st.dist = .03, anchor = c(x = -83, y = 41.3), family = "Times", transform = TRUE,
                 border.size = .5)+
  #scale_colour_viridis_d()+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank()) -> basinwide_map

png("figures/basin movement maps/basinwide map - plain.png", 
    width = 10, 
    height = 7,
    units = "in",
    res = 400)
  par(mar=c(5,3,2,2)+0.1)
  basinwide_map
dev.off()

### MAP - Interactive map showing detection histories across the lake ###############################
mapviewOptions(fgb = FALSE)

# make the interactive map
mapview(lake_receivers_filtered, 
        layer.name = "Receivers", 
        zcol = "n_detections",
        at = c(0,1,3,5,10,20,100,1e6))+
  #mapview(all_substrate, zcol = "SUBSTRATE", layer.name = "Substrate", col.regions = brewer.pal(3, "Set3"),
  #        label = NA, highlight = F)+
  #mapview(lake_receivers_at_large, legend = F, layer.name = "Missing receivers", fill = "firebrick3", color = "grey")+
  mapview(recap_points, 
          layer.name = "Recaps", 
          #fill = "red", 
          color = "black")+
  mapview(detection_points_summary, 
          layer.name = "Indiv. fish pts", 
          #fill = "red", 
          color = "grey")+
  mapview(detection_lines, 
          zcol = "transmitter_id", 
          lwd = 2, 
          col.regions = brewer.pal(12, "Set3"), 
          legend = F, 
          burst = T) -> smb_move_map

mapshot(smb_move_map, url = "figures/smb_basin_movement_2022.html")


