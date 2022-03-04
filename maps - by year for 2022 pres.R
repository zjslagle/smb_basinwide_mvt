source("movement summaries.R")

# Based on Fig 1, but for individual, annual maps
### Setup   #################################

# bounding box
fig_1_box = c(xmin = -83.5, ymin = 41.35, xmax = -81.55, ymax = 42.1)
fig_1_bbox <- st_bbox(fig_1_box) %>% 
  st_as_sfc() %>% #as a simple feature
  st_set_crs(4326) 
#list of fish IDs to plot
#interpolated_lines %>% st_drop_geometry() %>% select(transmitter_id) %>% unique() %>% as_vector()

# create consistent colors across maps
colors = viridis::viridis(8)
fish_colors = c("24047" = colors[1], 
                "24052" = colors[2],
                "24055" = colors[3],
                "24062" = colors[4],
                "24064" = colors[5],
                "24065" = colors[6],
                "24066" = colors[7],
                "24068" = colors[8])

target_fish = 24062

### Main Map     #################################################
# Year 1
ggplot()+
  scale_x_continuous(limits = c(fig_1_box["xmin"], fig_1_box["xmax"]))+ 
  scale_y_continuous(limits = c(fig_1_box["ymin"], fig_1_box["ymax"]))+
  geom_sf(data = us_and_can)+
  geom_sf(data = urban_areas, fill = "wheat3")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(aes(x = -83.4, y = 41.39, label = "OHIO"),
            fontface = "bold", size = 6, color = "grey", family = "Times")+
  geom_text(aes(x = -83.41, y = 42.05, label = "MICHIGAN"),
            fontface = "bold", size = 4, color = "grey", family = "Times")+
  geom_text(aes(x = -82.8, y = 42.1, label = "ONTARIO"),
            fontface = "bold", size = 6, color = "grey", family = "Times")+
  # geom_text(aes(x = -82.95, y = 41.65, label = "LAKE ERIE"),
  #           fontface = "bold", size = 8, color = "slategrey", family = "Times")+
  geom_text(aes(x = -82.08, y = 41.834, label = "CANADA\nUNITED STATES"),
            fontface = "bold", size = 4, color = "slategrey", family = "Times",
            angle = 31.5)+
  geom_sf(data = lake_receivers_filtered %>% filter(recover_date < ymd("2019-10-01")),
          shape = 21, fill = "darkgoldenrod1", size = 2)+
  geom_sf(data = interpolated_lines %>% filter(year_post_release == 1, transmitter_id == target_fish),  #LINES
          lwd = 2,
          color = "black",
          show.legend = FALSE)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .25, nudge_y = -.15)+
  geom_label(aes(x = -82.75, y = 41.39, label = "Sandusky"),
             family = "Times", fontface = "bold")+
  geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
  scale_color_manual(values = fish_colors)+
  scale_fill_manual(values = fish_colors)+
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()) -> map_year_1

# Year 2
ggplot()+
  scale_x_continuous(limits = c(fig_1_box["xmin"], fig_1_box["xmax"]))+ 
  scale_y_continuous(limits = c(fig_1_box["ymin"], fig_1_box["ymax"]))+
  geom_sf(data = us_and_can)+
  geom_sf(data = urban_areas, fill = "wheat3")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(aes(x = -83.4, y = 41.39, label = "OHIO"),
            fontface = "bold", size = 6, color = "grey", family = "Times")+
  geom_text(aes(x = -83.41, y = 42.05, label = "MICHIGAN"),
            fontface = "bold", size = 4, color = "grey", family = "Times")+
  geom_text(aes(x = -82.8, y = 42.1, label = "ONTARIO"),
            fontface = "bold", size = 6, color = "grey", family = "Times")+
  # geom_text(aes(x = -82.95, y = 41.65, label = "LAKE ERIE"),
  #           fontface = "bold", size = 8, color = "slategrey", family = "Times")+
  geom_text(aes(x = -82.08, y = 41.834, label = "CANADA\nUNITED STATES"),
            fontface = "bold", size = 4, color = "slategrey", family = "Times",
            angle = 31.5)+
  geom_sf(data = lake_receivers_filtered %>% filter(recover_date < ymd("2020-10-01") & recover_date > ymd("2019-10-01")), 
          shape = 21, fill = "darkgoldenrod1", size = 2)+
  geom_sf(data = interpolated_lines %>% filter(year_post_release == 2, transmitter_id == target_fish), 
          lwd = 1,
          color = "black",  
          show.legend = FALSE)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .25, nudge_y = -.15)+
  geom_label(aes(x = -82.75, y = 41.39, label = "Sandusky"),
             family = "Times", fontface = "bold")+
  geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
  scale_color_manual(values = fish_colors)+
  scale_fill_manual(values = fish_colors)+
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()) -> map_year_2

# Year 3
ggplot()+
  scale_x_continuous(limits = c(fig_1_box["xmin"], fig_1_box["xmax"]))+ 
  scale_y_continuous(limits = c(fig_1_box["ymin"], fig_1_box["ymax"]))+
  geom_sf(data = us_and_can)+
  geom_sf(data = urban_areas, fill = "wheat3")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(aes(x = -83.4, y = 41.39, label = "OHIO"),
            fontface = "bold", size = 6, color = "grey", family = "Times")+
  geom_text(aes(x = -83.41, y = 42.05, label = "MICHIGAN"),
            fontface = "bold", size = 4, color = "grey", family = "Times")+
  geom_text(aes(x = -82.8, y = 42.1, label = "ONTARIO"),
            fontface = "bold", size = 6, color = "grey", family = "Times")+
  # geom_text(aes(x = -82.95, y = 41.65, label = "LAKE ERIE"),
  #           fontface = "bold", size = 8, color = "slategrey", family = "Times")+
  geom_text(aes(x = -82.08, y = 41.834, label = "CANADA\nUNITED STATES"),
            fontface = "bold", size = 4, color = "slategrey", family = "Times",
            angle = 31.5)+
  geom_sf(data = lake_receivers_filtered %>% filter(recover_date > ymd("2020-10-01")), 
          shape = 21, fill = "darkgoldenrod1", size = 2)+
  geom_sf(data = interpolated_lines %>% filter(year_post_release == 3, transmitter_id == target_fish), 
          lwd = 2,
          color = "black", 
          show.legend = FALSE)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .25, nudge_y = -.15)+
  geom_label(aes(x = -82.75, y = 41.39, label = "Sandusky"),
             family = "Times", fontface = "bold")+
  geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = 1, col = "slategrey")+ #add US/CAN border
  ggsn::scalebar(dist = 15, model = "WGS84",location = "topleft", dist_unit = "km", 
                 x.min = fig_1_box["xmin"], y.min = fig_1_box["ymin"], 
                 x.max = fig_1_box["xmax"], y.max = fig_1_box["ymax"],
                 st.dist = .05, anchor = c(x = -83.5, y = 41.5), family = "Times", transform = TRUE,
                 border.size = .5)+
  scale_color_manual(values = fish_colors)+
  scale_fill_manual(values = fish_colors)+
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()) -> map_year_3

# print plots
png(filename = paste0("figures/basin movement maps/annual maps/", target_fish," year 1 map.png"), 
    width = 13, height = 7, units = "in", res = 400)
  par(mar=c(5,3,2,2)+0.1)
  map_year_1
dev.off()

png(filename = paste0("figures/basin movement maps/annual maps/", target_fish," year 2 map.png"), 
    width = 13, height = 7, units = "in", res = 400)
  par(mar=c(5,3,2,2)+0.1)
  map_year_2
dev.off()

png(filename = paste0("figures/basin movement maps/annual maps/", target_fish," year 3 map.png"), 
    width = 13, height = 7, units = "in", res = 400)
  par(mar=c(5,3,2,2)+0.1)
  map_year_3
dev.off()
