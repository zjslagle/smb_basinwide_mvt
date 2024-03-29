### Basinwide Manuscript map - Fig 1  ##################################################
source("3_movement summaries.R")


# 3-panelled map showing movement by year of project (begins Sept 2018)
### Setup   #################################

# bounding box
fig_1_box = c(xmin = -83.5, ymin = 41.35, xmax = -81.55, ymax = 42.1)
fig_1_bbox <- st_bbox(fig_1_box) %>% 
  st_as_sfc() %>% #as a simple feature
  st_set_crs(4326) 
#list of fish IDs to plot
#interpolated_lines %>% st_drop_geometry() %>% select(transmitter_id) %>% unique() %>% as_vector()

# create consistent colors and linetypes across maps
colors = viridis::viridis(16)
#colors = RColorBrewer::brewer.pal(8, "Paired")

fish_colors = c("24047" = colors[1], 
                "24052" = colors[3],
                "24055" = colors[5],
                "24062" = colors[7],
                "24064" = colors[9],
                "24065" = colors[11],
                "24066" = colors[13],
                "24068" = colors[15])


fish_ltys = c("24047" = 1, 
                "24052" = 52,
                "24055" = 3,
                "24062" = 4,
                "24064" = 5,
                "24065" = 6,
                "24066" = 7,
                "24068" = 8)

make_color = T
if(make_color == T){
  lake_color = "#c5dfed";
  color_mode = "srgb";
  filename = "figures/basin movement maps/Fig 1 - COLOR.pdf"}else{
    lake_color = "white";
    color_mode = "grey";
    filename = "figures/basin movement maps/Fig 1 - GREYSCALE.pdf"}

### Main Map     #################################################
# Year 1
ggplot()+
  scale_x_continuous(limits = c(fig_1_box["xmin"], fig_1_box["xmax"]))+ 
  scale_y_continuous(limits = c(fig_1_box["ymin"], fig_1_box["ymax"]))+
  geom_sf(data = us_and_can, size = .2)+
  geom_sf(data = urban_areas, fill = "wheat3", size = .2)+
  geom_sf(data = lake_erie, fill = lake_color, size = .2)+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(aes(x = -83.4, y = 41.39, label = "OHIO"),
            fontface = "bold", size = 5, color = "grey", family = "Times")+
  geom_text(aes(x = -83.41, y = 42.05, label = "MICHIGAN"),
            fontface = "bold", size = 4, color = "grey", family = "Times")+
  geom_text(aes(x = -82.8, y = 42.1, label = "ONTARIO"),
            fontface = "bold", size = 5, color = "grey", family = "Times")+
  # geom_text(aes(x = -82.95, y = 41.65, label = "LAKE ERIE"),
  #           fontface = "bold", size = 8, color = "slategrey", family = "Times")+
  geom_text(aes(x = -82.25, y = 41.76, label = "CANADA\nUNITED STATES"),
            fontface = "bold", size = 4, color = "slategrey", family = "Times",
            angle = 31.5)+
  geom_sf(data = lake_receivers_filtered %>% # plot receivers that COULD'VE detected fish, roughly
            filter(recover_date > release_date,
                   deploy_date < release_date+365), 
          shape = 21, fill = "white", size = 1)+
  geom_sf(data = interpolated_lines %>% filter(year_post_release == 1),  #LINES
          lwd = 1,
          aes(color = transmitter_id, lty = transmitter_id), 
          show.legend = FALSE)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .25, nudge_y = -.15)+
  geom_label(aes(x = -82.75, y = 41.39, label = "Sandusky"),
             family = "Times", fontface = "bold")+
  geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = .5, col = "slategrey")+ #add US/CAN border
  scale_color_manual(values = fish_colors)+
  scale_fill_manual(values = fish_colors)+
  scale_linetype_manual(values = fish_ltys)+
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()) -> map_year_1

# Year 2
ggplot()+
  scale_x_continuous(limits = c(fig_1_box["xmin"], fig_1_box["xmax"]))+ 
  scale_y_continuous(limits = c(fig_1_box["ymin"], fig_1_box["ymax"]))+
  geom_sf(data = us_and_can, size = .2)+
  geom_sf(data = urban_areas, fill = "wheat3", size = .2)+
  geom_sf(data = lake_erie, fill = lake_color, size = .2)+ #  #c5dfed = blue; change to "white" for best greyscale
  geom_text(aes(x = -83.4, y = 41.39, label = "OHIO"),
            fontface = "bold", size = 5, color = "grey", family = "Times")+
  geom_text(aes(x = -83.41, y = 42.05, label = "MICHIGAN"),
            fontface = "bold", size = 4, color = "grey", family = "Times")+
  geom_text(aes(x = -82.8, y = 42.1, label = "ONTARIO"),
            fontface = "bold", size = 5, color = "grey", family = "Times")+
  geom_text(aes(x = -82.25, y = 41.76, label = "CANADA\nUNITED STATES"),
            fontface = "bold", size = 4, color = "slategrey", family = "Times",
            angle = 31.5)+
  geom_sf(data = lake_receivers_filtered %>%
            filter(recover_date > release_date+365,
                   deploy_date < release_date+730),
                   shape = 21, fill = "white", size = 1)+
  geom_sf(data = interpolated_lines %>% filter(year_post_release == 2), 
          lwd = 1,
          aes(color = transmitter_id, lty = transmitter_id), 
          show.legend = FALSE)+
  geom_sf_label(data = place_names %>% filter(name == "Cleveland"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = -.1, nudge_y = .05)+
  geom_sf_label(data = place_names %>% filter(name == "Toledo"), 
                aes(label = name), alpha = .85, family = "Times", fontface = "bold",
                nudge_x = .25, nudge_y = -.15)+
  geom_label(aes(x = -82.75, y = 41.39, label = "Sandusky"),
             family = "Times", fontface = "bold")+
  geom_sf(data = us_and_can, fill = NA, lty = 4, lwd = .5, col = "slategrey")+ #add US/CAN border
  ggsn::scalebar(dist = 15, model = "WGS84",location = "topleft", dist_unit = "km", 
                 x.min = fig_1_box["xmin"], y.min = fig_1_box["ymin"], 
                 x.max = fig_1_box["xmax"], y.max = fig_1_box["ymax"],
                 st.dist = .05, st.size = 4,
                 anchor = c(x = -83.5, y = 41.52), 
                 family = "Times", transform = TRUE,
                 border.size = .5)+
  scale_color_manual(values = fish_colors)+
  scale_fill_manual(values = fish_colors)+
  scale_linetype_manual(values = fish_ltys)+
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()) -> map_year_2

### Combined Map w/ inset    ################################
#define new, wider bounding box
great_lakes_bounds <- c(xmin = -92.63, 
                        ymin = 41.18, 
                        xmax = -75.9, 
                        ymax = 49.37)

# make inset map - 
inset_map <- ggplot()+
  scale_x_continuous(limits = c(great_lakes_bounds["xmin"], 
                                great_lakes_bounds["xmax"]))+
  scale_y_continuous(limits = c(great_lakes_bounds["ymin"], 
                                great_lakes_bounds["ymax"]))+
  geom_sf(data = us_and_can, #plot land
          fill = "white",
          col = "grey70",
          size = .1)+        
  geom_sf(data = great_lakes, #plot great lakes
          fill = "#c5dfed", 
          col = "grey40",
          size = .1)+  
  geom_sf(data = fig_1_bbox, #adds a box around our main map area
          col = "black", 
          fill = NA,
          size = .5)+
  theme_minimal()+
  theme(axis.text = element_blank(),#remove graticule (degrees/axis labels)
        panel.border = element_rect(fill = NA,
                                    color = "black", #add black border around map
                                    size = 1)) 

#inset_map


### Now print it out!   #######################
pdf(filename, 
    width = 7, height = 8,
    colormodel = color_mode) #use "grey" for greyscale and "srgb" for color
  par(mar=c(2,2,2,2)-0.2)
  cowplot::plot_grid(map_year_1, map_year_2,
                     align = "v", ncol = 1,
                     labels = "AUTO",
                     label_fontfamily = "Times")
  print(inset_map, 
        vp = grid::viewport(x = 0.85,        # now add inset over main map (uses package grid)
                      y = 0.887, 
                      width = 0.3, 
                      height = 0.3))
dev.off()






#### Try to make endpoints by line  - currently NOT working  ###########
# issue - line start and endpoints overlap, so some points need to be duplicated.
#create annual endpoints
endpoints <- interpolated_lines %>%
  st_transform(crs = 32617) %>%
  st_line_sample(n = 2, sample = c(0,1)) %>% 
  st_transform(crs = 4326)

n = c(1,2)
test = st_sf(id = rep(seq_along(n), 19), geom = endpoints)

endpoints_sf <- st_sf(endpoints) #convert to sf object
endpoints <- st_join(test, interpolated_lines) #join data from lines to points

test_lines = interpolated_lines %>% filter(year_post_release == 2)
test_points = endpoints %>% filter(year_post_release == 2)

mapview(test_lines,
        zcol = "transmitter_id")+
  mapview(test_points)