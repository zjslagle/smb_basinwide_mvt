library(sf)
library(glatos)
library(raster)
library(gdistance)
library(mapview)
library(lubridate)
library(tidyverse)

#read in LE shapefile
lake_erie <- st_read("C:/Users/10173930/Documents/Maps/shapefiles/Lake_Erie_Shoreline.shp") %>% st_set_crs(4326)
lake_erie <- st_transform(lake_erie, crs = 3175)

### Format Receiver object   #############################################
#and receiver waypoints
lake_receivers <- read_glatos_receivers("data/GLATOS Export/GLATOS_receiverLocations_20220617_131806.csv")

# for clipping geoms, this box is all of lake erie: 
le_bounds<- st_bbox(c(xmin = -84.6599, ymin = 40.5407, xmax = -77.8264, ymax = 42.90138)) %>% 
  st_as_sfc() %>% st_set_crs(4326)
# and this is Lake St Clair:
lsc_bounds <- st_bbox(c(xmin = -83.3317, ymin = 42.2821, xmax = -82.0463, ymax = 42.9331)) %>% 
  st_as_sfc() %>% st_set_crs(4326)

# filter down to only recent receivers, make SF, cut out only LE receivers
lake_receivers <- lake_receivers %>%
  as_tibble() %>%
  filter(deploy_date_time > ymd("2018-09-01")) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326) %>%
  st_intersection(le_bounds) %>%
  st_difference(lsc_bounds) %>%
  st_transform(crs = 3175) #convert to NAD83 - Great Lakes (default for make_trans below, otherwise wouldn't work)

mapview(lake_receivers)+
  mapview(lake_erie)

### Make Transition layer    ##################################
memory.limit(size = 1000000)

# make transition layer with shapefile. res = resolution (units = degrees with this shapefile)
lake_erie_transition <- make_transition3(poly = lake_erie,
                                         receiver_points = lake_receivers,
                                         res = c(0.001, 0.001))

saveRDS(lake_erie_transition, "data/erie_trans_layer.rds")


# Making transition layer...
# Error: cannot allocate vector of size 2 Kb
# Error during wrapup: memory exhausted (limit reached?)
# Error: no more error handlers available (recursive errors?); invoking 'abort' restart
# Error in Sys.setenv(OGR_ENABLE_PARTIAL_REPROJECTION = orig) : 
#   could not allocate memory (0 Mb) in C function 'R_AllocStringBuffer'
# Error during wrapup: memory exhausted (limit reached?)
# Error: no more error handlers available (recursive errors?); invoking 'abort' restart

lake_erie_transition <- readRDS("data/erie_trans_layer.rds")

mapview(lake_erie_transition$rast)+
  mapview(lake_receivers)


