require("gdistance"); require("raster"); require("sp"); require("gganimate")

source("movement summaries.R")

bounds = c(xmin = -83.5605, ymin = 41.37740, xmax = -82.2669, ymax = 42.049)


main_lake_paths <- main_lake_paths %>%
  mutate(slider_color = case_when(month %in% c("September", "October", "November") ~ "darkorange",
                                  month %in% c("December", "January", "February") ~ "dodgerblue",
                                  month %in% c("March", "April", "May") ~ "chartreuse",
                                  T ~ "green3"))
 
main_lake_paths$month_year <-format(main_lake_paths$bin_timestamp, "%B %Y")
main_lake_paths$month_year %>% unique

make_frames(proc_obj = main_lake_paths, 
            recs = lake_receivers,
            out_dir = "figures/frames",
            background_xlim = c(main_lake_bbox["xmin"], main_lake_bbox["xmax"]),
            background_ylim = c(main_lake_bbox["ymin"]+.2, main_lake_bbox["ymax"]),
            animate = T,
            preview = F,
            tail_dur = 1,
            cex = 3,
            col = "forestgreen",
            recs.col = "grey40",
            timeslider.col = "black",
            timeslider.bg = "orangered",
            timeslider.cex = 4)

adjust_playback_time(input = "figures/frames/animation 2.0.mp4",
                     output_dir = "figures/frames",
                     scale_factor = 0.5,
                     ffmpeg = "C:/Program Files/ffmpeg/bin/ffmpeg.exe")
