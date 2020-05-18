# make retangle plots in an interative process

rm(list=ls())

setwd("~") # set home directory to documents
setwd("..")
basedir <- getwd()
# there has got to be a better way to name this folder 
# that will be made for each new field
study_folder <- "Trevor_phd_2020"

# search directory recursivly for 'PlotMapGenerator'
#C:\Users\Mack Naber\Box Sync\Field and Greenhouse studies data and maps\PlotMapGenerator
wd <- file.path(basedir,"Box Sync","Field and Greenhouse studies data and maps","PlotMapGenerator",study_folder)
setwd(wd) # wd is now out base of operations

library(tidyverse)
# load script of custom functions
source("~/Documents/mackcode/drive-download-20200502T154155Z-001/iter_plot_map_engine.R")

# load csv file of parameters

params_raw <- read_csv("~/Documents/mackcode/drive-download-20200502T154155Z-001/Nitrogen_2020/parameters_iter_plot_map.csv", col_names = FALSE, col_types = "cd") 

# the text file was too hard to deal with for my simple mind
params <- read_params(params_raw)

params <- params %>% mutate(plot_length = (bottom_subguard_length + top_subguard_length + subplot_length)*subplot_row_n + 
                   top_guard + bottom_guard)

#params_annotate <- read_params(read_csv("parameters_annotate.csv", col_names = FALSE , col_types = "cd")) 


# Just to have handy reference
# layer_levels <- c("study","plot","subplot","row","nonplot_planted","indi_plant")

field <- init_field_map(params$field_width,params$field_length)

#library(ggplot2)


field_plot <- ggplot() +
  geom_tile(data=field, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = layer_id),alpha =0.5)

# just the field diminsions
field_plot 

# next, the main plot boundaries
plot_area <- init_plot_map(x_length = params$plot_width, 
                           y_length = params$plot_length,
                           plot_n = params$plot_n,
                           plot_col_n = params$plot_col_n,
                           plot_row_n = params$plot_row_n) %>%
              mutate(x_mid = x_mid+params$field_setback_x,
                     y_mid = y_mid+params$field_setback_y)

field_boundary <- bind_rows(field,plot_area)

# plot boundaries over field boundaries
field_and_plots <- ggplot() +
 geom_tile(
   data = field_boundary,
   aes(x=x_mid, y=y_mid, width=x_length, height=y_length, fill = layer_id),
   alpha = 0.5
   )

field_and_plots
 
 one_subplot_area <- make_one_subplot_area()
 many_subplot_area <-make_many_subplot_area()
 
 plot_origins <- get_plot_origins(plot_area)
 
 many_subplots_area_origins <- get_many_subplots_area_origins(many_subplot_area)
 subplot_area<-spread_from_origin_subplot_area(many_subplots_area_origins)
 
 # okay show subplot boundaries 
 field_boundary <- bind_rows(field,plot_area,subplot_area) %>%
   mutate(id = as.character(id), id = str_pad(id, 4, pad = "0")) %>%
   mutate(fill = paste(layer_id,id,sep="_")) %>%
   mutate(x_mid_temp =  x_mid + params$spray_alley_width*plot_col) %>% # add spray alley
   mutate(x_mid = ifelse(layer_id == "study",x_mid,x_mid_temp)) %>%
   select(-x_mid_temp)
 
 # debugging subplots, plots and field 
field_plot_subplots <-  ggplot() +
  geom_tile(data = field_boundary, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill)) 
field_plot_subplots
  
# okay, now every row
one_subplot_row <- init_subplot_row(x_length = params$row_width, y_length = params$subplot_length,row_per_subplot = params$row_per_subplot) 
many_subplot_with_rows<-get_many_subplot_with_rows()
subplot_origins<-get_subplot_origins(subplot_area)
each_plot_subplot_with_rows<-get_each_plot_subplot_with_rows(many_subplot_with_rows,subplot_origins)

# debugging
# each row in each sub plot
# every_row <- ggplot() +
#   geom_tile(data = each_plot_subplot_with_rows,aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = as.factor(id)))
# every_row
# okay now plot every row

field_boundary <- bind_rows(field,plot_area,subplot_area,each_plot_subplot_with_rows) %>%
  mutate(id = as.character(id), id = str_pad(id, 4, pad = "0")) %>%
  mutate(fill = paste(layer_id,id,sep="_")) %>%
  mutate(x_mid_temp =  x_mid + params$spray_alley_width*plot_col) %>% # add spray alley
  mutate(x_mid = ifelse(layer_id == "study",x_mid,x_mid_temp)) %>%
  select(-x_mid_temp)

# debugging every row, subplots, plots and field 
every_row_subplot_plot_field <- ggplot() +
  geom_tile(data = field_boundary, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill),alpha = 0.75) 
#every_row_subplot_plot_field


# seperating by extant (Field, study, plots, subplots, rows)
# to choose better color pallets
# lotta green

# debugging every row, subplots, plots and field 


field_boundary_list <- split(field_boundary,field_boundary$layer_id)

colors_study <- c("light yellow") # background color
field_boundary_list$study$id <- factor(field_boundary_list$study$id)
n_study_id <- nlevels(field_boundary_list$study$id)

colors_plot <- colorRampPalette(c("blue", "green", "yellow", "red"))(length(unique(field_boundary_list$plot$id)))
# make the legend pretty
field_boundary_list$plot$id <- factor(field_boundary_list$plot$id)
n_plot_id <- nlevels(field_boundary_list$plot$id)

colors_subplot <- colorRampPalette(c("blue", "green", "yellow", "red"))(length(unique(field_boundary_list$subplot$id)))
field_boundary_list$subplot$id <- factor(field_boundary_list$subplot$id)
n_subplot_id <- nlevels(field_boundary_list$subplot$id)

colors_row <- colorRampPalette(c("blue", "green", "yellow", "red"))(length(unique(field_boundary_list$row$id)))
field_boundary_list$row$id <- factor(field_boundary_list$row$id)
n_row_id <- nlevels(field_boundary_list$row$id)


# https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin
#Some test data
# dat <- data.frame(x=runif(10),y=runif(10),
#                   grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)

#Create a custom color scale
# library(RColorBrewer)
# myColors <- brewer.pal(5,"Set1")
# names(myColors) <- levels(dat$grp)
# brewerpal palettes for qualitative palettes
# Accent	 8
# Dark2	 8
# Paired	 12
# Pastel1	 9
# Pastel2	 8
# Set1	 9
# Set2	 8
# Set3	 12

# colScale <- scale_colour_manual(name = "grp",values = myColors)

library(RColorBrewer)
subplot.color <- colorRampPalette(c("blue", "green", "yellow", "red"))(length(unique(field_boundary_list$subplot$id)))

# will plot this with geom_segment
# i will need x_start, y_start, x_end,y_end for each point
# I should be able to use the geom_segments as the coordinates for the text annotation

origin.dim <- data.frame(x=0,y=0)
origin.dim <- data.frame( x = field_boundary_list$subplot$x_mid - field_boundary_list$subplot$x_length/2,
                          y = field_boundary_list$subplot$y_mid - field_boundary_list$subplot$y_length/2)
offset = data.frame(x=10,y = 10)

plot_dimensions <- data.frame(segment_name = c("plot_length","plot_width",
                                               "subplot_length", "subplot_width"),
                              segment_name_ft = c(paste("plot_length", params$plot_length, sep = " "),
                                               paste("plot_width", params$plot_width, sep = " "),
                                               paste("subplot_length", params$subplot_length, sep = " "),
                                               paste("subplot_width", params$subplot_width, sep = " ")),
                              start_x = c(origin.dim$x[1]-offset$x, origin.dim$x[1],origin.dim$x[1]-offset$x,origin.dim$x[1] ) ,
                              end_x =   c(origin.dim$x[1]-offset$x, origin.dim$x[1]+params$plot_width, origin.dim$x[1]-offset$x,origin.dim$x[1]+params$subplot_width),
                              start_y = c(field_boundary_list$plot$y_mid[1] - field_boundary_list$plot$y_length[1]/2,
                                          origin.dim$y[1]-offset$y, origin.dim$y[1],origin.dim$y[1]-offset$y),
                              #start_y = c(origin.dim$y[1], origin.dim$y[1]-offset$y, origin.dim$y[1],origin.dim$y[1]-offset$y),
                              end_y =   c(field_boundary_list$plot$y_mid[1] + field_boundary_list$plot$y_length[1]/2,
                                         origin.dim$y[1]-offset$y,origin.dim$y[1]+params$subplot_length,origin.dim$y[1]-offset$y))
                              #end_y =   c(origin.dim$y[1]+params$plot_length,origin.dim$y[1]-offset$y,origin.dim$y[1]+params$subplot_length,origin.dim$y[1]-offset$y)) 


# annontate_df <- plot_dimensions %>%
#                 mutate(start_x = if_else(segment_name == "subplot length" ,start_x + sb_off_x,start_x),
#                 end_x = if_else(segment_name == "subplot length" ,end_x + sb_off_x,end_x)) %>%
#                 mutate(start_y = if_else(segment_name == "subplot width" ,start_y + sb_off_y,start_y),
#                 end_y = if_else(segment_name == "subplot width" ,end_y + sb_off_y,end_y)) %>%
#                 mutate(angle = c(90,0,90,0),
#                 text_x= if_else(angle == 90, start_x + ((end_x -start_x)/2)+ text_offset_x, start_x + ((end_x -start_x)/2)), 
#                 text_y = if_else(angle == 90, start_y + ((end_y -start_y)/2), start_y + ((end_y -start_y)/2) + text_offset_y))
#   
  
  # subplot label off set
#   sb_off_x <- 4
# sb_off_y <- -2
# text_offset_x <- 1
# text_offset_y <- 5

# require(tidyselect) # contains()

# debugging
#ifelse(grepl("non",df$loc_01),'outside','inside')

require(tidyverse)

params_annotate <- read_params(read_csv("~/Documents/mackcode/drive-download-20200502T154155Z-001/Nitrogen_2020/parameters_annotate.csv", col_names = FALSE , col_types = "cd")) %>%
                   pivot_longer(everything(),
                                names_to = c("unit","line",".value"),
                                names_pattern = "(^.*_.*)_(.*)_(....$)"
                   )



annontate_df <-  plot_dimensions%>% 
                 left_join(params_annotate,by = c("segment_name" ="unit")) %>% # drops the angle
                 mutate(line.x = (start_x+end_x)/2 + os.x, 
                        line.y = (start_y+end_y)/2 + os.y) %>% 
                 mutate(angle = if_else(str_detect(segment_name,"length") & line == "text", 
                         params_annotate[params_annotate$unit == "angle_all" ,]$os.y,
                         if_else(str_detect(segment_name,"width") & line == "text",
                         params_annotate[params_annotate$unit == "angle_all" ,]$os.x,0)))
 
  
  # # subplot lengths
  # mutate(start_x = if_else(segment_name == "subplot_length" ,start_x + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "length_arrow",]$x ,start_x),
  #        end_x = if_else(segment_name == "subplot_length" ,end_x + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "length_arrow",]$x,end_x)) %>%
  # mutate(start_y = if_else(segment_name == "subplot_width" ,start_y + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "width_arrow",]$y,start_y),
  #        end_y = if_else(segment_name == "subplot_width" ,end_y + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "width_arrow",]$y,end_y)) %>%
  # # subplot text
  # mutate(text_x = if_else(grepl( "^plot_length",plot_dimensions$segment_name) ,
  #                             (((end_x -start_x)/2) + params_annotate[params_annotate$unit == "plot" & params_annotate$line == "length_text",]$x),
  #                 if_else(grepl( "^plot_length",plot_dimensions$segment_name) ,
  #                             (((end_x -start_x)/2) + params_annotate[params_annotate$unit == "plot" & params_annotate$line == "width_text",]$x),
  #                 if_else(grepl("subplot_length",plot_dimensions$segment_name),
  #                            (((end_x -start_x)/2) + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "length_text",]$x),
  #                 if_else(grepl("subplot length",plot_dimensions$segment_name),
  #                            (((end_x -start_x)/2) + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "width_text",]$x)
  #                         (end_x -start_x)/2 )))),
  #        text_y = if_else(grepl("^plot_width",plot_dimensions$segment_name) ,
  #                             (((end_y -start_y)/2) + params_annotate[params_annotate$unit == "plot" & params_annotate$line == "width_text",]$y),
  #                if_else(grepl("^plot_width",plot_dimensions$segment_name) ,
  #                             (((end_y -start_y)/2) + params_annotate[params_annotate$unit == "plot" & params_annotate$line == "length_text",]$y),                  
  #                 if_else(grepl("subplot_width",plot_dimensions$segment_name),
  #                              (((end_y -start_y)/2) + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "width_text",]$y),
  #                 if_else(grepl("subplot_width",plot_dimensions$segment_name),
  #                              (((end_y -start_y)/2) + params_annotate[params_annotate$unit == "subplot" & params_annotate$line == "length_text",]$y),
  #                         (end_y -start_y)/2))))
  #        )%>%
  # #plot_dimensions%>% 
  # mutate(angle = if_else(grepl("length",plot_dimensions$segment_name), 
  #                        params_annotate[params_annotate$unit == "angle" ,]$y,
  #                        params_annotate[params_annotate$unit == "angle" ,]$x)) #%>%
  
  # set by hand
  # mutate(segement_name = if_else(segment_name == ))
  # mutate(sub_text_x = if_else(segment_name == "subplot length" & angle == 90,
  #                          + ((end_x -start_x)/2) + params_annotate$sub_text_off_x,
  #                          ((end_x -start_x)/2)),
  #        sub_text_x = if_else(segment_name == "subplot length" & angle != 90,
  #                             + ((end_y -start_y)/2) + params_annotate$sub_text_off_y,
  #                             ((end_y -start_y)/2)),
  #        plot_text_x = if_else(segment_name == "plot length" & angle == 90,
  #                             + ((end_x -start_x)/2) + params_annotate$plot_text_off_x,
  #                             ((end_x -start_x)/2)),
  #        plot_text_y = if_else(segment_name == "plot length" & angle != 90,
  #                             + ((end_y -start_y)/2) + params_annotate$plot_text_off_y,
  #                             ((end_y -start_y)/2)))


# # subplot label off set
# sb_off_x <- 4
# sb_off_y <- -2
# text_offset_x <- 1
# text_offset_y <- 5
# annontate_df <- plot_dimensions %>%
#                 mutate(start_x = if_else(segment_name == "subplot length" ,start_x + sb_off_x,start_x),
#                        end_x = if_else(segment_name == "subplot length" ,end_x + sb_off_x,end_x)) %>%
#                 mutate(start_y = if_else(segment_name == "subplot width" ,start_y + sb_off_y,start_y),
#                         end_y = if_else(segment_name == "subplot width" ,end_y + sb_off_y,end_y)) %>%
#                 mutate(angle = c(90,0,90,0),
#                 text_x= if_else(angle == 90, start_x + ((end_x -start_x)/2)+ text_offset_x, start_x + ((end_x -start_x)/2)), 
#                 text_y = if_else(angle == 90, start_y + ((end_y -start_y)/2), start_y + ((end_y -start_y)/2) + text_offset_y))
#   


library(grid) # for arrow function
# ggplot() +
#   geom_segment(data = annontate_df, aes(x= start_x,y=start_y, xend=end_x,yend=end_y),
#                                            arrow = arrow(angle=30,length = unit(0.25,"inches"),ends = "both",type="closed"))

#geom_segment(aes(xend=c(tail(x, n=-1), NA), yend=c(tail(y, n=-1), NA)),
#             arrow=arrow(length=unit(0.3,"cm")))


every_row_subplot_plot_field <- ggplot() +
  #geom_tile(data = field_boundary_list[["study"]], aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill),alpha = 0.75) 

           #fill = brewer.pal(length(unique(field_boundary_list$plot$plot_row)),"Set1")(length(unique(field_boundary_list$plot$id)))) +
           #fill = colorRampPalette(c("blue", "green", "yellow", "red"))(length(unique(field_boundary_list$plot$id))) )           +
  #geom_tile(data = field_boundary_list[["plot"]], aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill),alpha = 0.75) +
  #geom_tile(data = field_boundary_list[["subplot"]], aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill),alpha = 0.75) +
   geom_tile(data = field_boundary_list[["row"]], 
             aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill),color =  "black",alpha = 0.75) +
  geom_rect(data = field_boundary_list$subplot, 
            aes(xmin = field_boundary_list$subplot$x_mid - field_boundary_list$subplot$x_length/2,
           xmax = field_boundary_list$subplot$x_mid + field_boundary_list$subplot$x_length/2,
           ymin = field_boundary_list$subplot$y_mid - field_boundary_list$subplot$y_length/2,
           ymax = field_boundary_list$subplot$y_mid + field_boundary_list$subplot$y_length/2),
           #y=field_boundary_list$subplot$y_mid,
           #width=field_boundary_list$subplot$x_length,
           #height=field_boundary_list$subplot$y_length, 
           alpha = 0,
           #fill = rep(subplot.color,times = length(unique(field_boundary_list$plot$id))),alpha = 0 ,
           color =  "black",size=1.5) +
  annotate(geom = 'tile', x = field_boundary_list$plot$x_mid,
           y=field_boundary_list$plot$y_mid,
           width=field_boundary_list$plot$x_length,
           height=field_boundary_list$plot$y_length,
           #fill = colorRampPalette(c("blue", "yellow", "red"))(length(field_boundary_list$plot$plot_row)),
           alpha = 0, size = 2,
           color =  "blue") 


  
# 
# every_row_subplot_plot_field +
#   labs(title = "Example Plot") +
#   geom_segment(data = annontate_df, aes(x= start_x,y=start_y, xend=end_x,yend=end_y),
#                arrow = arrow(angle=30,length = unit(0.25,"inches"),ends = "both",type="closed")) +
#   geom_text(data = annontate_df,
#             aes(
#             label = segment_name,
#             x= start_x + ((end_x -start_x)/2),
#             y = start_y + ((end_y -start_y)/2)
#             )
#             )

every_row_subplot_plot_field +
  labs(title = "Trevor 2020 plot map draft Feb 24, 2020") +
  geom_segment(data = annontate_df, aes(x= start_x,y=start_y, xend=end_x,yend=end_y),
               arrow = arrow(angle=30,length = unit(0.25,"inches"),ends = "both",type="closed")) +
  geom_text(data = filter(annontate_df,line == "text"),
            aes(
              label = segment_name_ft,
              x= line.x, 
              y = line.y , 
              angle = angle
            )
  ) +
  scale_y_continuous(limits = c(0, 320), breaks = seq(0, 320, by = 10)) +
  scale_x_continuous(limits = c(0, 220), breaks = seq(0, 220, by = 10))



##################################
# Next for 2/20/20
# dims: planted alleys, spray alleys, planted length, planted width
# points for each plant, and summary table of seed pieces per row, variety ...




#######################################################################
#
# some stuff
#
#######################################################################

# are the trial wider than field 
max(field_boundary$x_mid) > params$field_width
# are the trial longer than field 
max(field_boundary$y_mid) > params$field_length


#######################################################################
#
#
# Convert from geom_tile coords to geom_rect to geom_poly 
# yeet yeet 
#
#######################################################################


# just call the function......
source("iter_plot_map_engine.R")

field_boundary_polygon <- tile2polygon(field_boundary,x_mid=x_mid,y_mid=y_mid,x_length=x_length,y_length=y_length)

# boom now its in polygon formate
field_boundary_polygon %>%
  filter(layer_id == "row") %>%
  ggplot(.) + geom_polygon(aes(x=x,y=y,group = interaction(plot,subplot,id), fill = id))


source("~/Documents/mackcode/drive-download-20200502T154155Z-001/point2latlongExamples.R")

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

origin <- data.frame(lon = -89.539541,lat = 44.119338,heading = 0.0)
options(digits = 10)
polyanna <- field_boundary_polygon %>%          
         mutate(d = c(sqrt(x^2+y^2)), # thats in feet
         alpha = round(rad2deg(ifelse(d==0,0,asin(x/d))),3),
         heading = origin$heading,
         bearing = round(alpha+ heading,4),
         lat = specify_decimal(convert_lat(lat0=origin$lat,lon0=origin$lon,dist=d,bearing=bearing),10),
         lon = specify_decimal(convert_lon(lat0=origin$lat,lon0=origin$lon,dist=d,bearing=bearing),10) ,  #
         lat0 = origin$lat,
         lon0 = origin$lon)

polyanna <-       field_boundary_polygon  %>%
       mutate(d = c(sqrt(x^2+y^2)), # thats in feet
       alpha = round(rad2deg(ifelse(d==0,0,asin(x/d))),3),
       heading = 1.0,
       bearing = round(alpha+ heading,4),
       lat = convert_lat(lat0=origin$lat,lon0=origin$lon,dist=d,bearing=bearing),
       lon = convert_lon(lat0=origin$lat,lon0=origin$lon,dist=d,bearing=bearing) ,  #
       #x1 = ifelse(x==0 & y == 0,0,d*sin((deg2rad(x/d)+brng))),
       #y1 = ifelse(x==0 & y == 0,0,d*(-1)*cos(
       #(deg2rad(y/d) +brng))),
       #x1 = sin(deg2rad(x/d)+brng),
       #y1 = cos(deg2rad(y/d)+brng),
       
       lat0 = origin$lat,
       lon0 = origin$lon)


polyanna%>%
  filter(layer_id == "row") %>%
  #filter(layer_id == "study") %>%
  #select(alpha)
ggplot() +
   geom_polygon(aes(x=lon,y=lat,group=interaction(plot,subplot,id), fill = id)) 


polyanna.row <- polyanna%>%
  filter(layer_id == "row")
# Trying to create SpatialPolygons dataframe object

###############################################################################
# Creating a unique ID for each individual polygon
library(sp)
library(rgdal)
polyanna.row %>% 
  mutate(polyid = paste0(
    id,
    str_pad(plot, 2, pad = "0"),
    str_pad(subplot, 1, pad = "0")
    )
  ) -> polyanna.row

# We need to make sure that the polygon ends where it starts
poly_list = list()
for (id in unique(polyanna.row$polyid)){
  print(id)
  tmp_pts = polyanna.row[polyanna.row$polyid == id,]
  # Ensure that the first point and last point is not the same.
  # If the same, then duplicate the first record at the end
  if (!all(tmp_pts[1, c('lat', 'lon')] == tmp_pts[nrow(tmp_pts), c('lat', 'lon')])){
    tmp_pts = rbind(tmp_pts, tmp_pts[1,])  
  }
  poly_list[id] = Polygons(list(Polygon(tmp_pts[, c('lon','lat')])), id)
  
}
spolys = SpatialPolygons(poly_list)
# Plot to see if it works
plot(spolys)

# To create spatialpolyDF, we need a dataframe with row names that match the ids we've used
#   in creating the spatialpolyDF object

# So we'll collapse the polyanna dataset to matcch
polyanna_collapsed = polyanna.row %>%
  # Arbitrarily using the lower right hand corner to select just one record per polygon
  filter(level == 'lr') %>%
  column_to_rownames("polyid") %>%
  # Dropping this column because its type isn't supported in shapefiles
  select(-c(level))

spolys_df = SpatialPolygonsDataFrame(spolys, data = polyanna_collapsed, match.ID = TRUE)

# Export to Shapefile
writeOGR(spolys_df, dsn = "Documents", layer = "mackplots", driver = "ESRI Shapefile")

###############################################################################

library(OpenStreetMap)
ws.ul <- c(44.12100, -89.540500 )
ws.lr <- c( 44.1190, -89.5380 ) # yesss...
ws.box <- data.frame(ul = ws.ul, lr = ws.lr)
ws.map <- openmap(upperLeft = ws.ul ,lowerRight = ws.lr , type = "bing")

map_latlon <- openproj(ws.map,projection = "+proj=longlat") 
trial.map.practice <- autoplot(map_latlon) +
  geom_polygon(data=polyanna.row,aes(x=lon,y=lat,group=interaction(plot,subplot,id), fill = id)) 

aspect_ratio = 16/9
ggsave(file="trial.every.row.lonlat.png",plot = trial.map.practice, height=7, width = 7*aspect_ratio)

# ggsave(paste(updated_dir,"total_N_all_tissue_year_2019_sq.png",sep="/"),
#        plot = total_N_all_tissue_year_2019, height = 7 , width = 7 * sq_aspect_ratio)