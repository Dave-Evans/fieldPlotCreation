# # make retangle plots in an interative process
# 
# rm(list=ls())
# 
# setwd("~") # set home directory to documents
# setwd("..")
# basedir <- getwd()
# 

# # search directory recursivly for 'PlotMapGenerator'
# #C:\Users\Mack Naber\Box Sync\Field and Greenhouse studies data and maps\PlotMapGenerator
# wd <- file.path(basedir,"Box Sync","Field and Greenhouse studies data and maps","PlotMapGenerator")
# setwd(wd) # wd is now out base of operations
# 
# library(tidyverse)



#params_raw <- read_csv("iter_plot_map_parameters.csv", col_names = FALSE, col_types = "cd") 

read_params <- function(params_raw=params_raw){
          params_raw[,2] = as.numeric(params_raw[,2])
          params <- t(params_raw[,2]) %>%
          `colnames<-` (unlist(params_raw[,1])) %>% as.data.frame(.)

}

# init_field_map - function to inititate field boundaries
layer_levels <- c("study","plot","subplot","row","nonplot_planted","indi_plant")


init_field_map <- function(x_length, y_length){
  out <- tibble(
    layer_id = "study",
    id = 1,
    plot = NA,
    subplot=NA,
    x_mid = x_length/2,
    y_mid = y_length/2,
    x_length = x_length,
    y_length = y_length,
    plot_col = NA,
    plot_row = NA,
    subplot_row = NA,
    subplot_col = NA,
    subplot_n = NA
    )
}

# field <- init_field_map(params$field_width,params$field_length)
# 
# library(ggplot2)
# 
# 
# field_plot <- ggplot() +
#   geom_tile(data=field, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = layer_id),alpha =0.5)

# init_plot_map - function to initiate plots
# this function generates the the first division of field into plot areas
# starting from field dimisions


init_plot_map <- function(x_length, y_length,plot_n,plot_col_n,plot_row_n){
  out <- tibble(
    id = seq(1,plot_n),
    layer_id = rep(layer_levels[2],times = plot_n),
    x_mid = rep(x_length/2,plot_n) + rep(seq(0,plot_col_n-1),each = plot_row_n)*x_length ,
    y_mid = rep(y_length/2,plot_n) + rep(seq(0,plot_row_n-1),times = plot_col_n)*y_length,
    x_length = rep(x_length,times=plot_n),
    y_length = rep(y_length,times=plot_n),
    plot_col = rep(seq(1,plot_col_n),each = plot_row_n),
    plot_row = rep(seq(1,plot_row_n),times = plot_col_n),
    subplot_row = NA,
    subplot_col = NA,
    subplot_n = NA
  )
}

#library(tidyverse)

# params$plot_length,60
# params$plot_width,40
# params$plot_n,20
# params$plot_col_n,4
# params$plot_row_n,5



# plot_area <- init_plot_map(x_length = params$plot_width, 
#                            y_length = params$plot_length,
#                            plot_n = params$plot_n,
#                            plot_col_n = params$plot_col_n,
#                            plot_row_n = params$plot_row_n) 
#   
# field_boundary <- bind_rows(field,plot_area)
  # debugging
   # ggplot() +
   # geom_tile(data = field_boundary, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = layer_id),alpha = 0.5)


   ####################################
   
   #continue from here replacing constants with variables defined in the params file
   # 1/22/2020
   #####################################
   
# init_subplot_map
  
init_subplot_map <- function(x_length, y_length, subplot_n, subplot_col_n, subplot_row_n, plot_col = 1, plot_row = 1){
    out <- tibble(
      id = seq(1,subplot_n),
      layer_id = rep(layer_levels[3],times = subplot_n),
      x_mid = rep(x_length/2,subplot_n) + rep(seq(0, subplot_col_n-1),each = subplot_row_n)*x_length ,
      y_mid = rep(y_length/2,subplot_n) + rep(seq(0, subplot_row_n-1),times = subplot_col_n)*y_length,
      x_length = rep(x_length, times=subplot_n),
      y_length = rep(y_length, times=subplot_n),
      plot_col = plot_col,
      plot_row = plot_row,
      subplot_row = rep(seq(1,subplot_row_n),times = subplot_col_n),
      subplot_col = rep(seq(1,subplot_col_n),each = subplot_row_n),
      subplot_n = subplot_n
    )
  }


# okay can this be moved to a parameters file in the same folder 
# to load and edit?
# dir()
# 
# left_subguard_width = 0
# right_subguard_width = 0 
# bottom_subguard_length = 0
# top_subguard_length = 2
# # plot based adjustments
# top_guard = 2
# bottom_guard = 2
# left_guard_width = 5
# right_guard_width = 5
# spray_alley_width <- 12
# row_width = 2.5
# 

make_one_subplot_area <- function(){
                    init_subplot_map(
                      x_length = params$row_per_subplot*params$row_width,
                      y_length = params$subplot_length,
                      subplot_n = params$subplot_n,
                      subplot_col_n = params$subplot_col_n,
                      subplot_row_n = params$subplot_row_n
                    ) %>%
                      mutate(x_mid = x_mid + subplot_col*params$left_subguard_width + (subplot_col-1)*params$right_subguard_width) %>%
                      mutate(y_mid = y_mid + subplot_row*params$bottom_subguard_length + (subplot_row-1)*params$top_subguard_length) %>%
                      mutate(x_mid = x_mid + plot_col*params$left_guard_width +  (plot_col-1)*params$right_guard_width) %>% 
                      mutate(y_mid = y_mid + plot_row*params$bottom_guard + (plot_row-1)*params$top_guard)
  }
 
make_many_subplot_area <- function(plot_area, one_subplot_area){ 
                          tibble(plot = rep(1:max(plot_area$id), each = max(one_subplot_area$id))) %>%
                          bind_cols(do.call("rbind", replicate(max(plot_area$id), one_subplot_area, simplify = FALSE))) 
                          }

get_plot_origins <- function(plot_area = plot_area){
                plot_area %>% 
                select(id:y_length) %>%
                mutate(x0 = x_mid - x_length/2, y0 = y_mid-y_length/2) %>%
                select(id,layer_id,x0,y0)
                }

get_many_subplots_area_origins <- function(many_subplot_area, plot_origins, plot_area, one_subplot_area) {
                              left_join(many_subplot_area, plot_origins, by = c("plot"="id")) %>%
                              select(-plot,-layer_id.y) %>%
                              rename(layer_id = layer_id.x) %>%
                              select(-plot_col, -plot_row) %>%
                              mutate(plot_col = rep(seq(1,max(plot_area$plot_col)),
                                                    each = max(plot_area$plot_row)*max(one_subplot_area$subplot_n)),
                                     plot_row = rep(rep(seq(1,max(plot_area$plot_row)),
                                                    times = max(plot_area$plot_col)),each = max(one_subplot_area$subplot_n) ))
                              }
                                                          

spread_from_origin_subplot_area <- function(many_subplots_area_origins=many_subplots_area_origins){
                many_subplots_area_origins %>%
                mutate(x_mid = x_mid + x0, y_mid = y_mid + y0) %>%
                select(id:subplot_n,plot_col:plot_row) 
                }

# field_boundary <- bind_rows(field,plot_area,subplot_area) %>%
#                   mutate(id = as.character(id), id = str_pad(id, 4, pad = "0")) %>%
#                   mutate(fill = paste(layer_id,id,sep="_")) %>%
#                   mutate(x_mid_temp =  x_mid + params$spray_alley_width*plot_col) %>% # add spray alley
#                   mutate(x_mid = ifelse(layer_id == "study",x_mid,x_mid_temp)) %>%
#                   select(-x_mid_temp)
# 
# # debugging subplots, plots and field 
# # ggplot() +
# #  geom_tile(data = field_boundary, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill)) 
# 

## init individual rows
## okay now we add rectangles for each row # but that is for tomorrow # mrn 1/21/2019

# start with subplot boarders
# make subplot origins
# draw in subplot rows.

# init_subplot_map

init_subplot_row <- function(x_length, y_length,row_per_subplot){
  out <- tibble(
    id = seq(1,row_per_subplot),
    layer_id = rep(layer_levels[4],times = row_per_subplot),
    x_mid = rep(x_length/2,row_per_subplot) + seq(0,row_per_subplot-1)*x_length ,
    y_mid = rep(y_length/2,row_per_subplot),
    x_length = rep(x_length,times=row_per_subplot),
    y_length = rep(y_length,times=row_per_subplot),
    plot_col = NA,
    plot_row = NA,
    subplot_row = NA, #rep(seq(1,row_per_subplot),times = subplot_col_n),
    subplot_col = NA #rep(seq(1,subplot_col_n),each = row_per_subplot)
  )
}



# one_subplot_row <- init_subplot_row(x_length = params$row_width, y_length = params$subplot_length,row_per_subplot = params$row_per_subplot) 
  # mutate(x_mid = x_mid + subplot_col*left_subguard_width + (subplot_col-1)*right_subguard_width) %>%
  # mutate(y_mid = y_mid + subplot_row*bottom_subguard_length + (subplot_row-1)*top_subguard_length) %>%
  # mutate(x_mid = x_mid + plot_col*left_guard_width +  (plot_col-1)*right_guard_width) %>% 
  # mutate(y_mid = y_mid + plot_row*bottom_guard + (plot_row-1)*top_guard)
# quick check
# ggplot() +
#   geom_tile(data = one_subplot_row,aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = as.factor(id)))


get_many_subplot_with_rows <- function(){
  tibble(subplot = rep(1:max(one_subplot_area$id), each = max(one_subplot_row$id))) %>% #  subplot id for each row
  bind_cols(do.call("rbind", replicate(max(max(one_subplot_area$id)), one_subplot_row, simplify = FALSE))) # like a merge, but repeat the row info for each subplot
  }

get_subplot_origins <- function(subplot_area=subplot_area){
                   subplot_area %>% select(subplot = id, sub_x_mid = x_mid, sub_y_mid = y_mid ,x_length:plot_row) %>%
                   mutate(x0 = sub_x_mid - x_length/2, y0 = sub_y_mid - y_length/2) %>%
                   select(-c(sub_x_mid:y_length)) %>%
                   mutate(plot = rep(seq(1,max(plot_area$id)),each = max(subplot)))
                 }
  
  
get_each_plot_subplot_with_rows <- function(many_subplot_with_rows=many_subplot_with_rows,subplot_origins=subplot_origins){
  tibble(plot = rep(1:max(plot_area$id), each = max(one_subplot_area$id)*max(one_subplot_row$id))) %>%
  bind_cols(do.call("rbind", replicate(max(plot_area$id), many_subplot_with_rows, simplify = FALSE))) %>% # cool, there is now 480 rows with identical mid points
  select(-c(plot_col:subplot_col)) %>% # dump the place holder columns full of NA
  left_join(subplot_origins, by = c("subplot"= "subplot", "plot"="plot")) %>% # 480 on the trial run - as expected
  mutate(x_mid = x_mid + x0, y_mid=y_mid + y0) %>% 
  select(-c(x0,y0))
 }

# # debugging
# # each row in each sub plot
# ggplot() +
#   geom_tile(data = each_plot_subplot_with_rows,aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = as.factor(id)))
# 



# field_boundary <- bind_rows(field,plot_area,subplot_area,each_plot_subplot_with_rows) %>%
#   mutate(id = as.character(id), id = str_pad(id, 4, pad = "0")) %>%
#   mutate(fill = paste(layer_id,id,sep="_")) %>%
#   mutate(x_mid_temp =  x_mid + params$spray_alley_width*plot_col) %>% # add spray alley
#   mutate(x_mid = ifelse(layer_id == "study",x_mid,x_mid_temp)) %>%
#   select(-x_mid_temp)
# 
# # debugging every row, subplots, plots and field 
#   ggplot() +
#   geom_tile(data = field_boundary, aes(x=x_mid,y=y_mid,width=x_length,height=y_length,fill = fill),alpha = 0.5) 
# 


################# 
# okay, what about adding rows for each non-trial planted area?


#####################################################
# convert tiles to polygon

# we'll have a minimum columns for c("layer_id","id","plot","subplot","xmid","y_mid","x_length","y_length")

# polygons are a set of points that have an order that matters. 
# lower left, lower right, upper right, upper left
# Field_boundary_polygon  <-     field_boundary %>% 
#        mutate(ll.x = x_mid - x_length/2 ,lr.x= x_mid + x_length/2,
#        ur.x= x_mid - x_length/2,ul.x= x_mid + x_length/2,
#        ll.y=y_mid - y_length/2, lr.y=y_mid - y_length/2,
#        ur.y=y_mid + y_length/2,ul.y = y_mid + y_length/2) %>%
#        pivot_longer(cols = c(ends_with("x"), ends_with("y")),
#                   names_to = c("level",".value"),
#                   names_pattern = "(..).(.)")
#                   
tile2polygon <- function(df,x_mid,y_mid,x_length,y_length) {
          df %>%
          mutate(ll.x = x_mid - x_length/2 ,lr.x= x_mid + x_length/2,
                 ur.x = x_mid + x_length/2,ul.x= x_mid - x_length/2,
                 ll.y=y_mid - y_length/2, lr.y=y_mid - y_length/2,
                 ur.y=y_mid + y_length/2,ul.y = y_mid + y_length/2) %>%
                 pivot_longer(cols = c(ends_with("x"), ends_with("y")),
                  names_to = c("level",".value"),
                  names_pattern = "(..).(.)") %>%
          mutate(level = factor(level,ordered=TRUE,c("ll","lr","ur","ul")))
}


# field_boundary_polygon <- tile2polygon(field_boundary,x_mid=x_mid,y_mid=y_mid,x_length=x_length,y_length=y_length)


  