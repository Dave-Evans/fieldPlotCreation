library(ggmap)
library(RgoogleMaps)
library(raster)
library(rgdal)

source('fieldPlotCreation_funtion.r')


tst <- plotRep(
    llLat = 43.300261,      # lower left latitude  
    llLong = -89.385877,     # lower left longitude
    ulLat = 43.300400,      # upper left corner latitude
    ulLong = -89.385877,    # upper left corner longitude
    lngth = 10,             # length of plot
    wdth = 40,              # width of plot
    NtoS = 5,               # number of plots in the rep running north to south
    EtoW = 6,               # no of plots in the rep running east to west
    fileName = "A2sp_Rep4_v2", # name of file --- currently not implemented
    metric = F)             # are the length and width in metric or english?

# transforming and finding center of plot
tst <- spTransform(tst, CRS("+proj=longlat +datum=WGS84"))

# writeOGR(tst, dsn = '.',layer = "test_plot.shp", driver = "ESRI Shapefile")
# tstimp = readOGR(dsn = 'C:/DataFiles/fieldPlotCreation', layer = 'test_plot.shp')

tst_ext = extent(tst)
long = mean(c(tst_ext@xmin, tst_ext@xmax))
lat = mean(c(tst_ext@ymin, tst_ext@ymax))

sat_image <- get_map(
    c(lon=long, lat=lat),
    zoom = 14, 
    maptype = "terrain", 
    source = "google")

# tst = fortify(tst)
# 
# plt_poly = sat_image + geom_polygon(
#         aes(x=long, y=lat, group=group), 
#  #       fill='grey', 
#  #       size=.2,
#         color='green', 
#         data=tst, 
#         alpha=0)

png('test.png')
    ggmap(get_map(c(lon=long, lat=lat),zoom = 20, maptype = "satellite", source = "google")) +#c(lon=long, lat=lat), 
    geom_polygon(aes(x = long, y = lat, group = id),#, group = group
                data = tst, colour = 'black', alpha = .5) 
dev.off()#+
    
# ggmap(get_map(maptype = "satellite", zoom = 8), extent = "device") +
#         geom_polygon(aes(x = lon, y = lat, group = plotOrder),
#                 data = zips, colour = NA, fill = "red", alpha = .5) +
#         geom_path(aes(x = lon, y = lat, group = plotOrder),
#                 data = zips, colour = "white", alpha = .7, size = .4)




# ggmap(sat_image)




