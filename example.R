
#testing
source('fieldPlotCreation.r')

tst <- plotRep(
    llLat = 43.300261,      # lower left latitude  
    llOng = -89.385877,     # lower left longitude
    ulLat = 43.300400,      # upper left corner latitude
    ulLong = -89.385877,    # upper left corner longitude
    lngth = 10,             # length of plot
    wdth = 40,              # width of plot
    NtoS = 5,               # number of plots in the rep running north to south
    EtoW = 6,               # no of plots in the rep running east to west
    fileName = "A2sp_Rep4_v2", # name of file --- currently not implemented
    metric = F)             # are the length and width in metric or english?
plot(tst)
labls <- tst@polygons[[1]]@labpt
writeOGR(tst, 'Test', 'id',driver = 'ESRI Shapefile')