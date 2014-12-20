# attempting plot creating for long term N trials
library(sp)
library(rgdal)
library(maptools)
# 15m long by 6m wide
# lngth <- 15
# wdth <- 6
# 4 plots N to S, and 5 E to W
NtoS <- 5
EtoW <- 6
llLat <- 43.300261
llLong <- -89.385877
ulLat <- 43.300400
ulLong <- -89.385877
lngth <- 10
wdth <- 40
metric <- F
# corners <- matrix(c(43.294178, -89.384254, 43.294701, -89.384248), 2,2, T)
# use only feet or meters
plotRep <- function(llLat, llLong, ulLat, ulLong, lngth, wdth, NtoS, EtoW, fileName = 'test', metric = T){
    corners <- matrix(c(llLat, llLong, ulLat, ulLong), 2,2, T)
    rownames(corners) <- c('ll','ul')
    colnames(corners) <- c('lat','long')
    corners <- data.frame(corners)
    coordinates(corners) <- ~long + lat
    #ll <- c(43.294178, -89.384254)
    #ul <- c(43.294701, -89.384248)
    proj4string(corners) <- CRS("+proj=longlat +datum=WGS84")
    wtm <- CRS("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +units=m +no_defs")
    corners <- spTransform(corners, wtm)
    crds <- data.frame(corners@coords)
    if (!metric){
       lngth <- lngth * 0.3048
       wdth <- wdth * 0.3048
    }
    
    y1 <- seq(crds[1,2],
                     # replace the 15 by l
              crds[2,2]+(lngth/2), lngth)
    x1 <- seq(crds[1,1],
              crds[1,1]+(EtoW*wdth)+2, wdth)
    # spinning into polygons
    gridPts <- expand.grid(y1,x1)
    gridPts <- data.frame(gridPts[,2],gridPts[,1])
    colnames(gridPts) <- c('X', 'Y')
    #
    plots <- NULL
    id <- 0
    for (i in 1:nrow(gridPts)){
#         i <- 6
        if (i %% EtoW == 0){next}
        id <- id + 1
        if (id > EtoW*NtoS){break}
        polyPts <- gridPts[i:(i+1),]
        polyPts <- rbind(polyPts,polyPts)
        polyPts[3:4,1] <- polyPts[3:4,1]+wdth
        indx <- c(1,2,4,3,1)
        rePts <- polyPts[indx,]
        rePolys <- Polygons(list(Polygon(rePts)), id)
        plots <- c(plots,rePolys)
    }
    spPolys <- SpatialPolygons(plots)

    dat <- data.frame(id = 1:(EtoW*NtoS))#, pltNum)
    spdf <- SpatialPolygonsDataFrame(spPolys,dat)
    proj4string(spdf) <- wtm
    plot(spdf)
    
    labls <- NULL
    for (lbl in 1:length(spdf@polygons)){
         labls <- rbind(labls, spdf@polygons[[lbl]]@labpt)
    }
    text(labls, labels = spdf@data$id)
#     return(spdf)
    spdfExp <- spTransform(spdf, CRS("+proj=longlat +datum=WGS84"))
    writeOGR(spdfExp['id'], paste(fileName,'.kml',sep = ''), 'id',driver = 'KML')
    # writing a shapefile
#     writeOGR(spdf, paste(fileName), 'id',driver = 'ESRI Shapefile')
}


plotRep(43.300261, -89.385877, ulLat = 43.300400, ulLong = -89.385877, 
        lngth = 10, wdth = 40, NtoS = 5, EtoW = 6, fileName = "A2sp_Rep4_v2", metric = F)
plotRep(43.300261, -89.385877, ulLat = 43.300400, ulLong = -89.385877, lngth = 3.048, wdth = 13.716, NtoS = 5, EtoW = 6, 
    fileName = "A2sp_Rep4", metric = T)
# writeOGR(meuse_ll["zinc"], paste(td, "meuse.kml", sep="/"),"zinc", "KML")
###############################################
 ### For a different rep
#corners <- spTransform(corners, wtm)
#crds <- data.frame(corners@coords)
#SpatialPoints:
#corners
#         long      lat
#[1,] 569948.4 313667.7
#[2,] 569948.5 313725.7
crds.2 <- crds
crds.2[,1] <- crds.2[,1]+(10.668+wdth*EtoW)
crds <- crds.2
y1 <- seq(crds[1,2],
                 # replace the 15 by l
          crds[2,2]+5, lngth)
x1 <- seq(crds[1,1],
          crds[1,1]+(5*6)+2, wdth)
# spinning into polygons
gridPts <- expand.grid(y1,x1)
gridPts <- data.frame(gridPts[,2],gridPts[,1])
colnames(gridPts) <- c('X', 'Y')
#
plots <- NULL
id <- 0
for (i in 1:nrow(gridPts)){
    if (i %% EtoW == 0){next}
    id <- id + 1
    if (id > EtoW*NtoS){break}
    polyPts <- gridPts[i:(i+1),]
    polyPts <- rbind(polyPts,polyPts)
    polyPts[3:4,1] <- polyPts[3:4,1]+wdth
    indx <- c(1,2,4,3,1)
    rePts <- polyPts[indx,]
    rePolys <- Polygons(list(Polygon(rePts)), id)
    plots <- c(plots,rePolys)
}
spPolys <- SpatialPolygons(plots)
#pltNum <- c(seq(101,107),
#            seq(111,117),
#            seq(121,127),
#            seq(131,137),
#            seq(141,147))
dat <- data.frame(id = 1:(EtoW*NtoS))#, pltNum)
spdf <- SpatialPolygonsDataFrame(spPolys,dat)
proj4string(spdf) <- wtm
plot(spdf)

labls <- NULL
for (lbl in 1:length(spdf@polygons)){
     labls <- rbind(labls, spdf@polygons[[lbl]]@labpt)
}
text(labls, labels = spdf@data$id)
spdfExp <- spTransform(spdf, CRS("+proj=longlat +datum=WGS84"))
writeOGR(spdfExp['id'], 'testKML2.kml', 'id',driver = 'KML')

 writeOGR(meuse_ll["zinc"], paste(td, "meuse.kml", sep="/"),"zinc", "KML")