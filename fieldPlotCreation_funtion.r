# attempting plot creating for long term N trials
library(sp)
library(rgdal)
library(maptools)
# use only feet or meters
# first four inputs should be geopraphic coordinates in wgs84, straight from GoogleMaps/Earth
plotRep <- function(llLat, llLong, ulLat, ulLong, lngth, wdth, NtoS, EtoW, 
                    fileName = paste('plot_',as.double(Sys.time()), sep = ''), metric = T){
    corners <- matrix(c(llLat, llLong, ulLat, ulLong), 2,2, T)
    rownames(corners) <- c('ll','ul')
    colnames(corners) <- c('lat','long')
    corners <- data.frame(corners)
    coordinates(corners) <- ~long + lat
    proj4string(corners) <- CRS("+proj=longlat +datum=WGS84")
    wtm <- CRS("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +units=m +no_defs")
    corners <- spTransform(corners, wtm)
    crds <- data.frame(corners@coords)
    if (!metric){
       lngth <- lngth * 0.3048
       wdth <- wdth * 0.3048
    }
    
    y1 <- seq(crds[1,2],
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
    #plot(spdf)
    
    labls <- NULL
    for (lbl in 1:length(spdf@polygons)){
         labls <- rbind(labls, spdf@polygons[[lbl]]@labpt)
    }
#     text(labls, labels = spdf@data$id)
    return(spdf)
    #spdfExp <- spTransform(spdf, CRS("+proj=longlat +datum=WGS84"))
    #Potentially  future support for kml or shapefile export
#     writeOGR(spdfExp['id'], paste(fileName,'.kml',sep = ''), 'id',driver = 'KML')
    # writing a shapefile
#     writeOGR(spdf, paste(fileName), 'id',driver = 'ESRI Shapefile')
}

