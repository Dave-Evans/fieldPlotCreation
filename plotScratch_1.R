# Task is to create a block of field plots, by specifying
#      one corner, length, and width of plots
library(sp)
# assuming a block that is 5X7 (101 to 107, 111 to 117, 121,131,141)
#          with a lower left hand (southWest) corner coordintates of
#          3300,1000.
#create a matrix of values for a plots with L = 20, W = 30
#       for first plots, 101-107
#       make sure it closes
x1 <- seq(3300,3300+(5*30),30)
y1 <- seq(1000,1000+(7*20),20)
# results in a two column matrix
#    with each pt represented
gridPts <- expand.grid(y1,x1)
gridPts <- data.frame(gridPts[,2],gridPts[,1])
colnames(gridPts) <- c('X', 'Y')
# 
plots <- NULL
id <- 0
for (i in 1:nrow(gridPts)){
    if (i %% 8 == 0){next} 
    id <- id + 1
    if (id > 35){break}
    polyPts <- gridPts[i:(i+1),]  
    polyPts <- rbind(polyPts,polyPts)
    polyPts[3:4,1] <- polyPts[3:4,1]+30
    indx <- c(1,2,4,3,1)
    rePts <- polyPts[indx,]
    rePolys <- Polygons(list(Polygon(rePts)), id)
    plots <- c(plots,rePolys)
}
spPolys <- SpatialPolygons(plots)
pltNum <- c(seq(101,107),
            seq(111,117),
            seq(121,127),
            seq(131,137),
            seq(141,147))
dat <- data.frame(id = 1:35, pltNum)
spdf <- SpatialPolygonsDataFrame(spPolys,dat)
plot(spdf)
#to grab each label pt
labls <- NULL
for (lbl in 1:length(spdf@polygons)){
     labls <- rbind(labls, spdf@polygons[[lbl]]@labpt)   
}
text(labls, labels = spdf@data$pltNum)
# to get labeling point
# spdf@polygons[[1]]@labpt



# find other corners, given lower left, and upper right

###  
pltTst <- Polygons(plots, 1)
spPolys <- SpatialPolygons(list(pltTst))
dat <- data.frame(id = 1)
spdf <- SpatialPolygonsDataFrame(spPolys,dat)
  
plt5 <- plots[[5]];plt16 <- plots[[16]] 
plt5 <- #SpatialPolygonsDataFrame(
                #SpatialPolygons(list(
                       Polygons(list(plt5),5)#)),
                                  #data.frame(id = 1)) 
plt16 <- #SpatialPolygonsDataFrame(
                #SpatialPolygons(list(
                       Polygons(list(plt16),16)#)),
                                  #data.frame(id = 1)) 
psPlts <- SpatialPolygons(list(plt5,plt16)) 
  
  rePolys <- Polygon(rePts)
  rePolys <- Polygons(list(rePolys), 1)
  spPolys <- SpatialPolygons(list(rePolys))

# xy <- matrix(c(0,0,30,0,30,20,0,20, 0,0),5,2,TRUE)
# then create the polygon
p  <- Polygon(xy)
p.s <- Polygons(list(p),1)
sps <- SpatialPolygons(list(p.s))
# set CRS
# proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# convert into spatialpolydataframe
dat <- data.frame(f = -9999)
spdf <- SpatialPolygonsDataFrame(sps,dat)






# HOW-TO
#create a matrix of values for a plot with L = 20, W = 30
#       make sure it closes
xy <- matrix(c(0,0,30,0,30,20,0,20, 0,0),5,2,TRUE)
# then create the polygon
p  <- Polygon(xy)
p.s <- Polygons(list(p),1)
sps <- SpatialPolygons(list(p.s))
# set CRS
# proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# convert into spatialpolydataframe
dat <- data.frame(f = -9999)
spdf <- SpatialPolygonsDataFrame(sps,dat)