# attempting plot creating for long term N trials
library(sp)
library(rgdal)
library(rgeos)
# library(maptools)
# use only feet or meters
# first four inputs should be geopraphic coordinates in wgs84, straight from GoogleMaps/Earth
#llLat = 43.299066      # lower left latitude  
#llLong = -89.354851     # lower left longitude
#ulLat = 43.299226    # upper left corner latitude
#ulLong = -89.354847   # upper left corner longitude
#lngth = 30             # length of plot
#wdth = 5              # width of plot
#NtoS = 2               # number of plots in the rep running north to south
#EtoW = 5               # no of plots in the rep running east to west
#fileName = "A2sp_Rep4_v2" # name of file --- currently not implemented
#metric = F             # are the length and width in metric or english?
#positionVect = c(43.294263, -89.381099, 43.294654, -89.381104)
#lngth = 15
#wdth = 15
#NtoS = 6
#EtoW = 6
#metric = F
#floatX = F

plotRep <- function(positionVect, lngth, wdth, NtoS, EtoW, floatX = TRUE,
                    fileName = paste('plot_',as.double(Sys.time()), sep = ''), metric = T){
	
#    corners <- matrix(c(llLat, llLong, ulLat, ulLong), 2,2, T)
	corners <- matrix(positionVect, 2, 2, T)
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
	
    y_seq <- seq(crds[1,2],
      crds[2,2]+lngth/2, length.out=NtoS+1)#lngth)
	### Use length.out=EtoW when constant width desired
	### User by=wdth when width can float
	if (floatX){
		x_seq <- seq(crds[1,1],
			crds[1,1]+(EtoW*wdth), by=wdth)		
	} else {
		x_seq <- seq(crds[1,1],
			crds[1,1]+(EtoW*wdth), length.out=EtoW)
	}

	cnter = 0
	polyStrList = "MULTIPOLYGON("
	
	for (y in y_seq){
		if (y == max(y_seq)) { break }
		for (x in x_seq){
			### Need this when letting width grow
			if (floatX & (x == max(x_seq)) ) { break }
			cnter=cnter+1
			pt1 = paste(x,y)
			pt2 = paste(x,y+lngth)
			pt3 = paste(x+wdth,y+lngth)
			pt4 = paste(x+wdth,y)
			plyStr = paste0(
				'((',
				paste(pt1,pt2,pt3,pt4,pt1,sep=","),
				')),')
			polyStrList = paste(polyStrList, plyStr,sep="")
#			polyTst = readWKT(plyStr)
		}
	}
	substr(polyStrList, nchar(polyStrList), nchar(polyStrList)) = ")"
	spPolys = readWKT(polyStrList)

    dat <- data.frame(id = 1:(EtoW*NtoS))#, pltNum)
	spPolys = disaggregate(spPolys)
    spdf <- SpatialPolygonsDataFrame(spPolys,dat)
    proj4string(spdf) <- wtm

    ## plot(spdf)
	## text(getSpPPolygonsLabptSlots(spdf), labels=spdf@data$id)

    ### Potentially  future support for kml or shapefile export
	## writeOGR(spdfExp['id'], paste(fileName,'.kml',sep = ''), 'id',driver = 'KML')

    ### writing a shapefile
     ##writeOGR(spdf,fileName, 'id',driver = 'ESRI Shapefile')
	 return(spdf)
}

