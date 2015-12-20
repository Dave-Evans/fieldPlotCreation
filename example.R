source("./fieldPlotCreation/fieldPlotCreation_funtion.r")

#llLat = 43.299066      # lower left latitude  
#llLong = -89.354851     # lower left longitude
#ulLat = 43.299226    # upper left corner latitude
#ulLong = -89.354847   # upper left corner longitude
#lngth = 30,             # length of plot
#wdth = 5,              # width of plot
#NtoS = 2,               # number of plots in the rep running north to south
#EtoW = 5,               # no of plots in the rep running east to west
#fileName = "A2sp_Rep4_v2" # name of file --- currently not implemented
#metric = F             # are the length and width in metric or english?

lngth = 15
wdth = 15
NtoS = 6
EtoW = 6
metric = F

posVect1 = c(43.294263, -89.381099, 43.294654, -89.381104)
posVect2 = c(43.294785, -89.382068, 43.295172, -89.382073)
posVect3 = c(43.295307, -89.382072, 43.295696, -89.382085)
posVect4 = c(43.295306, -89.380604, 43.295693, -89.380609)
#posVect5 = c(43.295315, -89.380599, 43.295699, -89.380601)
#posVect6 = c(43.295315, -89.380599, 43.295699, -89.380601)


vects = list(
	posVect1,
	posVect2,
	posVect3,
	posVect4)

for (i in 1:length(vects)){
	plotRep(vects[[i]], lngth=lngth, wdth=wdth, NtoS=6, EtoW=6, metric=F, floatX=T)
}
#tst <- plotRep(
#		positionVect = posVect1,
#		llLat = llLat,      # lower left latitude  
#		llLong = llLong,     # lower left longitude
#		ulLat = ulLat,      # upper left corner latitude
#		ulLong = ulLong,    # upper left corner longitude
#		lngth = lngth,             # length of plot
#		wdth = wdth,              # width of plot
#		NtoS = 6,               # number of plots in the rep running north to south
#		EtoW = 6,               # no of plots in the rep running east to west
#		fileName = "A2sp_Rep4_v2", # name of file --- currently not implemented
#		metric = F) 
