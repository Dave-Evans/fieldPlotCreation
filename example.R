library(RColorBrewer)
library(classInt)
source("./fieldPlotCreation/fieldPlotCreation_funtion.r")

options(stringsAsFactors=F)

posVect1 = c(
	43.294263, 			### lower left Lat,
	-89.381099,			### lower left Long,
	43.294654,			### upper left Lat
	-89.381104)			### upper left Long
lngth = 15				### length of each plot
wdth = 15				### width of each plot
NtoS = 6				### number of plots running up and down
EtoW = 6				### number running left ot right
metric = F           	### are the length and width in metric or english?

rep = plotRep(posVect1, lngth=lngth, wdth=wdth, NtoS=6, EtoW=6, metric=F, floatX=T)

