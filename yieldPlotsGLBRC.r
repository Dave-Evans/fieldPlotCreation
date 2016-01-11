library(RColorBrewer)
library(classInt)
source("./fieldPlotCreation/fieldPlotCreation_funtion.r")
source("./fieldPlotCreation/function_proper_legend.r")
options(stringsAsFactors=F)

lngth = 15
wdth = 15
NtoS = 6
EtoW = 6
metric = F

posVect1 = c(43.294263, -89.381099, 43.294654, -89.381104)

nclasses=6
pal = brewer.pal(nclasses, "YlGnBu")
plt = plotRep(posVect1, lngth=lngth, wdth=wdth, NtoS=6, EtoW=6, metric=F, floatX=T)

file_dt14 = "./fieldPlotCreation/yield 2014 + spatial code.csv"
file_dt15 = "./fieldPlotCreation/yield 2015 + spatial code.csv"
dt15 = read.csv(file_dt15)
dt14 = read.csv(file_dt14)
names(dt15)[4] = "yield"
names(dt14)[5] = "yield"
dtList = list(dt14, dt15)
names(dtList) = c("2014", "2015")
pdf("./fieldPlotCreation/Plot_Mapsv2.pdf", width=11, height=8)

par(mfrow=c(1,2),mar=c(5, 4, 4, 2) + 0.1)
for (i in 1:length(unique(dt14$plot))){
	id = unique(dt14$plot)[i]
	for (j in 1:length(dtList)){ 
		dt = dtList[[j]]
		dt.sb = subset(dt, plot == id)
		if (!id %in% unique(dt.sb$plot)) {
			
			plot(plt);
#			text(getSpPPolygonsLabptSlots(plt), labels=plt@data$id)
			next
			
		}
		plt.dt = merge(plt, dt.sb, by.x="id", by.y="spatial")
		classes = classIntervals(plt.dt@data$yield, nclasses)
		colrs = findColours(classes, pal)
		newTxt = properLegend(colrs, sig_figs=1)
		plot(plt.dt, col=colrs, main=paste("Plot",id, names(dtList)[j]))
		legend(
#			"bottomleft",
				x=min(coordinates(plt)[,1]),
				y=min(coordinates(plt)[,2]),
				legend=newTxt,
				title="",
				fill=pal,
				bty='n',
				horiz=F,
				ncol=1,
				cex=.75)
#		text(getSpPPolygonsLabptSlots(plt.dt), labels=plt.dt@data$id)
	}
}
dev.off()
