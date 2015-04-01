library(rgeos)
library(maptools)
library(GISTools)
library(maps)
library(sp)
library(reshape2)
library(rgdal)
library(jpeg)

setwd('C:/Users/fraterp/Documents/DNR/coding_PI_grids')


# open lake polygon file with NAD83 HARN WTM projection
lakes.nad <- read.table('wiHydroLayerData_NAD83_HARN_WTM.txt', header=T, sep='\t')
names(lakes.nad) <- c('wkt', 'wbic', 'length', 'area')
lakes.nad$wkt <- as.character(lakes.nad$wkt)

# this is data to calculate littoral zone
# it is percentage of lake <20 feet deep
lake.size <- read.csv('wl_row_waterbody_info.csv', header=T) # this is the depth > 20 ft data that you will need to get from Jenn
lake.size$shoreline.length <- as.character(lake.size$shoreline.length)
lake.size$shoreline.length <- gsub(' MILES', '', lake.size$shoreline.length)
lake.size$shoreline.length <- as.numeric(lake.size$shoreline.length)
lake.size$county <- as.character(lake.size$county)

ls.sub <- subset(lake.size, select=c(wbic, waterbody.seq.no, official.name, county, latitude,
				longitude, waterbody.type.code, official.size, official.size.units,
				above.20.feet.percent, shoreline.length))

# subset to just lakes; take out rivers and junk
ls.sub <- subset(ls.sub, 
			waterbody.type.code == 'LP' | 
			waterbody.type.code == 'RF' | 
			waterbody.type.code == 'DP' | 
			waterbody.type.code == 'OW')

# calculate sdf, littoral zone area, and number of littoral points
ls.sub$sdf <- ls.sub$shoreline.length / (2*sqrt(pi*ls.sub$official.size*0.0015626))
ls.sub$above.20.feet.percent <- ls.sub$above.20.feet.percent / 100
# ls.sub$above.20.feet.percent[is.na(ls.sub$above.20.feet.percent)] <- 0
ls.sub$littoral.acres <- ls.sub$official.size*(1 - ls.sub$above.20.feet.percent)

tiny.lakes <- subset(ls.sub, littoral.acres < 20 | is.na(littoral.acres))

# subset to lakes greater than or equal to 20 littoral acres
ls.sub <- subset(ls.sub, littoral.acres >= 20)

# subset out lakes michigan and superior
ls.sub <- ls.sub[!ls.sub$wbic==20,]
ls.sub <- ls.sub[!ls.sub$wbic==2751220,]

# remove duplicates that exist
ls.sub <- subset(ls.sub, !rownames(ls.sub) == 1155)
ls.sub <- subset(ls.sub, !rownames(ls.sub) == 15211)
ls.sub <- subset(ls.sub, !rownames(ls.sub) == 15212)
ls.sub <- subset(ls.sub, !rownames(ls.sub) == 21578)
ls.sub <- subset(ls.sub, !rownames(ls.sub) == 26864)
ls.sub <- subset(ls.sub, !rownames(ls.sub) == 28470)
ls.sub <- subset(ls.sub, !duplicated(ls.sub$wbic))


# calculate how many littoral zone points there should be
ls.sub$sdf[is.na(ls.sub$sdf)] <- 3.1
for (i in 1:nrow(ls.sub)) {
	if (ls.sub$sdf[i] < 1.5) {
		ls.sub$littoral.points[i] <- 88.5*log(ls.sub$littoral.acres[i]) - 183
	} else if (ls.sub$sdf[i] >1.5 & ls.sub$sdf[i] < 3) {
		ls.sub$littoral.points[i] <- 140.2*log(ls.sub$littoral.acres[i]) - 346
	} else {ls.sub$littoral.points[i] <- 170*log(ls.sub$littoral.acres[i]) - 427}
}
# calculate total number of points for lake
ls.sub$total.points <- ls.sub$littoral.points / (1 - ls.sub$above.20.feet.percent)

# this is an algorithm to calculate the correct meter spacing from points per acre
meters <- c(30, 35, 45, 55, 60, 70, 80, 100, 110) # based off of 'Creating PI grids using GME' by Michelle Nault
points.per.acre <- c(4,3,2,1.5,1,.8,.6,.4,.35) # based off of 'Creating PI grids using GME' by Michelle Nault
meter.spacing <- lm(meters ~ I(1 / points.per.acre) + points.per.acre)
# summary(meter.spacing)
# plot(meters~ points.per.acre)
# lines(points.per.acre, predict(meter.spacing), col='blue', lwd=2)

ls.sub$points.per.acre <- ls.sub$total.points / ls.sub$official.size
ls.sub$pt.m.space <- round(23.4980*(1 / ls.sub$points.per.acre) + -5.1332*ls.sub$points.per.acre + 44.0034, digits=0)



## merge lake information and attribute data together
all.nad <- merge(ls.sub, lakes.nad, by=c('wbic'), all.x=T)
# you wrote out a CSV of the all.nad data set for future use
# it is called lake.info.spatial.nad.csv and is located in the wd listed at the beginning of file

# remove lakes with grids that are already completed
done <- read.csv('completedGrids.csv', header=T)
names(done) <- tolower(names(done))

all.nad <- subset(all.nad, !(all.nad$wbic %in% done$wbic))

# take out lakes with no littoral information
rm.depths <- merge(all.nad, subset(lake.size, select=c(wbic, max.depth)), by=c('wbic'), all.x=T)
rm.depths <- unique(rm.depths)
rm.depths$max.depth <- gsub(' FEET', '', rm.depths$max.depth)
rm.depths$max.depth <- as.numeric(rm.depths$max.depth)
no.grid.made <- subset(rm.depths, above.20.feet.percent==0 & max.depth > 20 | is.na(max.depth))
rm.depths <- subset(rm.depths, !(max.depth > 20 & above.20.feet.percent==0))
all.nad <- rm.depths


# merge data and write out file of lakes without grids made
# these are lakes that were either too small or lacked littoral information
no.grid.made <- subset(no.grid.made, select= -c( 
				littoral.points, total.points, points.per.acre, pt.m.space,
				wkt, length, area, max.depth))
lakes.lacking.grid <- rbind(no.grid.made, tiny.lakes)
lakes.lacking.grid <- subset(lakes.lacking.grid, select= -c(littoral.acres))
no.grid.nad <- merge(lakes.lacking.grid, lakes.nad, by=c('wbic'))
write.table(no.grid.nad, file='lakesWithoutGridMade.txt', sep='\t', row.names=F, quote=F, na='')




# get rid of lakes that are missing wkt geometry
all.nad <- subset(all.nad, !(is.na(all.nad$wkt)))

# remove certain characters that cannot be in the name of a directory
all.nad$official.name <- gsub('[*]', '', all.nad$official.name)
all.nad$official.name <- gsub('#', '', all.nad$official.name)
all.nad$official.name <- gsub('&', 'and', all.nad$official.name)

# add in counties for the six lakes that are missing them
all.nad$county[all.nad$wbic==2092000] <- 'Chippewa'
all.nad$county[all.nad$wbic==2323800] <- 'Vilas'
all.nad$county[all.nad$wbic==2494500] <- 'Washburn'
all.nad$county[all.nad$wbic==2599000] <- 'Douglas'
all.nad$county[all.nad$wbic==2866800] <- 'Douglas'
all.nad$county[all.nad$wbic==2935800] <- 'Ashland'

# open the DNR logo for creating lake maps
dnr.logo <- readJPEG('DNRlogo.jpg')

# use this line to create directories for all counties in WI
setwd('C:/Users/fraterp/Documents/DNR/WI_PI_Sampling_Grids')
for (i in unique(all.nad$county)) dir.create(i)

## calculating and creating grid and polygon shapefiles
for (i in unique(all.nad$wbic)) {
	tryCatch({
	setwd('C:/Users/fraterp/Documents/DNR/WI_PI_Sampling_Grids')
	lake <- subset(all.nad, wbic == i);
	lake.id <- paste(lake$official.name, lake$county, lake$wbic, sep='_')
	county <- lake$county
	
	# create poly file and write it out as a shapefile
	lake.poly <- readWKT(lake$wkt, p4s=CRS('+init=epsg:3070'), id='0')
	lake.poly.latlong <- spTransform(lake.poly, CRS('+init=epsg:4326'))
	poly.data <- subset(lake, select=c(wbic, official.name, county, official.size, shoreline.length))
	names(poly.data) <- c('wbic', 'lake', 'county', 'area', 'shoreline')
	rownames(poly.data) <- 0
	setwd(county)
	lake.poly.latlong.df <- SpatialPolygonsDataFrame(lake.poly.latlong, data=poly.data)
	writeOGR(lake.poly.latlong.df, dsn=lake.id, layer=paste(lake.id, 'POLY', sep='_'), driver='ESRI Shapefile')
	
	# create grid layer
	lake.poly.coords <- NULL;
	for (j in 1:length(lake.poly@polygons[[1]]@Polygons)) {
		coords <- as.data.frame(lake.poly@polygons[[1]]@Polygons[[j]]@coords)
		lake.poly.coords <- rbind(lake.poly.coords, coords)
	}
	west <- min(lake.poly.coords$x)
	east <- max(lake.poly.coords$x)
	south <- min(lake.poly.coords$y)
	north <- max(lake.poly.coords$y)
	west.east <- seq(west, east, by=lake$pt.m.space)
	north.south <- seq(south, north, by=lake$pt.m.space)
	sq.grid <- expand.grid(west.east, north.south)
	names(sq.grid) <- c('x', 'y')
	sq.grid <- SpatialPoints(sq.grid, CRS('+init=epsg:3070'))
	points.over.poly <- over(sq.grid, lake.poly)
	grid.in.poly <- as.data.frame(sq.grid@coords[!is.na(points.over.poly),])
	
	# sort points from west to east and north to south
	pi.grid <- grid.in.poly[order(grid.in.poly$x, -grid.in.poly$y),]
	point.number <- nrow(pi.grid)

	# convert to SpatialPoints
	pi.grid <- SpatialPoints(pi.grid, CRS('+init=epsg:3070'))
	pi.grid.latlong <- spTransform(pi.grid, CRS('+init=epsg:4326'))
	plot.id.data <- data.frame(PLOT_ID=1:point.number, longitude=pi.grid.latlong$x, latitude=pi.grid.latlong$y)
	pi.grid.latlong.df <- SpatialPointsDataFrame(pi.grid.latlong, data=plot.id.data)
	attr(pi.grid.latlong.df@coords, 'dimnames')[[2]] <- c('Longitude', 'Latitude')
	gps.textfile <- as.data.frame(pi.grid.latlong.df)
	gps.textfile$PLOT_ID <- paste(toupper(substr(strsplit(lake$official.name, split=' ')[[1]][1], 1,4)), gps.textfile$PLOT_ID, sep='')
	gps.textfile$type <- rep('WAYPOINT', nrow(gps.textfile))
	gps.textfile <- gps.textfile[,c(4,1,3,2)]
	names(gps.textfile) <- c('type', 'ident', 'lat', 'long')
	writeOGR(pi.grid.latlong.df, dsn=lake.id, layer=paste(lake.id, '_', lake$pt.m.space, 'mpts', sep=''), driver='ESRI Shapefile', dataset_options=c('PLOT_ID=PLOTID'))
	setwd(lake.id)
	grid.lat <- pi.grid.latlong.df@coords[,2];
	grid.long <- pi.grid.latlong.df@coords[,1];
	pdf(paste(lake.id, lake$pt.m.space, 'mpts.pdf', sep=''), paper='special')
		plot(lake.poly.latlong.df);
		plot(pi.grid.latlong.df, add=T, pch=16, cex=0.7);
		text(pi.grid.latlong.df@coords[,1], pi.grid.latlong.df@coords[,2], labels=pi.grid.latlong.df@data$PLOT_ID, cex=0.5, adj=c(-.1,-1))
		legend('bottomleft', legend=paste(lake$official.name, '\n', 
						lake$county, ' County', '\n', 'WBIC ', lake$wbic, '\n',
						round(lake$official.size, 1), ' Acres', '\n',
						nrow(pi.grid.latlong.df), ' Sampling Points', '\n',
						lake$pt.m.space, 'm between points', '\n',
						'Site1:', '\n', 'Lat ', 
						round(as.numeric(pi.grid.latlong.df@coords[1,2]),2), '\n',
						'Long ', '\n', 
						round(as.numeric(pi.grid.latlong.df@coords[1,1]), 2), sep=''), bty='n', cex=0.8)
		na.box <- legend('topright', legend=c('\n', '\n', '\n', '\n', '\n'), bty='n')
		north.arrow(na.box$rect$left, na.box$rect$top - (na.box$rect$h / 2), len=na.box$rect$w / 3)
		ms.box <- legend('bottom', legend=c('', '', '', ''), bty='n')
		dnr.image <- legend('bottomright', legend=c('                  ', '', ''), bty='n')
		map.scale(mean(c(dnr.image$rect$left, ms.box$rect$left)), ms.box$rect$top - (ms.box$rect$h/1.5), relwidth=0.10, ratio=F, metric=F, cex=0.6)
		title(main=list('Sampling maps created using the 24K hydro layer available from the WDNR in spring 2015.
					We suggest checking these maps against the most recent aerial imagery or by field verification.  
					If major discrepancies exist, please notify DNR Baseline Aquatic Plants Survey at DNRBASELINEAQUATICPLANTS@wisconsin.gov.',
				cex=0.6, font=3))
		rasterImage(dnr.logo, xleft=dnr.image$rect$left,
				ybottom=dnr.image$rect$top - dnr.image$rect$h,
				xright=dnr.image$rect$left + dnr.image$rect$w,
				ytop=dnr.image$rect$top)
	dev.off()
	jpeg(paste(lake.id, lake$pt.m.space, 'mpts.jpeg', sep=''), height=10, width=10, units='in', res=100)
		plot(lake.poly.latlong.df);
		plot(pi.grid.latlong.df, add=T, pch=16, cex=0.7);
		text(pi.grid.latlong.df@coords[,1], pi.grid.latlong.df@coords[,2], labels=pi.grid.latlong.df@data$PLOT_ID, cex=0.5, adj=c(-.1,-1))
		legend('bottomleft', legend=paste(lake$official.name, '\n', 
						lake$county, ' County', '\n', 'WBIC ', lake$wbic, '\n',
						round(lake$official.size, 1), ' Acres', '\n',
						nrow(pi.grid.latlong.df), ' Sampling Points', '\n',
						lake$pt.m.space, 'm between points', '\n',
						'Site1:', '\n', 'Lat ', 
						round(as.numeric(pi.grid.latlong.df@coords[1,2]),2), '\n',
						'Long ', '\n', 
						round(as.numeric(pi.grid.latlong.df@coords[1,1]), 2), sep=''), bty='n', cex=0.8)
		na.box <- legend('topright', legend=c('\n', '\n', '\n', '\n', '\n'), bty='n')
		north.arrow(na.box$rect$left, na.box$rect$top - (na.box$rect$h / 2), len=na.box$rect$w / 3)
		ms.box <- legend('bottom', legend=c('', '', '', ''), bty='n')
		dnr.image <- legend('bottomright', legend=c('                  ', '', ''), bty='n')
		map.scale(mean(c(dnr.image$rect$left, ms.box$rect$left)), ms.box$rect$top - (ms.box$rect$h/1.5), relwidth=0.10, ratio=F, metric=F, cex=0.6)
		title(main=list('Sampling maps created using the 24K hydro layer available from the WDNR in spring 2015.
					We suggest checking these maps against the most recent aerial imagery or by field verification.  
					If major discrepancies exist, please notify DNR Baseline Aquatic Plants Survey at DNRBASELINEAQUATICPLANTS@wisconsin.gov.',
				cex=0.8, font=3))
		rasterImage(dnr.logo, xleft=dnr.image$rect$left,
				ybottom=dnr.image$rect$top - dnr.image$rect$h,
				xright=dnr.image$rect$left + dnr.image$rect$w,
				ytop=dnr.image$rect$top)
	dev.off()
	write.table(gps.textfile, file=paste(lake.id, '_', lake$pt.m.space, 'mpts.txt', sep=''), sep='\t', quote=F, row.names=F)
	readmeText <- "Sampling grids were created using the 24K hydro layer available from the WDNR in spring 2015. 
				We suggest checking these maps against the most recent aerial imagery or by field verification.  
				
				If major discrepancies exist, please notify DNR Baseline Aquatic Plants Survey at DNRBASELINEAQUATICPLANTS@wisconsin.gov.'
				
				This folder consists of a lake polygon file as well as a lake plant survey PI grid file. Both are in the WGS84 projection and coordinates are in latitude-longitude.
				
				In addition, there are maps of the lake and grid that can be printed out. Both a PDF and JPEG option should be available.
				
				Lastly, there is a text file containing grid coordinates that can be uploaded into a handheld GPS. The naming of this file is the same as the grid shapefile, so you may have to search for it. It is the file that is listed as a Text Document.

				Have fun sampling!"
	write.table(readmeText, file='readme.txt', quote=F, row.names=F, col.names=F)
	}, error=function(e) {cat('Error in ', lake.id, conditionMessage(e), '\n')})
}




