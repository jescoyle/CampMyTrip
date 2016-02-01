### This script visulaizes campground data from Reserve America and finds a preliminary itinerary for a West Coast road trip

# Load packages
library(XML) # for queries to campsites API
library(jsonlite) # for queries to Google Distance API
library(rgdal) # for spatial data
library(sp) # for spatial data
library(maps) # for US map
library(maptools) # for manipulating map
library(rgeos) # for buffer
library(ggmaps)

# Define directories
working_dir = 'C:/Users/jrcoyle/Documents/SF Jobs/Data Incubator/CampMySite/'

# Load campground data
camps = read.csv('campgrounds_US.csv')

# Define variables
camps_key = ''
camps_api_loc = 'http://api.amp.active.com/camping/campgrounds?'
sites_api_loc = 'http://api.amp.active.com/camping/campsites?'
goog_api_loc = 'https://maps.googleapis.com/maps/api/distancematrix/json?'
goog_dir_api_loc = 'https://maps.googleapis.com/maps/api/directions/json?'
goog_api_key = ''


my_proj = '+proj=aeqd +lat_0=40 +lon_0=-97 +units=km'


### Visualize campground locations ###

# Some campgrounds are missing coordinates
# Exclude for now. TO DO: USE GOOGLE LOOKUP TO GET COORDS
camps = subset(camps, !is.na(longitude))

# Some campgrounds have errors in their coordindates - fix them
subset(camps, longitude < -180)
camps[camps$facilityID==110452,'longitude'] = -121.86 # typo
subset(camps, longitude > 0)
camps[camps$longitude >0, 'longitude'] = -camps[camps$longitude >0, 'longitude'] # wrong sign
subset(camps, longitude > -66) # typos: TO DO: USE GOOGLE LOOKUP TO RESOLVE, EXCLUDE FOR NOW

# Convert camps to spatial data
camps_sp = camps
coordinates(camps_sp) = c('longitude','latitude')
proj4string(camps_sp) = CRS('+proj=longlat')
plot(camps_sp)
camps_eqd = spTransform(camps_sp, CRS(my_proj))

# Some campgrounds clearly in wrong place.
# For now, exclude using polygon of USA
usa = map('world', 'usa', fill=T)
usa_sp = map2SpatialPolygons(usa, usa$names, CRS('+proj=longlat'))
usa_sp = spTransform(usa_sp, CRS(my_proj))
usa_buffer = gBuffer(usa_sp, byid=F, width=20) # Use a buffer 20km wider than outline
inUS = over(camps_eqd, usa_buffer) # Check which camps in this polygon
camps_eqd = subset(camps_sp, !is.na(inUS))
camps_sp = subset(camps_sp, !is.na(inUS))
camps = subset(camps, !is.na(inUS))
plot(camps_sp)

# Load map of interstates: from http://www.nws.noaa.gov/geodata/catalog/transportation/html/interst.htm
roads = readOGR('interstates','in101503')
proj4string(roads) = CRS('+proj=longlat')
roads_eqd = spTransform(roads, CRS(my_proj))

## Calculate number of campgrounds within 25km of each interstate

# A function that finds all camps within a certain distance (rad) of a point (pt)
# rad : distance to search (in km)
# pt : c(lon, lat)
# dat : spatial data in lon-lat with camps
find_camps = function(pt, rad, dat){
	dists = spDistsN1(coordinates(dat), pt, longlat=T)
	dat[dists<rad,]
}

# Go through each line segment and calculate the number of camps within 25 km of each point that defines the segment
nlines = length(roads@lines)

ncamps = sapply(1:nlines, function(i){
	this_line = roads@lines[[i]]

	# Find midpoints of line segments
	these_points = coordinates(this_line)[[1]]
	npoints = nrow(these_points)
	if(npoints>1){
		mid_points = these_points[1:(npoints-1),] + (these_points[2:npoints,] - these_points[1:(npoints-1),])/2 
		mid_points = matrix(mid_points, ncol=2)
	} else {
		mid_points = these_points
	}
	# Search for camps within 25 km of midpoints
	found_camps = apply(mid_points, 1, function(pt) find_camps(pt, 25, camps_sp))
	sapply(found_camps, nrow)
})

# Create spatial lines data frame for plotting
line_list = sapply(1:nlines, function(i){
	this_line = roads@lines[[i]]
	these_points = coordinates(this_line)[[1]]
	linelist = sapply(1:(nrow(these_points)-1), function(j) Lines(Line(these_points[j:(j+1),]), paste(i,j,sep='_')))	
})

sl = SpatialLines(unlist(line_list, recursive=F), CRS('+proj=longlat'))
df = data.frame(ncamps=unlist(ncamps))
sldf = SpatialLinesDataFrame(sl, df, match.ID=F )
sldf_eqd = spTransform(sldf, CRS(my_proj))

## Create map
library(lattice)

# Read in better outline of North America
nam = readOGR('C:/Users/jrcoyle/Documents/UNC/GIS shape files/N Am Outline','na_base_Lambert_Azimuthal')
nam_eqd = spTransform(nam, CRS(my_proj))

# Define colors and cuts
mycol = read.csv('C:/Users/jrcoyle/Documents/UNC/Projects/blue2red_10colramp.txt')
mycol = apply(mycol,1,function(x) rgb(x[1],x[2],x[3],maxColorValue=256))
mycol = mycol[10:1]
use_col = c('#000000', colorRampPalette(c(mycol))(5))
use_col = c('#000000','#640603','#820D07','#B93712','#D7531A','#F37323')
use_col = c('#000000', colorRampPalette(c('darkred','orange'))(5))
colcuts = c(0,1,2, 5, 10, 20, max(df$ncamps))
sldf_eqd$ncamps_fact = cut(sldf_eqd$ncamps, colcuts, include.lowest=T, right=F)


# Define plot window and clip data
bbox_ll = SpatialPoints(matrix(c(-125, -65, 22, 48), nrow=2), CRS('+proj=longlat'))
bbox_eqd = spTransform(bbox_ll, CRS(my_proj))

pdf('campsites_25km.pdf', height=6, width=9)
trellis.par.set(axis.line=list(col=NA))
spplot(sldf_eqd, 'ncamps_fact', panel=function(x,y,subscripts,...){
		sp.polygons(nam_eqd, fill='grey80', col='white', lwd=1)
		sp.points(camps_eqd, pch=2, col='grey30', cex=.5)
		panel.polygonsplot(x,y,subscripts,...)
	}, lwd=3, col.regions=use_col, 
	colorkey=list(labels=list(labels=c('0','1','2-4','5-9','10-19','20-34'), cex=1), height=.3),
	xlim=coordinates(bbox_eqd)[,1], ylim=coordinates(bbox_eqd)[,2]
)
dev.off()


### Find campgrounds between two locations and summarize their desirability ###

# Define location from which to search
my_loc = c(-122.508545,37.762216)
dest_loc = c(-123.264885,44.578915)
stop_loc = c(-122.581029,41.847379)

# Find distance between my_loc and destination without stopover
dist_query = paste0(goog_api_loc, 'origins=', my_loc[2], ',', my_loc[1], 
	'&destinations=', dest_loc[2], ',', dest_loc[1],  
	'&key=', goog_api_key)
dists = fromJSON(dist_query)
straight_dist = dists$rows$elements[[1]][1]$distance$value
straight_time = dists$rows$elements[[1]][2]$duration$value

# Define maximum search distance
maxD = 100

# Find potential campgrounds within max distance
cg = find_camps(stop_loc, maxD, camps_sp)

results = data.frame()

# For each campground, calculate distances between campground, origin and destination
for(i in 1:nrow(cg)){
	cg_loc = coordinates(cg)[i,]
	dist_query = paste0(goog_api_loc, 'origins=', my_loc[2], ',', my_loc[1], '|', 
		dest_loc[2], ',', dest_loc[1], '&destinations=', cg_loc[2], ',', cg_loc[1], 
		'&key=', goog_api_key)
	dists = fromJSON(dist_query)
	DandT = t(sapply(1:2, function(i){
		c(dists$rows$elements[[i]][1]$distance$value, dists$rows$elements[[i]][2]$duration$value)
	}))

	new_row = data.frame(distTo=DandT[1,1], timeTo=DandT[1,2], distFrom=DandT[2,1], timeFrom=DandT[2,2])
	results = rbind(results, new_row)
}

cg = cbind(cg, results)

# Calculate time and distance added to trip by staying at campground
cg$distAdd = cg$distTo + cg$distFrom - straight_dist
cg$timeAdd = cg$timeTo + cg$timeFrom - straight_time

## Plot campgrounds on map colored by time and distance added to travel

colorby = factor(cg$contractType)
use_col = c('cornflowerblue','orange')

pdf('campgrounds_homeToinlaws.pdf', height=4, width=4)
par(mar=c(4,4,1,1))
plot(I(timeAdd/60) ~ I(distTo/1000), data=cg, pch=21, bg=use_col[colorby], 
	las=1, xlab='Distance from home (km)', ylab='Time added to travel (min)') 
legend('topleft', levels(colorby), pch=16, col=use_col, bty='n')
legend('topleft', c('',''), pch=1, bty='n')
dev.off()



