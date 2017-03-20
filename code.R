############################### SPECIES OCCURENCE ###############################
library(rgbif)

# count number of occurences by taxonomy code
# look up code at http://www.gbif.org/species
occ_count(taxonKey=3106632, georeferenced=TRUE)

# get occurence data
occurences <- occ_search(taxonKey=3106632, limit=2000, return='data')

# locations of occurences
locations <- cbind(occurences$decimalLatitude, occurences$decimalLongitude)
# remove missing values
locations <- locations[!is.na(locations[, 1]), ]

# inside california
locations <- locations[locations[, 2] > -120 & locations[ ,2] < -114, ]
locations <- locations[locations[, 1] > 32.4, ]

library(ggplot2)
library(ggmap)
library(maps)

# map of california
states <- map_data('state')
california <- states[states$region == 'california', ]

# mediterranean grass
grass <- data.frame(lat=locations[, 1], long=locations[, 2])

# plot mediterranean grass occurences
ggplot(data=california) + 
  geom_polygon(aes(x=long, y=lat), fill='orange', color='red') +
  geom_point(data=grass, aes(x=long, y=lat))

############################### SHAPEFILE ###############################
library(rgdal)
library(sp)
library(ggmap)
# folder that contains shapefile
setwd('/Users/linggeli/Downloads/CAPD')
# readOGR in rgdal to read shapefile
# inputs are directory and name without extension
CAPD <- readOGR('.', 'CPAD_2016b1_SuperUnits')
# project in WGS84 format
CAPD <- spTransform(CAPD, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
proj4string(CAPD) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# summary
summary(CAPD)

# state lands
state <- CAPD[CAPD$MNG_AG_LEV == 'State', ]
# fortify in ggmap to convert shapefile to dataframe
state <- fortify(state)
# reduce resolution by rounding
state.low <- unique(data.frame(long=round(state$long, 2), lat=round(state$lat, 2), group=state$group))

# plot state land areas
ggplot(data=state.low) +
  geom_polygon(aes(x=long, y=lat, group=group), colour='NA', fill='blue', alpha=0.5)

# state lands
federal <- CAPD[CAPD$MNG_AG_LEV == 'Federal', ]
# fortify in ggmap to convert shapefile to dataframe
federal <- fortify(federal)
# check out dataframe
head(federal)
# reduce resolution by rounding
federal.low <- unique(data.frame(long=round(federal$long, 2), lat=round(federal$lat, 2), group=federal$group))

# plot federal land areas
ggplot(data=federal.low) +
  geom_polygon(aes(x=long, y=lat, group=group), colour='NA', fill='green', alpha=0.5)

# google satellite map
cali <- get_googlemap(center=c(lon=-120, lat=37.5), maptype='satellite', zoom=6)
ggmap(cali)

# overlay map with federal land areas
ggmap(cali) + 
  geom_polygon(data=federal.low, aes(x=long, y=lat, group=group), colour='NA', fill='green', alpha=0.5)

# crop with scale
ggmap(cali) + 
  geom_polygon(data=federal.low, aes(x=long, y=lat, group=group), colour='NA', fill='green', alpha=0.5) +
  scale_x_continuous(limits=c(-124, -115)) 

# combine two dataframes
federal.low$level <- 'Federal'
state.low$level <- 'State'
protected <- rbind(federal.low, state.low)

# plot details
p <- ggmap(cali) + 
  geom_polygon(data=protected, aes(x=long, y=lat, group=group, fill=level), colour='NA', alpha=0.8) +
  annotate('text', x=-117.88, y=33.7, label='Irvine', colour='white', size=6) +
  labs(title='Protected Lands in California') + 
  theme(legend.position=c(0.2, 0.2)) +
  theme(plot.title=element_text(hjust=0.5))

############################### PRISM & RASTER ############################### 
library(prism)

options(prism.path='~/prismtmp')
get_prism_normals(type='tmean',resolution='800m', annual=T, keepZip=T)

ls_prism_data()

library(raster)

tmean <- raster('~/prismtmp/PRISM_tmean_30yr_normal_800mM2_annual_bil/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil')
plot(tmean)

# yosemite shapefile
yosemite <- CAPD[CAPD$PARK_NAME == 'Yosemite National Park', ]

plot(yosemite)

# use the same projection
crs(tmean) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 

# mask raster by yosemite shapefile
tmean_yosemite <- mask(tmean, yosemite)
# trim raster
tmean_yosemite <- trim(tmean_yosemite)

plot(tmean_yosemite, main="Average annual temperature")

# convert to regular dataframe
df <- data.frame(rasterToPoints(tmean_yosemite))
names(df)[1:3] <- c("lon", "lat", 'tmean')

ggplot()+
  geom_tile(data=df, aes(x=lon, y=lat, fill=tmean)) +
  scale_fill_gradient2(low='blue', mid='lightblue', high='green', midpoint=7)

############################### FEDDATA & RASTER ############################### 
library(FedData)

yosemitePolygon <- polygon_from_extent(raster::extent(-119.8875,-119.2042,37.49583,38.1875),
                                  proj4string="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

NED <- get_ned(template=yosemitePolygon, label="yosemite")

plot(NED)

# check raster resolution
res(NED)
# lower resolution to reduce size
NED <- aggregate(NED, fact=10)

crs(NED) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 

NED <- mask(NED, yosemite)
NED <- trim(NED)

NED.df <- data.frame(rasterToPoints(NED))

names(NED.df)[1:3] <- c("lon", "lat", 'elevation')

ggplot() +
  geom_tile(data=df, aes(x=lon, y=lat, fill=tmean)) +
  geom_contour(data=NED.df, aes(x=lon, y=lat, z=elevation), size=0.5, bins=10) +
  scale_fill_gradient2(low='blue', mid='lightblue', high='green', midpoint=7)
