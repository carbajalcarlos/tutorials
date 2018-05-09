###############################################
#### R code for the analysis in Session 1 - JSA
###############################################
#### Read in Scotland data - remember to set the working directory first
setwd("D:/code/r/tutorials/qstep-spatial_analysis")

dat <- read.csv(file="Scotland data.csv")
head(dat)


#### Subset to Glasgow
gla <- dat[dat$HB=="Greater Glasgow & Clyde" & dat$urban==1, ]


#### Read in the shapefiles
require(shapefiles, quietly = TRUE)
shp <- read.shp(shp.name = "datazone.shp")
dbf <- read.dbf(dbf.name = "datazone.dbf")


#### Make the rownames of gla the DZ column
rownames(gla) <- gla$DZ
gla$DZ <- NULL
head(gla)


#### Check on the dbf elements
head(dbf$dbf)


#### Merge the data to create the spatialPolygonsDataFrame object
library(CARBayes, quietly = TRUE)
library(sp, quietly = TRUE)
sp.gla <- combine.data.shapefile(data=gla, shp=shp, dbf=dbf)
class(sp.gla)


#### Plot the spatial data object 
plot(sp.gla)


#### View the data.frame within the spatial object
head(sp.gla@data)


#### Add the JSA proportions to the spatial data object for 2011.
sp.gla@data$propJSA2011 <- sp.gla@data$JSA2011 / sp.gla@data$workpop2011



#### Create the W matrix
require(spdep, quietly = TRUE)
W.nb <- poly2nb(sp.gla, row.names = rownames(sp.gla@data))
W <- nb2mat(W.nb, style = "B")
class(W)
dim(W)
W[1:10, 1:10]

#### Compute the Moran's I statistic
W.list <- nb2listw(W.nb, style = "B")
moran.mc(x = sp.gla@data$propJSA2011, listw = W.list, nsim = 10000)


#### Preliminary steps in using ggplot2
require(ggplot2, quietly = TRUE)
require(rgeos, quietly = TRUE)
require(maptools, quietly = TRUE)
sp.gla@data$id <- rownames(sp.gla@data)
temp1 <- fortify(sp.gla, region = "id")
sp.gla2 <- merge(temp1, sp.gla@data, by = "id")


#### Create a basic map
ggplot(data = sp.gla2, mapping = aes(x=long, y=lat, goup=group, fill = c(propJSA2011))) + 
    geom_polygon() + 
    coord_equal() + 
    xlab("Easting (m)") + 
    ylab("Northing (m)") + 
    labs(title = "JSA proportion in 2011", fill = "Proportion") +  
    theme(title = element_text(size=20))
    

library(RColorBrewer)
ggplot(data = sp.gla2, aes(x=long, y=lat, goup=group, fill = c(propJSA2011))) + 
    geom_polygon() + 
    coord_equal() + 
    xlab("Easting (m)") + 
    ylab("Northing (m)") + 
    labs(title = "JSA proportion in 2011", fill = "Proportion") +  
    theme(title = element_text(size=20)) + 
    scale_fill_gradientn(colors=brewer.pal(n=9, name="PuBuGn"))


#### Add a north arrow and scalebar
library(ggsn)
ggplot(data = sp.gla2, aes(x=long, y=lat, goup=group, fill = c(propJSA2011))) + 
    geom_polygon() + 
    coord_equal() + 
    xlab("Easting (m)") + 
    ylab("Northing (m)") + 
    labs(title = "JSA proportion in 2011", fill = "Proportion") +  
    theme(title = element_text(size=20)) + 
    scale_fill_gradientn(colors=brewer.pal(n=9, name="PuBuGn")) +
    north(sp.gla2, location="topright", symbol=16) + 
    scalebar(sp.gla2, dist=5)


#### Add a map underneath
## Create a new spatial data object with a long-lat coordinate system
library(rgdal)
sp.gla3 <- sp.gla
proj4string(sp.gla3) <- CRS("+init=epsg:27700")                   
sp.gla3 <- spTransform(sp.gla3, CRS("+init=epsg:4326"))  


## Turn the new spatial data object into a dataframe
sp.gla3@data$id <- rownames(sp.gla3@data)
temp1 <- fortify(sp.gla3, region = "id")
sp.gla4 <- merge(temp1, sp.gla3@data, by = "id")


## Define the centre and extent of the data set and get a map
library(ggmap)
extent <- bbox(sp.gla3)
centre <- apply(extent, 1,mean)
myMap <- get_map(location=centre, maptype="roadmap", zoom=10)


## Plot the map
ggmap(myMap) + 
    geom_polygon(data=sp.gla4, aes(x=long, y=lat, group=group, fill=c(propJSA2011)), alpha=0.9) +
    xlab("Longitude") + 
    ylab("Latitude") + 
    labs(title = "JSA proportion in 2011", fill = "Proportion") +  
    theme(title = element_text(size=20)) + 
    scale_fill_gradientn(colors=brewer.pal(n=9, name="PuBuGn"))  + 
    scale_x_continuous(limits = extent[1, ]) +
    scale_y_continuous(limits = extent[2, ])
    

