#################################################
#### R code for the analysis in Session 1 - Price
#################################################
#### Read in Scotland data - remember to set the working directory first
setwd("D:/code/r/tutorials/qstep-spatial_analysis")

dat <- read.csv(file="housedata.csv")
head(dat)


#### Read in the shapefiles
library(shapefiles)
shp <- read.shp(shp.name = "IG.shp")
dbf <- read.dbf(dbf.name = "IG.dbf")


#### Make the rownames of gla the IG column
rownames(dat) <- dat$IG
dat$IG <- NULL
head(dat)


#### Check on the dbf elements
head(dbf$dbf)


#### Merge the data to create the spatialPolygonsDataFrame object
library(CARBayes)
library(sp)
sp.gla <- combine.data.shapefile(data=dat, shp=shp, dbf=dbf)
class(sp.gla)


#### Plot the spatial data object 
plot(sp.gla)


#### View the data.frame within the spatial object
head(sp.gla@data)


#### Add the log price variable to the spatial data set
sp.gla@data$logprice <- log(sp.gla@data$price)



#### Create the W matrix
library(spdep)
W.nb <- poly2nb(sp.gla, row.names = rownames(sp.gla@data))
W <- nb2mat(W.nb, style = "B")
class(W)
dim(W)


#### Compute the Moran's I statistic
W.list <- nb2listw(W.nb, style = "B")
moran.mc(x = sp.gla@data$logprice, listw = W.list, nsim = 10000)


#### Preliminary steps in using ggplot2
library(ggplot2)
library(rgeos)
library(maptools)
sp.gla@data$id <- rownames(sp.gla@data)
temp1 <- fortify(sp.gla, region = "id")
sp.gla2 <- merge(temp1, sp.gla@data, by = "id")


#### Create a map
library(RColorBrewer)
library(ggsn)
ggplot(data = sp.gla2, aes(x=long, y=lat, goup=group, fill = c(logprice))) + 
    geom_polygon() + 
    coord_equal() + 
    xlab("Easting (m)") + 
    ylab("Northing (m)") + 
    labs(title = "Log property price in 2008", fill = "Log price") +  
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
    geom_polygon(data=sp.gla4, aes(x=long, y=lat, group=group, fill=c(logprice)), alpha=0.8) +
    xlab("Longitude") + 
    ylab("Latitude") + 
    labs(title = "Log property price in 2008", fill = "Log price") +    
    theme(title = element_text(size=20)) + 
    scale_fill_gradientn(colors=brewer.pal(n=9, name="PuBuGn"))


