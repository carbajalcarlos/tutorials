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


#### Merge the data to create the spatialPolygonsDataFrame object
library(CARBayes)
library(sp)
sp.gla <- combine.data.shapefile(data=dat, shp=shp, dbf=dbf)
class(sp.gla)


#### Add the log price variable to the spatial data set
sp.gla@data$logprice <- log(sp.gla@data$price)


#### Create the W matrix
library(spdep)
W.nb <- poly2nb(sp.gla, row.names = rownames(sp.gla@data))
W <- nb2mat(W.nb, style = "B")


#### Fit a non-spatial model to check for residual correlation
formula <- logprice ~ crime + rooms + sales + driveshop + factor(type)
model0 <- lm(formula, data=sp.gla@data)
summary(model0)
anova(model0)

#### Fit a non-spatial model to check for residual correlation
formula <- logprice ~ rooms + sales + driveshop + factor(type)
model0 <- lm(formula, data=sp.gla@data)
summary(model0)
anova(model0)

#### Compute the Moran's I statistic for the residuals from model0
W.list <- nb2listw(W.nb, style = "B")
moran.mc(x = residuals(model0), listw = W.list, nsim = 10000)


#### Run the spatial model
model1 <- S.CARleroux(formula=formula, family="gaussian", data=sp.gla@data, 
                      W=W, burnin=10000, n.sample=60000, thin=50, verbose=TRUE)
print(model1)


#### Convergence checking
plot(model1$samples$rho)
plot(model1$samples$phi[ ,123])


#### Covariate effects
model1$summary.results[2:8, 1:3]


#### Add the fitted prices to the spatial data object
sp.gla@data$fitted.price <- exp(model1$fitted.values)


#### fitted vs raw prices plot
library(ggplot2)
ggplot(sp.gla@data, aes(x=log(fitted.price), y=log(price))) + 
    geom_point(colour="blue", size=0.5) + 
    coord_equal() + 
    xlab("Fitted price") + 
    ylab("Raw price") + 
    geom_abline(slope=1, intercept=0, color="red") + 
    theme(text=element_text(face="bold", size=12))


#### Create the modified spatial data object
library(rgeos)
library(maptools)
sp.gla@data$id <- rownames(sp.gla@data)
temp1 <- fortify(sp.gla, region = "id")
sp.gla2 <- merge(temp1, sp.gla@data, by = "id")


#### Map the fitted probabilities and highight the outlier
library(ggsn)
library(RColorBrewer)
ggplot(data = sp.gla2, aes(x=long, y=lat, goup=group, fill = c(logprice))) + 
    geom_polygon() + 
    coord_equal() + 
    xlab("Easting (m)") + 
    ylab("Northing (m)") + 
    labs(title = "Estimated price in 2008", fill = "Log price") +  
    theme(title = element_text(size=20)) + 
    scale_fill_gradientn(colors=brewer.pal(n=9, name="Reds")) +
    north(sp.gla2, location="topright", symbol=16) + 
    scalebar(sp.gla2, dist=5)








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
    scale_fill_gradientn(colors=brewer.pal(n=9, name="Reds")) + 
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
    scale_fill_gradientn(colors=brewer.pal(n=9, name="Blues"))


