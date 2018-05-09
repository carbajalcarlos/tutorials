###############################################
#### R code for the analysis in Session 4 - JSA
###############################################
## Read in the data and fix the rownames to the datazone codes
dat <- read.csv(file="Scotland data.csv")
rownames(dat) <- dat$DZ
dat$DZ <- NULL


## Read in the shapefile
library(shapefiles)
shp <- read.shp(shp.name = "datazone.shp")
dbf <- read.dbf(dbf.name = "datazone.dbf")


#### Subset to Glasgow
gla <- dat[dat$HB=="Greater Glasgow & Clyde" & dat$urban==1, ]


#### Merge the data to create the spatialPolygonsDataFrame object
library(CARBayes)
library(sp)
sp.gla <- combine.data.shapefile(data=gla, shp=shp, dbf=dbf)
class(sp.gla)


#### Add the JSA proportions to the spatial data object
sp.gla@data$propJSA2011 <- sp.gla@data$JSA2011 / sp.gla@data$workpop2011


#### Create the spatial objects
library(spdep)
W.nb <- poly2nb(sp.gla, row.names = rownames(sp.gla@data))
W <- nb2mat(W.nb, style = "B")
W.list <- nb2listw(W.nb, style = "B")


#### Compute the Moran's I statistic
W.list <- nb2listw(W.nb, style = "B")
moran.mc(x = sp.gla@data$propJSA2011, listw = W.list, nsim = 10000)


#### Run the model
model1 <- S.CARleroux(formula=JSA2011~1, family="binomial", data=sp.gla@data, 
                      trials=sp.gla@data$workpop2011, W=W, burnin=10000, n.sample=60000, thin=50,
                      verbose=TRUE)
print(model1)


#### Output summaries
summary(model1)
summary(model1$samples)


#### Convergence checking
plot(model1$samples$rho)
plot(model1$samples$phi[ ,457])


#### Add the fitted proportions to the spatial data object
sp.gla@data$fitted.prop <- model1$fitted.values / sp.gla@data$workpop2011


#### fitted vs raw proportions plot
library(ggplot2)
ggplot(sp.gla@data, aes(x=fitted.prop, y=propJSA2011)) + 
    geom_point(colour="blue", size=0.5) + 
    coord_equal() + 
    xlab("Fitted proportion") + 
    ylab("Raw proportion") + 
    ggtitle("Comparison of JSA  proportions") + 
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
ggplot(data = sp.gla2, aes(x=long, y=lat, goup=group, fill = c(fitted.prop))) + 
    geom_polygon() + 
    coord_equal() + 
    xlab("Easting (m)") + 
    ylab("Northing (m)") + 
    labs(title = "Estimated JSA proportion in 2011", fill = "Proportion") +  
    theme(title = element_text(size=16)) + 
    scale_fill_gradientn(colors=brewer.pal(n=9, name="Reds")) +
    north(sp.gla2, location="topright", symbol=16) + 
    scalebar(sp.gla2, dist=5)



#### Compute the distributon for D
D.dist <- rep(NA, 1000)
    for(i in 1:1000)
    {
    #### Compute the dissimilarity index
    theta.all <- sum(model1$samples$fitted[i, ]) / sum(sp.gla@data$workpop2011)
    theta <- model1$samples$fitted[i, ] / sp.gla@data$workpop2011
    D <- sum(sp.gla@data$workpop2011 * abs(theta - theta.all)) / 
        (2 * sum(sp.gla@data$workpop2011) * theta.all * (1-theta.all))
    D.dist[i] <- D      
    }


#### Plot the distribution
indices.dist <- data.frame(D=D.dist)
ggplot(indices.dist, aes(x=D)) + 
    geom_density(aes(fill="red")) + 
    xlab("D") + 
    ylab("Density") + 
    labs(title = "Distribution of the D") +  
    theme(title = element_text(size=16), legend.position = "none")


#### Compute the estimate and 95% credible interval
quantile(indices.dist$D, c(0.5, 0.025, 0.975)) 
