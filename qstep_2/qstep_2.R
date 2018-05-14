#	Script for advanced R course

#---------------------------------------------------------------
#	A revision exercise
#---------------------------------------------------------------

# Read the data
path <- "http://www.stats.gla.ac.uk/~adrian/R-course/data"
connection <- url(paste(path, "centenarians.dat", sep = "/"))
centenarians <- read.table(connection)

# Loading the required packages
library(ggplot2)

# Plotting simple graph
qplot(x = Females90, y = Males90, data = centenarians,
      geom = "text", label = Region, size = Males + Females)

#--------------------------------------------------------------
#	Data management
#--------------------------------------------------------------

# Read the data
connection <- url(paste(path, "scottish-referendum.dat", sep = "/"))
ref <- read.table(connection)

library(dplyr)
filter(ref, Turnout > 85)
select(ref, Turnout, Voted.no)
arrange(ref, Turnout)
mutate(ref, Vote = Population * Turnout / 100)
group_by(ref, Turnout > 85)


#--------------------------------------------------------------
#	Maps and topographic data
#--------------------------------------------------------------

library(CARBayes)
library(CARBayesdata)

data(GGHB.IG)
data(pricedata)
missing.IG                <- setdiff(rownames(GGHB.IG@data), pricedata$IG)
missing.IG.row            <- which(missing.IG==rownames(GGHB.IG@data))
propertydata.spatial      <- GGHB.IG[-missing.IG.row, ]
propertydata.spatial@data <- data.frame(propertydata.spatial@data, pricedata)
propertydata              <- propertydata.spatial@data
head(propertydata)

plot(propertydata)

library(sp)
spplot(propertydata.spatial,"price",main="Median prices (in thousands)")

library(RColorBrewer)
display.brewer.all()
incol <- brewer.pal(n=9,name="GnBu")
spplot(propertydata.spatial,"crime",col.regions=incol,main="Crime rates (per 10000)",at=seq(0,max(propertydata.spatial$crime),length=9))

#Can we do a similar thing for a categorical variable?
#What palette  should we choose?
incol <- brewer.pal(n=4,name="Set1")
spplot(propertydata.spatial,"type",col.regions=incol,main="Prevalent property type")

#What if we want to choose our own colours?
levels(propertydata.spatial$type)
spplot(propertydata.spatial,"type",col.regions=c("blue","red","green","yellow"),main="Prevalent property type")


#--------------------------------------------------------------
#	Detailed control of graphics
#--------------------------------------------------------------

path <- "http://www.stats.gla.ac.uk/~adrian/R-course/data"
connection <- url(paste(path, "centenarians.dat", sep = "/"))
centenarians <- read.table(connection)

plot(Females90 ~ Males90, data = centenarians)

x <- centenarians$Males90
y <- centenarians$Females90
ch <- chull(x, y)
polygon(x[ch], y[ch], col = "grey", border = NA)
points(x, y)


#--------------------------------------------------------------
#	Maximum likelihood
#--------------------------------------------------------------

library(rpanel)

n <- length(aircond)
theta <- seq(0.005, 0.04, length = 50)
loglik <- n * log(theta) - theta * sum(aircond)
plot(theta, loglik, type = "l")

rp.likelihood("sum(log(dexp(data, theta)))", aircond, 0.005, 0.03)

rp.likelihood("sum(log(dgamma(data, theta[1], theta[2])))",
              aircond, c(0.3, 0.005), c(3, 0.06))


#---------------------------------------------------------------
#	Logistic regression
#---------------------------------------------------------------

library(ggplot2)

Conc   <- c(0.375, 0.75, 1.5, 3, 6, 12, 24)
Killed <- c(0, 1, 8, 11, 16, 18, 20)
N      <- rep(20, 7)

p <- Killed / N
qplot(Conc, p)
qplot(log(Conc), p)

x <- log(Conc)
model <- glm(cbind(Killed, N - Killed) ~ x, family = "binomial")
summary(model)

xgrid <- seq(min(x), max(x), length = 50)
fitted.curve <- predict(model, newdata = data.frame(x = xgrid), type = "response")
qplot(xgrid, fitted.curve, geom = "line")

p <- cbind(Killed, N - Killed)
library(rpanel)
rp.logistic(log(Conc), p)


#---------------------------------------------------------------
#	Models with random effects
#---------------------------------------------------------------

pname  <- url("http://www.stats.gla.ac.uk/~adrian/R-course/data/paste.dat")
paste <- read.table(pname, header = TRUE)   
paste$cask <- rep(1:3, 20)

library(ggplot2)
qplot(x = Strength, y = Batch, data = paste, col = factor(cask))
qplot(x = Strength, y = cask,  data = paste, facets = ~Batch)

library(nlme)
model <- lme(Strength ~ 1, random = ~1|Batch/cask, data = paste)
summary(model)
plot(model)
qqnorm(model)

qqnorm(model, ~ ranef(., level = 1))
qqnorm(model, ~ ranef(., level = 2))

intervals(model)


rname  <- url("http://www.stats.gla.ac.uk/~adrian/R-course/data/reading.dat")
d <- read.table(rname, header = TRUE)

library(ggplot2)
qplot(Age, Readatt, data = d)
qplot(Age, Readatt, data = d, facets = ~ School)
qplot(Age, Readatt, data = d, facets = ~ School, group = Pupil, geom = "line")
qplot(Age, Readatt, data = d, facets = ~ School, group = Pupil, geom = "line", col = Sex)
qplot(Age, Readatt, data = d, facets = ~ School, group = Pupil, geom = "line", col = Race)
qplot(Age, Readatt, data = d, facets = Race ~ Sex)
qplot(Age, Readatt, data = d, facets = ~ Race, col = Sex)

model1 <- lme(Readatt ~ Age, random = ~1 | School/Pupil, data = d)
plot(model1)

model2 <- lme(Readatt ~ Age, random = ~Age | School/Pupil, data = d)
plot(model2)
anova(model1, model2)

model3 <- lme(Readatt ~ Age * Sex * Race, random = ~Age | School/Pupil, data = d)
plot(model3)
summary(model3)

model4 <- lme(Readatt ~ Age + Sex + Race, random = ~Age | School/Pupil, data = d)
plot(model4)
summary(model4)

anova(model3, model4)


#-----------------------------------------------------------------
#	Flexible regression
#-----------------------------------------------------------------

library(sm)
plot(Score1 ~ Longitude, data = trawl)
model <- lm(Score1 ~ Longitude, data = trawl)
abline(model)

sm.regression(trawl$Longitude, trawl$Score1)

Zone93   <- (trawl$Year == 1 & trawl$Zone == 1)
Position <- cbind(trawl$Latitude, Longitude = -trawl$Longitude)
sm.regression(Position[Zone93,], trawl$Score1[Zone93])


#-------------------------------------------------------------
#	Simulation and coding
#-------------------------------------------------------------

y  <- rnorm(40)
qq <- qqnorm(y)
cor(qq$x, qq$y)

n      <- 40
nsim   <- 1000
qq.vec <- rep(0, nsim)
for (i in 1:nsim) {
  y  <- rnorm(n)
  qq <- qqnorm(y, plot.it = FALSE)
  qq.vec[i] <- cor(qq$x, qq$y)
}
hist(qq.vec)

qq.obs <- 0.973
abline(v = qq.obs, lty = 2, col = "red")
length(qq.vec[qq.vec < qq.obs]) / length(qq.vec)

e   <- 1
x   <- 0
inc <- 1
while (inc > 0.00001) {
  x    <- x + 1
  inc <- inc / x
  e    <- e + inc
}
e

y <- 3
if (y < 10) a <- "Small"
else a <- "Not so small"
a

qq.pvalue <- function(x, nsim = 1000) {
  n      <- length(x)
  qq.vec <- rep(0, nsim)
  for (i in 1:nsim) {
    y  <- rnorm(n)
    qq <- qqnorm(y, plot.it = FALSE)
    qq.vec[i] <- cor(qq$x, qq$y)
  }
  hist(qq.vec)
  qq     <- qqnorm(x, plot.it = FALSE)
  qq.obs <- cor(qq$x, qq$y)
  abline(v = qq.obs, lty = 2, col = "red")
  length(qq.vec[qq.vec < qq.obs]) / length(qq.vec)
}

x <- rnorm(50)
qq.pvalue(x)
qq.pvalue(x, nsim = 5000)


cor.calc <- function(y) {
  qq <- qqnorm(y, plot.it = FALSE)
  cor(qq$x, qq$y)
}
Y <- matrix(rnorm(n * nsim), nrow = nsim)
qq.vec <- apply(Y, 1, cor.calc)
hist(qq.vec)