#	Scripts for R course
#	09.05.16

#---------------------------------------------------------------
#	An example session: the Scottish referendum
#---------------------------------------------------------------

# Read the data
connection <- url("http://www.stats.gla.ac.uk/~adrian/R-course/data/scottish-referendum.dat")
ref <- read.table(connection, header = TRUE)

# See what's there
ref
ref$Unemployment.rate
ref$Turnout
hist(ref$Turnout) 
boxplot(ref$Voted.no)

# Calculate the number of voters in each region and the overall percentage of `no' voters
ref$Vote <- ref$Population * ref$Turnout / 100
sum(ref$Voted.no * ref$Vote) / sum(ref$Vote) 

# Plot unemployment rates against percentage who voted no.
plot(ref$Unemployment.rate, ref$Voted.no)

# Plot as above but points are replaced by Council names
plot(ref$Unemployment.rate, ref$Voted.no, type = "n")
text(ref$Unemployment.rate, ref$Voted.no, ref$Scottish.Council)   

# Look at the ggplot2 package
library(ggplot2)
# install.packages("ggplot2")

# Plot unemployment rates against percentage who voted no.
qplot(Unemployment.rate, Voted.no, data = ref)

# Plot as above but points are replaced by Council names and scaled by vote
qplot(Unemployment.rate, Voted.no, data = ref, 
      label = Scottish.Council, geom = "text", size = Vote / 1000)   

#--- this is my code

# Plot as above but points are replaced by Council names and scaled by vote
qplot(Aged.over.65, Voted.no, data = ref, 
      label = Scottish.Council, geom = "text", size = Vote / 1000)   

qplot(Scottish.identity.only, Voted.no, data = ref, 
      label = Scottish.Council, geom = "text", size = Vote / 1000)

qplot(Scottish.identity.only, Turnout, data = ref, 
      label = Scottish.Council, geom = "text", size = Vote / 1000)

#---

# Points scaled by size
qplot(Unemployment.rate, Voted.no, data = ref, size = Vote / 1000)

# Now try this with turnout
qplot(Unemployment.rate, Turnout, data = ref,
      label = Scottish.Council, geom = "text", size = Vote/1000)
qplot(Unemployment.rate, Turnout, data = ref, size = Vote / 1000)

# Scottish Identity Only
qplot(Scottish.identity.only, Voted.no, data = ref, size = Vote / 1000)
qplot(Scottish.identity.only, Turnout,  data = ref, size = Vote / 1000)

#---------------------------------------------------------------
#	More on modern graphics: conditioning and facets
#---------------------------------------------------------------

connection <- url("http://www.stats.gla.ac.uk/~adrian/R-course/data/reading.dat")
reading <- read.table(connection, header = TRUE)

qplot(Age, Readatt, facets = ~ Sex, data = reading)

#----
# Added code.

qplot(x = Age, y = Readatt, facets = ~ Sex + Race, data = reading)

#---------------------------------------------------------------
#	More on graphics: world health
#---------------------------------------------------------------

connection <- url("http://www.stats.gla.ac.uk/~adrian/R-course/data/world.dat")
world <- read.table(connection)

qplot(year, log(gdp),  data = world, geom = "boxplot", group = year)
qplot(year, life, data = world, geom = "boxplot", group = year)


world2007 <- subset(x = world, subset = year == 2007)

world1949 <- subset(x = world, subset = year == 1949)
world1950 <- subset(x = world, subset = year == 1950)
world1951 <- subset(x = world, subset = year == 1951)

sum(is.na(world1949$gdp))
sum(is.na(world1950$gdp))
sum(is.na(world1951$life))

qplot(x = gdp, y = life, data = world2007)
qplot(gdp, life, data = world2007, label = country, geom = "text")

world50 <- subset(world, year == 1957 | year == 2007)
qplot(gdp, life, data = world50, col = factor(year))
qplot(gdp, life, data = world50, col = factor(year), group = country) + geom_line(col = "grey")


#----
#Google example

rm(gdp)
install.packages("rpanel")
library(rpanel)
rp.bubbleplot(log(gdp), log(co2.emissions), 1960:2007, 
              size = population, col = life.expectancy,
              interpolate = TRUE)

#-------------------------------------------------------------
#	Multiple regression
#-------------------------------------------------------------

connection <- url("http://www.stats.gla.ac.uk/~adrian/R-course/data/cofe.dat")
cofe <- read.table(connection, header = TRUE)

pairs(cofe)
model <- lm(Giving ~ Employ + Attend, data = cofe)
anova(model)
summary(model)
drop1(model, test = "F")


#---------------------------------------------------------------
#	Categorical data: Titanic
#---------------------------------------------------------------
install.packages("vcd")
library(vcd)
mosaic(~ Sex, data = Titanic)
mosaic(~ Sex + Survived, data = Titanic)
mosaic(~ Sex + Age + Survived, data = Titanic)
mosaic(Titanic)


#---------------------------------------------------------------
#	Interactive graphics
#---------------------------------------------------------------

library(rpanel)
rp.regression(CofE$Employ, CofE$Giving)
install.packages("tkrplot")
