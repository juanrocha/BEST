### BEST Analysis
# This script explores the data of the BEST project: games with thresholds in 4 fishing communitites in the Colombian Caribeean coast. The first part summarizes and plot the data in different ways. The second takes ideas from StrategicNetworks.R to explore if is possible to understand strategic behaviour of humans from a data oriented prespective.
# Raw data was processed with script ExtractDataBEST.R

# by Juan Carlos Rocha
# juan.rocha@su.se
# version March 16, 2016

# clean and load weapons:
rm(list=ls())

require(ggplot2)
require(network)
require(sna)
require(dplyr)
require(RColorBrewer)
require(GGally)
library(vegan)
library(cluster)
library(NbClust);library(kohonen)
library(mclust); library(clValid)

setwd("~/Dropbox/BEST/Colombia/0_Game data") # here is the data
dat <- read.csv(file="~/Dropbox/BEST/Colombia/0_Game data/full_data_long.csv", row.names=1) # in long format, short format also available

# set directory for figures
setwd('~/Documents/Projects/BEST - Beijer/Figs & results')

## Preliminary data exploration

names(dat)
str(dat)
summary(dat)

# Correct & unify place, treatment names
levels(dat$Place)[3] <- "Las Flores"
levels(dat$Treatment)[c(1,3)] <- 'Uncertainty'
levels(dat$Treatment)[c(2,7)] <- 'Risk'
levels(dat$Treatment)[c(3,5)] <- 'Base line'
levels(dat$Treatment)[c(4,5)] <- 'Threshold'

dat$part <- dat$Round >6

# Time series

g <- ggplot(data=dat, aes(x=Round, y=NewStockSize)) + geom_line(stat='smooth', aes(color=Place, group=Place))
g + facet_grid(Treatment ~ part)

g2 <- g + stat_summary(fun.data='mean_cl_boot', geom='smooth') + ggtitle('Treatments in Colombia \n Second part of the game') + theme(text= element_text(family='Helvetica', size=10))  # working but with lots of warnings.

# Matrix of treatment per place, smooth over player decisions

g <- ggplot(dat=dat, aes(x=Round, y=value)) + 
		#geom_line(aes(group=ID_player, alpha=0.1)) + 
		stat_summary(fun.data='mean_cl_boot', geom='smooth') +
		facet_grid(Treatment ~ Place) 

	#should be equivalent but is not. Prefer the one above.
g <- ggplot(dat=dat, aes(x=Round, y=value)) + 
		geom_smooth(stat='smooth', method='loess', span=0.2) +
		facet_grid(Treatment ~ Place)
#quartz.save(file='MeanExtractionSmooth.png', type='png', dpi=100)

## All players, all places, all treatments... not very informative
g <- ggplot(dat=dat, aes(x=Round, y=value)) + 
		geom_line(stat='identity', aes(color=Player, group=ID_player)) +
		facet_grid(Treatment ~ Place)

## Only players of one place, per day 
g <- ggplot(dat=subset(dat, dat$Place == 'Las Flores' ), aes(x=Round, y=value), group=Player) + 
		geom_line(stat='identity', aes(color=Player)) +
		geom_point(stat='identity', aes(color=Player), alpha=0.5)+
		#geom_jitter(stat='identity', aes(color=Player), alpha=0.5)+
		facet_grid(Treatment ~ Date)

### Phase reconstruction
# one player
datP1 <- subset(dat, dat$Place == 'Las Flores' & dat$Session == 'tarde' & dat$Player == 'P1' & dat$Date == '30216' & dat$Treatment == 'Uncertainty')

data <- data.frame(x1=datP1$value[1:15], x2=datP1$value[2:16], round=datP1$Round[2:16])

p <- ggplot(data=data, aes(x1, x2, color=round))+ geom_path()


## very annoying if one wants to recover each path on x[i] * x[i+1] plane
# Now let's try x[i] * y[i]
datP1 <- subset(dat, dat$Place == 'Las Flores' & dat$Session == 'tarde' & dat$Date == '30216' )

p <- ggplot(data=datP1, aes(x=value, y=StockSizeBegining), group=Player) + 
		geom_path(aes(color=Round)) + facet_grid( Player ~ Treatment) +
		ggtitle('Phase space trajectory per player\n Las Flores') + 
		theme(text= element_text(family='Helvetica', size=8))
#quartz.save(file='PhaseTrajectoryLasFlores.png', type='png', dpi=100)

datP1 <- subset(dat, dat$Place == 'Taganga'  )

## Now instead of paths for trajectories use points with different alpha values to represent time and colours to represent people. Note that as the dataset is constructucted, one person (unique player) is a combination of session & date levels.
# force it using character features

	# Extract the strings
	# tr <- as.character(datP1$Treatment)
	d <- as.character(datP1$Date)
	s <- as.character(datP1$Session)
	# p <- as.character(datP1$Player)
	
	# create master string
	ms <- paste(d,s, sep="_")

	# return the vector to the dataset as factor
	datP1$Group <- as.factor(ms)

p <- ggplot(data=datP1, aes(x=value, y=NewStockSize), group=Player) + 
		geom_point(aes(color=Player, alpha=Round/16)) + 
		geom_path(aes(color=Player, alpha=0.5)) +
		facet_grid( Group ~ Treatment) +
		ggtitle('Phase space trajectory per player\n Taganga') + 
		theme(text= element_text(family='Helvetica', size=8))

p
# quartz.save(file='PhaseSpaceTrajectories_Buenavista.png', type='png', dpi=100)		

### How StockSize influence the decision in value

g <- ggplot(dat, aes(x=value, y=StockSizeBegining,color=Treatment )) +
		geom_point() +
		geom_smooth( method='loess') +
		ggtitle('Local polynomial regression on Stock size vs Decision')

# quartz.save(file='loess_StockSize_Decision.png', type='png')



## Make a first approximation to the stability landscape diagrams 

p <- ggplot(data=datP1, aes(x=value, y=NewStockSize), group=Group) + 
		geom_point(aes(color=Group, alpha=0.5)) + 
		facet_grid( . ~ Treatment) +
		ggtitle('Phase space per group \n Taganga') + 
		theme(text= element_text(family='Helvetica', size=8))

library(plot3D)



pm <- ggpairs(data=dat, columns=c('StockSizeBegining', 'NewStockSize', 'Treatment','Place'), mapping=aes(color= Treatment, alpha=0.5)) + theme(text= element_text(family='Helvetica', size=8))

ggpairs(data=dat, columns=c('StockSizeBegining', 'NewStockSize', 'value'), mapping=aes(color= Place, alpha=0.5))

ggpairs(data=dat, columns=c('Treatment', 'value', 'Place'), mapping=aes(color= Place))

pm <- ggpairs(data=subset(dat, dat$part == T), columns=c('StockSizeBegining', 'NewStockSize', 'Treatment','Place'), upper= list(continuous='density'), lower=list(continuous='points'), mapping=aes(color= Place, alpha=0.5), title='Color by treatment, second part') + theme(text= element_text(family='Helvetica', size=8)) 

# quartz.save('GeneralSummary_ColTreatment_2part.png', type='png')


## Contours works fine for stability landscapes for now.

c <- ggplot(data=dat, aes(y=NewStockSize, x=StockSizeBegining), group=Session) + geom_density_2d(aes(color=part)) + facet_grid( Treatment ~ Place)

# quartz.save('GeneralSummary_contours_treatment.png', type='png')

## Create a string for the level group as you did before but for the complete dataset

	d <- as.character(dat$Date)
	s <- as.character(dat$Session)
	# create master string
	ms <- paste(d,s, sep="_")

	# return the vector to the dataset as factor
	dat$Group <- as.factor(ms)


c <- ggplot(data=dat, aes(y=NewStockSize, x=StockSizeBegining), group=Session) + stat_density_2d(aes(color=Group, alpha=0.5), n=100, h=20) + facet_grid( Treatment ~ Place)

# quartz.save('GeneralSummary_contours_groups.png', type='png')

### Strategies: can you identify players strategies?
# similar as before but now to create a players ID
	tr <- as.character(dat$Treatment)
	d <- as.character(dat$Date)
	s <- as.character(dat$Session)
	p <- as.character(dat$Player)
	
	# create master string
	ms <- paste(d,s,tr,p, sep="_") # there should be a string per player

	# return the vector to the dataset as factor
	dat$ID_player <- as.factor(ms)

players <- (reshape::cast(dat, ID_player ~ Round))[,-1] #delete playersID
place <- reshape::cast(dat, ID_player ~  Place)[,-1] # 16 because calculate length of 'value'
group <- reshape::cast(dat, ID_player ~  Group)[,-1]
treat <- reshape::cast(dat, ID_player ~  Treatment)[,-1]
context <- cbind(place,treat) # don't use group yet, maybe for aes

## PCA & MDS: running pca or mds creates an error because of missing values. An alternative to deal with them is setting NA to zeroes and adding a column on the context dataset indicating that there were missing values. Missing values on the dataset means collapse of the resource (except for a group on the first dat)


# Identify where the NA are
	play.collapse <- apply(players, 1, anyNA)

# deal with NA
	players [is.na(players)] <- 0
	anyNA(players) #checking, should be false now
context <- cbind(context,play.collapse)

pca <- rda(players, context) #, context
	plot(pca, col= levelCols[as.vector(fitSOM$unit.classif)])

# note: I ran mds with 'euclidean' distance and 'manhattan'. Euclidean gave an error saying there is not enough data. manhattan calculated but didn't reach convergence. Morisita, jaccard, kulczynski, horn, rau, don't reach convergence neither. Frequent error on different measures: Stress is (nearly) zero - you may have insufficient data. It does work however if I use the transpose. It calculates the ordering of rounds on mds$points, but the ordering of players can be found too at mds$species
mds <- metaMDS(players, dist= 'manhattan', trymax=400, autotransform=F, k=2)
# mds2 <- metaMDS(players, dist= 'mahalanobis', trymax=1000, autotransform=F, k=3, previous.best = mds) # solution from http://stackoverflow.com/questions/14434794/no-stable-solution-using-metamds-in-vegan
ef1 <- envfit(mds, context, permu=999)
stressplot(mds)
	plot(mds, type='p', display=c('sites', 'species'), cex=0.8)	
	plot(ef1, p.max=0.05, col='blue', cex=0.8)

## PCA is working, MDS is not

# Is there clusters on the players?
## Validating number of clusters
## Internal validation
intern <- clValid(obj=as.matrix(players), nClust=2:9, clMethods=c('hierarchical', 'kmeans', 'diana', 'som', 'pam', 'sota', 'clara', 'model', 'fanny','agnes'), validation='internal')

## Stability validation
stab <- clValid(obj=as.matrix(players), nClust=2:6, clMethods=c('hierarchical', 'kmeans', 'diana', 'som', 'pam', 'sota', 'clara', 'model', 'fanny','agnes'), validation='stability')

summary(intern) # 2 clusters, hierarchical
summary(stab) # 2 with hierarchical, 6 with som

# Fit 6 clusters with som

fitSOM <- som(as.matrix(players), grid= somgrid(3,2,'hexagonal'))
levelCols <- brewer.pal(6, "Set1")

## Nonesense ... 
quartz()
plot(pca, type='none', main= 'Clusters with SOM')
points(pca, cex=1, lwd=1, col= levelCols[as.vector(fitSOM$unit.classif)]) # this is the matrix of coordinates for the first 2PC pca$CCA$u[,c(1,2)]
ordihull(pca, groups=as.vector(fitSOM$unit.classif), label=T, cex=0.8, 
			col='purple', lty=1)



# Note to self: For combining matrix probabilities (transition prob matrix) see discussion here: https://www.physicsforums.com/threads/can-you-combinie-two-transition-probability-matrices.580126/


## Stats:
# Are distributions significantly different?
# quartz()
# g <- ggplot(dat, aes(value, group = Treatment)) +
		# stat_summary(fun.data='mean_cl_boot', geom='crossbar')
		# geom_crossbar(mapping = aes(group = Treatment))
	


wt <- wilcox.test(x=filter(dat, Treatment == 'Risk')$value, 
			y=filter(dat, Treatment == 'Threshold')$value)

kruskal.test( value ~ Treatment, data=dat)
kruskal.test( value ~ Place, data=dat)



















####
# Checking the database for survey data

require(gdata)

survey <- read.xls(xls='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.xlsx', sheet=1)

surv <- read.csv2(file='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.csv', header=T, na.strings = '.')

key <- read.xls(xls='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.xlsx', sheet=2)


# search for NA

nas <- apply(surv, 2, anyNA)

sum.na <- function (x){ sum (is.na(x))} 
nas.rows <- apply(surv[,nas], 2, sum.na)

sort(nas.rows, dec=T)

## which columsn are binary

bin <- which(key$Data.type == 'binary')

facs <- apply(surv[,bin], 2, as.factor)

apply(facs, 2, levels)

for (i in 1:length(bin)){
	 class(surv[,bin[i]]) <- 'factor'	
}

names(surv[,bin])



