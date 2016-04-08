## Network analysis of Providence experimental games.
## Data was collected by Juan Camilo Cardenas and Daniel Castillo
## I used the data previously to model the dynamics of lobster fishery SES
## This script shows a different view of the analysis based on 'network strategies'
## written by Juan Rocha, juan.rocha@su.se
## September 2015

# clean work environment
rm(list=ls())

# required libraries
require(ggplot2)
require(network)
require(sna)
# require(igraph)


# load data
setwd('~/Documents/Projects/BEST - Beijer')
dat <- read.csv2(file='~/Documents/Projects/BEST - Beijer/ProvidenceDataJCC.csv', header=T)

## Preliminary visualizations

# typical time series per player
g <- ggplot(data=dat[dat$instit == '4H4L',] , aes(round, x, colour=jugador))
g2 <- g + geom_line() + facet_grid( group ~. ) + theme(text= element_text(family='Helvetica', size=10)) + ggtitle('Treatment 4H4L') 

# summary of all players for one treatment
g <- ggplot(data=dat[dat$instit == '4H4L',] , aes(round, x))
g3 <- g + stat_summary(fun.data='mean_cl_normal', geom='smooth') + ggtitle('Treatment 4H4L') + facet_grid( group ~. )+ theme(text= element_text(family='Helvetica', size=10))

# summary of all players by treatments
g <- ggplot(data=dat , aes(round, x))
g4 <- g + stat_summary(fun.data='mean_cl_normal', geom='smooth') + ggtitle('All') + facet_grid( group ~ instit )+ theme(text= element_text(family='Helvetica', size=7))


# summary of all treatments
g <- ggplot(data=dat , aes(round, x))
g5 <- g + stat_summary(fun.data='mean_cl_normal', geom='smooth') + ggtitle('Treatments in Providence, Colombia') + facet_grid( instit ~. )+ theme(text= element_text(family='Helvetica', size=10))

dat$period <- dat$round > 10

g6 <- ggplot(data=dat, aes(x), group=period) + geom_histogram(aes(y=..density..), binwidth=1)  + ggtitle('Treatments in Providence, Colombia') + facet_grid( instit ~ period)+ theme(text= element_text(family='Helvetica', size=10))


## Constructing strategic networks
# one player
n=4
datP1 <- dat[dat$jugador==levels(dat$jugador)[n],]

# phase reconstruction of strategic dynamics
data <- data.frame(x1=datP1$x[2:21], x2=datP1$x[3:22], round=datP1$round[3:22])
ggplot(data=data, aes(x1, x2, color=round))+ geom_path() #+ scale_fill_gradient2(low='green', mid='grey', high='red', #colours
#                       midpoint=10, name='Round')
# plot(x=datP1$x[2:21], y=datP1$x[3:22], type='l')

net <- network.initialize(n=9, dir=T)
network.vertex.names(net) <- 0:8
edges <- data[,1:2]
# weight <- table(dat[,1:2])

# this convert the edge list on a adjacency matrix with weights±!! nrows and ncols was calculated by looking at the dims of the table.
ad.mat <- matrix(table(edges), nrow=8, ncol=8, dimnames=list(dimnames(table(edges))$x1, dimnames(table(edges))$x2))

#complete the isolates
# ad.mat <- rbind(ad.mat, matrix(0,2,6)); rownames(ad.mat) <- 0:8
# ad.mat <- cbind(ad.mat, matrix(0,9,3)); colnames(ad.mat) <- 0:8

net <- network(ad.mat, dir=T, loops=T)
set.edge.value(net, attrname='count', value=ad.mat)

# quartz(w=4, h=4)
# par(bg="black", col="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
plot.network(net, 
			label= network.vertex.names(net),
			usearrows=T,
			mode='circle',
			# thresh=5,
			# attrname='mentions',
			displayisolates=T,
			interactive=F,
			vertex.cex= sna::degree(net, gmode='digraph'),
			edge.lwd=ad.mat,
			edge.col='grey20',
			edge.curve=0.1,
			vertex.col='gold',
			usecurve=T,
			uselen=T,
			edge.len=0.001,
			label.cex=1,
			label.col='black',
			vertex.border='red',
			vertex.lwd=0.3,
			label.pos=0
			 )
	title('Player 4')
			 
# with igraph
library(igraph)

## Now with Caro's data
list.files()
dat <- read.csv2(file="Caro_Lab_Data.csv", header=T)
dat <- dat[c(2,6,7,21,23:32)] # select only columns of interest

# time lines
g <- ggplot( data = dat[dat$treatmentDescr == 'T',], aes(indClaimRound, round), group=subject.)
g2 <- g + geom_line() + facet_grid( group ~. ) + theme(text= element_text(family='Helvetica', size=10)) + ggtitle('Treatment 4H4L') 


### preliminar analysis with Colombian BEST data

dat <- read.csv2(file="~/Dropbox/BEST/Colombia/0_Game data/DataOverview.csv", header = T)

names(dat)

# plot
g <- ggplot( data = dat , aes(x=Round, y=StockSizeAfterExtraction), group = Location) + geom_line(stat='smooth', aes(color = Location))

g  + facet_grid( Treatment ~ . )

# summary of all treatments
g <- ggplot(data=dat , aes(x=Round, y=StockSizeAfterExtraction))

g1 <- ggplot(data= subset(dat, dat$Round >0 & dat$Round<7) , aes(x=Round, y=StockSizeAfterExtraction))

g2 <- ggplot(data= subset(dat, dat$Round > 6 & dat$Round < 17) , aes(x=Round, y=StockSizeAfterExtraction))


g5 <- g2 + stat_summary(fun.data='mean_cl_boot', geom='smooth') + ggtitle('Treatments in Colombia \n Second part of the game') + facet_grid( Treatment ~ Location )+ theme(text= element_text(family='Helvetica', size=10)) 

# quartz.save(file='Col_Mean_SD_boot_2.pdf', type='pdf')


subset(dat, dat$Round )