### BEST Analysis
# This script explores the data of the BEST project: games with thresholds in 4 
# fishing communitites in the Colombian Caribeean coast. The first part summarizes 
# and plot the data in different ways. The second takes ideas from StrategicNetworks.R
# to explore if is possible to understand strategic behaviour of humans from a 
# data oriented prespective.
# Raw data was processed with script ExtractDataBEST.R

# by Juan Carlos Rocha
# juan.rocha@su.se
# version March 16, 2016

# clean and load weapons:
rm(list=ls())

library(ggplot2)
# library(network)
# library(sna)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(GGally)
# library(vegan)
# library(cluster)
# library(NbClust);library(kohonen)
# library(mclust); library(clValid)
library(ineq) # for gini

setwd("~/Dropbox/BEST/Colombia/0_Game data") # here is the data
dat <- read.csv(file="~/Dropbox/BEST/Colombia/0_Game data/160427_corrected_full_data_long.csv", row.names=1) # in long format, short format also available

# set directory for figures
setwd('~/Documents/Projects/BEST - Beijer/Figs & results')

## Preliminary data exploration

names(dat)
str(dat)
summary(dat)

# Correct & unify place, treatment names
# levels(dat$Place)[3] <- "Las Flores"
# levels(dat$Treatment)[c(1,3)] <- 'Uncertainty'
# levels(dat$Treatment)[c(2,7)] <- 'Risk'
# levels(dat$Treatment)[c(3,5)] <- 'Base line'
# levels(dat$Treatment)[c(4,5)] <- 'Threshold'

# Correct date, session, player as factor
# dat$Date <- as.factor(dat$Date)
# levels(dat$Date) <- c('2016-02-09', '2016-02-01', '2016-02-02','2016-02-03','2016-02-04','2016-02-05','2016-02-10','2016-02-12') # standard dates
# levels(dat$Session) <- c('am','pm')
# levels(dat$Player) <- c('1','2','3','4')

# dat$part <- dat$Round > 6

# Create player ID's as in Surveys.R
dat <- transform (dat, ID_player = interaction(Date, Treatment, Session, Player, drop = TRUE))
# Create ID group
dat <- transform(dat, group = interaction (Date, Treatment, Session, drop=T))

# We need to make NA explicit: this is, rounds that were not played (as zeroes) because resource was collapsed
summary (dat)
dat2 <- dplyr::select(dat, -StockSizeBegining, -SumTotalCatch, -IntermediateStockSize, -Regeneration, -NewStockSize,-part) %>%
  spread(key=Round, value=value)

dat3 <- dplyr::select(dat2, 8:23)
dat3 <- as.matrix(dat3)
dat3[is.na(dat3)] <- 0
dat2[,8:23] <- dat3

dat3 <- dat2 %>%
  gather(Round, value, 8:23)
dat3$Round <- as.numeric(dat3$Round)

dat <- full_join(dat3, dat)
str(dat)
summary(dat)
dat <- gdata::drop.levels(dat)

dat.noNA <- dat
summary (dat.noNA)
dat.noNA$StockSizeBegining[is.na(dat.noNA$StockSizeBegining)] <-  0
dat.noNA$SumTotalCatch[is.na(dat.noNA$SumTotalCatch)] <-  0
dat.noNA$IntermediateStockSize[is.na(dat.noNA$IntermediateStockSize)] <-  0
dat.noNA$Regeneration[is.na(dat.noNA$Regeneration)] <-  0
dat.noNA$NewStockSize[is.na(dat.noNA$NewStockSize)] <-  0

dat <- dat.noNA
# write.csv(dat, file='160427_corrected_full_data_long.csv')

# check that ID's are equal in both datasets
# levels(dat$ID_player) %in% levels(surv.dat$ID_player)
# levels(surv.dat$ID_player) %in% levels(dat$ID_player)
# 
# # Now you can join them and use both datasets for stats!!
# full <- full_join(dat, surv.dat, by= c('ID_player' = 'ID_player', 'Session' = 'Session', 
#                                         'Date' = 'date', 'Place' = 'locationName', 'Round'='round'))
# str(full)

### Group level data
group_dat <- dat %>%
  select (Treatment, Place, group, Round, StockSizeBegining, IntermediateStockSize, 
          Regeneration, NewStockSize, part) %>%
  unique ()


# Time series

g <- ggplot(data=group_dat, aes(x=Round, y=NewStockSize)) 
+ geom_smooth(stat='smooth', aes(color=Place, group=Place))
g + facet_grid(Treatment ~ .)

g2 <- g + stat_summary(fun.data='mean_cl_boot', geom='smooth') 
        + ggtitle ('Treatments in Colombia \n Second part of the game') 
        + theme(text= element_text(family='Helvetica', size=10))  # working but with lots of warnings.

g <- ggplot(data= filter(group_dat, part == T), aes(x=Round, y=NewStockSize, group= group)) +
  geom_line(aes(color = group, group = group), show.legend = F) +
  facet_grid (. ~ Treatment)
g

# Matrix of treatment per place, smooth over player decisions

g <- ggplot(dat= filter (group_dat, part == 1) 
              , aes(x=Round, y=IntermediateStockSize)) + geom_line(aes(colour = group, alpha = 0.2), show.legend = FALSE) +
		geom_vline( aes( xintercept=6, color='red', alpha=0.1), show.legend = F) + 
		stat_summary(fun.data='mean_cl_boot', geom='smooth') + # option 'mean_cl_boot I like the most but normal assumes normality
		facet_grid(.~Treatment)# + ggtitle ('Intermediate Stock Size')

g

# quartz.save(file='160919_rawData_2part.png', type='png', dpi=200, width = 8, height=3)

#should be equivalent but is not. Prefer the one above.
g <- ggplot(dat=dat, aes(x=Round, y=value)) + 
		geom_smooth(stat='smooth', method='loess', span=0.2) +
		facet_grid(Treatment ~ Place) 


#quartz.save(file='MeanExtractionSmooth.png', type='png', dpi=100)

## All players, all places, all treatments... not very informative
g <- ggplot(dat=dat, aes(x=Round, y=value)) + 
		geom_line(stat='identity', aes(color=group, group=ID_player), show.legend = F) +
		facet_grid(Treatment ~ Place) + coord_cartesian (ylim=c(0,12))

## Only players of one place, per day 
g <- ggplot(dat=subset(dat, dat$Place == 'Las Flores' ),
            aes(x=Round, y=value), group=Player) + 
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
datP1 <- dat %>%
  filter(dat$Place == 'Las Flores' & dat$Session == 'pm' & Date == "2016-02-04" ) %>%
  gdata::drop.levels()

p <- ggplot(data=datP1, aes(x=value, y=StockSizeBegining), group=Player) + 
		geom_path(aes(color=Round)) + facet_grid(Treatment~Player) +
		ggtitle('Phase space trajectory per player\n Las Flores, 2016-02-04-pm ') + 
		theme(text= element_text(family='Helvetica', size=9))
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



pm <- ggpairs(data=group_dat, columns=c( 'NewStockSize', 'Treatment','Place'), #'StockSizeBegining',
              mapping=aes(color= Treatment, alpha=0.5))+ 
              theme(text= element_text(family='Helvetica', size=8))
pm
ggpairs(data=dat, columns=c('StockSizeBegining', 'NewStockSize', 'value'), 
        mapping=aes(color= Place, alpha=0.5))

ggpairs(data=dat, columns=c('Treatment', 'value', 'Place'), mapping=aes(color= Place))

pm <- ggpairs(data= filter(dat, dat$part == T), 
              columns=c( 'NewStockSize', 'Treatment','Place'), 
              upper= list(continuous='density'), lower=list(continuous='points'), 
              mapping=aes(color= Place, alpha=0.5), title='Treatment effects') 

pm   + theme(text= element_text(family='Helvetica', size=6)) 

quartz.save('160913_TreatmentEffects.png', type='png', width = 7, height = 7)


## Contours works fine for stability landscapes for now.

c <- ggplot(data=dat, aes(y=NewStockSize, x=StockSizeBegining), group=Session) + 
        geom_density_2d(aes(color=part)) + facet_grid( Treatment ~ Place)

# quartz.save('GeneralSummary_contours_treatment.png', type='png')

## Create a string for the level group as you did before but for the complete dataset

	d <- as.character(dat$Date)
	s <- as.character(dat$Session)
	# create master string
	ms <- paste(d,s, sep="_")

	# return the vector to the dataset as factor
	dat$Group <- as.factor(ms)


c <- ggplot(data=dat, aes(y=NewStockSize, x=StockSizeBegining), group=Session) + 
  stat_density_2d(aes(color=group, alpha=0.5), n=100, h=15, show.legend = F) + 
  facet_grid( Treatment ~ Place)
c
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
group <- reshape::cast(dat, ID_player ~  group)[,-1]
treat <- reshape::cast(dat, ID_player ~  Treatment)[,-1]
context <- cbind(place,treat) # don't use group yet, maybe for aes

## PCA & MDS: running pca or mds creates an error because of missing values. 
# An alternative to deal with them is setting NA to zeroes and adding a column 
# on the context dataset indicating that there were missing values.
# Missing values on the dataset means collapse of the resource (except for a group on the first dat)


# Identify where the NA are
	play.collapse <- apply(players, 1, anyNA)

# deal with NA
	players [is.na(players)] <- 0
	anyNA(players) #checking, should be false now
context <- cbind(context,play.collapse)

pca <- rda(players, context) #, context
	plot(pca, col= levelCols[as.vector(fitSOM$unit.classif)])

# note: I ran mds with 'euclidean' distance and 'manhattan'. Euclidean gave an error saying there is not enough data. manhattan calculated but didn't reach convergence. Morisita, jaccard, kulczynski, horn, rau, don't reach convergence neither. Frequent error on different measures: Stress is (nearly) zero - you may have insufficient data. It does work however if I use the transpose. It calculates the ordering of rounds on mds$points, but the ordering of players can be found too at mds$species
mds <- metaMDS(players, dist= 'manhattan', autotransform=F, k=2)
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



# Note to self: For combining matrix probabilities (transition prob matrix) 
# see discussion here: https://www.physicsforums.com/threads/can-you-combinie-two-transition-probability-matrices.580126/


## Stats:
# Are distributions significantly different?
# quartz()

# For regression Lindahl et al uses group cooperation index as dependent variable
dat <- mutate(dat, crossThreshold = IntermediateStockSize - 28)

g <- ggplot(dat, aes(Treatment, crossThreshold)) + # SumTotalCatch
  geom_jitter(width = 0.2, aes(color = Treatment, alpha = 0.2), show.legend = FALSE) +
  geom_boxplot(aes(alpha = 0.1)) + 
  facet_grid( Place ~ part) + coord_flip() + ggtitle ('Threshold deviation before and after treatment')
	
g <- ggplot(dat, aes(Place, crossThreshold)) + # SumTotalCatch
  geom_jitter(width = 0.2, aes(color = Place, alpha = 0.2), show.legend = FALSE) +
  geom_boxplot(aes(alpha = 0.1)) +
  facet_grid( Treatment ~ part) + coord_flip()+ ggtitle ('Threshold deviation before and after treatment')

g <- ggplot (filter(dat, part == 1), aes(Round, crossThreshold, group = group)) +
  geom_hline(yintercept = 0, color = 'grey', show.legend = FALSE) +
  geom_vline (xintercept = 6, color = 'grey', show.legend = FALSE) +
  geom_line(aes(color = group), show.legend = FALSE) +
  facet_grid(. ~Treatment )

g

  
quartz.save(file='160610_SumTotalCatch_Treatment.png', type = 'png', dpi=200)

# Examples of how the test is run

wt <- wilcox.test(x=filter(dat, Treatment == 'Risk')$value, 
			y=filter(dat, Treatment == 'Threshold')$value)

kruskal.test( value ~ Treatment, data=dat)
kruskal.test( value ~ Place, data=dat)

mod <- aov(crossThreshold ~ Treatment, data = dat)
summary (mod)
mod <- lm (crossThreshold ~ Treatment, data = dat)

# look what I found!
pwt <- pairwise.wilcox.test(x= group_dat$IntermediateStockSize, g =  dat$Treatment, paired = T, p.adj = 'bonf')












####
# Checking the database for survey data

library(gdata)

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

### J-160525: some products for Caroline on demand. Now I'm working with dat.noNA

summary(dat.noNA)

GroupStats<- dat.noNA %>% 
  group_by(group) %>%
  summarise (m_stockSize = mean (StockSizeBegining),
             m_TotalCatch = mean (SumTotalCatch),
             m_IntStock = mean (IntermediateStockSize),
             m_regRate = mean (Regeneration),
             m_NewSS = mean (NewStockSize),
             sd_stockSize = sd (StockSizeBegining),
             sd_TotalCatch = sd (SumTotalCatch),
             sd_IntStock = sd (IntermediateStockSize),
             sd_regRate = sd (Regeneration),
             sd_NewSS = sd (NewStockSize))

# write.csv(GroupStats, file = 'GroupStats.csv')

g <- ggplot(filter(dat, part ==1), aes(Place, SumTotalCatch)) + # SumTotalCatch
  geom_jitter(width = 0.2, aes(color = Place, alpha = 0.2), show.legend = FALSE) +
  geom_boxplot(aes(alpha = 0.1)) +
  facet_grid( Treatment ~ .) + coord_flip()+ ggtitle ('Threshold deviation before and after treatment')

g

### GINI coefficients

summary (dat)
str(dat)

gini0 <- filter(dat, part == 0) %>%
  group_by (ID_player, group) %>%
  summarize(earning = sum (value)) %>%
  group_by (group) %>%
  summarize (gini_st1 = ineq(earning, type = 'Gini'))

gini1 <- filter(dat, part == 1) %>%
  group_by (ID_player, group) %>%
  summarize(earning = sum (value)) %>%
  group_by (group) %>%
  summarize (gini_st2 = ineq(earning, type = 'Gini'))

gini_round <- dat %>%
  group_by (ID_player, group, Round) %>%
  summarize(earning = sum (value)) %>%
  group_by (group, Round) %>%
  summarize (gini_round = ineq(earning, type = 'Gini'))


gini <- left_join(gini, dplyr::select(dat, Treatment, Place, group) , by = 'group')
gini$low <- gini$gini < 0.015 

 
g <- ggplot(gini, aes(Treatment, gini)) + 
  geom_jitter (width = 0.5, aes (color = Place, alpha = 0.2), show.legend = T) +
  geom_boxplot(aes (alpha = 0.1))  + facet_grid(. ~ Place) + coord_flip() + ggtitle ('Gini on fishers earnings\n Stage 2')
g

quartz.save(file='coop_place_byStage.png', type = 'png')  



## join gini and original data
dat <- left_join (dat, gini, by = c('group', 'Place', 'Treatment'))


c <- ggplot(data=dat, aes(y=NewStockSize, x=StockSizeBegining), group=group) + 
  stat_density_2d(aes(color=low, alpha=0.5), n=100, h=15, show.legend = T) + 
  facet_grid( Treatment ~ Place)
c



# quartz.save('GeneralSummary_contours_groups_lowGini.png', type='png')




## Now calculate Gini's on expected values for next round
# obs ID
dat <- mutate (dat, round_lev = as.factor(Round))
surv <- mutate(surv, round_lev = as.factor(round))
dat <- transform (dat, ID_Obs = interaction(Date, Treatment, Session, Player, round_lev, drop = TRUE))
surv <- transform (surv, ID_Obs = interaction(date, treatmentName, Session, playerNo, round_lev, drop = TRUE) )


expected <- surv %>%
  dplyr::select ( indGuessStockSize, round_lev, ID_Obs) %>%
  left_join(dat)


summary (expected)
str(expected)
str(dat)

filter (expected, indGuessStockSize > 50) %>%
  dplyr::select ( indGuessStockSize, ID_Obs, value, NewStockSize)


## following the same procedure for gini, the gini of expected is our proxy for cooperation
coop <- expected %>% # filter(expected, part == 1)
  group_by (ID_player, group) %>%
  summarize(avg_exp = mean(indGuessStockSize, na.rm = TRUE)) %>%
  group_by (group)%>%
  summarize (coop_gini = ineq(avg_exp, type = 'Gini'))

coop <- left_join(coop, dplyr::select(dat, Treatment, Place, group, part) , by = 'group')

g <- ggplot(coop, aes(Treatment, coop_gini)) + 
  #geom_jitter (width = 0.5, aes (color = Place, alpha = 0.2), show.legend = T) +
  geom_boxplot(aes (alpha = 0.1))  + facet_grid( Place ~ part) + coord_flip() +
  ggtitle ('Gini expected stock size on the next round\n Proxy of cooperation?')
g

