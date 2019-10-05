### Replicating Schill manuscript
## Juan Rocha
## June 2018
## juan.rocha@su.se

## load libraries
rm(list = ls())
set.seed(12345)
library(tidyverse)
library(network)
# library(sna)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(GGally)
library(moments)

# library(ggmap)
# library(maptools)
# library(maps)
# library(mapproj)

library(grid)
library(gridExtra)
library(plm)
library(ineq)

## load data
## this is the same as data prep for NCC figures file
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
dat <- transform(dat, ID_player = interaction(Date, Treatment, Session, Player, drop = TRUE))
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

## change names of places and reorder levels
levels(dat$Place) <- c("D", "B", "A", 'C')
dat$Place <- factor(dat$Place, levels(dat$Place)[c(3,2,4,1)])
levels(dat$Place)

str(dat)
levels(dat$Treatment)[1] <- "Baseline"
levels(dat$Treatment)[4] <- "Ambiguity"

## reorder the treatment levels
str(dat)
dat$Treatment <- factor(dat$Treatment, levels(dat$Treatment)[c(1,3,2,4)])
levels(dat$Treatment)


### Group level data
group_dat <- dat %>%
  select(Treatment, Place, group, Round, StockSizeBegining, IntermediateStockSize, SumTotalCatch,
         Regeneration, NewStockSize, part, Date, Session) %>%
  unique()

## calculate gini
gini0 <- filter(dat, part == 0) %>%
  group_by (ID_player, group, Place, Treatment, part) %>%
  summarize(earning = sum (value, na.rm = T)) %>%
  group_by (group, Place, Treatment, part) %>%
  summarize (gini_st1 = ineq(earning, type = 'Gini'))

gini1 <- filter(dat, part == 1) %>%
  group_by (ID_player, group, Place, Treatment, part) %>%
  summarize(earning = sum (value, na.rm = T )) %>%
  group_by (group, Place, Treatment, part) %>%
  summarize (gini_st2 = ineq(earning, type = 'Gini'))

gini_round <- dat %>%
  group_by (group, Round, Place, Treatment) %>%
  summarize (gini_round = ineq(value, type = 'Gini'))

group_dat <- left_join(group_dat, gini0, by = c('group', "Place", "Treatment", 'part'))
group_dat <- left_join(group_dat, gini1, by = c('group', "Place", "Treatment", 'part'))
group_dat <- left_join(group_dat, gini_round, by = c('group', 'Round', "Place", "Treatment"))

group_dat <- mutate( group_dat, day = ifelse(Date == "2016-02-01" |Date == "2016-02-03"|Date == "2016-02-05"|Date == "2016-02-09", 1, 2 ))
group_dat <- mutate( group_dat, group_color = interaction(Session, day))


group_dat <- as_tibble(group_dat)









