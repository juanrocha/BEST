### BEST Analysis
# This script explores the data of the BEST project: games with thresholds in 4
# fishing communitites in the Colombian Caribeean coast. The first part summarizes
# and plot the data in different ways. The second takes ideas from StrategicNetworks.R
# to explore if is possible to understand strategic behaviour of humans from a
# data oriented prespective.
# Raw data was processed with script ExtractDataBEST.R

# by Juan Carlos Rocha
# juan.rocha@su.se
# version 170222

# clean and load weapons:
rm(list=ls())

library(ggplot2)
# library(network)
# library(sna)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(GGally)
library(ggmap)
# library(vegan)
# library(cluster)
# library(NbClust);library(kohonen)
# library(mclust); library(clValid)
library(ineq) # for gini

library(gridBase)
library(grid)
library(gridExtra)


## for cleaning models
library(car)
library(broom)
library(sandwich); library(lmtest)


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
  select (Treatment, Place, group, Round, StockSizeBegining, IntermediateStockSize, SumTotalCatch,
          Regeneration, NewStockSize, part, Date, Session) %>%
  unique ()

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




#### Load Map
load('~/Documents/Projects/BEST - Beijer/BEST/mapBEST.RData')
g <- ggmap(Colombia) +
  geom_path(data = data.frame( lon = c(-75, -75, -74, -74, -75), lat = c(10.6,  11.6, 11.6, 10.6, 10.6)),
            aes(colour = 'red', alpha = 0.5), show.legend = F) +
  theme_void(base_size = 5)

g

f2a <- ggmap(map) +
  geom_point(aes(x=lon, y=lat, colour='Orange', size=2), data=coords, alpha=0.5,
             show.legend = F) + theme_minimal(base_size = 5) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_text(aes(x=lon, y=lat), data= coords ,
            label=c('A', 'C', 'D', 'B'),
            size=4, colour='blue', nudge_y = 0.03) + ggtitle('a)') +
  inset(grob = ggplotGrob(g),
        xmin = -75 , xmax = -74.4, ymin = 11.1 , ymax = 11.48)


#######################

#########################
### Figures for NCC paper
#########################


## new figure needs the stage-wise means for both stock size and gini
by_group_treat1 <- group_by(filter(group_dat, part == F), Treatment, Place, group) %>%
  summarise(mean_st1 = median(IntermediateStockSize))

by_group_treat2 <- group_by(filter(group_dat, part == T), Treatment, Place, group) %>%
  summarise(mean_st2 = median(IntermediateStockSize))

by_group_catch <- group_by(filter(group_dat, part == T), Treatment, Place, group) %>%
  summarise(catch_st2 = mean(SumTotalCatch))

stockSize <- left_join(by_group_treat1, by_group_treat2) %>%
  mutate(sw_mean = (mean_st1 + mean_st2) / 2)

ginis <- left_join(select(gini0, -part),
                   select(gini1, -part)) %>%
  mutate(sw_gini = (gini_st1 + gini_st2)/2)

stagewise <- left_join(ginis, stockSize)

### Figure 1
# this is the mean to be included in fig1c
by_group_treat <- group_by(filter(group_dat, part == T), Treatment, Place, group) %>%
  summarise(mean_group = mean(IntermediateStockSize)) %>%
  summarise(., mean = mean(mean_group), ymax = max(mean_group), ymin = min(mean_group))

# f1a <- ggplot(data = group_dat, aes(IntermediateStockSize, group = part)) +
#   geom_density(aes(fill = part), show.legend = T) +
#   geom_vline(xintercept = 28, color = 'blue', show.legend = F) +
#   facet_wrap(~Treatment, nrow=2, ncol = 2) + labs(fill = "Stage", x = "Stock size") +
#   scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5), labels=c("1", "2")) +
#   ggtitle(label = 'a)') + theme_minimal(base_size = 5) +
#   theme(legend.position = c(0.1, 0.85), legend.key.size = unit(0.2, 'cm'))
#
# f1b <- ggplot(data = filter(group_dat, part == T), aes(IntermediateStockSize, group = Treatment))+
#   geom_density(aes(fill = Treatment, color = Treatment)) + xlab('Stock size') +
#   geom_vline(xintercept = 28, color = 'blue', show.legend = F) +
#   scale_fill_manual(values = alpha(rev(brewer.pal(4,'Spectral')), 0.4)) +
#   scale_color_manual(values = alpha(rev(brewer.pal(4,'Spectral')), 0.4)) + ggtitle('b)') +
#   theme_minimal(base_size = 5) + theme(legend.position = c(0.2, 0.70), legend.key.size = unit(0.3, 'cm'))

# Box plots option that Caroline didn't like
# f1c <-  ggplot(data = filter(group_dat, part == T), aes(Treatment, IntermediateStockSize)) +
#   geom_boxplot(aes(fill = Treatment), show.legend = F, notch = T)  +
#   scale_fill_manual(values = alpha(rev(brewer.pal(4,'Spectral')), 0.7)) +
#   geom_point(data = by_group_treat, aes(Treatment, mean), colour = 'gold', fill = 'gold',
#              shape = 20, show.legend = F) +
#   facet_wrap(~Place, nrow = 1) + ggtitle('c)') + theme_gray(base_size = 5) +
#     ylab('Stock size')

## From ggplot help website:
# Because the bars and errorbars have different widths
# we need to specify how wide the objects we are dodging are
# dodge <- position_dodge(width=0.9)
#
# f1c <-  ggplot(data = by_group_treat, aes(Place, mean, fill = Treatment)) +
#   geom_bar( stat = 'identity', position = dodge, show.legend = F)  +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.3) +
#   scale_fill_manual(values = alpha(rev(brewer.pal(4,'Spectral')), 0.7)) +
#   # geom_point(data = by_group_treat, aes(Treatment, mean), colour = 'gold', fill = 'gold',
#   #            shape = 20, show.legend = F) +
#   ggtitle('c)') + theme_minimal(base_size = 5) +  theme(legend.key.size = unit(0.2, 'cm')) +
#   ylab('Stock size')

## Combine the figure
# g <- list ( f1a, f1b, f1c)
#
# source('~/Dropbox/Code/multiplot.R')
# layout <- matrix(c(1,2,3), ncol = 3, nrow = 1, byrow = T)
# multiplot(plotlist = g, layout = layout)
#
# quartz.save(file = '170311_Schill_NCC_Fig1.png', type = 'png', dpi = 1000,
#             pointsize = 5, family = "helvetica", width = 6, height = 2)

f1a <- ggplot(data = filter(by_group_treat2, Treatment == "Baseline" | Treatment == "Threshold"),
              aes(mean_st2)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(by_group_treat2, Treatment == "Baseline" | Treatment == "Threshold") %>%
               group_by(Treatment) %>%
               summarise(m = mean(mean_st2)),
    mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  geom_vline(xintercept = c(28), color = c('red'), show.legend = F)+
  ggtitle(label = 'a)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Mean intermediate stock size')  + ylim (c(0,0.1)) + xlim (c(0,50))

f1b <- ggplot(data = filter(by_group_treat2, Treatment == "Baseline" | Treatment == "Risk"),
              aes(mean_st2)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(by_group_treat2, Treatment == "Baseline" | Treatment == "Risk") %>%
               group_by(Treatment) %>%
               summarise(m = mean(mean_st2)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  geom_vline(xintercept = c(28), color = c('red'), show.legend = F)+
  ggtitle(label = 'b)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Mean intermediate stock size') + ylim (c(0,0.1)) + xlim (c(0,50))

f1c <- ggplot(data = filter(by_group_treat2, Treatment == "Baseline" | Treatment == "Ambiguity"),
              aes(mean_st2)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(by_group_treat2, Treatment == "Baseline" | Treatment == "Ambiguity") %>%
               group_by(Treatment) %>%
               summarise(m = mean(mean_st2)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  geom_vline(xintercept = c(28), color = c('red'), show.legend = F)+
  ggtitle(label = 'c)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Mean intermediate stock size') + ylim (c(0,0.1)) + xlim (c(0,50))

f1d <- ggplot(data = filter(by_group_catch, Treatment == "Baseline" | Treatment == "Threshold"),
              aes(catch_st2)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(by_group_catch, Treatment == "Baseline" | Treatment == "Threshold") %>%
               group_by(Treatment) %>%
               summarise(m = mean(catch_st2)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  ggtitle(label = 'd)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Mean catch')+ ylim (c(0,0.4)) + xlim (c(0,15))

f1e <- ggplot(data = filter(by_group_catch, Treatment == "Baseline" | Treatment == "Risk"),
              aes(catch_st2)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(by_group_catch, Treatment == "Baseline" | Treatment == "Risk") %>%
               group_by(Treatment) %>%
               summarise(m = mean(catch_st2)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F) +
  ggtitle(label = 'e)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Mean catch')+ ylim (c(0,0.4)) + xlim (c(0,15))

f1f <- ggplot(data = filter(by_group_catch, Treatment == "Baseline" | Treatment == "Ambiguity"),
              aes(catch_st2)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(by_group_catch, Treatment == "Baseline" | Treatment == "Ambiguity") %>%
               group_by(Treatment) %>%
               summarise(m = mean(catch_st2)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  ggtitle(label = 'f)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Mean catch')+ ylim (c(0,0.4)) + xlim (c(0,15))



## Combine the figure
g <- list ( f1a, f1b, f1c, f1d, f1e, f1f)

source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1:6), ncol = 3, nrow = 2, byrow = T)
multiplot(plotlist = g, layout = layout)

quartz.save(file = '170330_Schill_NCC_Fig1.pdf', type = 'pdf', dpi = 1200,
            pointsize = 5, family = "helvetica", width = 5, height = 2.5)


### Figure 2
source('~/Documents/Projects/BEST - Beijer/BEST/games_howell/games_howell.R')


# Caro wants: boxplots of stagewise mean of  intstocksize and gini
f2a <- ggplot(data = stagewise, aes(x = Place, y = sw_mean)) +
  geom_boxplot(notch = F, aes(color = Place), show.legend = F) +
  geom_jitter(aes(color = Place), show.legend = F, width = 0.25) +
  ylab("Stagewise mean of Stock Size") +
  ggtitle('a)') + theme_minimal(base_size = 5)

f2b <- ggplot(data = stagewise, aes(Place, sw_gini)) +
  geom_boxplot(notch = F, aes(color = Place), show.legend = F) +
  geom_jitter(aes(color = Place), show.legend = F, width = 0.25) +
  ylab("Stagewise mean of Gini") +
  ggtitle('b)') + theme_minimal(base_size = 5)


# f2a <- ggplot(filter(df, variable == "Gini", type == "Treatment"), aes(estimate, rowname, color = Stage)) +
#   geom_point(aes(size = 1), show.legend = F) + scale_size_continuous(trans = "reverse", range = c(0,2), breaks = c(0.01, 0.1, 0.5)) +
#   geom_errorbarh(aes(xmax = conf.high, xmin = conf.low, height = .3)) +
#   geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
#   scale_color_manual(values = alpha(c('gold', 'blue'), 0.8)) +
#   #facet_grid(~ type , drop = T) +
#   labs(title = 'a)', x = 'Difference', y = 'Treatments') +
#   theme_gray(base_size = 5) #+ theme(legend.position = c(0.1, 0.70))
#
# f2b <- ggplot(filter(df, variable == "Gini", type == "Location"), aes(estimate, rowname, color = Stage)) +
#   geom_point(aes(size = 1), show.legend = F) + scale_size_continuous(trans = "reverse", range = c(0,2), breaks = c(0.01, 0.1, 0.5)) +
#   geom_errorbarh(aes(xmax = conf.high, xmin = conf.low, height = .3)) +
#   geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
#   scale_color_manual(values = alpha(c('gold', 'blue'), 0.8)) +
#   #facet_grid(~ type , drop = T) +
#   labs(title = 'b)', x = 'Difference', y = 'Location') +
#   theme_gray(base_size = 5) #+ theme(legend.position = c(0.1, 0.70))

userfriendlyscience::oneway(y=grouped_df$intStockSA, x=grouped_df$location,  posthoc="games-howell")
library(userfriendlyscience)

df_treatment <- list(); df_place <- list()
vars <- names(stagewise)[4:9]

# for (i in seq_along(vars)){
#   df_treatment[[i]] <- with(stagewise, oneway(x=Treatment, y= vars[i], posthoc = 'games-howell'))
#   df_treatment[[i]]$intermediate$posthoc$variable <- vars[i]
#
#   df_place[[i]] <- with(stagewise, oneway(x=Place, y= vars[i], posthoc = 'games-howell'))
#   df_place[[i]]$intermediate$posthoc$variable <- vars[i]
# }


df_treatment[[1]] <- with(stagewise, oneway(x=Treatment, y=gini_st1, posthoc = 'games-howell')); df_treatment[[1]]$intermediate$posthoc$variable <- vars[1]
df_treatment[[2]] <- with(stagewise, oneway(x=Treatment, y=gini_st2, posthoc = 'games-howell' )); df_treatment[[2]]$intermediate$posthoc$variable <- vars[2]
df_treatment[[3]] <- with(stagewise, oneway(x=Treatment, y=sw_gini, posthoc = 'games-howell' )); df_treatment[[3]]$intermediate$posthoc$variable <- vars[3]
df_treatment[[4]] <- with(stagewise, oneway(x=Treatment, y=mean_st1, posthoc = 'games-howell' )); df_treatment[[4]]$intermediate$posthoc$variable <- vars[4]
df_treatment[[5]] <- with(stagewise, oneway(x=Treatment, y=mean_st2, posthoc = 'games-howell' )); df_treatment[[5]]$intermediate$posthoc$variable <- vars[5]
df_treatment[[6]] <- with(stagewise, oneway(x=Treatment, y=sw_mean, posthoc = 'games-howell' )); df_treatment[[6]]$intermediate$posthoc$variable <- vars[6]
y <- lapply(df_treatment, function(x){rownames(x$intermediate$posthoc)})
df_treatment <- lapply(df_treatment, function(x){x$intermediate$posthoc}) %>% bind_rows
df_treatment$groups <- y[[1]]
df_treatment$groups <- as.factor(df_treatment$groups)

df_place[[1]] <- with(stagewise, oneway(x=Place, y=gini_st1, posthoc = 'games-howell' )); df_place[[1]]$intermediate$posthoc$variable <- vars[1]
df_place[[2]] <- with(stagewise, oneway(x=Place, y=gini_st2 , posthoc = 'games-howell')); df_place[[2]]$intermediate$posthoc$variable <- vars[2]
df_place[[3]] <- with(stagewise, oneway(x=Place, y=sw_gini, posthoc = 'games-howell' )); df_place[[3]]$intermediate$posthoc$variable <- vars[3]
df_place[[4]] <- with(stagewise, oneway(x=Place, y=mean_st1, posthoc = 'games-howell' )); df_place[[4]]$intermediate$posthoc$variable <- vars[4]
df_place[[5]] <- with(stagewise, oneway(x=Place, y=mean_st2 , posthoc = 'games-howell')); df_place[[5]]$intermediate$posthoc$variable <- vars[5]
df_place[[6]] <- with(stagewise, oneway(x=Place, y=sw_mean, posthoc = 'games-howell' )); df_place[[6]]$intermediate$posthoc$variable <- vars[6]
y <- lapply(df_place, function(x){rownames(x$intermediate$posthoc)})
df_place <- lapply(df_place, function(x){x$intermediate$posthoc}) %>% bind_rows
df_place$groups <- y[[1]]
df_place$groups <- as.factor(df_place$groups)

names(df_treatment)[c(1,2,3)] <- c("Difference",'lower', 'upper')
names(df_place)[c(1,2,3)] <- c("Difference",'lower', 'upper')
#
# write.csv(df_treatment, file = 'games_howell_treatment.csv')
# write.csv(df_place, file = 'games_howell_place.csv')

f2c <- ggplot(filter(df_treatment, variable == "mean_st1" | variable == "mean_st2" | variable == "sw_mean" ) ,
              aes(Difference, groups, color = variable)) +
  geom_point(aes(shape = variable), show.legend = T) +
  geom_errorbarh(aes(xmax = upper, xmin = lower, height = .1), show.legend = T) +
  geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
 # scale_color_manual(values = alpha(c('gold', 'blue', "orange"), 0.8)) +
  #facet_grid(~ variable , drop = T) +
  labs(title = 'c)', x = 'Difference', y = 'Treatments') +
  theme_minimal(base_size = 5) #+ theme(legend.position = c(0.1, 0.70))

f2d <- ggplot(filter(df_place, variable == "mean_st1" | variable == "mean_st2" | variable == "sw_mean" ),
              aes(Difference, groups, color = variable))  +
  geom_point(aes(shape = variable), show.legend = F) +
  geom_errorbarh(aes(xmax = upper, xmin = lower, height = .1), show.legend = F) +
  geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
  # scale_color_manual(values = alpha(c('gold', 'blue'), 0.8)) +
  facet_grid(~ variable , drop = T) +
  labs(title = 'c)', x = 'Difference', y = 'Location') +
  theme_minimal(base_size = 5) #+ theme(legend.position = c(0.1, 0.70))

f2e <- ggplot(filter(df_treatment, variable == "gini_st1" | variable == "gini_st2" | variable == "sw_gini" ),
              aes(Difference, groups, color = variable)) +
  geom_point(aes(shape = variable), show.legend = T) +
  geom_errorbarh(aes(xmax = upper, xmin = lower, height = .1), show.legend = T) +
  geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
  # scale_color_manual(values = alpha(c('gold', 'blue', "orange"), 0.8)) +
  #facet_grid(~ variable , drop = T) +
  labs(title = 'e)', x = 'Difference', y = 'Treatments') +
  theme_minimal(base_size = 5) #+ theme(legend.position = c(0.1, 0.70))

f2f <- ggplot(filter(df_place, variable == "gini_st1" | variable == "gini_st2" | variable == "sw_gini" ),
              aes(Difference, groups, color = variable))  +
  geom_point(aes(shape = variable), show.legend = F) +
  geom_errorbarh(aes(xmax = upper, xmin = lower, height = .1), show.legend = F) +
  geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
  # scale_color_manual(values = alpha(c('gold', 'blue'), 0.8)) +
  facet_grid(~ variable , drop = T) +
  labs(title = 'd)', x = 'Difference', y = 'Location') +
  theme_minimal(base_size = 5) #+ theme(legend.position = c(0.1, 0.70))


# f2c <- ggplot(df, aes(estimate, term, color = Stage)) +
#   geom_point(aes(size = p_value)) + scale_size_continuous(trans = "reverse", range = c(0,2), breaks = c(0.01, 0.1, 0.5)) +
#   geom_errorbarh(aes(xmax = conf.high, xmin = conf.low, height = .3)) +
#   geom_vline(xintercept = 0, color = 'grey', show.legend = F, linetype = 2) +
#   scale_color_manual(values = alpha(c('gold', 'blue'), 0.8)) +
#   facet_wrap(~ type , nrow = 2, ncol = 1, drop = T) +
#   labs(title = 'c)', x = 'Differences in Intermediate Stock Size', y = 'Difference terms') +
#   theme_gray(base_size = 5) #+ theme(legend.position = c(0.1, 0.70))

## Combine the figure
g <- list (f2a, f2b, f2d, f2f)

# g <- list (f2a, f2b, f2c, f2d, f2e, f2f)
quartz()
source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1:4), 2,2, byrow = F)
multiplot(plotlist = g, layout = layout)

quartz.save(file = '170319_Schill_NCC_Fig4_median.png', type = 'png', dpi = 600, width = 4, height = 4)



### Figure 3
# c <- ggplot(data=dat, aes(y=NewStockSize, x=StockSizeBegining), group= group) +
#   geom_density_2d(aes(color=part)) + facet_grid( Treatment ~ Place)
# c

f3b <-  ggplot(data = filter(group_dat, part == TRUE), aes(StockSizeBegining, IntermediateStockSize)) +
  geom_point(aes(color = group_color), show.legend = F, size = 0.6) +
  geom_density_2d(aes(color = group_color), show.legend = F,  n=100, h=15, alpha = 0.4) +
  #scale_color_gradient(low = "#0000FFCC", high = "#FF0000CC") +
  # scale_color_manual(values = rep(brewer.pal(n = 4, "Set1"), 16)) +
  facet_grid(Treatment ~ Place)  + ylab("Intermediate Stock Size") + xlab('Stock Size Begining') +
  ggtitle('b)') + theme_gray(base_size = 8)



f3a <- ggplot(data = filter(group_dat, part == T), aes(y =IntermediateStockSize, x= Round, group = group)) +
  geom_hline(yintercept = 28, color = 'pink') +
  geom_line(aes(color = gini_st2), show.legend = T) + geom_point(aes(color = gini_st2), show.legend = T, size = 0.5) +
  scale_color_gradient(low = "#0000FFCC", high = "#FF0000CC") +
  facet_grid(Treatment ~ Place) + ylab("Intermediate Stock Size") +
  ggtitle('a)') + theme_gray(base_size = 8)



# pm <- ggpairs(data= group_dat,
#               columns=c( 'IntermediateStockSize', 'Treatment','gini_round'),
#               upper= list(continuous='density'), lower=list(continuous='points'),
#               mapping=aes(color= Place, alpha=0.5), title='Treatment effects')

## Combine the figure
g <- list (f3a, f3b)

source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1,2), 1,2, byrow = F)
multiplot(plotlist = g, layout = layout)

# quartz.save(file = '170512_Happy_Birthday_Caroline.pdf', type = 'pdf', dpi = 1200, width = 8, height = 4)

ggsave(filename = '170512_Happy_Birthday_Caroline.pdf', device = 'pdf', dpi = 1200, width = 8, height = 4, plot = multiplot(plotlist = g, layout = layout))


### check the agreements on survey data

surv <- read.csv2(file='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.csv', header=T, na.strings = '.')
# key <- gdata::read.xls(xls='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.xlsx', sheet=2)

surv.dat <- dplyr::select(surv, date, locationName, am, pm, treatmentName, groupID, color, playerNo, round, indClaim, indGuessStockSize, 241:370)

surv.dat <- filter(surv.dat, round > 0 )
str(surv.dat)

# standardize with same levels as game data
levels(surv.dat$date) <- as.factor(as.Date(levels(surv.dat$date), format='%d/%m/%Y')[c(7,12:16,7,9,9,11,11, 12:16)])
surv.dat$Session <- as.factor (ifelse(surv.dat$am == 1, 'am', 'pm'))
levels(surv.dat$treatmentName) <- c('Base line', 'Base line', 'Risk', 'Risk', 'Threshold', 'Threshold','Threshold', 'Uncertainty', 'Uncertainty') # unify spelling
surv.dat$playerNo <- as.factor(surv.dat$playerNo)

# Create unique player IDs [faster way from http://stackoverflow.com/questions/13566562/creating-a-unique-id-in-r]
surv.dat <- transform (surv.dat, ID_player = interaction(date, treatmentName, Session, playerNo, drop = TRUE))
surv.dat <- transform (surv.dat, group = interaction(date, treatmentName, Session, drop = TRUE))

### modifications for carolines figure
levels(surv.dat$color)[c(1,3,5,7)] <- c("Azul","Morado","Naranja","Verde")
surv.dat$color[is.na(surv.dat$color)] <- "Morado"


agree <- surv.dat %>%
  select(group, groupID, color, Round = round #, # agree1 = A11.3..agreement, agree2 = B11.3..agreement,
         # exact1 = A3.1..agreement.exact, exact2 = B3.1..Bgreement.exBct,
         # range1 = A3.2..agreement.range, range2 = B3.2..agreement.range,
         #comm1 = A11.2..communication, comm2 = B11.2..communication,
         #type1 = A11.3.1..type.agreement, type2 = B11.3.1..type.agreement,
         # breaks1 = A11.5..did.anyone.break.the.agreement., breaks2 = B11.5..did.anyone.break.the.agreement.
         # ,         talk1 = A1..did.they.talk., talk2 = B1..did.they.tBlk.
         ) %>%
  unique()

agree$Round <- as.numeric(agree$Round)
group_dat <- left_join(group_dat, agree, by = c('group', 'Round'))

group_dat$agree1 <- as.factor(group_dat$agree1 )
group_dat$agree2 <- as.factor(group_dat$agree2 )
# group_dat$agree1[is.na(group_dat$agree1)] <- 3





# filter(surv.dat, is.na(color))
# filter(surv.dat, date == "2016-02-02", locationName == "Taganga", am == 1) %>% select(color) %>% summary


#### Plotting whether the groups where under treatment or not.

group_dat$climate <- ifelse(group_dat$Treatment == "Baseline" | group_dat$Round < 7, 0,
                            ifelse(group_dat$Treatment == "Threshold" & group_dat$Round > 6, 1,
                                   ifelse(group_dat$Regeneration < 10 & group_dat$IntermediateStockSize < 28, 1, 0)
                                   ))

head(group_dat[c(1,6,8,14)], 100) ## seems correct / has some errors once activated the threshold it should not get back

events <- read.csv2('~/Downloads/eventData-1.csv', header = T)
str(events)

events <-  events %>%
  select (event, Treatment = treatment, Place = place, group = R_ID_group, Round = round) %>%
  unique ()

events <- full_join(group_dat, events)
events$Treatment <- as.factor(events$Treatment)
events$Treatment <- factor(events$Treatment, levels(events$Treatment)[c(2,4,3,1)])


s1a <- ggplot(data = group_dat, aes(y =IntermediateStockSize, x= Round, group = group)) +
  geom_hline(yintercept = 28, color = 'pink') +
  geom_line(aes(color = gini_round), show.legend = T) +
  geom_point(aes(color = gini_round) , show.legend = T, size = 1) + #, shape = factor(agree1)
  scale_color_gradient(low = 'blue', high = 'red') +
  geom_rect(xmin=6.1, xmax = 6.9, ymin = 0, ymax =50, fill = 'white') +
  facet_grid(Treatment ~ Place) + ylab("Intermediate Stock Size") +
  theme_gray(base_size = 8) + theme(legend.position = 'bottom') # ggtitle('a) Assisstant 1') +

s1b <- ggplot(data = filter(group_dat, part == T), aes(y =IntermediateStockSize, x= Round, group = group)) +
  geom_hline(yintercept = 28, color = 'pink') +
  geom_line(aes(color = gini_round), show.legend = T) +
  geom_point(aes(color = gini_round, shape = factor(agree2)) , show.legend = T, size = 1) +
  scale_color_gradient(low = 'blue', high = 'red') +
  facet_grid(Treatment ~ Place) + ylab("Intermediate Stock Size") +
  + theme_gray(base_size = 5) #  ggtitle('b) Assisstant 2')

sm <- ggplot(data = filter(events, part == T), aes(y =IntermediateStockSize, x= Round, group = group)) +
  geom_hline(yintercept = 28, color = 'pink') +
  geom_line(aes(color = event), show.legend = T) + geom_point(aes(color =  event), show.legend = T, size = 0.5) +
  scale_color_manual(values = c("#0000FFCC", "#FF0000CC")) +
  #geom_rect(xmin=6.1, xmax = 6.9, ymin = 0, ymax =50, fill = 'white') +
  #geom_polygon(data = df3, mapping = aes(x = x, y = y, fill = value , group = id), show.legend = F) +
  facet_grid(Treatment ~ Place) + ylab("Intermediate Stock Size") +
  theme_gray(base_size = 8) + theme(legend.position = 'bottom')

## Combine the figure
g <- list (s1a, s1b)

source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1,2), 1,2, byrow = F)
multiplot(plotlist = g, layout = layout)

quartz.save(file = 'Schill_NCC_SupMat_gini_fullrounds.pdf', type = 'pdf', dpi = 1200, width = 5, height = 5)



#################
# supplementary figures by Caro's request
####

## Density plot like in Fig 1b but not based on all observations but only the mean group values, so only 16 values per density plot

by_group_treat <- group_by(filter(group_dat, part == T, gini_st2 < 0.015), Treatment, Place, group) %>%
  summarise(mean_group = mean(IntermediateStockSize))


f1b <- ggplot(data = by_group_treat, aes(mean_group, group = Treatment))+
  geom_density(aes(fill = Treatment, color = Treatment)) + xlab('Mean of Intermediate stock size per group') +
  geom_vline(xintercept = 28, color = 'blue', show.legend = F) +
  scale_fill_manual(values = alpha(rev(brewer.pal(4,'Spectral')), 0.4)) +
  scale_color_manual(values = alpha(rev(brewer.pal(4,'Spectral')), 0.4)) + ggtitle('Gini on stage 2 was < 0.015') +
  theme_gray(base_size = 7) + theme(legend.position = c(0.2, 0.70), legend.key.size = unit(0.2, 'cm'))


## Time series of Stage 2 of mean stock size per treatment plus standard deviation. So 4 lines in one graph

f3a <- ggplot(data = filter(group_dat, part == T, gini_st2 > 0.015), aes(y =IntermediateStockSize, x= Round, group = Treatment)) +
  geom_hline(yintercept = 28, color = 'pink') + geom_smooth(stat='smooth', aes(fill=Treatment, color = Treatment, group=Treatment)) +
  ggtitle('Gini on stage 2 was > 0.015')


### Agreements

g <- ggplot(data = surv.dat, aes(round, A11.3..agreement), group  = group) +
  geom_line(aes(color = group), show.legend = FALSE) +
  facet_grid(treatmentName ~ locationName)
g

g <- ggplot(data = surv.dat, aes(round, B3..agreement), group  = group) +
  geom_line(aes(color = group), show.legend = FALSE) +
  facet_grid(treatmentName ~ locationName)
g

### combine the agreements on the time lines

f3a <- ggplot(data = filter(group_dat, part == T), aes(y =IntermediateStockSize, x= Round, group = group)) +
  geom_hline(yintercept = 28, color = 'pink') +
  geom_line(aes(color = gini_round), show.legend = T) + geom_point(aes(color = gini_round), show.legend = T, size = 0.5) +
  scale_color_gradient(low = "#0000FFCC", high = "#FF0000CC") +
  facet_grid(Treatment ~ Place) + ylab("Intermediate Stock Size") +
  ggtitle('a)') + theme_gray(base_size = 5)




####### Figure with model

by_group_catch <- group_by(filter(group_dat, part == T), Treatment, Place, group) %>%
  summarise(catch_st2 = mean(SumTotalCatch))

by_group_below_t <- group_by(filter(group_dat, part == T), Treatment, Place, group) %>%
  mutate(round_below = IntermediateStockSize < 28) %>%
  group_by(group, Treatment, Place) %>%
  #filter(round_below == T) %>%
  #summarize(round_min = min(Round))
  summarize(rounds_below = sum(round_below)/10)

df <- left_join(stagewise, by_group_catch)
df <- left_join(df, by_group_below_t)

fit1 <- lm(mean_st2 ~ gini_st1 + mean_st1 + Place + Treatment, data = df )
fit2 <- lm(catch_st2 ~ gini_st1 + mean_st1 + Place + Treatment, data = df )
fit3 <- lm(rounds_below ~ gini_st1 + mean_st1 + Place + Treatment, data = df )
summary(fit3)

df1 <- tidy( coeftest(fit1, vcov = vcovHC(fit1, "HC3")) ) %>% mutate(hi = estimate + std.error, low = estimate - std.error)
df1$variable <- "Mean stock size"
df2 <- tidy( coeftest(fit2, vcov = vcovHC(fit2, "HC3")) ) %>% mutate(hi = estimate + std.error, low = estimate - std.error)
df2$variable <- "Mean catch"
df3 <- tidy( coeftest(fit3, vcov = vcovHC(fit3, "HC3")) ) %>% mutate(hi = estimate + std.error, low = estimate - std.error)
df3$variable <- "% rounds below threshold"

df <- dplyr::bind_rows(df1,df2,df3)
df$variable <- factor(df$variable, levels = c("Mean stock size","Mean catch","% rounds below threshold") )
levels(df$variable)

write.csv(df, file = 'Fig3_models_HC3corrected_v2.csv')

## graph
g <- ggplot(df, aes(estimate, term, xmin = low, xmax = hi, height = 0, color = variable)) +
  geom_vline(xintercept = 0, color = "grey" ) +
  geom_point(show.legend = F) +
  geom_errorbarh( show.legend = F) +
  theme_minimal(base_size = 7) +
  facet_wrap( ~ variable, scales = "free_x")

g

quartz.save(file = '170316_Schill_NCC_Fig3.png', type = 'png', dpi = 600, width = 4, height = 2)

# welch_aov = userfriendlyscience::oneway(y=df$sw_mean, x=df$Place,  posthoc="games-howell")
# round(welch_aov$intermediate$posthocTGH$output$games.howell, 3)

library(car)
vif(fit1)
library(sandwich); library(lmtest)
coeftest(fit1, vcov = vcovHC(fit1, "HC3"))
coeftest(fit2, vcov = vcovHC(fit2, "HC3"))
coeftest(fit3, vcov = vcovHC(fit3, "HC3"))



######### Following the code of the statisticians
reslm = residuals(fit1)
rafalib::mypar()
par(mfrow=c(2,2))
with(gini0, plot(gini_st1, reslm), xlab="giniSt1")
with(stockSize, plot(mean_st1, reslm), xlab="intStockSt1")

# par(mfrow=c(1,2))
boxplot(reslm ~ Place, data = stagewise, lwd = 2, ylab = 'residuals', outline=FALSE, ylim=range(reslm))
stripchart(reslm ~ Place, vertical = TRUE, data = stagewise,
           method = "jitter", add = TRUE, pch = 20, col = 'blue', cex=1.5)
boxplot(reslm ~ Treatment, data = stagewise, lwd = 2, ylab = 'residuals', outline=FALSE, ylim=range(reslm))
stripchart(reslm ~ Treatment, vertical = TRUE, data =stagewise,
           method = "jitter", add = TRUE, pch = 20, col = 'blue', cex=1.5)




######### Supplementary material
f1a <- ggplot(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Threshold", part == TRUE),
              aes(IntermediateStockSize)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Threshold", part == TRUE) %>%
               group_by(Treatment) %>%
               summarise(m = mean(IntermediateStockSize)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  geom_vline(xintercept = c(28), color = c('red'), show.legend = F)+
  ggtitle(label = 'a)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Intermediate stock size')  + ylim (c(0, 0.1)) + xlim (c(0,50))

f1b <- ggplot(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Risk", part == TRUE),
              aes(IntermediateStockSize)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Risk", part == TRUE) %>%
               group_by(Treatment) %>%
               summarise(m = mean(IntermediateStockSize)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  geom_vline(xintercept = c(28), color = c('red'), show.legend = F)+
  ggtitle(label = 'b)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Intermediate stock size') + ylim (c(0,0.1)) + xlim (c(0,50))

f1c <- ggplot(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Ambiguity", part == TRUE),
              aes(IntermediateStockSize)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Ambiguity", part == TRUE) %>%
               group_by(Treatment) %>%
               summarise(m = mean(IntermediateStockSize)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  geom_vline(xintercept = c(28), color = c('red'), show.legend = F)+
  ggtitle(label = 'c)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.2, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Intermediate stock size') + ylim (c(0,0.1)) + xlim (c(0,50))

f1d <- ggplot(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Threshold", part == TRUE),
              aes(SumTotalCatch)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Threshold", part == TRUE) %>%
               group_by(Treatment) %>%
               summarise(m = mean(SumTotalCatch)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  ggtitle(label = 'd)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.7, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Total catch') + ylim (c(0,0.2)) + xlim (c(0, 40))

f1e <- ggplot(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Risk", part == TRUE),
              aes(SumTotalCatch)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Risk", part == TRUE) %>%
               group_by(Treatment) %>%
               summarise(m = mean(SumTotalCatch)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F) +
  ggtitle(label = 'e)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.7, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Total catch') + ylim (c(0,0.2)) + xlim (c(0,40))

f1f <- ggplot(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Ambiguity", part == TRUE),
              aes(SumTotalCatch)) +
  geom_density(aes(fill = Treatment)) +
  scale_fill_manual(values = alpha(c('gold', 'blue'), 0.5)) + scale_color_manual(values = alpha(c('gold', 'blue'), 1)) +
  geom_vline(data = filter(group_dat, Treatment == "Baseline" | Treatment == "Ambiguity", part == TRUE) %>%
               group_by(Treatment) %>%
               summarise(m = mean(SumTotalCatch)),
             mapping = aes(xintercept = m, color = Treatment), show.legend = F)+
  ggtitle(label = 'f)') + theme_minimal(base_size = 5) +
  theme(legend.position = c(0.7, 0.9), legend.key.size = unit(0.2, 'cm')) +
  xlab('Total catch') + ylim (c(0,0.2)) + xlim (c(0,40))



## Combine the figure
g <- list ( f1a, f1b, f1c, f1d, f1e, f1f)

source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1:6), ncol = 3, nrow = 2, byrow = T)
multiplot(plotlist = g, layout = layout)

quartz.save(file = '170330_Schill_NCC_SMFig1.pdf', type = 'pdf', dpi = 1200,
            pointsize = 5, family = "helvetica", width = 5, height = 2.5)


#
####### Smoothed time lines for SM

g2 <- ggplot(data= group_dat,
            aes(x=Round, y=IntermediateStockSize)) +
  geom_smooth(stat='smooth', aes(color=Place, group=Place)) +
  geom_rect(xmin=6, xmax = 7, ymin = 0, ymax =50, fill = 'white') +
  ggtitle(label = 'b)') + theme_grey(base_size = 6)  #+ ylim (c(0,50)) + xlim (c(0,16))

g1 <- ggplot(data= group_dat,
            aes(x=Round, y=IntermediateStockSize)) +
  geom_smooth(stat='smooth', aes(color=Treatment, group=Treatment)) +
  geom_rect(xmin=6, xmax = 7, ymin = 0, ymax =50, fill = 'white') +
  ggtitle(label = 'a)') + theme_grey(base_size = 6) # + ylim (c(0,50)) + xlim (c(0,16))

## Combine the figure
g <- list (g1,g2 )

layout <- matrix(c(1:2), ncol = 2, nrow = 1, byrow = T)
multiplot(plotlist = g, layout = layout)

quartz.save(file = '170330_Schill_NCC_SMFig3.pdf', type = 'pdf', dpi = 1200,
            pointsize = 5, family = "helvetica", width = 7, height = 2.5)
