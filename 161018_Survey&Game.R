


## Load survey data
# Instead of sourcing the file, run all the lines except the last one with the command unique. -Line 260
#source('~/Documents/Projects/BEST - Beijer/BEST/160525_ErrorIdentificationSurvey.R')


## load game data


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
dat <- transform (dat, ID_obs = interaction (Date, Treatment, Session, Player, Round, drop = TRUE))
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


dat <- mutate (dat, crossThreshold = ifelse(dat$Treatment == 'Base line' | dat$part == FALSE, 
                                            dat$IntermediateStockSize - 20, 
                                            dat$IntermediateStockSize - 28))

dat <- mutate (dat, threshold = ifelse (dat$Treatment == "Base line" | dat$part == FALSE, 20, 28 ))

# dat <- mutate (dat, crossThreshold = StockSizeBegining - 28)

## Use the deviation from threshold, and dev_t_divided by 4

dat <- dat %>%
  mutate (dev_drop = ifelse(dat$Treatment == 'Base line' | dat$part == FALSE,
                            ((dat$IntermediateStockSize - 20)) ,  # - dat$value
                            ((dat$IntermediateStockSize - 28))   )) #- dat$value

dat <- dat %>%
  mutate (optimal = (StockSizeBegining - threshold) / 4) %>%
  mutate (cooperation = optimal - value)

# create dummies for Treatments 0 for rounds 1-6, 1 for 7-16, and 1 for all base line.
dat <- dat %>% 
  mutate (BL = ifelse (dat$Treatment == "Base line" | dat$part == FALSE , 1,0) ,
          TR = ifelse (dat$Treatment == 'Threshold' & dat$part == TRUE, 1, 0) , 
          U = ifelse (dat$Treatment == 'Uncertainty' & dat$part == TRUE, 1, 0) , 
          R = ifelse (dat$Treatment == 'Risk' & dat$part == TRUE, 1, 0) 
  )

### Group level data
group_dat <- dat %>%
  select (Treatment, Place, group, Round, StockSizeBegining, IntermediateStockSize, 
          Regeneration, NewStockSize, part, BL,TR,U,R) %>%
  unique ()


# check that ID's are equal in both datasets
levels(dat$ID_player) %in% levels(surv$ID_player) ## all true
levels(surv$ID_player) %in% levels(dat$ID_player) ## all true

## Extract only what you need from survey from now
surv.dat <- select (surv, c(1:24, 371:374))
dim(surv.dat)

# Now you can join them and use both datasets for stats!!
full <- full_join(dat, surv.dat, by= c('ID_player' = 'ID_player', 'Session' = 'Session',
                                        'Date' = 'date', 'Place' = 'locationName', 'Round'='round', 'ID_obs' = "ID_Obs"))
str(full)
full$indGuessStockSize [full$indGuessStockSize > 50] <- NA

### Let the fun begin

g <- ggplot( dat = full, aes (indGuessStockSize, NewStockSize)) + 
  geom_point( aes(color = group), show.legend = F) +
  facet_grid(Treatment ~ Place)

hist(full$indGuessStockSize)

full <- mutate (full, coop_exp = ( NewStockSize +1) / (indGuessStockSize +1) )

g <- ggplot( dat = full, aes ( Round, coop_exp)) + 
  geom_line( aes(alpha = 0.5, color = ID_player, group = ID_player), show.legend = F) +
  facet_grid(Treatment ~ Place)


group_dat <- left_join( group_dat,
                        full %>% group_by (group, Round) %>%
                          summarise( mean_coop = mean (coop_exp, na.rm = T),
                                     var_coop = var(coop_exp, na.rm = T)),
                        by = c('group' =  'group', 'Round' = 'Round'))

g <- ggplot( dat = group_dat, aes ( mean_coop)) + 
  geom_density( aes(alpha = 0.5, color = group, group = group), show.legend = F) +
  facet_grid(Treatment ~ Place)
