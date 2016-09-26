## Testing models
## Ideas from meeting with Matias June 7-8 2016
## Notes: we can run the analysis at the individual level or group level
## for group level we need to summarize / agregate the data

library(ggplot2)
library(dplyr)



setwd("~/Dropbox/BEST/Colombia/0_Game data") # here is the data
dat <- read.csv(file="~/Dropbox/BEST/Colombia/0_Game data/160427_corrected_full_data_long.csv", row.names=1) # in long format, short format also available

## Some transformations first:
# dat <- mutate(dat, crossThreshold = IntermediateStockSize - 28)

dat <- mutate (dat, crossThreshold = ifelse(dat$Treatment == 'Base line' | dat$part == FALSE, 
                                            dat$IntermediateStockSize - 20, 
                                            dat$IntermediateStockSize - 28))

dat <- mutate (dat, threshold = ifelse (dat$Treatment == "Base line" | dat$part == FALSE, 20, 28 ))

# dat <- mutate (dat, crossThreshold = StockSizeBegining - 28)


str(dat)
summary(dat)

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
  mutate (BL = ifelse (dat$Treatment != "Base line" , 0,1) ,
          TR = ifelse (dat$Treatment == 'Threshold' & dat$part == TRUE, 1, 0) , 
          U = ifelse (dat$Treatment == 'Uncertainty' & dat$part == TRUE, 1, 0) , 
          R = ifelse (dat$Treatment == 'Risk' & dat$part == TRUE, 1, 0) 
          )

## verify by filtering
bl <- dat %>%
  filter (Treatment == 'Base line') %>% 
  mutate (deviation = IntermediateStockSize - 20) # change 28 to 20 depending if you use treatment or base line

test <- dat %>% 
  filter (Treatment == 'Base line') %>%
  select (dev_drop)

# test is correct, the mean should be 1 and only 1, meaning all values is TRUE
mean(bl$deviation == test)
rm('bl','test') # remove the objects from the test

dat.i <- select(dat, ID_player, Round, value, group, Treatment, Place, Session, Date, Player, StockSizeBegining, SumTotalCatch
                , IntermediateStockSize, Regeneration, NewStockSize, part, dev_threshold) # group is the key nested structure
rm(dat.g)
#correct spelling
names(dat)[16] <- 'dev_threshold'

## modeling libraries
# install.packages(pkgs =  'apsrtable')

ols <- lm(dev_threshold ~ Treatment + Place, data = dat)
summary (ols) # ordinary least squares regression does not consider heterogeneity across groups or time
plot(ols)
anova (ols)

mod <- aov(dev_threshold ~ Treatment + Place, data = dat)
summary (mod)

## Fixed effects using least squares dummy variable model

fixed.dum <- lm(dev_drop ~ Treatment + factor (Place) - 1, data = dat ) # one needs to rest 1 to the factors (?)
summary(fixed.dum)

# fixed effect with plm
library (plm)

fixed <- plm (dev_drop ~ BL + U + R + TR , data = dat, index = c('ID_player' ,'Round'), model = 'within')
summary (fixed)

random <- plm (dev_drop ~  BL + U + R + TR , data = dat , index = c('ID_player' ,'Round'), model = 'random') #  , effect = 'time'  index = c('Round')
summary (random) # if effect = 'time' then it works, but not with other options... Treatment does not vary over time!

# fixed or random
phtest(fixed, random)


## modes
hist(dat$dev_threshold)
hist (dat$crossThreshold)
summary(diff(dat$Treatment))

###
p <- ggplot (data = dat %>% group_by(group, Round, Place, Treatment) %>% summarize ( mean_ext = mean(value))
             , aes(x = Round, y = mean_ext, group = group)) +
  geom_smooth(stat='smooth', aes(color=group, group=group), show.legend = F)
 p + facet_grid(Treatment ~ .)
p


### figure for Caro

g <- ggplot ( data = dat , aes(Round, dev_drop, group = group)) +
  geom_hline(yintercept = 0, color = 'grey', show.legend = FALSE) +
  geom_vline (xintercept = 6, color = 'grey', show.legend = FALSE) +
  geom_line(aes(color = group), show.legend = FALSE) +
  facet_grid(Place ~Treatment )

g


g <- ggplot(dat, aes(Treatment, SumTotalCatch)) + # SumTotalCatch
  #geom_jitter(width = 0.2, aes(color = Place, alpha = 0.2), show.legend = FALSE) +
  geom_boxplot(aes(alpha = 0.1))  +
  facet_grid( . ~ part) + ggtitle ('Sum Total Catch per Treatment')

g
