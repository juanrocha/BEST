## Analysis paper II
## Juan Rocha
## juan.rocha@su.se

## read data:
### load dataset
## Survey data
source('~/Documents/Projects/BEST - Beijer/BEST/160525_ErrorIdentificationSurvey.R')

#key
key <- read.csv2(file = '~/Dropbox/BEST/Colombia/Survey/key_consolidado_survey.csv', encoding = "Latin-1" )
key <- key [c(1:16,23:240),c(2:5)]
key$Name.in.datasheet <- as.character(key$Name.in.datasheet)
levels(key$Data.type)[3] <- "binary"
key <- droplevels(key)
key$Column.datasheet <- seq(1:234)

# load game data in long format, short format also available
dat <- read.csv(file="~/Dropbox/BEST/Colombia/0_Game data/160427_corrected_full_data_long.csv", row.names=1)

# Create player ID's as in Surveys.R
dat <- transform(dat, ID_player = interaction(Date, Treatment, Session, Player, drop = TRUE))
# Create ID group
dat <- transform(dat, group = interaction (Date, Treatment, Session, drop=T))
dat <- as_tibble(dat) %>%
  rename(ind_extraction = value)

# reorder levels
dat$Treatment <- factor(dat$Treatment, levels(dat$Treatment)[c(1,3,2,4)])
# levels(dat$Treatment)

dat <- mutate (dat, threshold = ifelse (dat$Treatment == "Base line" | dat$part == FALSE, 20, 28 ))
dat <- dat %>% mutate(
  dummy_threshold = ifelse(NewStockSize - threshold > 0, FALSE, TRUE))

## Use the deviation from threshold, and dev_t_divided by 4
dat <- dat %>%
  mutate (dev_drop = ifelse(dat$Treatment == 'Base line' | dat$part == FALSE,
                            ((dat$IntermediateStockSize - 20)) ,  # - dat$value
                            ((dat$IntermediateStockSize - 28))   )) #- dat$value
## here cooperation is calculated.
dat <- dat %>%
  mutate (optimal = (StockSizeBegining - threshold) / 4) %>%
  mutate (cooperation = ifelse(
    StockSizeBegining == 0, NA,
    ifelse((4*optimal) < 0 & ind_extraction == 0, 0, optimal - ind_extraction))) %>%
  mutate(cooperation2 = ifelse(
    optimal < 1 & ind_extraction == 0, 1,
    ifelse(optimal == 0 & ind_extraction == 1, 1.5,
           ifelse(optimal < 1 & ind_extraction > 0, ind_extraction, ind_extraction / optimal)
    )))

## use the demeaned extraction as a try:
dat <- dat %>%
  mutate(demean_extraction = (ind_extraction - mean(ind_extraction))/ sd(ind_extraction)) %>%
  mutate(stock_ratio = ind_extraction/StockSizeBegining)

## test of normality:
# shapiro.test(dat$cooperation2)
# interpretation: if p > 0.05 then is normal, if not, it's not normally distributed.


## coordination is calculated next
## J191030: I need to keep missing values so the coordination estimate is not biased by zeroes representing non extraction from rounds not played.
dat <- dat %>%
  mutate(ind_extraction_na = ifelse(is.na(StockSizeBegining),  NA, ind_extraction)) 
## J191023: This version uses ind_extraction wit missing values - correct.

dist_group <- function(x, datos){ # x will be the character identifier for each player
  y <- datos %>% select(ID_player, Round, ind_extraction_na, group) %>%
    filter(group == substr(x,start = 1, stop = nchar(x) - 2)) %>% # filter per group based on ID_player
    select(-group) %>% spread(Round, ind_extraction_na)
  z <- vegan::vegdist(y[-1], "bray", na.rm = TRUE) # Bray-curtis is bounded 0:1 with zero absolute similarity and 1 complete different
  player <- substr(x, start = nchar(x), stop = nchar(x)) # the player is the last number of the string
  mean_dist <- colSums(as.matrix(z))[as.numeric(player)] / 3 # divided by the other 3 players. Note the dist to self is 0
  df <- data_frame(ID_player = x, mean_dist = mean_dist)
  return(df)
}

x <- lapply(levels(dat$ID_player), datos = dat, dist_group)
x <- bind_rows(x)
x$ID_player <- as.factor(x$ID_player)
x <- mutate(x, coordination = 1-mean_dist)

## This fails because map expects all elements to be the same size
# test <-  map2(levels(dat$ID_player), dat, dist_group)
coord0 <-  lapply(levels(dat$ID_player), datos = dat %>% filter(part == FALSE), dist_group) %>%
  bind_rows() %>%
  mutate(coordination0 = 1-mean_dist)

coord1 <-  lapply(levels(dat$ID_player), datos = dat %>% filter(part == TRUE), dist_group) %>%
  bind_rows() %>%
  mutate(coordination1 = 1-mean_dist)

coord <- left_join(coord0, coord1, by = "ID_player") %>%
  left_join(x, by = "ID_player")


### For the analysis proposed by Jorge I need to get rid of missing values, set all NA to zero before calculating anything else.
dat <-  dat %>%
  replace_na(list(StockSizeBegining = 0, SumTotalCatch = 0, IntermediateStockSize = 0, Regeneration = 0, NewStockSize = 0))

# rm(y , z, player, mean_dist, df)

## J191023: One idea is to run the regression with the difference instead of the second round alone

ind_coop <- dat %>% #filter(part == TRUE) %>%
  select( Treatment, Place, ID_player, group, Round, cooperation2, part, Player) %>%
  group_by(Treatment, Place, ID_player, group, part, Player) %>%
  summarize(Cooperation = mean(cooperation2, na.rm = T),
            variance = var(cooperation2, na.rm = T),
            skewness = skewness(cooperation2, na.rm = T),
            med_coop = median(cooperation2, na.rm = T))

ind_coop_baseline <- dat %>% filter(part == FALSE) %>%
  select( Treatment, Place, ID_player, group, Round, cooperation2, part, Player) %>%
  group_by(Treatment, Place, ID_player, group, part, Player) %>%
  summarize(Cooperation_bl = mean(cooperation2, na.rm = T),
            variance_bl = var(cooperation2, na.rm = T),
            skewness_bl = skewness(cooperation2, na.rm = T),
            med_coop_bl = median(cooperation2, na.rm = T))

ind_coop <- dat %>%
  group_by(ID_player, part) %>%
  summarize(
    mean_extraction = mean(ind_extraction, na.rm = TRUE),
    mean_prop_extr = mean(stock_ratio, na.rm = TRUE),
    var_extraction = var(ind_extraction, na.rm = TRUE),
    var_prop_extr = var(stock_ratio, na.rm = TRUE)
  ) %>%
  right_join(ind_coop)

## The easiest way is vectorizing. I've tried with DB dplyr verbs but did not succed at calculating the difference within one df.

ind_coop1 <- ind_coop %>%
  filter(part == FALSE) %>%
  select(-skewness, -part) %>%
  rename(
    mean_extraction1 = mean_extraction,
    mean_prop_extr1 = mean_prop_extr,
    var_extraction1 = var_extraction,
    var_prop_extr1 = var_prop_extr,
    Cooperation1 = Cooperation,
    variance1 = variance,
    med_coop1 = med_coop
  ) %>%
  left_join(.,
            coord0 %>% 
              select(coordination1 = coordination0, -mean_dist, ID_player) ,
            by = "ID_player")

ind_coop2 <- ind_coop %>%
  filter(part == TRUE) %>%
  select(-skewness, part) %>%
  rename(
    mean_extraction2 = mean_extraction,
    mean_prop_extr2 = mean_prop_extr,
    var_extraction2 = var_extraction,
    var_prop_extr2 = var_prop_extr,
    Cooperation2 = Cooperation,
    variance2 = variance,
    med_coop2 = med_coop
  )%>%
  left_join(.,
            coord1 %>% 
              select(coordination2 = coordination1, -mean_dist, ID_player) ,
            by = "ID_player")


diff_dat <- ind_coop1 %>%
  left_join(ind_coop2) %>%
  mutate(
    mean_extraction = mean_extraction2 - mean_extraction1,
    mean_prop_extr = mean_prop_extr2 - mean_prop_extr1,
    var_extraction = var_extraction2 - var_extraction1,
    var_prop_extr = var_prop_extr2 - var_prop_extr1,
    Cooperation = Cooperation2 - Cooperation1,
    variance = variance2 - variance1,
    med_coop = med_coop2 - med_coop1,
    coordination = coordination2 - coordination1
  ) %>%
  select(-ends_with("1"), -ends_with("2"))

# summarize_at(
#   c("mean_extraction"),
#   diff
# )

## Assign diff_dat as ind_coop so the rest of the code works without changing names

# ind_coop <- diff_dat ## now the variables are the differences!

exp_notes <- as_tibble(exp_notes)
agreements <- exp_notes %>%
  select(39,98, A.round, ID_player, ID_Obs) %>%
  mutate(
    round = as.numeric(A.round),
    agreement = pmax(A11.3..agreement, B11.3..agreement, na.rm = TRUE)) %>%
  filter(round>6) %>%
  group_by(ID_player) %>%
  summarize(prop_ag = sum(agreement, na.rm = T)/10)


risk_amb <- exp_notes %>% select(119:130,132) %>% unique()

risk <- risk_amb %>% select(13,
                            Risk_0_38k = 1, Risk_13k =2, Risk_10_19k = 3,
                            Risk_7_25k = 4, Risk_4_31k = 5, Risk_2_36k = 6)

risk <- risk %>%
  mutate(Risk_0_38k = forcats::fct_recode(Risk_0_38k, NULL = '', '1' = '|')) %>% mutate(Risk_0_38k = as.numeric(as.character(Risk_0_38k))) %>%
  gather(key = Risk, value = choice, 2:7, na.rm = FALSE) %>%
  filter(choice == 1)

risk$Risk <- as.factor(risk$Risk)
levels(risk$Risk) <- c(6,2,1,5,4,3)
risk$Risk <- as.numeric(risk$Risk)

# J190219: Note that there are few people with no choice in the risk task. So one looses something here (7obs), need to go back to original data.

### J180102: There is errors also on the ambiguity elicitation task data. The group of 2016-02-09.Threshold.am all players have NAs.
amb <- risk_amb %>% select(13, Amb_0_38k = 7, Amb_13k =8, Amb_10_19k = 9, Amb_7_25k = 10, Amb_4_31k = 11, Amb_2_36k = 12)

## for the people with two choices, I leave only one manually, but note, this needs to be checked with raw data and change afterwards here to correct for the right one.
# this command shows the errors:
# amb %>% group_by(ID_player) %>% summarize(choice = sum(Amb_0_38k, Amb_13k, Amb_10_19k, Amb_7_25k ,Amb_4_31k ,Amb_2_36k)) %>% filter(choice == 0 | choice == 2 | is.na(choice))
## Manual corrections
amb[amb$ID_player == "2016-02-01.Threshold.am.2", "Amb_4_31k"] <- 0
amb[amb$ID_player == "2016-02-05.Uncertainty.am.2", "Amb_10_19k"] <- 0
amb[amb$ID_player == "2016-02-02.Base line.am.4", "Amb_10_19k"] <- 1

## note, this still keeps the NA players and they are dropped when choice == 1, but at least there is no duplicates now.
## The dataset still only has 252/256 obs, for 4 people all choices were coded as zero.
amb <- amb %>%
  gather(key = Amb, value = choice, 2:7) %>%
  filter(choice == 1)

amb$Amb <- as.factor(amb$Amb)
levels(amb$Amb) <- c(6,2,1,5,4,3)
amb$Amb <- as.numeric(amb$Amb)

n <- dim(ind_coop2)[2]

## ind_coop will be the dataframe for the regressions. Note that first I construct it with data for the second part of the game,
## then I add control variables from the survey, and last I will combine again with control variables from the first part of the game.
ind_coop <- left_join(ind_coop2, surv, by = "ID_player") %>%  ## Now drop the columns that are not useful for now in the regression
  select( c(1:n,
            life_satisfaction = 19+n, EE_before = 20+n, partner_in_group = 21+n,
            fishing_age=25+n,fishing_last_yr = 29+n, week_days = 43+n, ND_hrs = 44+n, ND_kg = 45+n, ND_pesos =46+n,
            BD_kg = 49+n, BD_pesos = 50+n, BD_how_often = 51+n, group_fishing = 52+n, boat = 58+n,
            take_home= 84+n, sale= 85+n, give_away = 87+n,
            fishing_future = 88+n, fishing_children=90+n, history_rs = 96+n,  sharing_art=137+n,
            belongs_coop=139+n, age=157+n, education = 158+n, education_yrs=159+n))

ind_coop$BD_how_often[is.na(ind_coop$BD_how_often)] <- 0

ind_coop$ID_player <- as.character(ind_coop$ID_player)
risk$ID_player <- as.character(risk$ID_player)
amb$ID_player <- as.character(amb$ID_player)
x$ID_player <- as.character(x$ID_player)

ind_coop <- left_join(ind_coop, x, by = "ID_player")

ind_coop <- left_join(ind_coop, select(risk, 1,2), by = "ID_player")


### here is the error now
ind_coop <- left_join(ind_coop, select(amb, 1,2), by = "ID_player")

## log-transform money related variables

ind_coop <- mutate(ind_coop, ND_log_pesos = log(ND_pesos), BD_log_pesos = log1p(BD_pesos))



## correct education, there is two variables with the same info, unify to avoid missing values:

ind_coop <- ind_coop %>%
  mutate(education_yr = ifelse(
    !is.na(education_yrs), education_yrs,
    ifelse(
      education == 1, 0,
      ifelse(
        education == 2, 5,
        ifelse(education == 3, 11, 16)
      )
    )
  ))

ind_coop <- ind_coop %>%
  left_join(agreements)

## correct fishing_children:

ind_coop <- ind_coop %>%
  mutate(
    fishing_children = ifelse(
      is.na(fishing_children), 0,
      ifelse(
        (fishing_children < 2 | fishing_children == 4), 0, 1
      )
    )
  )

## correct fishing_cfuture:
## Dropping fishing future, it has too many missing values:
## ind_coop$fishing_future %>% table()
## and the coding reveals that 4 was missing values. I don't know if code that as "don't know" or real 
## missing values given that we also have NAs in the dataset.
## ind_coop$fishing_future %>% table(useNA = "always")
## 
## attempt with sharing art
# levels(surv[,64]) # ask LMS


# surv %>%
#   select(group_fishing_partner = 21, fishing_group = 52, art = 64, fishing_future=88,
#          same_spp = 92, dramatic_changes = 96, share_art = 137, coop = 139) %>%
#   mutate(art = as.character(art)) %>%
#   skimr::skim()
#   #pull(coop) %>% unique()
