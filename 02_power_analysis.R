# load libraries and read data first (scipts 00, 01)

dat %>% skimr::skim()

# Finkbeiner is the only paper with a similar design as ours, but do not report variances. 
# They report an experiment with 6 treatments and N = 180 participants. No power analysis reported.
# Maldonado et al PlosONE does not report SD either. In addition, most of the papers with a similar
# design as our paper report group level stats, not individuals. Only Lindahl report average and variance of earnings.

## From our raw data:
dat %>% select(ind_extraction_na, Treatment) %>%
  group_by(Treatment) %>% 
  #summarize(var = var(ind_extraction_na, na.rm = TRUE))
  summarize(x = mean(ind_extraction_na, na.rm = TRUE)) %>%
  pull(x) %>% var()

var(dat$ind_extraction_na, na.rm = TRUE) # 2.838112

## using the observed treatments:
power.anova.test(
  groups = 4, # treatments
  between.var = 0.02783727, # alternative the variance of the variance within groups 0.86
  within.var = 4.15, # 
  sig.level = 0.05,
  power = 0.8
)

## So the sample size should be min 5 with low within variance, and min 8 with high within variance


## for a two sample example:
power.t.test(
  n = NULL,
  delta = 1,
  sd = 1.68, # from our data: sd(dat$ind_extraction_na, na.rm = T)
  sig.level = 0.05,
  power = 0.9,
  alternative = "two.sided"
)

## suggest that one needs 60 people if there was only one treatment.


## Power with our data: 0.18 but this is taking into account the variability over time, which is not independent.
power.anova.test(
  groups = 4, # treatments
  between.var = 0.02783727, # variance of the group means
  within.var = 2.838112, # min is 2
  sig.level = 0.05,
  n = 64 # number of observations per group
)


### Our data is however nested: 16 rounds * 256 individuals.and dependent over time
df_nested <- dat %>%
  group_by(ID_player) %>%
  summarize(
    mean_extraction = mean(ind_extraction_na, na.rm = TRUE),
    mean_prop_extr = mean(stock_ratio, na.rm = TRUE),
    var_extraction = var(ind_extraction_na, na.rm = TRUE),
    var_prop_extr = var(stock_ratio, na.rm = TRUE)
  ) %>%
  mutate(key = as.character(ID_player)) %>%
  separate(col = key, into = c("date", "treatment", "session", "player_num"), sep = "[\\.]") 

df_nested %>% 
  dplyr::select(-date, -session, -player_num) %>% # pull(mean_extraction) %>% var()
  group_by(treatment) %>%
  summarize(meanx = mean(mean_extraction)) %>% pull(meanx) %>% var()

# Power: 0.7439908
  power.anova.test(
    groups = 4, # treatments
    between.var = 0.04382807, # variance of the group means
    within.var = 0.859434, # df_nested %>% pull(mean_extraction) %>% var()
    sig.level = 0.05,
    n = 64 # number of observations per group
  )

# The time variability hinders power, but when calcualitng power again taking into account the nested desing
# that is, averaging over time per individual, power goes up. It is not as high as one would like (>0.8), but not bad
# either. It makes sense to average over time because the decisions are not time-independent, 
# what happens in the past influence the future
# 
# Using the data from the lab experiment: email from Caroline 15 April, note she calculated SD but variance is sd^2
# Power: 0.7439908
  power.anova.test(
    groups = 4, # treatments
    between.var = var(c(2.591951, 2.818955, 2.566607, 3.029356)),
    within.var = 1.078872^2, # variance of the group means var = sd^2 
    sig.level = 0.05,
    #n = 87 # number of observations per group: In Schill 2015 there is different
    # number of ind assigned to treatments from 70, 73, 77, 87
    n = NULL,
    power = 0.9
  )

# Caroline's paper has a power (1 - type II error probability) of 0.67 when using 70 as groups, and 0.78 when using 87. The appropriate number of groups for their variances was from 90 (power 0.8) to 117 (power 0.9)