### Regression exploration of panel data
### Juan Rocha
### juan.rocha@su.se
### 20171228

## load libraries
rm(list = ls())
set.seed(12345)
library(tidyverse)
library(network)
library(stringr)
library(forcats)
library(broom)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(GGally)
library(moments)

library(ggmap)
library(maptools)
library(maps)
library(mapproj)

library(grid)
library(gridExtra)
library(plm)


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
dat <- transform (dat, ID_player = interaction(Date, Treatment, Session, Player, drop = TRUE))
# Create ID group
dat <- transform(dat, group = interaction (Date, Treatment, Session, drop=T))
dat <- as_tibble(dat)

# reorder levels
dat$Treatment <- factor(dat$Treatment, levels(dat$Treatment)[c(1,3,2,4)])
# levels(dat$Treatment)

dat <- mutate (dat, threshold = ifelse (dat$Treatment == "Base line" | dat$part == FALSE, 20, 28 ))

## Use the deviation from threshold, and dev_t_divided by 4
dat <- dat %>%
  mutate (dev_drop = ifelse(dat$Treatment == 'Base line' | dat$part == FALSE,
                                ((dat$IntermediateStockSize - 20)) ,  # - dat$value
                                 ((dat$IntermediateStockSize - 28))   )) #- dat$value
## here cooperation is calculated.
dat <- dat %>%
  mutate (optimal = (StockSizeBegining - threshold) / 4) %>%
  mutate (cooperation = optimal - value)



## Play with regressions: inspiration is brought from previous file 161012_regressions

ols <- lm(cooperation ~ Treatment + Place, data = filter (dat, part == TRUE))
summary(ols)
plot(ols)

## anova
anova(ols)
aov.mod <- aov(cooperation ~ Treatment + Place, data = filter (dat, part == TRUE))
tuk <- TukeyHSD(aov.mod)

## Figure 1 draft (for SM make same figure with part == F)
g1 <- ggplot(data = dat %>%
    group_by(Treatment) %>% filter(part == TRUE) %>%
    summarize(avg = mean(cooperation, na.rm = TRUE)),
    aes(y=avg, x=Treatment)) + geom_col(alpha = 0.75) +
    geom_text(aes(label = round(avg, 2)), hjust = "inward", size = 2)+
    coord_flip() + ggtitle("a") +
    ylab("mean cooperation") + theme_light(base_size = 6)
    #geom_errorbar(aes(ymin = low, ymax = high), width = .25)

g2 <- ggplot(data = dat %>%
    group_by(Place) %>% filter(part == TRUE) %>%
    summarize(avg = mean(cooperation, na.rm = TRUE)),
    aes(y=avg, x=Place)) + geom_col(alpha = 0.75) +
    geom_text(aes(label = round(avg, 2)), hjust = "inward", size = 2)+
    coord_flip() + ggtitle("b") +
    ylab("mean cooperation") + theme_light(base_size = 6)
    #geom_errorbar(aes(ymin = low, ymax = high), width = .25)

g3 <- ggplot(
    data = dat %>% group_by(Treatment) %>% filter(part == TRUE), aes(Treatment, cooperation)) + geom_boxplot(alpha = .5, fill = "grey", notch = TRUE) +
    # inset(
    #     grob = ggplotGrob(g0a),
    #     ymin = -14.5, ymax = -7.5, xmin = 0.5, xmax = 1.95
    # ) +
    coord_flip() + ggtitle("c") + theme_light(base_size = 6)

g4 <- ggplot(
    data = dat %>% group_by(Place) %>% filter(part == TRUE), aes(Place, cooperation)) + geom_boxplot(alpha = .5, fill = "grey", notch = TRUE) +
    # inset(
    #     grob = ggplotGrob(g0b),
    #     ymin = -14, ymax = -6, xmin = 3.1, xmax = 4.5
    # ) +
    coord_flip() + ggtitle("d")+ theme_light(base_size = 6)


g5 <- ggplot(data = tidy(tuk) %>% filter(term == "Treatment"),
 aes(x=estimate, y=comparison)) +
    geom_point(aes(shape = ifelse(adj.p.value < 0.05, "< 0.05" , "> 0.05")), size = 2, show.legend = FALSE) +
    scale_shape_manual(name = "p value", values = c(19,1)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = .25)) +
    geom_vline(xintercept = 0, color = "grey") + ggtitle("e")+ theme_light(base_size = 6)
    # + facet_wrap(~term)


g6 <- ggplot(data = tidy(tuk) %>% filter(term == "Place"),
 aes(x=estimate, y=comparison)) +
    geom_point(aes(shape = ifelse(adj.p.value < 0.05, "< 0.05" , "> 0.05")), size = 2, show.legend = FALSE) +
    scale_shape_manual(name = "p value", values = c(19,1)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = .25)) +
    geom_vline(xintercept = 0, color = "grey") + ggtitle("f")+ theme_light(base_size = 6)
    # + facet_wrap(~term)

quartz(height = 2.5, width = 6)
## Combine the figure old way
gg <- list(g1,g2,g3,g4,g5,g6)
source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1:6), ncol = 3, nrow = 2, byrow = F)
multiplot(plotlist = gg, layout = layout)



## fixed effects with dummy variables
fixed.dum <- lm(cooperation ~ Treatment + Place + ID_player +  mean_dist, data = filter (df, part == TRUE) )
summary(fixed.dum)


## pooling models (pooled ols):
out <- list()
out[[1]] <- plm(cooperation ~  0 + Treatment + Place , data = filter(df, part == T), index = c('ID_player' ,'Round'), model = "pooling", effect = "individual")

out[[2]] <- plm(cooperation ~  0 +Treatment + Place + mean_dist , data = filter(df, part == T), index = c('ID_player' ,'Round'), model = "pooling", effect = "individual")

out[[3]] <- plm(cooperation ~  0 +Treatment + Place + mean_dist + lag(StockSizeBegining, 0:4) +
age + fishing_age  + sale + take_home + life_satisfaction + ND_log_pesos + BD_log_pesos + week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children + history_rs + education + Risk + Amb, data = filter(df, part == T), index = c('ID_player' ,'Round'), model = "pooling", effect = "individual")

out[[4]] <- plm(cooperation ~  0 +Treatment + Place + mean_dist + lag(StockSizeBegining, 0:4) + age + fishing_age  + sale + take_home + life_satisfaction + ND_log_pesos + BD_log_pesos + week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children + history_rs + education + Risk + Amb + group, data = filter(df, part == T), index = c('ID_player' ,'Round'), model = "pooling", effect = "individual")

df_list <- lapply(out, function(x){tidy(coeftest(x, vcov = vcovDC))})
for (i in 1:4){df_list[[i]]$model <- paste("model", i, sep = " ")}
df_list <- bind_rows(df_list) %>% as_tibble()

# df_list
df_r2 <- data_frame(
    model = c("model 1", "model 2", "model 3", "model 4"),
    r_squared = unlist(lapply(out, plm::r.squared))
)

g <- ggplot(df_list %>% mutate(term = as_factor(term) %>% fct_rev()) %>% filter(!stringr::str_detect(term, "group")) , aes(estimate, term, group = model)) +
    geom_vline(xintercept = 0, color = "grey84", linetype = 2) +
    geom_point(aes(shape = ifelse(
        p.value < 0.05, "< 0.05" ,
            ifelse(p.value < 0.1, "< 0.1", "> 0.1")
        )), size = 2, show.legend = TRUE) +
    scale_shape_manual(name = "p value", values = c(19,7,1)) +
    geom_errorbarh(aes(xmin = estimate - std.error , xmax = estimate + std.error, height = .25))+
    geom_text(data = df_r2, x=-Inf, y=Inf, hjust = 0, vjust = 45, size = 3, aes(label = paste("italic(R) ^ 2 == ", round(r_squared,2))), parse = T) + theme_light(base_size = 6)+ theme(legend.position = "bottom")+
    facet_wrap(~model, ncol = 4, scales = "free_x")

g

waldtest(out[[3]], out[[4]], vcov = vcovNW) # shows that the group term is indeed significant and necessary, despite it only increases 3% on R^2

# pgmm(cooperation ~ Treatment + Place + lag(StockSizeBegining, 0:1) | lag(cooperation, 2:10) , data = filter(dat, part == T), index = c('ID_player' ,'Round')) %>% summary()
# this model doesn't work, it throughs error. The explanation is available here, but in short, the rhs does not change over time.
# https://stackoverflow.com/questions/11404141/problems-with-within-and-random-models-in-plm-package

fix0 <- lm(cooperation ~ group, data = filter(dat, part == TRUE))
fix1 <- lm(cooperation ~ Treatment + Place, data = filter(dat, part == TRUE))
fix2 <- lm(cooperation ~ Treatment + Place + Round, data = filter(dat, part == TRUE))
fix3 <- lm(cooperation ~ Treatment + Place + Round + group, data = filter(dat, part == TRUE))
fix4 <- lm(cooperation ~ Treatment + Place + group, data = filter(dat, part == TRUE))

## the first part of the paper should describe the main treatment and place differences: there is a treatment and place effect, and it's significant after controling for group and round. Key result, people in general cooperate, People in treatments approaches but don't cross the threshold (in average), and when crossed, our data shows that the magnitude of crossing are not sufficient (in average) to be classified as defeators.

## Now the next question is what makes people cooperate? is there individual attributes other than the place where they live? - this should be the second round of analysis where we use survey data and risk / ambiguity elicitation tasks.

## first you need to aggregate the data in summary over time and add the elicitation task data:


dist_group <- function(x){ # x will be the character identifier for each player
  y <- dat %>% select(ID_player, Round, value, group) %>%
    filter(group == substr(x,start = 1, stop = nchar(x) - 2)) %>% # filter per group based on ID_player
    select(-group) %>% spread(Round, value)
  z <- vegan::vegdist(y[-1], "bray") # Bray-curtis is bounded 0:1 with zero absolute similarity and 1 complete different
  player <- substr(x, start = nchar(x), stop = nchar(x)) # the player is the last number of the string
  mean_dist <- colSums(as.matrix(z))[as.numeric(player)] / 3 # divided by the other 3 players. Note the dist to self is 0
  df <- data_frame(ID_player = x, mean_dist = mean_dist)
  return(df)
}

x <- lapply(levels(dat$ID_player), dist_group)
x <- bind_rows(x)
x$ID_player <- as.factor(x$ID_player)

# rm(y , z, player, mean_dist, df)

ind_coop <- dat %>% #filter(part == TRUE) %>%
  select( Treatment, Place, ID_player, group, Round, cooperation, part, Player) %>%
  group_by(Treatment, Place, ID_player, group, part, Player) %>%
  summarize(Cooperation = mean(cooperation, na.rm = T),
            variance = var(cooperation, na.rm = T),
            skewness = skewness(cooperation, na.rm = T),
            med_coop = median(cooperation, na.rm = T))

exp_notes <- as_tibble(exp_notes)
risk_amb <- exp_notes %>% select(119:130,132) %>% unique()

risk <- risk_amb %>% select(13,
    Risk_0_38k = 1, Risk_13k =2, Risk_10_19k = 3,
    Risk_7_25k = 4, Risk_4_31k = 5, Risk_2_36k = 6)

risk <- risk %>%
    mutate(Risk_0_38k = forcats::fct_recode(Risk_0_38k, NULL = '', '1' = '|')) %>% mutate(Risk_0_38k = as.numeric(as.character(Risk_0_38k))) %>%
    gather(key = Risk, value = choice, 2:7) %>%
    filter(choice == 1)

risk$Risk <- as.factor(risk$Risk)
levels(risk$Risk) <- c(6,2,1,5,4,3)
risk$Risk <- as.numeric(risk$Risk)

### J180102: There is errors also on the ambiguity elicitation task data. The group of 2016-02-09.Threshold.am all players have NAs.
amb <- risk_amb %>% select(13, Amb_0_38k = 7, Amb_13k =8, Amb_10_19k = 9, Amb_7_25k = 10, Amb_4_31k = 11, Amb_2_36k = 12)

## for the people with two choices, I leave only one manually, but note, this needs to be checked with raw data and change afterwards here to correct for the right one.
# this command shows the errors:
amb %>% group_by(ID_player) %>% summarize(choice = sum(Amb_0_38k, Amb_13k, Amb_10_19k, Amb_7_25k ,Amb_4_31k ,Amb_2_36k)) %>% filter(choice == 0 | choice == 2 | is.na(choice))
## Manual corrections
amb[amb$ID_player == "2016-02-01.Threshold.am.2", "Amb_4_31k"] <- 0
amb[amb$ID_player == "2016-02-05.Uncertainty.am.2", "Amb_10_19k"] <- 0
amb[amb$ID_player == "2016-02-02.Base line.am.4", "Amb_10_19k"] <- 1

## note, this still keeps the NA players and they are dropped when choice == 1, but at least there is no duplicates now.

amb <- amb %>%
  gather(key = Amb, value = choice, 2:7) %>%
  filter(choice == 1)

amb$Amb <- as.factor(amb$Amb)
levels(amb$Amb) <- c(6,2,1,5,4,3)
amb$Amb <- as.numeric(amb$Amb)

ind_coop <- left_join(ind_coop, surv, by = "ID_player") %>%  ## Now drop the columns that are not useful for now in the regression
  select( c(1:21, life_satisfaction = 29, EE_before = 30, partner_in_group = 31,
            fishing_age=35,fishing_last_yr = 39, week_days = 53, ND_hrs = 54, ND_kg = 55, ND_pesos =56,
            BD_kg = 59, BD_pesos = 60, BD_how_often = 61, group_fishing = 62, boat = 68,
            take_home= 94, sale= 95, give_away = 97,
            fishing_future = 98, fishing_children=100, history_rs = 106,  sharing_art=147,
            belongs_coop=149, age=167, education = 168, education_yrs=169 ))

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

ind_coop <- mutate(ind_coop, ND_log_pesos = log(ND_pesos), BD_log_pesos = log(1+BD_pesos))

# Interesting cols with too many NAs: yrs_living_twon=169, education_yrs=164, rs_when=103,GF_how_many = 59,   GF_how_often = 58, BD_how_often =56,
ols <- list()
for (i in 1:4){
   ols[[i]] <- lm(Cooperation ~ age + fishing_age  + sale + take_home + life_satisfaction + ND_log_pesos + BD_log_pesos
                  + week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children
                  + history_rs +Risk + Amb + Treatment ,
                  data = filter(ind_coop, Player == i, part == TRUE)) # filter(ind_coop, playerNo == 4)
}

ols[[5]] <- lm(Cooperation ~  age + fishing_age  + sale + take_home + life_satisfaction + ND_log_pesos + BD_log_pesos
                  + week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children
                  + history_rs +Risk +mean_dist + education + Amb + Treatment ,
               data = filter(ind_coop, part == TRUE))

## Remember that you should not use Place and socio-economic variables at the same time without checking if they are correlated. For example, if people in las flores is less educated, putting education and place can vanish the effect of education being taken over by place.

 df <- left_join(dat, ind_coop)

ols <- list()

ols[[1]] <- lm(Cooperation ~ age + fishing_age  + sale + take_home +
        life_satisfaction + ND_log_pesos + BD_log_pesos +
        week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children + history_rs + education +
        Risk + Amb ,
        data = filter(ind_coop, part == TRUE))
ols[[2]] <- lm(med_coop ~ age + fishing_age  + sale + take_home +
        life_satisfaction + ND_log_pesos + BD_log_pesos +
        week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children + history_rs + education +
        Risk + Amb ,
        data = filter(ind_coop, part == TRUE))

ols[[3]] <- lm(variance ~ age + fishing_age  + sale + take_home +
        life_satisfaction + ND_log_pesos + BD_log_pesos +
        week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children + history_rs + education +
        Risk + Amb ,
        data = filter(ind_coop, part == TRUE))

ols[[4]] <- lm(mean_dist ~ age + fishing_age  + sale + take_home +
        life_satisfaction + ND_log_pesos + BD_log_pesos +
        week_days + ND_hrs + BD_how_often + group_fishing + boat + sharing_art + fishing_children + history_rs + education +
        Risk + Amb ,
        data = filter(ind_coop, part == TRUE))

lapply(ols, summary)

ols <- lapply(ols, tidy)

ols[[1]]$resp <- "mean cooperation"
ols[[2]]$resp <- "median cooperation"
ols[[3]]$resp <- "variance"
ols[[4]]$resp <- "coordination"


df_coop <- bind_rows(ols)

g <- ggplot(df_coop %>% mutate(term = as_factor(term) %>% fct_rev()) , aes(estimate, term, group = resp)) +
    geom_vline(xintercept = 0, color = "black", linetype = 2) +
    geom_point(aes(shape = ifelse(p.value < 0.05, "< 0.05" , "> 0.05")), size = 2, show.legend = TRUE) +
    scale_shape_manual(name = "p value", values = c(19,1)) +
    geom_errorbarh(aes(xmin = estimate - std.error , xmax = estimate + std.error, height = .25)) +
    facet_wrap(~resp, ncol = 4, scales = "free_x")

ggpairs()

fit <- lm(variance ~  Risk + Amb +
    age + fishing_age  + sale + take_home +
    life_satisfaction + ND_log_pesos + BD_log_pesos + week_days + ND_hrs +
    BD_how_often + group_fishing + boat + sharing_art + fishing_children +
    history_rs + mean_dist + Place + Treatment ,
    data = filter(ind_coop, part == TRUE))

summary(fit)

fit0 <- lm(cooperation ~ Treatment + mean_dist,
    data = filter(df, part == TRUE)
)

summary(fit0)

ind_coop %>% ggplot(aes(x=mean_dist, y = Cooperation)) + geom_point(aes(shape = Treatment, color = Place)) + geom_density2d()


### test of ordination methods and clustering analysis

library(vegan)

coop_dat <- dat %>% select(ID_player, Round, cooperation) %>% spread(., Round, cooperation) %>% skimr::skim()

# players <- (reshape::cast(dat, ID_player ~ Round))[,-1] #delete playersID
# place <- reshape::cast(dat, ID_player ~  Place)[,-1] # 16 because calculate length of 'value'
# group <- reshape::cast(dat, ID_player ~  group)[,-1]
# treat <- reshape::cast(dat, ID_player ~  Treatment)[,-1]
# context <- cbind(place,treat) # don't use group yet, maybe for aes

# pca <- rda(players, context)
# mds <- metaMDS(players, dist= 'manhattan', trymax=400, autotransform=F, k=2)
#
# ef1 <- envfit(mds, context, permu=999)


pca <- rda(ind_coop %>% ungroup() %>% select(Cooperation, variance, mean_dist),
    ind_coop %>% ungroup() %>% select(22:29, 31,33:45,48:51), na.option = na.omit)

mds <- metaMDS(
    ind_coop %>% ungroup() %>% filter(part == T) %>% select(Cooperation, variance, mean_dist),
    dist= 'manhattan', trymax=400, autotransform=F, k=2
)

ef1 <- envfit(
    mds,
    ind_coop %>% ungroup() %>% select(22:29, 31,33:45,48:51),
    na.rm = TRUE, permu=999)

par(mfrow=c(1,3))
  plot(pca)
  plot(mds, type='p', display=c('sites', 'species'), cex=0.8)
  plot(ef1, p.max=0.05, col='blue', cex=0.8)
  stressplot(mds)

library(factoextra)

pca <- PCA(ind_coop %>% ungroup() %>% filter(part == T) %>% select(Cooperation, variance, mean_dist,22:29, 31,33:45,48:51), graph = T) %>% suppressWarnings()

pca <- PCA(ind_coop %>% ungroup() %>% filter(part == T) %>% select(-ID_player, - part, -Player, -skewness, -med_coop, -date, -locationName, -locationNo, -dayOne, -dayTwo, -am, -pm, -treatmentName, -experimenterName, -experimenterNo,-groupID, -education, -belongs_coop, -education_yrs, -group, -Place), quali.sup = 1)

fviz_pca_biplot(pca, habillage=ind_coop$Place, repel=TRUE, label="var", addEllipses=TRUE) + xlab("PC 1 82.602 %") + ylab("PC 2 17.398 %")

knitr::kable(sweep(pca$var$coord[,1:2],2,sqrt(pca$eig[1:ncol(pca$var$coord),1])[1:2],FUN="/"))


### magic command to get missing values
ind_coop %>% ungroup() %>% filter(part == T) %>% select(ID_player,Cooperation, variance, mean_dist,22:29, 31,33:45,48:51) %>% group_by(ID_player) %>% gather(key = var, value, 2:30) %>% ungroup() %>% group_by(var)%>% summarize(nas = sum(is.na(value))) %>% arrange(desc(nas))
