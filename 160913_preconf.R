## 160913

# load data by running lines 0-105 in AnalysisBEST.R

## Contours works fine for stability landscapes for now.

c <- ggplot(data = d, aes(y=NewStockSize, x=StockSizeBegining)) + 
  stat_density_2d(aes(color=group, alpha=0.5), n=100, h=15, show.legend = F) + 
  facet_grid( Treatment ~ Place) + ggtitle ('After treatment')
c

quartz.save('GeneralSummary_contours_groups_R7-16.png', type='png', width= 7, height = 7, dpi = 200)


### test for differences between treatmets at group level

wt <- wilcox.test(x=filter(group_dat, Treatment == 'Base line')$IntermediateStockSize, 
                  y=filter(group_dat, Treatment == 'Uncertainty')$IntermediateStockSize)



kruskal.test( IntermediateStockSize ~ Treatment, data=group_dat)
kruskal.test( IntermediateStockSize ~ Place, data=group_dat)

mod <- aov(IntermediateStockSize ~ Treatment, data = group_dat)
summary (mod)
mod <- lm (IntermediateStockSize ~ Treatment, data = group_dat)

# look what I found!
pwt <- pairwise.wilcox.test(x= group_dat$IntermediateStockSize, 
                            g =  group_dat$Treatment, paired = T, p.adj = 'bonf')
summary(pwt)
pwt <- pairwise.wilcox.test(x= group_dat$IntermediateStockSize,
                            g =  group_dat$Place, paired = T, p.adj = 'bonf')

d <- group_dat %>% filter( part == 1)
pwt <- pairwise.wilcox.test ( x=d$IntermediateStockSize, g = d$Treatment, paired =T,p.adj  = 'bonf')


pm <- ggpairs(data= dat, 
              columns=c( 'cooperation', 'Treatment','Place'), 
              upper= list(continuous='density'), lower=list(continuous='points'), 
              mapping=aes(color= Treatment, alpha=0.5), title='Treatment effects') 

pm   + theme(text= element_text(family='Helvetica', size=8)) 

quartz.save('160920_IndTreatmentEffects_2.png', type='png', width = 7, height = 7, dpi =300)

# 
# ggplot(data = dat, aes(cooperation),  group = group_by(group)) + geom_boxplot( alpha = 0.3, show.legend = FALSE) + 
#   geom_vline(xintercept = 0, color = "grey") + theme_bw() + facet_grid(Treatment ~ Place)

ggplot(data = mutate(dat, recoop = rescale01(cooperation)), 
       aes(recoop , fill = ID_player) ) + geom_density( alpha = 0.3, , show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "grey") + theme_bw() + facet_grid(Treatment ~ Place)


pwt <- pairwise.wilcox.test(x= dat$cooperation, 
                            g =  dat$Treatment, paired = T, p.adj = 'bonf')
summary(pwt)
pwt <- pairwise.wilcox.test(x= dat$cooperation,
                            g = dat$Place, paired = T, p.adj = 'bonf')





# in case you do it on paper or slides
library(knitr)
kable(pwt$p.value)




group_dat <- group_dat %>% 
  mutate (BaseLine = ifelse (group_dat$Treatment != "Base line" , 0,1) ,
          Threshold = ifelse (group_dat$Treatment == 'Threshold' & group_dat$part == TRUE, 1, 0) , 
          Uncertainty = ifelse (group_dat$Treatment == 'Uncertainty' & group_dat$part == TRUE, 1, 0) , 
          Risk = ifelse (group_dat$Treatment == 'Risk' & group_dat$part == TRUE, 1, 0), 
          Taganga = ifelse (group_dat$Place == "Taganga", 1, 0),
          Tasajera = ifelse (group_dat$Place == "Tasajera", 1, 0),
          Buenavista = ifelse (group_dat$Place == "Buenavista", 1, 0),
          LasFlores = ifelse (group_dat$Place == "Las Flores", 1, 0)
  )



## ordinary least squares
ols <- lm(IntermediateStockSize ~ Treatment, data = group_dat)
summary (ols) # ordinary least squares regression does not consider heterogeneity across groups or time
plot(ols)


## Fixed effects using least squares dummy variable model
fixed.dum <- lm(IntermediateStockSize ~ factor(Treatment) -1 , data = group_dat ) # one needs to rest 1 to the factors (?)
summary(fixed.dum)


## Formula
fmr <- IntermediateStockSize ~  Uncertainty + Risk + Threshold + Round + Place
fmr <- IntermediateStockSize ~ BaseLine + Uncertainty + Risk + Threshold + Buenavista + Taganga + Tasajera + LasFlores
fmr <- IntermediateStockSize ~ Buenavista + Taganga + Tasajera + LasFlores 

# fixed effect with plm
library (plm)
fixed <- plm (fmr , data = group_dat, index = c('group' ,'Round'), model = 'within')
summary (fixed)
# fixef(fixed) # display fixed effects (constants for each group)

random <- plm (fmr , data = group_dat,index = c('group' ,'Round'), model = 'random') #  , effect = 'time'  index = c('Round')
summary (random) 
# if effect = 'time' then it works, but not with other options... Treatment does not vary over time!

# fixed or random? 
phtest(fixed, random) # if p-value is <0.05 use fixed effects

# fiexed or ols?
pFtest(fixed, ols) # testing for fixed effects, null: OLS better than fixed
# If p-value is <0.05 then fixed effects model is a better choice


## Random effects needs to be used
## coomparision at group and individual levels

random.grp <- plm(IntermediateStockSize ~  Uncertainty + Risk + Threshold + Place + Round , 
                  data = group_dat, index = c('group' ,'Round'), model = 'random')
random.ind <- plm(cooperation ~ Treatment + Place + Round, data = dat, 
                  index = c('ID_player' ,'Round'), model = 'random')

df1 <- summary (random.ind)$coefficients
df2 <- summary(random.grp)$coefficients
df1 <- as.data.frame(df1)
df2 <- as.data.frame(df2)

rownames(df1)[c(1:4)] <- c("Intercept", "Risk", "Threshold", "Uncertainty")
rownames(df2)[1] <- "Intercept"

df1$Parameters <- rownames(df1)
df2$Parameters <- rownames(df2)
df1$Model <- "Individual"
df2$Model <- "Group"

df <- rbind(df1,df2)
names(df)[2] <- "StdError"
df$Parameters 

df$Parameters <-  as.factor(df$Parameters)

levels(df$Parameters)

df$Parameters <-  factor (df$Parameters, levels = as.character(levels(df$Parameters))[rev(c(1,5,21,22, 2:4,13:20, 6:12))])


ggplot(data = filter(df, Parameters != "Intercept") ,
       aes( y = Estimate, x = Parameters , color = Model)) + 
  geom_point(stat = "identity") +   coord_flip() +
  geom_errorbar(aes(ymax = Estimate + StdError, ymin = Estimate - StdError)) +
  theme_bw(base_size = 10) + geom_hline(yintercept=0, color = "gray")


quartz.save (file = "models.png", width = 4, height = 5, type = 'png', dpi = 200)

####### The example above only uses treatment effects.

fixed.time <- plm (IntermediateStockSize ~ BaseLine + Uncertainty + Risk + Threshold  + factor (Round),
              data = group_dat, index = c('group', 'Round'), model = 'within')
summary (fixed.time)

# testing time-fixed effects. The null is that no time-fixed effects needed:
pFtest( fixed.time, fixed) # if p <0.05 use time-fixed effects
plmtest(fixed, c('time'), type = 'bp') # if p <0.05 use time-fixed effects



###pooling vs fix effects
mod.w <- plm(IntermediateStockSize ~ BaseLine + Uncertainty + Risk + Threshold, 
             data = group_dat, index = c('group', 'Round'), model = 'within')
mod.p <- plm(IntermediateStockSize ~ BaseLine + Uncertainty + Risk + Threshold, 
             data = group_dat, index = c('group', 'Round'), model = 'pooling')
summary(mod.p)
pFtest(mod.w, mod.p)



### read the survey data
q <- 142 # 25, 46, 51, 98, 142, 157, 159

a0 <- dplyr::select(surv, id = 235, q = q, place = locationName, treatment = treatmentName)
if (key$Data.type [q] == "binary" ) a0$q <- as.factor(a0$q)
if (key$Data.type [q] == "ordinal" ) a0$q <- as.factor(a0$q)

ggplot( data = a0, aes (x= q , fill = place)) + 
  geom_density(alpha = 0.4) + theme_minimal(base_size = 10, base_family = "Helvetica") +
   xlab("years since member") + ggtitle("Belong to cooperative?") # + scale_x_log10()

quartz.save (file = 'cooperative_member_since.png', type = 'png', dpi = 200, width = 3, height = 3)


### cooperation

coop <- dat %>% group_by (ID_player) %>%
  summarize ( coop = median (cooperation), var_coop = var(cooperation)) %>%
  unique()

dat <- left_join(dat, coop, by = "ID_player")
