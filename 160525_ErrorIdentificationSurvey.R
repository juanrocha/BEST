# Survey data cleaning
# Juan Carlos Rocha
# juan.rocha@su.se
# 160705

rm(list=ls())

# load libraries
library (ggplot2)
library (tidyr)
library (dplyr)

# load data

# survey <- read.xls(xls='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.xlsx', sheet=1)
surv <- read.csv2(file='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_ 160530.csv', header=T, na.strings = '.')

## # standardize with same levels as game data
surv <- filter(surv, round >0) 
levels(surv$date) <- as.factor(as.Date(levels(surv$date), format='%d/%m/%Y')[c(7,12:16,7,9,9,11,11, 12:16)])
surv$Session <- as.factor (ifelse(surv$am == 1, 'am', 'pm'))
levels(surv$treatmentName) <- c('Base line', 'Base line', 'Risk', 'Risk', 'Threshold', 'Threshold','Threshold', 'Uncertainty', 'Uncertainty') # unify spelling
surv$playerNo <- as.factor(surv$playerNo)
# Create unique player IDs
surv <- transform (surv, ID_player = interaction(date, treatmentName, Session, playerNo, drop = TRUE))


## Finding problems with survey
surv <- tbl_df(surv)
surv <- as.data.frame (surv)

## create observation ID for later
surv <- mutate(surv, round_lev = as.factor(round))
surv <- transform (surv, ID_Obs = interaction(date, treatmentName, Session, playerNo, round_lev, drop = TRUE) )

# question is a function that graphic numeric questions in the survey
question <- function(dat, q1, q2, q3, fun){ # dat = survey, q = is the colname of the question
  a0 <- dplyr::select(dat, col1=q1, col2=q2, place=q3)
  g <- ggplot(data = aggregate(col2 ~ col1 + place, data=a0, FUN= fun ), aes (x=col2, fill=place))+
    geom_bar(stat='count', na.rm=TRUE) + theme_minimal(base_size = 10, base_family = "Helvetica")
  return (g)
}


# Q32
surv[, c(2,32, 372)] %>% 
  group_by(ID_player) %>%
  summarize(n_obs = n(), avg = mean (X8..fishing.most.of.the.time.)) %>%
  filter(avg != 0, avg !=1)
# see the error
filter (surv[, c(2,32, 372)], ID_player == "2016-02-01.Base line.pm.3")
  
# Q35
surv[, c(2,35, 372)] %>%  #names()
  group_by(ID_player) %>%
  summarize(n_obs = n(), avg = mean ( X11..no.fishing.last.year.)) %>%
  filter(avg != 0, avg !=1)
# see the error
filter (surv[, c(2,35, 372)], ID_player == "2016-02-02.Risk.pm.3")


# Q100
q <- 149 # question
surv[, c(2,q, 372)] %>%  names()

surv[, c(2,q, 372)] %>% # names()
  group_by(ID_player) %>%
  summarize(n_obs = n(), avg = mean ( X42.4..role )) %>%  filter (avg < 1)
  filter(avg != 0, avg !=1)
# see the error
filter (surv[, c(2,q, 372)], ID_player == "2016-02-02.Threshold.pm.3" )


filter (surv[, c(2,q,372)], X51..How.long.have.you.been.living.here. == 'years')



### Errors with Lina Ma: 160714

q <- 32
n <- noquote(names(surv)[q])
n

surv[, c(2,q, 372)] %>% 
  group_by(ID_player) %>%
  summarize(n_obs = n(), avg = mean ( X8..fishing.most.of.the.time. )) %>%
  filter(avg != 0, avg !=1) # , avg !=2, avg !=3, avg !=4

filter (surv[, c(2,q, 374)], ID_player == "2016-02-01.Base line.pm.3")


filter(surv, X16.b.COP.normal.day > 0.5e+06 ) %>% select (c(372,52,2,65,56))
filter(surv, X16.f.COP.bad.day > 0.2e+06 ) %>% select (c(372,52,2,65,56))
filter(surv, X18.2.how.many.are.you > 20 ) %>% select (c(372,60,2, 70,163))
filter(surv, X45..age < 20 ) %>% select (c(372,163,2, 70,163))



question (filter(surv, X16.b.COP.normal.day < 1e+06 ), q1=372, q2=52, q3 = 2, fun=mean)  + ggtitle('Earnings in Col$ in a normal day') ## error
summary(surv[,52]) # one of the datapoins is 3 million pesos per day!



### Correct the Survey file manually. All lines below are run to solve coding problems.
## A new file is saved with unique rows per player and with player ID per observation.
## Done by Juan on 160615


# Q 32
surv [ surv$ID_Obs == '2016-02-01.Base line.pm.3.12', 32] <- 0
# Q 35
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 35] <- 0
# Q 51
table (surv[51])
str(surv[,51])
surv[,51] <- as.numeric(gsub("([.])", ",", surv[,51], fixed = T))
# Q 53
surv[,53] <- as.numeric(surv[,53])
# Q 96
p <- surv[surv[,96] == 5, 374]
surv[is.element(surv$ID_Obs, p), 96] <- 4
# Q 102
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 102] <- 0
## Q 98
surv [ surv$ID_player == '2016-02-05.Threshold.am.4', 98] # nidia needs to correct
## Q 100
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 100] <- 1
## Q 104
table(surv[104])

## Q 123 Nidia should correct

## Q 132
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 132] <- 0
## Q 134
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 134] <- 0
## Q 136
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 136] <- 0
## Q 138
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 138] <- 0
## Q 140
surv [ surv$ID_Obs == '2016-02-05.Threshold.am.4.16', 140] <- 1
## Q 143
table(surv[143]) # nidia needs to correct
## Q 148
table(surv[148]) 
class(surv[, 148])
levels(surv[, 148])[1] <- '2016'
surv$X42.3..since.when. <- as.numeric(as.character(surv$X42.3..since.when.)) # corrected
## Q 
