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
surv <- read.csv2(file='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_ 160530.csv', 
                  header=T, na.strings = '.', encoding = 'utf-8')

## # standardize with same levels as game data
surv <- filter(surv, round >0) 
levels(surv$date) <- as.factor(as.Date(levels(surv$date), format='%d/%m/%Y')[c(7,12:16,7,9,9,11,11, 12:16)])
surv$Session <- as.factor (ifelse(surv$am == 1, 'am', 'pm'))
levels(surv$treatmentName) <- c('Base line', 'Base line', 'Risk', 'Risk', 'Threshold', 'Threshold','Threshold', 'Uncertainty', 'Uncertainty') # unify spelling
surv$playerNo <- as.factor(surv$playerNo)
# Create unique player IDs
surv <- transform (surv, ID_player = interaction(date, treatmentName, Session, playerNo, drop = TRUE))


## Finding problems with survey
# surv <- tbl_df(surv)
# surv <- as.data.frame (surv)

## create observation ID for later
surv <- mutate(surv, round_lev = as.factor(round))
surv <- transform (surv, ID_Obs = interaction(date, treatmentName, Session, playerNo, round_lev, drop = TRUE) )

# question is a function that graphic numeric questions in the survey
# question <- function(dat, q1, q2, q3, fun){ # dat = survey, q = is the colname of the question
#   a0 <- dplyr::select(dat, col1=q1, col2=q2, place=q3)
#   g <- ggplot(data = aggregate(col2 ~ col1 + place, data=a0, FUN= fun ), aes (x=col2, fill=place))+
#     geom_bar(stat='count', na.rm=TRUE) + theme_minimal(base_size = 10, base_family = "Helvetica")
#   return (g)
# }


# # Q32
# surv[, c(2,32, 372)] %>%
#   group_by(ID_player) %>%
#   summarize(n_obs = n(), avg = mean (X8..fishing.most.of.the.time.)) %>%
#   filter(avg != 0, avg !=1)
# # see the error
# filter (surv[, c(2,32, 372)], ID_player == "2016-02-01.Base line.pm.3")
# 
# # Q35
# surv[, c(2,35, 372)] %>%  #names()
#   group_by(ID_player) %>%
#   summarize(n_obs = n(), avg = mean ( X11..no.fishing.last.year.)) %>%
#   filter(avg != 0, avg !=1)
# # see the error
# filter (surv[, c(2,35, 372)], ID_player == "2016-02-02.Risk.pm.3")
# 
# 
# # Q100
# q <- 96# question
# surv[, c(2,q, 372)] %>%  names()
# 
# surv[, c(2,q, 372)] %>% # names()
#   group_by(ID_player) %>%
#   summarize(n_obs = n(), avg = mean ( X29..fishing.children )) %>%   filter(avg != 0, avg !=1, avg !=2, avg !=3, avg != 4) #filter (avg == 1946)
#   filter(avg != 0, avg !=1)
# # see the error
# filter (surv[, c(2,q, 372)], ID_player == "2016-02-02.Risk.pm.3" )

# 
# filter (surv[, c(2,q,372)], X51..How.long.have.you.been.living.here. == 'years')
# 
# filter (surv[, c(2,q,372)], X49..since.when.do.you.live.here == 1960)
# 
# ### Errors with Lina Ma: 160714
# 
# q <- 32
# n <- noquote(names(surv)[q])
# n
# 
# surv[, c(2,q, 372)] %>%
#   group_by(ID_player) %>%
#   summarize(n_obs = n(), avg = mean ( X8..fishing.most.of.the.time. )) %>%
#   filter(avg != 0, avg !=1) # , avg !=2, avg !=3, avg !=4
# 
# filter (surv[, c(2,q, 374)], ID_player == "2016-02-01.Base line.pm.3")
# 
# 
# filter(surv, X16.b.COP.normal.day > 0.5e+06 ) %>% select (c(372,52,2,65,56))
# filter(surv, X16.f.COP.bad.day > 0.2e+06 ) %>% select (c(372,52,2,65,56))
# filter(surv, X18.2.how.many.are.you > 20 ) %>% select (c(372,60,2, 70,163))
# filter(surv, X45..age < 20 ) %>% select (c(372,163,2, 70,163))
# 
# 
# 
# question (filter(surv, X16.b.COP.normal.day < 1e+06 ), q1=372, q2=52, q3 = 2, fun=mean)  + ggtitle('Earnings in Col$ in a normal day') ## error
# summary(surv[,54]) # one of the datapoins is 3 million pesos per day!



### Correct the Survey file manually. All lines below are run to solve coding problems.
## A new file is saved with unique rows per player and with player ID per observation.
## Done by Juan on 160615
# Q 26
surv [ surv$ID_player == '2016-02-05.Threshold.am.4',26] <- 0

# Q 32
surv [ surv$ID_Obs == '2016-02-01.Base line.pm.3.12', 32] <- 0
# Q 35
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 35] <- 0
# Q 51
# table (surv[51])
str(surv[,51])
surv[,51] <- as.numeric(gsub("([.])", ",", surv[,51], fixed = T))
# Q 53
surv[,53] <- as.numeric(surv[,53])
# Q 55
surv[,55] <- as.numeric(as.character(surv[,55]))

# Q 96
p <- surv[surv[,96] == 5, 374]
surv[is.element(surv$ID_Obs, p), 96] <- 4
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 96] <- 0

# Q 102
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 102] <- 0
## Q 98
surv [ surv$ID_player == '2016-02-05.Threshold.am.4', 98] <- 1
## Q 100
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 100] <- 1
## Q 104
# table(surv[104])
levels(surv[,104])[c(1,2,3,4,14,15,16, 34:39)] <- c('2015', '2005', '2000', '1965', '2016','2016', '1996' , '2016', '1986', '2012', '2010', '2015', '2016')
surv[,104] <- as.numeric(as.character(surv[,104]))
## Q 105
# table(surv[105])
levels(surv[,105]) <- c(1,1,0.1,10,10,1,11,12,1,13,14,14,15,15,16,18,18, (2016-1965), 2,2,1,20,8,5,4,2,1,21,27,3,
                        3,30,35, 4,4,46,5,6,1,7,1,8, rep(NA, 4), 3,1,1,rep(NA,19))
surv[,105] <- as.numeric(as.character(surv[,105]))

## Q 109
# table(surv[109])
levels(surv[,109])[c(1,2,4, 29:32)] <- c(2006, 2001, 1965, 1986, 2012, 2015, 2015 )
surv[,109] <- as.numeric(as.character(surv[,109]))

## Q 110
# table(surv[110])
levels(surv[,110]) <- c(1,10,10,11,12,  13,15,15,16,18  , 51,2,2,2,20,  8,5,2,1,21,  25,27,3,30,4,  4,5,1,1,50, 1,1,8, rep(NA, 12))
surv[,110] <- as.numeric(as.character(surv[,110]))

## Q 114
# table(surv[114])
levels(surv[,114]) [c(1,22,23)] <- c(1965,1,NA)
surv[,114] <- as.numeric(as.character(surv[,114]))

## Q 115
# table(surv[115])
levels(surv[,115]) <- c(1,10,11,12,15,16,51,10,2,2,20,12,8,2,1,21,27,3,4,4,5,5,6,7,8, rep(NA, 9))
surv[,115] <- as.numeric(as.character(surv[,115]))

## Q 119
# table(surv[119])
levels(surv[,119])[1] <- 2016
surv[,119] <- as.numeric(as.character(surv[,119]))

## Q 120
# table(surv[120])
levels(surv[,120]) <- c(1,0,10,11,12,15,16,8,21,3,4,5,rep(NA,4))
surv[,120] <- as.numeric(as.character(surv[,120]))


## Q 123 Nidia should correct
surv[surv$ID_player == "2016-02-05.Threshold.am.4" , 123] <- 0
## Q 126
surv[surv$ID_player == "2016-02-03.Threshold.pm.1" , 126] <- 1
surv[surv$ID_player == "2016-02-04.Uncertainty.am.2" ,126]  <- 0


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
# table(surv[143]) # nidia needs to correct
surv[surv$ID_player == "2016-02-02.Threshold.pm.3" , 143] <- 1 

## Q 148
# table(surv[148]) 
# class(surv[, 148])
levels(surv[, 148])[1] <- '2016'
surv$X42.3..since.when. <- as.numeric(as.character(surv$X42.3..since.when.)) # corrected
## Q 149 needs to be corrected
# table(surv[149]) ## It seems an error on coding, it should have been 1-6 but instead we have 0-5
## Q 163 # already corrected by Nidia
#filter(surv, is.na(X45..age) ) %>% select (c(372,163,2)) # they are real missing values

## Q 165
# names(table(surv[165]))
# filter(surv, X46.1.specifications.years == '10 de primaria') %>% select (c(372,165,2)) # ilogical response
surv [ surv$ID_player == '2016-02-05.Threshold.am.4', 165] <- '4 de primaria'
levels(surv[,165])[1:5] <- c(1,6,1,1,10) 
surv[,165] <- droplevels(surv[,165])
levels(surv[,165])[c(4,6,8,9:64)] <- c(16,11,7,2,2,2,7,2,3,3,8,3,3,3,3,3,4,9,4,4,
                                       10,5,6,6,7,7,7,8,9,9,8,11,11,10,7,7,11, 11,
                                       9,10,7,8,16,11,10,12,2,11,16, 11, 3,14,
                                       14,14,14,14,14,14,12)
surv[,165] <- droplevels(surv[,165])
levels(surv[,165])[c(15,16)] <- c(12,11)
surv[,165] <- droplevels(surv[,165])
surv$X46.1.specifications.years <- as.numeric(as.character(surv$X46.1.specifications.years)) #corrected
## Q 166 needs to be corrected by nidia
surv[surv$ID_player == "2016-02-05.Threshold.am.4" , 166]  <- 1


## Q 168
# table(surv[,168])/16
levels(surv [ ,168])[c(46,47)] <- c(2015, 2015)
surv$X49..since.when.do.you.live.here <- as.numeric(as.character( surv$X49..since.when.do.you.live.here )) 

# filter (surv[, c(2,168,372)], X49..since.when.do.you.live.here == 1981)
surv [ surv$ID_player == '2016-02-03.Threshold.pm.4'  , 168] <-  1979 # reviewed by Nidia

## Q 170
# table(surv[,170])/16
# filter(surv, X51..How.long.have.you.been.living.here. == 'years')%>% select (c(372,170,2))
levels(surv[,170])[41] <- NA
surv[,170] <- as.numeric(as.character(surv[,170]))
## Q 171
# table(surv[,171])/16
levels(surv[,171]) <- c(1,2,3.5,0.5,7)
surv[,171] <- as.numeric(as.character(surv[,171]))

### Correcting other errors related to duplication
surv[surv$ID_Obs == '2016-02-01.Base line.am.4.16' , "X39.5.change.fishing.area"] <- 0
surv[surv$ID_Obs == '2016-02-01.Threshold.am.2.16' , "X38..main.cause" ] <- NA
surv [surv$ID_Obs == '2016-02-01.Threshold.am.3.16' , 'X42.5..benefits.coop'] <- surv [surv$ID_Obs == '2016-02-01.Threshold.am.3.15' , 'X42.5..benefits.coop']
surv [surv$ID_Obs == '2016-02-01.Base line.pm.2.16' , 'X39.2.1.why.'] <- surv [surv$ID_Obs == '2016-02-01.Base line.pm.2.15' , 'X39.2.1.why.']
surv [surv$ID_player == '2016-02-01.Threshold.pm.1' , 197] <- 'nieto'
surv [surv$ID_player == '2016-02-02.Uncertainty.am.1' , 'X53.2.age.C'] <- 15
surv [surv$ID_player == '2016-02-02.Risk.pm.3' , 'X34..which.'] <- 'pargo rojo'
surv [surv$ID_player == '2016-02-02.Risk.pm.3' , 'X31..same.spp.since.you.started'] <- 1
surv [surv$ID_player == '2016-02-02.Risk.pm.3' , 'X53.2.age.C'] <- 24
surv [surv$ID_player == '2016-02-02.Uncertainty.pm.1' , 'X35.2.what.happened.2'] <- 'albacora'
surv [surv$ID_player == '2016-02-02.Uncertainty.pm.2' , 'X53.1.relationship.household.member.B'] <- 'mama'
surv [surv$ID_player == '2016-02-03.Threshold.pm.3' , 'X22.3.Transportation.2'] <- 'bote'
surv [surv$ID_player == '2016-02-04.Uncertainty.am.1' , 'X35.5.causes1'] <- 'sedimentacion'
surv [surv$ID_player == '2016-02-04.Risk.pm.2' , 'X32...what.changed.'] <- "jurel, carito, sierra, pargo"
surv [surv$ID_player == '2016-02-01.Uncertainty.am.4' , 'X16.b.COP.normal.day'] <- 15000

surv <- unique(surv[c(1:16,23:240, 372)]) # cleaned survey

####### Once the survey is corrected, one can select the unique values
# names(surv)
# sum(!duplicated(surv[23:240]))
# 
# d <- duplicated( surv$ID_player [!duplicated(surv[23:240])] )
# p <- surv$ID_player [!duplicated(surv[23:240])][d]   [1]
# 
# surv [surv$ID_player == as.character(p) & !duplicated(surv[23:240]), 23:240]
# surv [surv$ID_player == as.character(p) , c(107:110)]
# 
# which (colSums(apply ( surv [surv$ID_player == p & !duplicated(surv[23:240]), 23:240], 2, duplicated  )) ==0)
# surv [surv$ID_player == as.character(p) , 'X16.b.COP.normal.day']



### Create unique files for survey and games
# names(surv)
# identifying rows are 1:16, they are required to create the ID_player or ID_Obs
# game cols: 17:22
# experimenter notes 241:358
# risk game 359:end
# dim(unique(surv[c(1:16,23:240)])) # Survey
# dim(unique(surv[c(1:22,241:374)])) # Games + expNotes


### combine game data from consolidado file + game data from field
# dat <- read.csv(file="~/Dropbox/BEST/Colombia/0_Game data/160427_corrected_full_data_long.csv", row.names=1) # in long format, short format also available
# # Create player ID's as in Surveys.R
# dat <- transform (dat, ID_player = interaction(Date, Treatment, Session, Player, drop = TRUE))
# dat <- transform(dat, ID_Obs)
# # Create ID group
# dat <- transform(dat, group = interaction (Date, Treatment, Session, drop=T))
# ## create observation ID for later
# dat <- mutate(dat, round_lev = as.factor(Round))
# dat <- transform (dat, ID_Obs = interaction(Date, Treatment, Session, Player, round_lev, drop = TRUE) )
# 
# names(dat)
# dim(dat)
# names(unique(surv[c(1:22,241:374)]))
# dim(unique(surv[c(1:22,241:374)]))
# games <- left_join (dat, unique(surv[c(1:22,241:374)]), by = 'ID_Obs')
# 
# # clean up unnecessary columns
# games <- select(games, -Session.y, -round_lev.y, -ID_player.y, -round_lev.x,
#                 -date,-locationName, -locationNo, -treatmentName, -practiceRound )
# 
# ## Cleaned files
# games <- rename(games, ID_player = ID_player.x , Session = Session.x) # cleaned games

# surv <- unique(surv[c(1:16,23:240, 372)]) # cleaned survey

# 
# getwd()
# write.csv2(unique(surv[c(1:16,23:240, 372)]), file = 'cleaned_survey.csv', row.names = FALSE)
# write.csv2(games, file = 'cleaned_games_ExpNotes.csv', row.names = FALSE)
# 
# clss <- sapply(surv, class)