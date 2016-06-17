## Finding problems with survey
surv <- tbl_df(surv)
surv <- as.data.frame (surv)

## create observation ID for later
surv <- mutate(surv, round_lev = as.factor(round))
surv <- transform (surv, ID_Obs = interaction(date, treatmentName, Session, playerNo, round_lev, drop = TRUE) )



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
q <- 370 # question
surv[, c(2,q, 372)] %>%  names()

surv[, c(2,q, 372)] %>% # names()
  group_by(ID_player) %>%
  summarize(n_obs = n(), avg = mean ( X6..u..2000..36000 )) %>%  #filter (avg <1)
  filter(avg != 0, avg !=1)
# see the error
filter (surv[, c(2,q, 372)], ID_player == "2016-02-05.Threshold.am.4" )


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
surv [ surv$ID_player == '2016-02-05.Threshold.am.4', 98]
## Q 100
surv [ surv$ID_Obs == '2016-02-02.Risk.pm.3.16', 100] <- 1
## Q 104
table(surv[104])

## Q 123
