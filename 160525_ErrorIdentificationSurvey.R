## Finding problems with survey
surv <- tbl_df(surv)
surv <- as.data.frame (surv)


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
filter (surv[, c(2,q, 372)], ID_player == "2016-02-05.Threshold.am.4")


filter (surv[, c(2,q,372)], X51..How.long.have.you.been.living.here. == 'years')
