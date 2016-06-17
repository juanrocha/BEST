## Unique file management
# This script creates a unique file per dataset, so it makes sure there is only
# one observation when needed: one answer in the survey, 16 answers in the game 
# per person.



# raw data loaded
surv <- read.csv2(file='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_ 160530.csv', header=T, na.strings = '.')

# game data from survey file: this have information per round
# game is columns 1:22, 241:357 experimental notes from assisstants, ids 371,372
# risk and uncertainty games are columns 359:370
games <- surv %>%
  select(c(371,372, 1:22, 241:357, 359:370))


# surveys we have unecesary duplications because of the games rounds. Columsn are
survey <- surv %>%
  select (-c(1:17, 19:22, 241:357, 359:370)) %>%
  filter (round == 11)

surv2 <- surv %>%
  select (-c(1:17, 19:22, 241:357, 359:370)) %>%
  filter (round == 16)

str(surv2)

all.equal (survey, surv2)


## now the survey should be clean
names(survey)
