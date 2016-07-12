# Survey data analysis
# Juan Carlos Rocha
# juan.rocha@su.se
# 160411

rm(list=ls())

# load libraries
library (ggplot2)
library (tidyr)
library (dplyr)
library(gdata)
library(GGally)

# load data

# survey <- read.xls(xls='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.xlsx', sheet=1)
surv <- read.csv2(file='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_ 160530.csv', header=T, na.strings = '.')

# Hadleys package is nice at detecting the errors in survey but make it difficult to handle them.
# surv <- readxl::read_excel ('~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_ 160530.xlsx', sheet = 1, na = '.')

#key <- read.xls(xls='~/Dropbox/BEST/Colombia/Survey/Consolidado-Game_Survey_database_.xlsx', sheet=2, na.strings = '.')
key <- read.csv2(file = '~/Dropbox/BEST/Colombia/Survey/key_consolidado_survey.csv')


# for now work only with the first part of the survey data to create ID_player and merge with game data
str(surv)
head(colnames(surv), 10)
surv.dat <- dplyr::select(surv, date, locationName, am, pm, treatmentName, groupID, playerNo, round, indClaim, indGuessStockSize)

surv.dat <- filter(surv.dat, round > 0 )

str(surv.dat)
# standardize with same levels as game data
levels(surv.dat$date) <- as.factor(as.Date(levels(surv.dat$date), format='%d/%m/%Y')[c(7,12:16,7,9,9,11,11, 12:16)])
surv.dat$Session <- as.factor (ifelse(surv.dat$am == 1, 'am', 'pm'))
levels(surv.dat$treatmentName) <- c('Base line', 'Base line', 'Risk', 'Risk', 'Threshold', 'Threshold','Threshold', 'Uncertainty', 'Uncertainty') # unify spelling
surv.dat$playerNo <- as.factor(surv.dat$playerNo)

# Create unique player IDs [faster way from http://stackoverflow.com/questions/13566562/creating-a-unique-id-in-r]
surv.dat <- transform (surv.dat, ID_player = interaction(date, treatmentName, Session, playerNo, drop = TRUE))

# Now I know it works, do it for the complete survey file
surv <- filter(surv, round >0) 
levels(surv$date) <- as.factor(as.Date(levels(surv$date), format='%d/%m/%Y')[c(7,12:16,7,9,9,11,11, 12:16)])
surv$Session <- as.factor (ifelse(surv$am == 1, 'am', 'pm'))
levels(surv$treatmentName) <- c('Base line', 'Base line', 'Risk', 'Risk', 'Threshold', 'Threshold','Threshold', 'Uncertainty', 'Uncertainty') # unify spelling
surv$playerNo <- as.factor(surv$playerNo)

surv <- transform (surv, ID_player = interaction(date, treatmentName, Session, playerNo, drop = TRUE))

head(colnames(surv),20)

head(str(surv))

# J160530: I got new weird NA with Nidia's file.
surv$locationNo[is.na(surv$locationNo)]
summary(surv$locationNo)

## Visualize some of the questions ##

# Have you participated on an economic game before?
# surv[,31] <- as.factor(surv[,31]) # Life satisfaction Note: make it as factor or not doesn't change the output of the graph
a0 <- select(surv, ID_player, X2..lifeSatisfaction, locationName)
aggregate( X2..lifeSatisfaction ~ ID_player + locationName, data=a0, mean)

g <- ggplot(data = aggregate(X2..lifeSatisfaction ~ ID_player + locationName, data=a0, mean), aes(x= X2..lifeSatisfaction, fill=locationName)) + #, group=ID_player
    geom_bar(stat='count', na.rm=TRUE)

g

# question is a function that speed up the steps above

question <- function(dat, q1, q2, q3, fun){ # dat = survey, q = is the colname of the question
  a0 <- dplyr::select(dat, col1=q1, col2=q2, place=q3)
  g <- ggplot(data = aggregate(col2 ~ col1 + place, data=a0, FUN= fun ), aes (x=col2, fill=place))+
    geom_bar(stat='count', na.rm=TRUE) + theme_minimal(base_size = 10, base_family = "Helvetica")
  return (g)
}
#q1 is the column number of ID_player, and q2 = X2..lifeSatisfaction
q1 <- question (surv, q1=372, q2=25, q3=2, fun=mean) + ggtitle('Life satisfaction\n 1= very satisfied : 4 = very unsatisfied')

# have participated in economic experiments before?
q2 <- question (surv, q1=372, q2=26, q3 = 2, fun=mean) + ggtitle('Have you participated in economic experiments before?')
# what are the rows with 2!! It should be binary 1/0
surv[surv[,26] == 2, 1:10]

# did you played with any of your fishing partners?
q3 <- question (surv, q1=372, q2=27, q3 = 2, fun=mean)  + ggtitle('Did you played with any of your fishing partners?')

q4 <- question (surv, q1=372, q2=28, q3 = 2, fun=mean)  + ggtitle('Were you surprised at the end?')

q5 <- question (surv, q1=372, q2=29, q3 = 2, fun=mean)  + ggtitle('How many extra rounds were you expecting?\n 0 = none; 1 = <5; 2 = >5')

pm <- ggmatrix(
  plots = list(q1,q2,q3,q5,q5),
  nrow = 2, ncol = 3,
  showAxisPlotLabels = T, showStrips = TRUE
  
)
pm  + theme_minimal(base_size = 10, base_family = "Helvetica")
# the nice layout with titles can be plotted using gridExtra::grid.arrange() I used in report.



question (surv, q1=372, q2=31, q3 = 2, fun=mean)  + ggtitle('7. How old did you start fishing?')

g<- question (surv, q1=372, q2=32, q3 = 2, fun=mean)  + ggtitle('8. Do you fish most of the time?') # error here on player 2016-02-01.Base line.pm.3    Taganga 0.0625
# cell (206,32) in excel file
g$data

question (surv, q1=372, q2=33, q3 = 2, fun=mean)  + ggtitle('9. Have you been fishing here since you started?')

question (surv, q1=372, q2=35, q3 = 2, fun=mean)  + ggtitle('11. Last year, there were months when you have not fished?')
# error in cell ()

question (surv, q1=372, q2=36, q3 = 2, fun=mean)  + ggtitle('Did not fish in January')

question (surv, q1=372, q2=49, q3 = 2, fun=mean)  + ggtitle('Days fishing in a normal week') # repeat graph with position = 'dodge' in geom_bar

question (surv, q1=372, q2=50, q3 = 2, fun=mean)  + ggtitle('Fishing hours in a normal day')

question (surv, q1=372, q2=51, q3 = 2, fun=mean)  + ggtitle('Kg of fish in a normal day') ## error!!! summary(surv[,51]) reveals there is . and , used for decimals
class(surv[,51])
str(surv[,51])

question (surv, q1=372, q2=52, q3 = 2, fun=mean)  + ggtitle('Earnings in Col$ in a normal day') ## error
summary(surv[,52]) # one of the datapoins is 3 million pesos per day!

question (surv, q1=372, q2=53, q3 = 2, fun=mean)  + ggtitle('Kg of fish in a good day') # error

question (surv, q1=372, q2=56, q3 = 2, fun=mean)  + ggtitle('Earnings Col$ in a bad day') # error in distribution
table(surv[,56])/16

question (surv, q1=372, q2=56, q3 = 2, fun=mean) + geom_histogram(aes(alpha=0.2))  + ggtitle('Earnings Col$ in a bad day')

question (surv, q1=372, q2=57, q3 = 2, fun=mean)  + ggtitle('How often do you have a bad day?\n 1 = 1/year, 2 = 1/month, 3 = 1/week, 4 = n/week')

question (surv, q1=372, q2=58, q3 = 2, fun=mean)  + ggtitle('Do you fish with someone else?')

question (surv, q1=372, q2=59, q3 = 2, fun=mean)  + ggtitle('How often?\n 1=rare, 2=1/2times, 3=most, 4=always')

question (surv, q1=372, q2=60, q3 = 2, fun=mean)  + ggtitle('How many are you?') # repeat this plot with density and there is error on raw data
table(surv[,60])/16

question (surv, q1=372, q2=61, q3 = 2, fun=mean)  + ggtitle('Same crew?')

question (surv, q1=372, q2=62, q3 = 2, fun=mean)  + ggtitle('Who decides when to fish?\n 0=me 1=some else')

question (surv, q1=372, q2=64, q3 = 2, fun=mean)  + ggtitle('Do you fish by boat or lancha?')

question (surv, q1=372, q2=65, q3 = 2, fun=mean)  + ggtitle('Are you the captain?')

question (surv, q1=372, q2=66, q3 = 2, fun=mean)  + ggtitle('Do you own the boat?')

question (surv, q1=372, q2=67, q3 = 2, fun=mean)  + ggtitle('Do you fish in the same place?')

question (surv, q1=372, q2=89, q3 = 2, fun=mean)  + ggtitle('Do you own the fishing art?')

question (surv, q1=372, q2=90, q3 = 2, fun=mean)  + ggtitle('How much do you take homes?\n 0=none, 1=some, 2=half, 3=>half, 4=all')

question (surv, q1=372, q2=91, q3 = 2, fun=mean)  + ggtitle('How much do you sell?\n 0=none, 1=some, 2=half, 3=>half, 4=all')

question (surv, q1=372, q2=93, q3 = 2, fun=mean)  + ggtitle('How much do you give away?\n 0=none, 1=some, 2=half, 3=>half, 4=all')

question (surv, q1=372, q2=93, q3 = 2, fun=mean)  + ggtitle('Do you think you will be fishermen in 10yrs?\n 0=NO, 1=no, 2=yes, 3=YES, 4=dont know')

question (surv, q1=372, q2=96, q3 = 2, fun=mean)  + ggtitle('Do you think your children will fish?\n 0=NO, 1=no, 2=yes, 3=YES, 4=dont know')
# errors table(surv[,96])/16 levels(as.factor(surv[,96])) hist(surv[,96]) surv[surv[,96] >4, c(372,96,2)]
## Errors in 2016-02-05.Threshold.am.4 Buenavista, numbers >5

question (surv, q1=372, q2=98, q3 = 2, fun=mean)  + ggtitle('Have you been fishing the same spp?') # should be binary
# errors surv[surv[,97] >2, c(372,98,2)] surv[surv[,98] >3, c(372,98,2)] 
# errors in 2016-02-05.Threshold.am.4 Buenavista

question (surv, q1=372, q2=100, q3 = 2, fun=mean)  + ggtitle('Is there any species that you dont fish as much as before?')
# error

question (surv, q1=372, q2=102, q3 = 2, fun=mean)  + ggtitle('Dramatic changes?')
# error

question (surv, q1=372, q2=104, q3 = 2, fun=mean)  + ggtitle('When?') ### Errors, horribly coded! summary(surv[,104])

question (surv, q1=372, q2=105, q3 = 2, fun=mean)  + ggtitle('How long, still missing?') # summary(surv[,105]) ... not binary

question (surv, q1=372, q2=109, q3 = 2, fun=mean)

# do something fancier for the temporal / spatial dynamics of non-fishing



# But it shouldn't be a problem to plot from the long dataset without selecting!
# everything is duplicated 16 times due to round, indClaim, and so forth. Fuck!
# 
# TODO: automatize it - so, write a function that for each question subset the dataset
# select only relevant columns for the question, including player id and session (the last ones)
# then use one of the uninteresting columns to summarize (round, or indClaim), casting the subset of data
# With that selected data frame plot the results and run regressions


head(surv, 50)[,17:35]

g <- ggplot(data=surv, aes(x=X2..lifeSatisfaction, colour=treatmentName)) +
  geom_bar()
g




reshape::cast(select(surv, ID_player, X2..lifeSatisfaction), formula = X2..lifeSatisfaction ~ ID_player)
a1 <- dplyr::select(a, X2..lifeSatisfaction)

aggregate(X2..lifeSatisfaction ~ ID_player, data=a0, mean) # this works! but how do I make it work inside ggplot?
reshape::cast (a1, formula =  X2..lifeSatisfaction ~ID_player , mean)

surv %>%
  select(ID_player, X2..lifeSatisfaction)%>%
  summarise(m = mean(X2..lifeSatisfaction, na.rm=T))

names(surv)
dim(a)


 
# # search for NA
# nas <- apply(surv, 2, anyNA)
# sum.na <- function (x){ sum (is.na(x))} 
# nas.rows <- apply(surv[,nas], 2, sum.na)
# 
# sort(nas.rows, dec=T)
# 
# ## which columsn are binary
# bin <- which(key$Data.type == 'binary')
# facs <- apply(surv[,bin], 2, as.factor)
# apply(facs, 2, levels)
# 
# for (i in 1:length(bin)){
#   class(surv[,bin[i]]) <- 'factor'	
# }
# 
# names(surv[,bin])

### idea for text data:
txt <- key$Data.type == 'text'

a0 <- select(surv, ID_player, X6.1.comment. , locationName)
aggregate( X2..lifeSatisfaction ~ ID_player + locationName, data=a0, mean)

g <- ggplot(data = aggregate(X2..lifeSatisfaction ~ ID_player + locationName, data=a0, mean), aes(x= X2..lifeSatisfaction, fill=locationName)) + #, group=ID_player
  geom_bar(stat='count', na.rm=TRUE)

g

