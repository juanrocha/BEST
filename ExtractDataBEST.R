# # BEST - Beijer
# # Funciton to extract field game data from excel files
# # by Juan Carlos Rocha
# # juan.rocha@su.se
# # Version: February 2016


# clean
rm (list=ls())

# required libraries
require ('gdata')
help(package='gdata')

setwd("~/Dropbox/BEST/Colombia/0_Game data/TAGANGA")

files <- list.files()

## Manual demonstration
df <- read.xls(files[1], sheet=2, header=T)

# trim file
df <- df[-c(24:dim(df)[1]) , -c(12:dim(df)[2])]
df2 <- data.frame(df[c(1:11),], df[c(13:23),])

rownames(df2) <- c('Round',
				  'StockSizeBegining',
				  'Player',
				  'P1',
				  'P2',
				  'P3',
				  'P4',
				  'SumTotalCatch',
				  'IntermediateStockSize',
				  'Regeneration',
				  'NewStockSize')

df2 <- df2 [-3,-c(1,8:12)]
dat <- t(df2)
dat <- apply(dat, 2, as.numeric) # convert all values to numeric
v <- !is.na(rowSums(dat[,3:6]))
dat <- dat[v,]

# End of Manual demonstration: for the functions the only change is the chunks of file that gets combined on the dataframe df2 [line24], and the columns that get deleted [line38].

# Functions for long and short sheets

extract.long <- function(file, sheet){
	df <- read.xls(file, sheet=sheet, header=T)
	treatment <- sheetNames(file)[sheet]
	# trim file
	df <- df[-c(36:dim(df)[1]) , -c(12:dim(df)[2])]
	df2 <- data.frame(df[c(1:11),], df[c(13:23),], df[c(25:35),])
	rownames(df2) <- c('Round',
				  'StockSizeBegining',
				  'Player',
				  'P1',
				  'P2',
				  'P3',
				  'P4',
				  'SumTotalCatch',
				  'IntermediateStockSize',
				  'Regeneration',
				  'NewStockSize')
				  
	df2 <- df2 [-3,-c(1,8:12,23)]
	dat <- t(df2)
	dat <- apply(dat, 2, as.numeric) # convert all values to numeric
	v <- !is.na(rowSums(dat[,3:6]))
	dat <- dat[v,]
	dat <- as.data.frame(dat)
	dat$Treatment <- rep(treatment, dim(dat)[1])
	return(dat)
}

extract.short<- function(file, sheet){
	df <- read.xls(file, sheet=sheet, header=T)
	treatment <- sheetNames(file)[sheet]
	# trim file
	df <- df[-c(24:dim(df)[1]) , -c(12:dim(df)[2])]
	df2 <- data.frame(df[c(1:11),], df[c(13:23),])
	rownames(df2) <- c('Round',
				  'StockSizeBegining',
				  'Player',
				  'P1',
				  'P2',
				  'P3',
				  'P4',
				  'SumTotalCatch',
				  'IntermediateStockSize',
				  'Regeneration',
				  'NewStockSize')
				  
	df2 <- df2 [-3,-c(1,8:12)]
	dat <- t(df2)
	dat <- apply(dat, 2, as.numeric) # convert all values to numeric
	v <- !is.na(rowSums(dat[,3:6]))
	dat <- dat[v,]
	dat <- as.data.frame(dat)
	dat$Treatment <- rep(treatment, dim(dat)[1])
	return(dat)
}

# extract.long(file=files[1], sheet=4) #prueba Uncertainty
# extract.short(file=files[1], sheet=2) # prueba

# Function for each exel file:

extract <- function(file){
	
	# Insert a for loop here to go through each file in 'files'
	info <- strsplit(file, split="([.])")
	info <- strsplit(info[[1]][1], split='_')
	
	place <- info[[1]][2]
	session <- info[[1]][4]
	date <- info[[1]][3]
	#file is the excel file from Caroline, there is one per session
	#Place is a character string with the place name
	a <- extract.long(file=file, sheet=1) 
	b <- extract.long(file=file, sheet=4)
	c <- extract.short(file=file, sheet=2)
	d <- extract.short(file=file, sheet=3) 
	z <- rbind(a,b,c,d)
	z$Place <- rep(place, dim(z)[1])
	z$Session <- rep(session, dim(z)[1])
	z$Date <- rep(date, dim(z)[1])
	return(z)
}

setwd("~/Dropbox/BEST/Colombia/0_Game data/TAGANGA")
files <- list.files()

taganga <- extract(files[1])
for (i in 2:length(files)){
	part <- extract(files[i])
	taganga <- rbind(taganga, part)
}

setwd("~/Dropbox/BEST/Colombia/0_Game data/BUENAVISTA")
files <- list.files()

buenavista<- extract(files[1])
for (i in 2:length(files)){
	part <- extract(files[i])
	buenavista <- rbind(buenavista, part)
}


setwd("~/Dropbox/BEST/Colombia/0_Game data/LAS FLORES")
files <- list.files()

flores<- extract(files[1])
for (i in 2:length(files)){
	part <- extract(files[i])
	flores <- rbind(flores, part)
}

setwd("~/Dropbox/BEST/Colombia/0_Game data/TASAJERA")
files <- list.files()

tasajera<- extract(files[1])
for (i in 2:length(files)){
	part <- extract(files[i])
	tasajera <- rbind(tasajera, part)
}

## Due to different files naming, there is errors on the tables for each place. Now I correct them before merging them on the full dataset. Place and Session are exchanged
 
head(tasajera) # error
head(taganga) # right
head(flores) # error
head(buenavista) # error

# It's easier to keep taganga as it is, change the funtion and run it again with right order.

# Now merge datasets

full.data <- rbind(taganga, flores, tasajera, buenavista)

setwd("~/Dropbox/BEST/Colombia/0_Game data")
write.csv(full.data, file='full_data_short.csv')

## Make it long data
require(reshape)
full.data <- melt(full.data, id.vars=names(full.data)[c(1,2,7:14)], variable_name='Player')

write.csv(full.data, file='full_data_long.csv')