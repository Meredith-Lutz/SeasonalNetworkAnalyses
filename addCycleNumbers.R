setwd('G:/My Drive/Graduate School/Research/Projects/TemporalNets')
library(RPostgreSQL)
library(chron)
library(stringr)

cycleDates	<- read.csv('cycleDates.csv')

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "diadema_fulvus",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "postgres")

listFocals 	<- dbGetQuery(con, "SELECT * from main_tables.list_focals
					left join main_tables.list_sessions
					on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;")

focal_start_str	<- data.frame(str_split_fixed(as.character(listFocals$focal_start_time), ' ', n = 2))
colnames(focal_start_str)	<- c('focal_date', 'focal_time')
focal_start_chron	<- chron(dates. = as.character(focal_start_str$focal_date), times. = as.character(focal_start_str$focal_time),
	format = c(dates = 'y-m-d', times = 'h:m:s'))
cycleStart_str	<- data.frame(str_split_fixed(as.character(cycleDates$startDate), ' ', n = 2))
colnames(cycleStart_str)	<- c('cycleStart_date', 'cycleStart_time')
cycleStart_chron	<- chron(dates. = as.character(cycleStart_str$cycleStart_date), times. = as.character(cycleStart_str$cycleStart_time),
	format = c(dates = 'y-m-d', times = 'h:m:s'))
cycleStop_str	<- data.frame(str_split_fixed(as.character(cycleDates$endDate), ' ', n = 2))
colnames(cycleStop_str)	<- c('cycleStop_date', 'cycleStop_time')
cycleStop_chron	<- chron(dates. = as.character(cycleStop_str$cycleStop_date), times. = as.character(cycleStop_str$cycleStop_time),
	format = c(dates = 'y-m-d', times = 'h:m:s'))
listFocals$focal_start_chron	<- focal_start_chron
cycleDates$cycleStart_chron	<- cycleStart_chron
cycleDates$cycleStop_chron	<- cycleStop_chron

listFocals$cycleNumber	<- NA
for(i in 1:dim(cycleDates)[1]){
	cycleStart	<- cycleDates[i,]$cycleStart_chron
	cycleStop	<- cycleDates[i,]$cycleStop_chron
	cycleNumber	<- cycleDates[i,]$cycle
	listFocals[focal_start_chron < cycleStop & focal_start_chron > cycleStart, ]$cycleNumber	<- cycleNumber
}

write.csv(listFocalsCleaned, 'focalCycleNumbers.csv')

listFocalsCleaned	<- listFocals[listFocals$pin_code_name != 'Odilion',]
