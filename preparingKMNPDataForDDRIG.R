#########################################################################
##### Preparing KMNP to Plot DDRIG nets for 2018-2019 for group III #####
#########################################################################

setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data')
library(RPostgreSQL)
library(chron)
library(stringr)

socialData	<- read.csv('All_Social_Data_8-25-20.csv')
scan		<- read.csv('Focal Activity NN combined clean 11-16-20_ML.csv')

scan$monthNum	<- ifelse(scan$Month == 'Jan', 1, ifelse(
					scan$Month == 'Feb', 2, ifelse(
					scan$Month == 'Mar', 3, ifelse(
					scan$Month == 'Apr', 4, ifelse(
					scan$Month == 'May', 5, ifelse(
					scan$Month == 'Jun', 6, ifelse(
					scan$Month == 'Jul', 7, ifelse(
					scan$Month == 'Aug', 8, ifelse(
					scan$Month == 'Sep', 9, ifelse(
					scan$Month == 'Oct', 10, ifelse(
					scan$Month == 'Nov', 11, 12)))))))))))

socialData$monthNum	<- ifelse(socialData$Month == 'Jan', 1, ifelse(
						socialData$Month == 'Feb', 2, ifelse(
						socialData$Month == 'Mar', 3, ifelse(
						socialData$Month == 'Apr', 4, ifelse(
						socialData$Month == 'May', 5, ifelse(
						socialData$Month == 'Jun', 6, ifelse(
						socialData$Month == 'Jul', 7, ifelse(
						socialData$Month == 'Aug', 8, ifelse(
						socialData$Month == 'Sep', 9, ifelse(
						socialData$Month == 'Oct', 10, ifelse(
						socialData$Month == 'Nov', 11, 12)))))))))))

gpIIIScan	<- scan[scan$Group == 'III',]
gpIIIScan$SB.Obs.ID	<- factor(gpIIIScan$SB.Obs.ID)
gpIII1819Scan	<- rbind(gpIIIScan[gpIIIScan$monthNum >= 6 & gpIIIScan$Year == 2018,], gpIIIScan[gpIIIScan$monthNum <= 5 & gpIIIScan$Year == 2019,])

nonMLScan	<- gpIII1819Scan[gpIII1819Scan$Observer != 'Meredith',] #Scans are occasionally skipped in AO, but doesn't reflect obstime

obsTimePerAnimalPerMonth		<- aggregate(nonMLScan[,1], by = list(month = nonMLScan$monthNum, focal_individual_id = nonMLScan$Focal), FUN = length)
colnames(obsTimePerAnimalPerMonth)	<- c('month', 'focal_individual_id', 'numScans')
obsTimePerAnimalPerMonth$numHours	<- obsTimePerAnimalPerMonth$numScans/6

###########################################
### Get Meredith's focal hour durations ###
###########################################

## Connect to database
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'verreauxi_2019', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'postgres')

focalList	<- dbGetQuery(con, 'select main_tables.list_focals.session_start_time, 
				main_tables.list_focals.focal_start_time, main_tables.list_focals.focal_end_time,
				main_tables.list_focals.focal_individual_id, main_tables.list_sessions.group_id,
				main_tables.list_sessions.pin_code_name from main_tables.list_focals
				left join main_tables.list_sessions
				on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')

MLgpIII	<- focalList[focalList$group_id == '3',]

focal_start_str			<- data.frame(str_split_fixed(as.character(MLgpIII$focal_start_time), ' ', n = 2))
colnames(focal_start_str)	<- c('focal_date', 'focal_start_time')
focal_start_chron			<- chron(dates. = as.character(focal_start_str$focal_date), times. = as.character(focal_start_str$focal_start_time),
						format = c(dates = 'y-m-d', times = 'h:m:s'))
focal_end_str			<- data.frame(str_split_fixed(as.character(MLgpIII$focal_end_time), ' ', n = 2))
colnames(focal_end_str)		<- c('focal_date', 'focal_end_time')
focal_end_chron			<- chron(dates. = as.character(focal_end_str$focal_date), times. = as.character(focal_end_str$focal_end_time),
						format = c(dates = 'y-m-d', times = 'h:m:s'))
MLgpIII$focal_start_chron	<- focal_start_chron
MLgpIII$focal_end_chron		<- focal_end_chron
MLgpIII$duration			<- MLgpIII$focal_end_chron - MLgpIII$focal_start_chron

MLgpIII$month			<- NA
MLgpIII[MLgpIII$focal_start_time >= '2019-01-01' & MLgpIII$focal_start_time < '2019-02-01',]$month	<- 1
MLgpIII[MLgpIII$focal_start_time >= '2019-02-01' & MLgpIII$focal_start_time < '2019-03-01',]$month	<- 2
MLgpIII[MLgpIII$focal_start_time >= '2019-03-01',]$month	<- 3

obsTimePerAnimalPerMonthML		<- aggregate(MLgpIII$duration*24, by = list(month = MLgpIII$month, focal_individual_id = MLgpIII$focal_individual_id), FUN = sum)

obsTimePerAnimalPerMonth$numHoursWithML	<- obsTimePerAnimalPerMonth$numHours
obsTimePerAnimalPerMonth[1:3, 'numHoursWithML']	<- obsTimePerAnimalPerMonth[1:3, 'numHoursWithML'] + obsTimePerAnimalPerMonthML[1:3, 'x']
obsTimePerAnimalPerMonth[13:15, 'numHoursWithML']	<- obsTimePerAnimalPerMonth[13:15, 'numHoursWithML'] + obsTimePerAnimalPerMonthML[4:6, 'x']
obsTimePerAnimalPerMonth[25:27, 'numHoursWithML']	<- obsTimePerAnimalPerMonth[25:27, 'numHoursWithML'] + obsTimePerAnimalPerMonthML[7:9, 'x']
obsTimePerAnimalPerMonth[37:39, 'numHoursWithML']	<- obsTimePerAnimalPerMonth[37:39, 'numHoursWithML'] + obsTimePerAnimalPerMonthML[10:12, 'x']