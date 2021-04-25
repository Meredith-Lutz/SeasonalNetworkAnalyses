######################################
######################################
##### Temporal Network Analysis ######
######################################
######################################
library(RPostgreSQL)
library(igraph)
library(chron)
library(stringr)
library(lubridate)
library(mgcv)
library(lme4)

#########################
#########################
### Preparing MF Data ###
#########################
#########################
setwd('C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work/SeasonalNetworkAnalyses')
#setwd('G:/My Drive/Graduate School/Research/Projects/TemporalNets/SeasonalNetworkAnalyses')

## Source functions
source('createNetworkFunction.R') #Edge weights are either counts or duration
source('createObsMatrix.R')
source('C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work/CleanAOData/CleanSocialDataFunctions.R')
#source('G:/My Drive/Graduate School/Research/AO/CleanAOData/CleanAOData/CleanSocialDataFunctions.R')

## Connect to database
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = dbname, host = 'raja.db.elephantsql.com', port = 5432,
								 user = user, password = password)


allData	<- dbGetQuery(con, 'select group_id, pin_code_name, focal_start_time, 
	focal_end_time, focal_individual_id, behavior_time, actor, subject, category, behavior, start_stop,
	initiator, mutual, latitude, longitude from main_tables.all_focal_data_view;')
focalList	<- dbGetQuery(con, 'select main_tables.list_focals.session_start_time, 
				main_tables.list_focals.focal_start_time, main_tables.list_focals.focal_end_time,
				main_tables.list_focals.focal_individual_id, main_tables.list_sessions.group_id,
				main_tables.list_sessions.pin_code_name from main_tables.list_focals
				left join main_tables.list_sessions
				on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')

scanData	<- dbGetQuery(con, 'select * from main_tables.all_scan_data_view;')

# get rid of NAs
allData <- allData[!is.na(allData$focal_start_time),]

## Add 4 week rotation #'s to data
allData$rotation	<- 0
allData[allData$focal_start_time >= '2019-09-09' & allData$focal_start_time <= '2019-10-04',]$rotation	<- 1
allData[allData$focal_start_time >= '2019-10-07' & allData$focal_start_time <= '2019-11-01',]$rotation	<- 2
allData[allData$focal_start_time >= '2019-11-04' & allData$focal_start_time <= '2019-11-29',]$rotation	<- 3
allData[allData$focal_start_time >= '2019-12-02' & allData$focal_start_time <= '2020-01-17',]$rotation	<- 4
allData[allData$focal_start_time >= '2020-01-20' & allData$focal_start_time <= '2020-02-14',]$rotation	<- 5
allData[allData$focal_start_time >= '2020-02-17' & allData$focal_start_time <= '2020-03-13',]$rotation	<- 6

###########################
### Match up start ends ###
###########################
dyadIDs	<- apply(as.matrix(allData[, c('actor', 'subject')]), 1, createID)
allDataID	<- cbind(allData, dyadIDs)
allDataIDNoNA	<- allDataID[is.na(allDataID$actor) == FALSE,]

focal_start_str	<- data.frame(str_split_fixed(as.character(allDataIDNoNA$focal_start_time), ' ', n = 2))
colnames(focal_start_str)	<- c('focal_date', 'focal_time')
focal_start_chron	<- chron(dates. = as.character(focal_start_str$focal_date), times. = as.character(focal_start_str$focal_time),
                           format = c(dates = 'y-m-d', times = 'h:m:s'))
behavior_time_str	<- data.frame(str_split_fixed(as.character(allDataIDNoNA$behavior_time), ' ', n = 2))
colnames(behavior_time_str)	<- c('behav_date', 'behav_time')
behavior_time_chron	<- chron(dates. = as.character(behavior_time_str$behav_date), times. = as.character(behavior_time_str$behav_time),
                             format = c(dates = 'y-m-d', times = 'h:m:s'))
allDataIDNoNA$focal_start_chron	<- focal_start_chron
allDataIDNoNA$behavior_time_chron	<- behavior_time_chron

social	<- allDataIDNoNA[is.na(allDataIDNoNA$category) == FALSE,]
affil		<- social[social$category == 'Affilative' & social$pin_code_name != 'Odilon',]
agg		<- social[social$category == 'Aggressive' & social$pin_code_name != 'Odilon' & social$pin_code_name != 'Sonne' & social$pin_code_name != 'Mamy',]
sub		<- social[social$category == 'Submissive' & social$pin_code_name != 'Odilon' & social$pin_code_name != 'Sahoby' & social$pin_code_name != 'Sonne' & social$pin_code_name != 'Mamy',]
info		<- social[social$category == 'Information' & social$pin_code_name == 'Meredith',]
grt		<- social[social$behavior == 'Greet' & (social$pin_code_name == 'Onja' | social$pin_code_name == 'Hasina' | social$pin_code_name == 'Diary'),]
socialSub	<- rbind(affil, agg, sub, info, grt)

cleanedData	<- data.frame()
errorFile	<- data.frame()

cleaned	<- cleanAllFocalData(socialSub, cleanedData, errorFile)

socialData	<- cleaned[[1]]



#####################################
### Getting focal list sorted out ###
#####################################
focalList	<- focalList[focalList$focal_individual_id != 'Ml',]

focalList$rotation	<- 0
focalList[focalList$focal_start_time >= '2019-09-09' & focalList$focal_start_time <= '2019-10-04',]$rotation	<- 1
focalList[focalList$focal_start_time >= '2019-10-07' & focalList$focal_start_time <= '2019-11-01',]$rotation	<- 2
focalList[focalList$focal_start_time >= '2019-11-04' & focalList$focal_start_time <= '2019-11-29',]$rotation	<- 3
focalList[focalList$focal_start_time >= '2019-12-02' & focalList$focal_start_time <= '2020-01-17',]$rotation	<- 4
focalList[focalList$focal_start_time >= '2020-01-20' & focalList$focal_start_time <= '2020-02-14',]$rotation	<- 5
focalList[focalList$focal_start_time >= '2020-02-17' & focalList$focal_start_time <= '2020-03-13',]$rotation	<- 6

focalListNoVo				<- focalList[!(focalList$focal_individual_id == 'Vo' & focalList$rotation >= 2),]

focalListNoOdilon				<- focalListNoVo[focalListNoVo$pin_code_name != 'Odilon',]
list_focal_start_str			<- data.frame(str_split_fixed(as.character(focalListNoOdilon$focal_start_time), ' ', n = 2))
colnames(list_focal_start_str)	<- c('focal_date', 'focal_time')
list_focal_start_chron			<- chron(dates. = as.character(list_focal_start_str$focal_date), times. = as.character(list_focal_start_str$focal_time),
                                  format = c(dates = 'y-m-d', times = 'h:m:s'))
focal_stop_str				<- data.frame(str_split_fixed(as.character(focalListNoOdilon$focal_end_time), ' ', n = 2))
colnames(focal_stop_str)		<- c('behav_date', 'behav_time')
focal_stop_chron				<- chron(dates. = as.character(focal_stop_str$behav_date), times. = as.character(focal_stop_str$behav_time),
                             format = c(dates = 'y-m-d', times = 'h:m:s'))
focalListNoOdilon$focal_start_chron		<- list_focal_start_chron
focalListNoOdilon$focal_stop_chron		<- focal_stop_chron
focalListNoOdilon$focalDuration		<- as.numeric(focalListNoOdilon$focal_stop_chron - focalListNoOdilon$focal_start_chron)*24 #puts it in hours

#No Odilon is ok for affiliative behaviors, for other behaviors need to subset out various other observers' data

###########################
###########################
### Preparing KMNP Data ###
###########################
###########################
#setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data')

socialDataKMNP	<- read.csv('All_Social_Data_8-25-20.csv', stringsAsFactors = FALSE)
scan			<- read.csv('Focal Activity NN combined clean 11-16-20_ML.csv')

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

socialDataKMNP$monthNum	<- ifelse(socialDataKMNP$Month == 'Jan', 1, ifelse(
						socialDataKMNP$Month == 'Feb', 2, ifelse(
						socialDataKMNP$Month == 'Mar', 3, ifelse(
						socialDataKMNP$Month == 'Apr', 4, ifelse(
						socialDataKMNP$Month == 'May', 5, ifelse(
						socialDataKMNP$Month == 'Jun', 6, ifelse(
						socialDataKMNP$Month == 'Jul', 7, ifelse(
						socialDataKMNP$Month == 'Aug', 8, ifelse(
						socialDataKMNP$Month == 'Sep', 9, ifelse(
						socialDataKMNP$Month == 'Oct', 10, ifelse(
						socialDataKMNP$Month == 'Nov', 11, 12)))))))))))
#VL, VV, TH, VN only ones in group, Laura's scan data isn't available, so don't know how many hours of obs to add
gpIIISocial	<- socialDataKMNP[socialDataKMNP$Focal %in% c('Thor', 'Venus', 'Vervet', 'Velo') & socialDataKMNP$Observer != 'Laura' &
					as.character(socialDataKMNP$Date) >= '2018-06-01' & as.character(socialDataKMNP$Date) < '2019-06-01',]
gpVISocial	<- socialDataKMNP[socialDataKMNP$Focal %in% c('Mafia', 'Nectar', 'Neptune', 'Nancy', 'Nyx', 'Eclair', 'Emily') & socialDataKMNP$Observer != 'Laura' &
					as.character(socialDataKMNP$Date) >= '2018-06-01' & as.character(socialDataKMNP$Date) < '2019-06-01',]

gpIIIScan	<- scan[scan$Group == 'III',]
gpVIScan	<- scan[scan$Group == 'VI' & scan$Focal %in% c('Mafia', 'Nectar', 'Nyx', 'Eclair', 'Emily', 'Egret', 'Nancy', 'Neptune'),]

gpIII1819Scan	<- rbind(gpIIIScan[gpIIIScan$monthNum >= 6 & gpIIIScan$Year == 2018,], gpIIIScan[gpIIIScan$monthNum <= 5 & gpIIIScan$Year == 2019,])
gpVI1819Scan	<- rbind(gpVIScan[gpVIScan$monthNum >= 6 & gpVIScan$Year == 2018,], gpVIScan[gpVIScan$monthNum <= 5 & gpVIScan$Year == 2019,])

nonMLScan	<- gpIII1819Scan[gpIII1819Scan$Observer != 'Meredith',] #Scans are occasionally skipped in AO, but doesn't reflect obstime
nonMLScangpVI	<- gpVI1819Scan[gpVI1819Scan$Observer != 'Meredith',] #Scans are occasionally skipped in AO, but doesn't reflect obstime


obsTimePerAnimalPerMonth		<- aggregate(nonMLScan[,1], by = list(month = nonMLScan$monthNum, focal_individual_id = nonMLScan$Focal), FUN = length)
colnames(obsTimePerAnimalPerMonth)	<- c('month', 'focal_individual_id', 'numScans')
obsTimePerAnimalPerMonth$numHours	<- obsTimePerAnimalPerMonth$numScans/6

obsTimePerAnimalPerMonth6		<- aggregate(nonMLScangpVI[,1], by = list(month = nonMLScangpVI$monthNum, focal_individual_id = nonMLScangpVI$Focal), FUN = length)
colnames(obsTimePerAnimalPerMonth6)	<- c('month', 'focal_individual_id', 'numScans')
obsTimePerAnimalPerMonth6$numHours	<- obsTimePerAnimalPerMonth6$numScans/6

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

MLgpVI	<- focalList[focalList$group_id == '6',]

focal_start_str			<- data.frame(str_split_fixed(as.character(MLgpVI$focal_start_time), ' ', n = 2))
colnames(focal_start_str)	<- c('focal_date', 'focal_start_time')
focal_start_chron			<- chron(dates. = as.character(focal_start_str$focal_date), times. = as.character(focal_start_str$focal_start_time),
						format = c(dates = 'y-m-d', times = 'h:m:s'))
focal_end_str			<- data.frame(str_split_fixed(as.character(MLgpVI$focal_end_time), ' ', n = 2))
colnames(focal_end_str)		<- c('focal_date', 'focal_end_time')
focal_end_chron			<- chron(dates. = as.character(focal_end_str$focal_date), times. = as.character(focal_end_str$focal_end_time),
						format = c(dates = 'y-m-d', times = 'h:m:s'))
MLgpVI$focal_start_chron	<- focal_start_chron
MLgpVI$focal_end_chron		<- focal_end_chron
MLgpVI$duration			<- MLgpVI$focal_end_chron - MLgpVI$focal_start_chron

MLgpVI$month			<- NA
MLgpVI[MLgpVI$focal_start_time >= '2019-01-01' & MLgpVI$focal_start_time < '2019-02-01',]$month	<- 1
MLgpVI[MLgpVI$focal_start_time >= '2019-02-01' & MLgpVI$focal_start_time < '2019-03-01',]$month	<- 2
MLgpVI[MLgpVI$focal_start_time >= '2019-03-01',]$month	<- 3

obsTimePerAnimalPerMonthML6		<- aggregate(MLgpVI$duration*24, by = list(month = MLgpVI$month, focal_individual_id = MLgpVI$focal_individual_id), FUN = sum)

obsTimePerAnimalPerMonth6$numHoursWithML	<- obsTimePerAnimalPerMonth6$numHours
obsTimePerAnimalPerMonth6[4:6, 'numHoursWithML']	<- obsTimePerAnimalPerMonth6[4:6, 'numHoursWithML'] + obsTimePerAnimalPerMonthML6[3:5, 'x']
obsTimePerAnimalPerMonth6[16:18, 'numHoursWithML']	<- obsTimePerAnimalPerMonth6[16:18, 'numHoursWithML'] + obsTimePerAnimalPerMonthML6[6:8, 'x']
obsTimePerAnimalPerMonth6[28:30, 'numHoursWithML']	<- obsTimePerAnimalPerMonth6[28:30, 'numHoursWithML'] + obsTimePerAnimalPerMonthML6[9:11, 'x']
obsTimePerAnimalPerMonth6[40:42, 'numHoursWithML']	<- obsTimePerAnimalPerMonth6[40:42, 'numHoursWithML'] + obsTimePerAnimalPerMonthML6[15:17, 'x']
obsTimePerAnimalPerMonth6[52:54, 'numHoursWithML']	<- obsTimePerAnimalPerMonth6[52:54, 'numHoursWithML'] + obsTimePerAnimalPerMonthML6[12:14, 'x']
eclaire	<- data.frame(month = c(1,3), focal_individual_id = rep('Eclair', 2), numScans = rep(0, 2), numHours = rep(0, 2), numHoursWithML = obsTimePerAnimalPerMonthML6[1:2, 3])
nyx		<- data.frame(month = c(1,2), focal_individual_id = rep('Nyx', 2), numScans = rep(0, 2), numHours = rep(0, 2), numHoursWithML = obsTimePerAnimalPerMonthML6[18:19, 3])
obsTimePerAnimalPerMonth6	<- rbind(obsTimePerAnimalPerMonth6, eclaire, nyx)

###########################################
###########################################
### Generating monthly overlap matrices ###
###########################################
###########################################

#First assume that all animals in the group are able to overlap with everyone, then remove the appropriate amounts
diadema2	<- sort(c('On', 'Ta', 'Zr', 'Kr', 'Gg', 'Jb', 'Tk', 'Kt', 'Gr', 'Bl'))
diadema3	<- sort(c('Pk', 'Mk', 'An', 'Bk', 'Sm', 'Or'))
fulvus2	<- sort(c('Af', 'Kl', 'Sf', 'Fz', 'Ps', 'Pr', 'Mn', 'Ld'))
fulvus3	<- sort(c('Zk', 'Vt', 'So', 'Kd', 'Pm', 'Gy')) #removed Rk
verreauxi3	<- sort(c('Thor', 'Velo', 'Venus', 'Vervet'))
verreauxi6	<- sort(c('Emily', 'Nectar', 'Neptune', 'Mafia', 'Nancy'))

listAnimalsByGroup	<- list(diadema2, diadema3, fulvus2, fulvus3)
listGroups		<- c('Diadema 2', 'Diadema 3', 'Fulvus 2', 'Fulvus 3')
listFocalByGroup	<- list(focalListNoOdilon[focalListNoOdilon$group_id == listGroups[1],], focalListNoOdilon[focalListNoOdilon$group_id == listGroups[2],],
                         focalListNoOdilon[focalListNoOdilon$group_id == listGroups[3],], focalListNoOdilon[focalListNoOdilon$group_id == listGroups[4],])

allFocalDurations	<- list()

for(i in 1:4){ #for groups
  tempList <- list()
  for(j in 1:6){ #for rotations
    groupsFocals	<- listFocalByGroup[[i]]
    relevantFocals	<- groupsFocals[groupsFocals$rotation == j,]
    relevantFocals$focal_individual_id	<- factor(relevantFocals$focal_individual_id, levels = listAnimalsByGroup[[i]])
    focalTable		<- data.frame(focal_individual_id = listAnimalsByGroup[[i]], duration = tapply(relevantFocals$focalDuration, relevantFocals$focal_individual_id, FUN = sum))
    tempList[[j]]	<- focalTable
  }
  allFocalDurations[[i]]	<- tempList
}

#For KMNP
KMNPDurationsV3	<- list()
for(i in 1:12){
	monthObs	<- obsTimePerAnimalPerMonth[obsTimePerAnimalPerMonth$month == i, c(2,5)]
	KMNPDurationsV3[[i]]	<- monthObs
}

KMNPDurationsV6	<- list()
for(i in 1:12){
	monthObs	<- obsTimePerAnimalPerMonth6[obsTimePerAnimalPerMonth6$month == i,]
	monthObs$focal_individual_id	<- factor(monthObs$focal_individual_id, levels = verreauxi6)
   	focalTable		<- data.frame(focal_individual_id = verreauxi6, duration = tapply(monthObs$numHoursWithML, monthObs$focal_individual_id, FUN = sum))
	KMNPDurationsV6[[i]]	<- focalTable
}
	
		
d2ObsMatList	<- lapply(allFocalDurations[[1]], createObsMatrix) #Need to adjust rotation 1 for Vo
d3ObsMatList	<- lapply(allFocalDurations[[2]], createObsMatrix)
f2ObsMatList	<- lapply(allFocalDurations[[3]], createObsMatrix) #Need to adjust rotation 1 for Af
f3ObsMatList	<- lapply(allFocalDurations[[4]], createObsMatrix) #Need to adjust rotation 1 for Rk
v3ObsMatList	<- lapply(KMNPDurationsV3, createObsMatrix)
v6ObsMatList	<- lapply(KMNPDurationsV6, createObsMatrix)
allObsMatList	<- list(d2ObsMatList, d3ObsMatList, f2ObsMatList, f3ObsMatList)

################################
### Removing problem animals ###
################################
socialData1	<- socialData[socialData$actor != 'Ml' & socialData$subject != 'Ml' & socialData$actor != 'UNK' & socialData$subject != 'UNK',]
socialData2	<- socialData1[!((socialData1$actor == 'Vo' | socialData1$subject == 'Vo') & socialData1$startTime >= '2019-08-01'),]
socialData3	<- socialData2[!((socialData2$actor == 'Rk' | socialData2$subject == 'Rk') & socialData2$startTime >= '2019-08-01'),]
gpIIISocial	<- gpIIISocial[gpIIISocial$Initiator %in% verreauxi3 & gpIIISocial$Receiver %in% verreauxi3,]
gpVISocial	<- gpVISocial[gpVISocial$Initiator %in% verreauxi6 & gpVISocial$Receiver %in% verreauxi6,]


#################################################
### Plot Monthly Networks for Timeline Figure ###
#################################################
rot1	<- socialData3[socialData3$rotation == 1,]
rot2	<- socialData3[socialData3$rotation == 2,]
rot3	<- socialData3[socialData3$rotation == 3,]
rot4	<- socialData3[socialData3$rotation == 4,]
rot5	<- socialData3[socialData3$rotation == 5,]
rot6	<- socialData3[socialData3$rotation == 6,]

d2grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = diadema2, type = 'duration', durs = rot1$duration)
d3grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = diadema3, type = 'duration', durs = rot1$duration)
f2grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = fulvus2, type = 'duration', durs = rot1$duration)
f3grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = fulvus3, type = 'duration', durs = rot1$duration)

d2grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = diadema2, type = 'duration', durs = rot2$duration)
d3grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = diadema3, type = 'duration', durs = rot2$duration)
f2grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = fulvus2, type = 'duration', durs = rot2$duration)
f3grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = fulvus3, type = 'duration', durs = rot2$duration)

d2grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = diadema2, type = 'duration', durs = rot3$duration)
d3grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = diadema3, type = 'duration', durs = rot3$duration)
f2grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = fulvus2, type = 'duration', durs = rot3$duration)
f3grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = fulvus3, type = 'duration', durs = rot3$duration)

d2grm4	<- createNet(rot4$actor, rot4$subject, rot4$behavior, 'Groom', subjects = diadema2, type = 'duration', durs = rot4$duration)
d3grm4	<- createNet(rot4$actor, rot4$subject, rot4$behavior, 'Groom', subjects = diadema3, type = 'duration', durs = rot4$duration)
f2grm4	<- createNet(rot4$actor, rot4$subject, rot4$behavior, 'Groom', subjects = fulvus2, type = 'duration', durs = rot4$duration)
f3grm4	<- createNet(rot4$actor, rot4$subject, rot4$behavior, 'Groom', subjects = fulvus3, type = 'duration', durs = rot4$duration)

d2grm5	<- createNet(rot5$actor, rot5$subject, rot5$behavior, 'Groom', subjects = diadema2, type = 'duration', durs = rot5$duration)
d3grm5	<- createNet(rot5$actor, rot5$subject, rot5$behavior, 'Groom', subjects = diadema3, type = 'duration', durs = rot5$duration)
f2grm5	<- createNet(rot5$actor, rot5$subject, rot5$behavior, 'Groom', subjects = fulvus2, type = 'duration', durs = rot5$duration)
f3grm5	<- createNet(rot5$actor, rot5$subject, rot5$behavior, 'Groom', subjects = fulvus3, type = 'duration', durs = rot5$duration)

d2grm6	<- createNet(rot6$actor, rot6$subject, rot6$behavior, 'Groom', subjects = diadema2, type = 'duration', durs = rot6$duration)
d3grm6	<- createNet(rot6$actor, rot6$subject, rot6$behavior, 'Groom', subjects = diadema3, type = 'duration', durs = rot6$duration)
f2grm6	<- createNet(rot6$actor, rot6$subject, rot6$behavior, 'Groom', subjects = fulvus2, type = 'duration', durs = rot6$duration)
f3grm6	<- createNet(rot6$actor, rot6$subject, rot6$behavior, 'Groom', subjects = fulvus3, type = 'duration', durs = rot6$duration)

v3mats	<- list()
for(i in c(1:4, 6:12)){
	socialDat	<- gpIIISocial[gpIIISocial$monthNum == i,]
	print(i)
	behavior	<- 'Groom'
	monthMat	<- createNet(socialDat$Initiator, socialDat$Receiver, socialDat$Behavior, behavior, subjects = verreauxi3, type = 'duration', durs = socialDat$Duration.Seconds)
	v3mats[[i]]	<- monthMat
}

v6mats	<- list()
for(i in c(1:12)){
	socialDat	<- gpVISocial[gpVISocial$monthNum == i,]
	print(i)
	behavior	<- 'Groom'
	monthMat	<- createNet(socialDat$Initiator, socialDat$Receiver, socialDat$Behavior, behavior, subjects = verreauxi6, type = 'duration', durs = socialDat$Duration.Seconds)
	v6mats[[i]]	<- monthMat
}

#For simplicity right now, just make may a month of 0's even though the data hasn't been entered
v3mats[[5]]	<- v3mats[[12]]

mats	<- list(list(d2grm1, d2grm2, d2grm3, d2grm4, d2grm5, d2grm6), 
             list(d3grm1, d3grm2, d3grm3, d3grm4, d3grm5, d3grm6),
             list(f2grm1, f2grm2, f2grm3, f2grm4, f2grm5, f2grm6),
             list(f3grm1, f3grm2, f3grm3, f3grm4, f3grm5, f3grm6))

## Adjust for observation time
matRates	<- list() #these are in secs Grooming/hour
for(i in 1:4){
  tempList	<- list()
  for(j in 1:6){
    obsMat		<- allObsMatList[[i]][[j]]
    tempList[[j]]	<- mats[[i]][[j]]/obsMat
  }
  matRates[[i]]	<- tempList
}

## Zero out the NA's
for(i in 1:4){
  for(j in 1:6){
    matRates[[i]][[j]][is.na(matRates[[i]][[j]])]	<- 0
  }
}

v3MatRates	<- list()
for(i in 1:12){
	v3MatRates[[i]]	<- v3mats[[i]]/v3ObsMatList[[i]]
}

v6MatRates	<- list()
for(i in 1:12){
	v6MatRates[[i]]	<- v6mats[[i]]/v6ObsMatList[[i]]
}

## Zero out the NA's
for(i in 1:12){
    v6MatRates[[i]][is.na(v6MatRates[[i]])]	<- 0
}


d2nets	<- lapply(matRates[[1]], graph_from_adjacency_matrix, mode = "directed", weighted = TRUE, add.rownames = TRUE)
d3nets	<- lapply(matRates[[2]], graph_from_adjacency_matrix, mode = "directed", weighted = TRUE, add.rownames = TRUE)
f2nets	<- lapply(matRates[[3]], graph_from_adjacency_matrix, mode = "directed", weighted = TRUE, add.rownames = TRUE)
f3nets	<- lapply(matRates[[4]], graph_from_adjacency_matrix, mode = "directed", weighted = TRUE, add.rownames = TRUE)
v3nets	<- lapply(v3MatRates, graph_from_adjacency_matrix, mode = "directed", weighted = TRUE, add.rownames = TRUE)
v6nets	<- lapply(v6MatRates, graph_from_adjacency_matrix, mode = "directed", weighted = TRUE, add.rownames = TRUE)

#nets		<- list(d2nets, d3nets, f2nets, f3nets, v3nets, v6nets)
nets		<- list(d2nets, d3nets, f2nets, f3nets)

set.seed(7) #321 for 2-4, #7 for 1 and 6
layd2	<- layout.fruchterman.reingold(d2nets[[2]])
layd3	<- layout.fruchterman.reingold(d3nets[[1]])
layf2	<- layout.fruchterman.reingold(f2nets[[3]])
layf3	<- layout.fruchterman.reingold(f3nets[[1]])
layv3	<- layout.fruchterman.reingold(v3nets[[1]])
layv6	<- layout.fruchterman.reingold(v6nets[[1]])
lays	<- list(layd2, layd3, layf2, layf3, layv3, layv6)

pdf('verreauxi3.PDF', width = 8, height = 12)
### Color by community structure
par(mfrow = c(3, 4), mar = c(0, 0, 0, 0), lwd = 2, bg=NA)
for(i in 6:6){
  commStrx	<- list()
  for(j in 1:12){
    commStrx[[j]]	<- data.frame(comm = matrix(membership(cluster_optimal(nets[[i]][[j]])), dimnames = list(listAnimalsByGroup[[i]])))
    #print(commStrx[[j]])
    commStrx[[j]]$frameColor	<- 'black'
    commStrx[[j]]$nodeColor		<- gsub('1', 'forestgreen', commStrx[[j]]$comm)
    commStrx[[j]]$nodeColor		<- gsub('2', 'midnightblue', commStrx[[j]]$nodeColor)
    if(max(commStrx[[j]]$comm == 3)){
      commStrx[[j]]$nodeColor	<- gsub('3', 'goldenrod', commStrx[[j]]$nodeColor)
    }
    if(max(commStrx[[j]]$comm == 4)){
      commStrx[[j]]$nodeColor	<- gsub('3', 'goldenrod', commStrx[[j]]$nodeColor)
      commStrx[[j]]$nodeColor	<- gsub('4', 'darkcyan', commStrx[[j]]$nodeColor)
    }
    if(max(commStrx[[j]]$comm == 5)){
      commStrx[[j]]$nodeColor	<- gsub('3', 'goldenrod', commStrx[[j]]$nodeColor)
      commStrx[[j]]$nodeColor	<- gsub('4', 'darkcyan', commStrx[[j]]$nodeColor)
      commStrx[[j]]$nodeColor	<- gsub('5', 'lightcoral', commStrx[[j]]$nodeColor)
    }
    #if(i == 1 & j > 1){ #Voa
    #	commStrx[[j]][10,]$frameColor	<- NA
    #	commStrx[[j]][10,]$nodeColor	<- NA #Voa
    #}
    if(i == 3 & j == 1){ #Akofa - need to double check this
      commStrx[[j]][1,]$frameColor	<- NA
      commStrx[[j]][1,]$nodeColor	<- NA
    }
    #if(i == 4 & j > 1){ #Raketa
    #	commStrx[[j]][4,]$frameColor	<- NA
    # commStrx[[j]][4,]$nodeColor	<- NA
    #}
    #print(commStrx)
    plot.igraph(nets[[i]][[j]], layout = lays[[i]], 
                edge.color = 'grey35', edge.curved = F, vertex.label = NA, vertex.label.color = NA, vertex.frame.color = commStrx[[j]]$frameColor,
                vertex.size = 25, edge.arrow.size = 0.15, vertex.color = commStrx[[j]]$nodeColor, edge.width = E(nets[[i]][[j]])$weight/10,
                mark.groups = cluster_optimal(nets[[i]][[j]]), mark.col = rgb(0,0,0,0.1), 
                mark.border = c("forestgreen","midnightblue","goldenrod","darkcyan","lightcoral"),
                mark.border.cex = 200)
  }
}
dev.off()



###############################
### Plot coef of var thru t ###
###############################
#dev.off()
par(mfrow=c(2,2))
cvs <- as.data.frame(1)
for (j in 1:4){
  for (i in 1:6) {
    matRatesi <- diag.remove(matRates[[j]][[i]])
    cvs[i,] <- sd(matRatesi, na.rm=T)/mean(matRatesi, na.rm=T)
  }
  plot((1:6), cvs[,1], xlab = "Cycle", ylab = "Coeff of Variation", main = "Grooming", ylim = c(1, 5), pch = 20, cex=2)
}

###################################
###################################
### Verifying Seasonality in Net###
###################################
###################################
socialData3$week		<- isoweek(socialData3$focal_start_time)
socialData3$conWeek	<- ifelse(socialData3$week > 30, 
				   socialData3$week - 36, socialData3$week + 13)
focalListNoOdilon$week		<- isoweek(focalListNoOdilon$focal_start_time)
focalListNoOdilon$conWeek	<- ifelse(focalListNoOdilon$week > 30, 
				   focalListNoOdilon$week - 36, focalListNoOdilon$week + 13)
# real data starts with week 1 in the conWeek variable which loops betweem Dec and Jan
# training is weeks -4 to 0

groups				<- c("Diadema 2", "Diadema 3", "Fulvus 2", "Fulvus 3")
behaviors				<- c("Groom", "Play", "Contact", "Proximity")
summarizedWeeklyNetProps	<- data.frame(week = numeric(), 
						group = character(), behav = character(), 
						density = numeric(), avgStrength = numeric(),
						edgeDiff = numeric(), modularity = numeric())
summarizedMonthlyNetProps	<- data.frame(rotation = numeric(), 
						group = character(), behav = character(), 
						density = numeric(), avgStrength = numeric(),
						edgeDiff = numeric(), modularity = numeric())



cv	<- function(data){
	return(sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE))
}

#Weekly 
for(i in 1:24){
	for(j in groups){
		for(k in behaviors){
			#print(i)
			#print(j)
			#print(k)
			subset	<- socialData3[socialData3$conWeek == i & 
						socialData3$group_id == j & 
						socialData3$behavior == k,]
			#print(dim(subset))
			if(dim(subset)[1] == 0){
				next
			}
			obsSubset	<- focalListNoOdilon[focalListNoOdilon$conWeek == i &
					   focalListNoOdilon$group_id == j,]
			obsSubset$focal_individual_id	<- factor(obsSubset$focal_individual_id)
			focalTable		<- data.frame(focal_individual_id = levels(obsSubset$focal_individual_id), duration = tapply(obsSubset$focalDuration, obsSubset$focal_individual_id, FUN = sum))
			obsMat		<- createObsMatrix(focalTable)
			if(k == "Proximity"){
				prxMat	<- createNet(subset$actor, subset$subject, subset$behavior, k, subjects = levels(obsSubset$focal_individual_id), type = 'duration', durs = subset$duration)
				appMat	<- createNet(subset$actor, subset$subject, subset$behavior, k, subjects = levels(obsSubset$focal_individual_id), type = 'count')
 				prxMatAdj	<- prxMat/obsMat
				appMatAdj	<- appMat/obsMat
				prxNet	<- graph_from_adjacency_matrix(prxMatAdj, mode = "directed", weighted = TRUE, add.rownames = TRUE)
				appNet	<- graph_from_adjacency_matrix(appMatAdj, mode = "directed", weighted = TRUE, add.rownames = TRUE)
				prxDen	<- edge_density(prxNet)
				appDen	<- edge_density(appNet)
				prxStrength	<- mean(strength(prxNet, mode = "all", weights = E(prxNet)$weight))
				appStrength <- mean(strength(appNet, mode = "all", weights = E(appNet)$weight))
				prxMod	<- modularity(prxNet, membership = membership(cluster_optimal(prxNet)), E(prxNet)$weight)
				appMod	<- modularity(appNet, membership = membership(cluster_optimal(appNet)), E(appNet)$weight)
				prxEdgeDiff	<- cv(E(prxNet)$weight)
				appEdgeDiff	<- cv(E(appNet)$weight)
				prxLine	<- c(week = i, group = j, behav = k, density = prxDen, avgStrength = prxStrength, edgeDiff = prxEdgeDiff, modularity = prxMod)
				appLine	<- c(week = i, group = j, behav = "Approach", density = appDen, avgStrength = appStrength, edgeDiff = appEdgeDiff, modularity = appMod)		
				twoLines	<- rbind(prxLine, appLine)
				colnames(twoLines)	<- c("week", "group_id", "behavior", 
								"density", "avgStrength", "edgeDiff", "modularity")
				summarizedWeeklyNetProps<- rbind(summarizedWeeklyNetProps, twoLines, stringsAsFactors = FALSE)
			}
			else{
				behavMat	<- createNet(subset$actor, subset$subject, subset$behavior, k, subjects = levels(obsSubset$focal_individual_id), type = 'duration', durs = subset$duration)
				behavMatAdj	<- behavMat/obsMat
				behavNet	<- graph_from_adjacency_matrix(behavMatAdj, mode = "directed", weighted = TRUE, add.rownames = TRUE)
				behavDen	<- edge_density(behavNet)
				behavStrength<- mean(strength(behavNet, mode = "all", weights = E(behavNet)$weight))
				behavMod	<- modularity(behavNet, membership = membership(cluster_optimal(behavNet)), E(behavNet)$weight)
				behavEdgeDiff<- cv(E(behavNet)$weight)
				behavLine	<- c(week = i, group = j, behav = k, density = behavDen, avgStrength = behavStrength, edgeDiff = behavEdgeDiff, modularity = behavMod)
				summarizedWeeklyNetProps<- rbind(summarizedWeeklyNetProps, behavLine, stringsAsFactors = FALSE)
				colnames(summarizedWeeklyNetProps)	<- c("week", "group_id", "behavior", 
									"density", "avgStrength", "edgeDiff", "modularity")
			}
		}
	}
}

#Monthly
for(i in 1:6){
	for(j in groups){
		for(k in behaviors){
			#print(i)
			#print(j)
			#print(k)
			subset	<- socialData3[socialData3$rotation == i & 
						socialData3$group_id == j & 
						socialData3$behavior == k,]
			#print(dim(subset))
			if(dim(subset)[1] == 0){
				next
			}
			obsSubset	<- focalListNoOdilon[focalListNoOdilon$rotation == i &
					   focalListNoOdilon$group_id == j,]
			obsSubset$focal_individual_id	<- factor(obsSubset$focal_individual_id)
			focalTable		<- data.frame(focal_individual_id = levels(obsSubset$focal_individual_id), duration = tapply(obsSubset$focalDuration, obsSubset$focal_individual_id, FUN = sum))
			obsMat		<- createObsMatrix(focalTable)
			if(k == "Proximity"){
				prxMat	<- createNet(subset$actor, subset$subject, subset$behavior, k, subjects = levels(obsSubset$focal_individual_id), type = 'duration', durs = subset$duration)
				appMat	<- createNet(subset$actor, subset$subject, subset$behavior, k, subjects = levels(obsSubset$focal_individual_id), type = 'count')
 				prxMatAdj	<- prxMat/obsMat
				appMatAdj	<- appMat/obsMat
				prxNet	<- graph_from_adjacency_matrix(prxMatAdj, mode = "directed", weighted = TRUE, add.rownames = TRUE)
				appNet	<- graph_from_adjacency_matrix(appMatAdj, mode = "directed", weighted = TRUE, add.rownames = TRUE)
				prxDen	<- edge_density(prxNet)
				appDen	<- edge_density(appNet)
				prxStrength	<- mean(strength(prxNet, mode = "all", weights = E(prxNet)$weight))
				appStrength <- mean(strength(appNet, mode = "all", weights = E(appNet)$weight))
				prxMod	<- modularity(prxNet, membership = membership(cluster_optimal(prxNet)), E(prxNet)$weight)
				appMod	<- modularity(appNet, membership = membership(cluster_optimal(appNet)), E(appNet)$weight)
				prxEdgeDiff	<- cv(E(prxNet)$weight)
				appEdgeDiff	<- cv(E(appNet)$weight)
				prxLine	<- c(rotation = i, group = j, behav = k, density = prxDen, avgStrength = prxStrength, edgeDiff = prxEdgeDiff, modularity = prxMod)
				appLine	<- c(rotation = i, group = j, behav = "Approach", density = appDen, avgStrength = appStrength, edgeDiff = appEdgeDiff, modularity = appMod)		
				twoLines	<- rbind(prxLine, appLine)
				colnames(twoLines)	<- c("rotation", "group_id", "behavior", 
								"density", "avgStrength", "edgeDiff", "modularity")
				summarizedMonthlyNetProps<- rbind(summarizedMonthlyNetProps, twoLines, stringsAsFactors = FALSE)
			}
			else{
				behavMat	<- createNet(subset$actor, subset$subject, subset$behavior, k, subjects = levels(obsSubset$focal_individual_id), type = 'duration', durs = subset$duration)
				behavMatAdj	<- behavMat/obsMat
				behavNet	<- graph_from_adjacency_matrix(behavMatAdj, mode = "directed", weighted = TRUE, add.rownames = TRUE)
				behavDen	<- edge_density(behavNet)
				behavStrength<- mean(strength(behavNet, mode = "all", weights = E(behavNet)$weight))
				behavMod	<- modularity(behavNet, membership = membership(cluster_optimal(behavNet)), E(behavNet)$weight)
				behavEdgeDiff<- cv(E(behavNet)$weight)
				behavLine	<- c(rotation = i, group = j, behav = k, density = behavDen, avgStrength = behavStrength, edgeDiff = behavEdgeDiff, modularity = behavMod)
				summarizedMonthlyNetProps<- rbind(summarizedMonthlyNetProps, behavLine, stringsAsFactors = FALSE)
				colnames(summarizedMonthlyNetProps)	<- c("rotation", "group_id", "behavior", 
									"density", "avgStrength", "edgeDiff", "modularity")
			}
		}
	}
}

colnames(summarizedWeeklyNetProps)	<- c("week", "group_id", "behavior", 
		"density", "avgStrength", "edgeDiff", "modularity")

#Weekly
summarizedWeeklyNetProps$species	<- ifelse(summarizedWeeklyNetProps$group_id %in% c("Diadema 2", "Diadema 3"), "diadema", "fulvus")

summarizedWeeklyNetProps$week			<- as.numeric(summarizedWeeklyNetProps$week)
summarizedWeeklyNetProps$avgStrength	<- as.numeric(summarizedWeeklyNetProps$avgStrength)
summarizedWeeklyNetProps$density		<- as.numeric(summarizedWeeklyNetProps$density)
summarizedWeeklyNetProps$edgeDiff		<- as.numeric(summarizedWeeklyNetProps$edgeDiff)
summarizedWeeklyNetProps$modularity		<- as.numeric(summarizedWeeklyNetProps$modularity)
summarizedWeeklyNetProps$group_id		<- as.factor(summarizedWeeklyNetProps$group_id)
summarizedWeeklyNetProps$behavior		<- as.factor(summarizedWeeklyNetProps$behavior)
summarizedWeeklyNetProps$species		<- as.factor(summarizedWeeklyNetProps$species)

grm	<- summarizedWeeklyNetProps[summarizedWeeklyNetProps$behavior == "Groom",]
play	<- summarizedWeeklyNetProps[summarizedWeeklyNetProps$behavior == "Play",]
cnt	<- summarizedWeeklyNetProps[summarizedWeeklyNetProps$behavior == "Contact",]
app	<- summarizedWeeklyNetProps[summarizedWeeklyNetProps$behavior == "Approach",]
prx	<- summarizedWeeklyNetProps[summarizedWeeklyNetProps$behavior == "Proximity",]

#Monthly
summarizedMonthlyNetProps$species	<- ifelse(summarizedMonthlyNetProps$group_id %in% c("Diadema 2", "Diadema 3"), "diadema", "fulvus")

summarizedMonthlyNetProps$rotation		<- as.numeric(summarizedMonthlyNetProps$rotation)
summarizedMonthlyNetProps$avgStrength	<- as.numeric(summarizedMonthlyNetProps$avgStrength)
summarizedMonthlyNetProps$density		<- as.numeric(summarizedMonthlyNetProps$density)
summarizedMonthlyNetProps$edgeDiff		<- as.numeric(summarizedMonthlyNetProps$edgeDiff)
summarizedMonthlyNetProps$modularity	<- as.numeric(summarizedMonthlyNetProps$modularity)
summarizedMonthlyNetProps$group_id		<- as.factor(summarizedMonthlyNetProps$group_id)
summarizedMonthlyNetProps$behavior		<- as.factor(summarizedMonthlyNetProps$behavior)
summarizedMonthlyNetProps$species		<- as.factor(summarizedMonthlyNetProps$species)

summarizedMonthlyNetProps$color	<- ifelse(summarizedMonthlyNetProps$group_id == "Diadema 2", "#27408b",
							ifelse(summarizedMonthlyNetProps$group_id == "Diadema 3", "#36648b",
							ifelse(summarizedMonthlyNetProps$group_id == "Fulvus 2", "#8b5a00",
							"#eec900"))) 
grmMonth	<- summarizedMonthlyNetProps[summarizedMonthlyNetProps$behavior == "Groom",]
playMonth	<- summarizedMonthlyNetProps[summarizedMonthlyNetProps$behavior == "Play",]
cntMonth	<- summarizedMonthlyNetProps[summarizedMonthlyNetProps$behavior == "Contact",]
appMonth	<- summarizedMonthlyNetProps[summarizedMonthlyNetProps$behavior == "Approach",]
prxMonth	<- summarizedMonthlyNetProps[summarizedMonthlyNetProps$behavior == "Proximity",]


setwd("C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work")
png("summarizedProximityWeeklyNets.png", width = 8, height = 8, units = "in", res = 300)
par(mfrow = c(2,2))
for(i in 1:4){
	plot(prx$week, prx[,i+3], col = as.factor(prx$group_id), pch = 16, main = colnames(prx)[i+3], xlab = "Week", ylab = "")
	}
dev.off()

png("summarizedcontactMonthlyNets.png", width = 8, height = 8, units = "in", res = 300)
par(mfrow = c(2,2))
for(i in 1:4){
	plot(cntMonth$rotation, cntMonth[,i+3], col = cntMonth$color, pch = 16, main = colnames(cntMonth)[i+3], xlab = "Rotation", ylab = "")
	} 
dev.off()

############## 
### GAMM's ###
############## 

##Play results
#Need to adjust the nesting of group_id inside species 
#Significant effect of week for diadema but not fulvus, nice seasonal curve higher in dry season than wet season
model1 <- gamm(avgStrength ~ s(rotation, k = 4, by = species), random = list(group_id=~1), data = grmMonth, family = gaussian)

###Plot for presentation
#Sig effect of grm in fulvus
N			<- length(model1$gam$fit)
nd_rotation 	<- seq(1,6,length=N)
nd_diadema		<- data.frame(rotation = nd_rotation,  group_id = rep('Diadema 2', N), species = rep('diadema', N))
nd_fulvus		<- data.frame(rotation = nd_rotation, species = rep('fulvus', N), newgroup = rep('Fulvus 2', N))
predictedFulvusGrm	<- predict(model1$gam, newdata = nd_fulvus, type = 'response', se.fit = TRUE)
predictedDiademaGrm	<- predict(model1$gam, newdata = nd_diadema, type = 'response', se.fit = TRUE)

theta_hatFulvusGrm	<- predictedFulvusGrm$fit 
theta_hatDiademaGrm	<- predictedDiademaGrm$fit 

x 				<- nd_rotation
ub_thetaFulvusGrm		<- theta_hatFulvusGrm + qnorm(0.975)*predictedFulvusGrm$se.fit/2
lb_thetaFulvusGrm		<- theta_hatFulvusGrm - qnorm(0.975)*predictedFulvusGrm$se.fit/2
ub_thetaDiademaGrm	<- theta_hatDiademaGrm + qnorm(0.975)*predictedDiademaGrm$se.fit/2
lb_thetaDiademaGrm	<- theta_hatDiademaGrm - qnorm(0.975)*predictedDiademaGrm$se.fit/2
xx				<- c(x,rev(x))
yy_thetaFulvusGrm		<- c(ub_thetaFulvusGrm,rev(lb_thetaFulvusGrm))
yy_thetaDiademaGrm	<- c(ub_thetaDiademaGrm,rev(lb_thetaDiademaGrm))

fulvusLine	<- rgb(139, 90, 0, max = 255)
fulvusPoly	<- rgb(139/255, 90/255, 0/255, alpha = 0.25)
diademaLine	<- rgb(0, 32, 96, max = 255)
diademaPoly	<- rgb(0, 32/255, 96/255, alpha = 0.25)

png("PredictedDiademaFulvusGAMGroomingAvgStrength.png", units = 'in', width = 8, height = 8, res = 300)
matplot(x,cbind(theta_hatDiademaGrm, theta_hatFulvusGrm),lty = c(1, 1),lwd = c(2, 2),
	col = c(diademaLine, fulvusLine), cex.lab = 1.5, cex.axis = 1.5, 
	ylim = c(25, 125), ylab = 'Predicted Average Strength of Groom', 
	xaxt = "n", type = 'l', xlab = 'Month')
axis(1, cex.axis = 1.5, at = seq(from = 1, to = 6, length.out = 7), labels = c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
box()
polygon(xx, yy_thetaDiademaGrm, col = diademaPoly, border = NA)
polygon(xx, yy_thetaFulvusGrm, col = fulvusPoly, border = NA)
dev.off()

model1b <- gamm(avgStrength ~ s(rotation, k = 4, by = species), random = list(group_id=~1), data = playMonth, family = gaussian)

###Plot for presentation
#Sig effect of ply  in diadema
nd_rotation 	<- seq(1,6,length=N)
predictedFulvusPly	<- predict(model1b$gam, newdata = nd_fulvus, type = 'response', se.fit = TRUE)
predictedDiademaPly	<- predict(model1b$gam, newdata = nd_diadema, type = 'response', se.fit = TRUE)

theta_hatDiademaPly	<- predictedDiademaPly$fit 
theta_hatFulvusPly	<- predictedFulvusPly$fit 

ub_thetaDiademaPly	<- theta_hatDiademaPly + qnorm(0.975)*predictedDiademaPly$se.fit/2
lb_thetaDiademaPly	<- theta_hatDiademaPly - qnorm(0.975)*predictedDiademaPly$se.fit/2
ub_thetaFulvusPly		<- theta_hatFulvusPly + qnorm(0.975)*predictedFulvusPly$se.fit/2
lb_thetaFulvusPly		<- theta_hatFulvusPly - qnorm(0.975)*predictedFulvusPly$se.fit/2
yy_thetaDiademaPly	<- c(ub_thetaDiademaPly,rev(lb_thetaDiademaPly))
yy_thetaFulvusPly		<- c(ub_thetaFulvusPly,rev(lb_thetaFulvusPly))



png("PredictedDiademaFulvusGAMPlayAvgStrength.png", units = 'in', width = 8, height = 8, res = 300)
matplot(x,cbind(theta_hatDiademaPly, theta_hatFulvusPly),lty = c(1, 1),lwd = c(2, 2),
	col = c(diademaLine, fulvusLine), cex.lab = 1.5, cex.axis = 1.5, 
	ylim = c(-30, 100), ylab = 'Predicted Average Strength of Play', 
	xaxt = "n", type = 'l', xlab = 'Month')
axis(1, cex.axis = 1.5, at = seq(from = 1, to = 6, length.out = 7), labels = c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
box()
polygon(xx, yy_thetaDiademaPly, col = diademaPoly, border = NA)
polygon(xx, yy_thetaFulvusPly, col = fulvusPoly, border = NA)
dev.off()


png("MonthlyAveStrengthPlayGamm.png")
par(mfrow = c(1,2))
plot(model1$gam, shade = TRUE, select = 1, ylab = "Smoothed Effect of Month", xaxt = "n", xlab = "Month", main = "Diadema")
axis(1, at = seq(from = 1, to = 6, length.out = 7), labels = c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
abline(h=0, lty = 5, col = "red")

plot(model1$gam, shade = TRUE, select = 2, ylab = "Smoothed Effect of Month", xaxt = "n", xlab = "Month", main = "Fulvus")
axis(1, at = seq(from = 1, to = 6, length.out = 7), labels = c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
abline(h=0, lty = 5, col = "red")

#Significant effect of week for diadema but not fulvus, but struggling to estimate exact curve
model2 <- gamm(edgeDiff ~ s(week, by = species), random = list(group_id=~1), data = play, family = gaussian)
#No significant effect of week for either group, although slight seasonal variations for diadema
model3 <- gamm(density ~ s(week, by = species), random = list(group_id=~1), data = play, family = gaussian)
#Modularity a mess
model4 <- gamm(modularity ~ s(week, by = species), random = list(group_id=~1), data = play, family = gaussian)

##Grm
#Average strength does not depend on week in either species
#No time effects on anything, edgeDiff is closest but having hard time estimating the curve possibility of group size effects
model5 <- gamm(avgStrength ~ s(week, by = species), random = list(group_id=~1), data = grm, family = gaussian)
model6 <- gamm(edgeDiff ~ s(week, by = species), random = list(group_id=~1), data = grm, family = gaussian)
model7 <- gamm(density ~ s(week, by = species), random = list(group_id=~1), data = grm, family = gaussian)
model8 <- gamm(modularity ~ s(week, by = species), random = list(group_id=~1), data = grm, family = gaussian)

##Those are the 1 meter proximity
#Generally stronger seasonal effects for fulvus, significant effect for averageStrength
model9 <- gamm(avgStrength ~ s(week, by = species), random = list(group_id=~1), data = prx, family = gaussian)
model10 <- gamm(edgeDiff ~ s(week, by = species), random = list(group_id=~1), data = prx, family = gaussian)
model11 <- gamm(density ~ s(week, by = species), random = list(group_id=~1), data = prx, family = gaussian)
model12 <- gamm(modularity ~ s(week, by = species), random = list(group_id=~1), data = prx, family = gaussian)
	
model13 <- gamm(avgStrength ~ s(week, by = species), random = list(group_id=~1), data = cnt, family = gaussian)
model14 <- gamm(edgeDiff ~ s(week, by = species), random = list(group_id=~1), data = cnt, family = gaussian)
model15 <- gamm(density ~ s(week, by = species), random = list(group_id=~1), data = cnt, family = gaussian)
model16 <- gamm(modularity ~ s(week, by = species), random = list(group_id=~1), data = cnt, family = gaussian)
	
	
model17 <- gamm(avgStrength ~ s(week, by = group_id), random = list(group_id=~1), data = app, family = gaussian)
model18 <- gamm(edgeDiff ~ s(week, by = species), random = list(group_id=~1), data = app, family = gaussian)
model19 <- gamm(density ~ s(week, by = species), random = list(group_id=~1), data = app, family = gaussian)
model20 <- gamm(modularity ~ s(week, by = species), random = list(group_id=~1), data = app, family = gaussian)
	

	
########################################
########################################
### Temporal Network Models via Amen ###
########################################
########################################
library(amen)

demo	<- read.csv("C:/Users/cecil/OneDrive/Desktop/Github Work/demographicData.csv")

demo 	<- demo[order(demo$ID),]

##########################################
### Organizing demographic information ###
##########################################
d2demo	<- demo[demo$group == 'Diadema 2',]
d3demo	<- demo[demo$group == 'Diadema 3',]
f2demo	<- demo[demo$group == 'Fulvus 2',]
f3demo	<- demo[demo$group == 'Fulvus 3',]

###########################################
### Calculate rank & covariate matrices ###
###########################################
d2AgeDiff	<- 0*table(diadema2)%*%t(table(diadema2))
d3AgeDiff	<- 0*table(diadema3)%*%t(table(diadema3))

d2SexDiff	<- 0*table(diadema2)%*%t(table(diadema2))
d3SexDiff	<- 0*table(diadema3)%*%t(table(diadema3))

for(i in diadema2){
	for(j in diadema2){
		actorAge		<- d2demo[d2demo$ID == i,]$ageCat
		recipAge		<- d2demo[d2demo$ID == j,]$ageCat
		actorSex		<- d2demo[d2demo$ID == i,]$sex
		recipSex		<- d2demo[d2demo$ID == j,]$sex
		d2AgeDiff[i, j]	<- abs(actorAge - recipAge)
		d2SexDiff[i, j]	<- ifelse(actorSex == recipSex, 1, 0)
	}
}

for(i in diadema3){
	for(j in diadema3){
		actorAge		<- d3demo[d3demo$ID == i,]$ageCat
		recipAge		<- d3demo[d3demo$ID == j,]$ageCat
		actorSex		<- d3demo[d3demo$ID == i,]$sex
		recipSex		<- d3demo[d3demo$ID == j,]$sex
		d3AgeDiff[i, j]	<- abs(actorAge - recipAge)
		d3SexDiff[i, j]	<- ifelse(actorSex == recipSex, 1, 0)
	}
}

tempDyadCov	<- array(c(d2AgeDiff, d2SexDiff), dim = c(10,10,2))
dyadCov	<- array(rep(tempDyadCov, 6), dim = c(10,10,2,6))

d2demoNoVo	<- d2demo[d2demo$ID != "Vo",]
d2Sex		<- d2demoNoVo$sex
indivCov	<- array(rep(d2Sex, 6), dim = c(10, 1, 6))
y		<- array(unlist(matRates[[1]]), dim = c(10, 10, 6))

#Working with diadema 2
model1 <- ame_rep(y, Xdyad = dyadCov, Xrow = indivCov, Xcol = indivCov, 
		family = "nrm", R = 0, rvar = TRUE, cvar = TRUE, dcor = TRUE, 
		symmetric = FALSE)
# Error in if (!any(apply(X, 3, function(x) { : 
  missing value where TRUE/FALSE needed
# In addition: Warning messages:
  1: In var(c(x), na.rm = TRUE) : NAs introduced by coercion
  2: In var(c(x), na.rm = TRUE) : NAs introduced by coercion



