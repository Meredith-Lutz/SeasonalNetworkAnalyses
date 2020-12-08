######################################
######################################
##### Temporal Network Analysis ######
######################################
######################################
library(RPostgreSQL)
library(igraph)
library(chron)
library(stringr)

#########################
#########################
### Preparing MF Data ###
#########################
#########################
setwd('G:/My Drive/Graduate School/Research/Projects/TemporalNets/SeasonalNetworkAnalyses')

## Source functions
source('createNetworkFunction.R') #Edge weights are either counts or duration
source('createObsMatrix.R')
source('G:/My Drive/Graduate School/Research/AO/CleanAOData/CleanAOData/CleanSocialDataFunctions.R')


## Connect to database
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'diadema_fulvus', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'postgres')


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

table(rot6$pin_code_name)

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
setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data')

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

listAnimalsByGroup	<- list(diadema2, diadema3, fulvus2, fulvus3, verreauxi3, verreauxi6)
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


##################################
### Preparing social data sets ###
##################################
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

nets		<- list(d2nets, d3nets, f2nets, f3nets, v3nets, v6nets)

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



