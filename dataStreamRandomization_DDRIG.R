################################################################
##### Generate simulation results to show data sufficiency #####
################################################################

library(RPostgreSQL)
library(asnipe)
library(aweek)

setwd('G:/My Drive/Graduate School/Research/Projects/TemporalNets')
source('createNetworkFunction.R')
source('createObsMatrix.R')

## Connect to database
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'diadema_fulvus', host = 'localhost', port = 5433,
								 user = 'postgres', password = 'Animalbehavior1#')

allData	<- dbGetQuery(con, 'select group_id, pin_code_name, focal_start_time, 
	focal_end_time, focal_individual_id, behavior_time, actor, subject, category, behavior, start_stop,
	initiator, mutual, latitude, longitude from main_tables.all_focal_data_view;')
focalListAll	<- dbGetQuery(con, 'select main_tables.list_focals.session_start_time, 
				main_tables.list_focals.focal_start_time, main_tables.list_focals.focal_end_time,
				main_tables.list_focals.focal_individual_id, main_tables.list_sessions.group_id,
				main_tables.list_sessions.pin_code_name from main_tables.list_focals
				left join main_tables.list_sessions
				on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')
day1	<- '2019-09-09'
ow <- set_week_start("Sunday")
allData$weekNum	<- date2week(allData$focal_start_time, numeric = TRUE)
focalListAll$weekNum	<- date2week(focalListAll$focal_start_time, numeric = TRUE)
table(focalListAll$group_id, focalListAll$weekNum)

grm		<- allData[allData$category == 'Aggressive' & allData$start_stop == 'Start',]

grmD3A	<- grm[grm$group_id == 'Diadema 3' & grm$weekNum == 37,]
grmD3		<- grmD3A[grmD3A$subject != 'Ml' & grmD3A$actor != 'Ml',]
focalListD3	<- focalListAll[focalListAll$group_id == 'Diadema 3' & focalListAll$focal_individual_id != 'Ml' & focalListAll$weekNum == 37, 4]
grmD2		<- grm[grm$group_id == 'Diadema 2' & grm$weekNum == 39,]
focalListD2	<- focalListAll[focalListAll$group_id == 'Diadema 2' & focalListAll$weekNum == 38, 4]
grmF3		<- grm[grm$group_id == 'Fulvus 3' & grm$weekNum == 38,]
focalListF3	<- focalListAll[focalListAll$group_id == 'Fulvus 3' & focalListAll$weekNum == 38, 4]
grmF2		<- grm[grm$group_id == 'Fulvus 2' & grm$weekNum == 39,]
focalListF2	<- focalListAll[focalListAll$group_id == 'Fulvus 2' & focalListAll$weekNum == 39, 4]

###################################################################
##### Implement data stream permuatation based on Farine 2017 #####
###################################################################
permuteData	<- function(data, numberPerm, listFocalsID){
	data		<- data[!is.na(data$behavior),]
	testStats	<- c()
	listFocals	<- unique(data$focal_start_time)
	listFocalsNoNA	<- listFocals[!is.na(listFocals)]
	count	<- 0
	while(count < numberPerm){
		print(paste('Working on permutation ', count))
		firstFocalTime	<- sample(listFocals, 1)
		secondFocalTime	<- sample(listFocals, 1)
		firstFocalData	<- data[data$focal_start_time == firstFocalTime & is.na(data$focal_start_time) == FALSE,]
		secondFocalData	<- data[data$focal_start_time == secondFocalTime & is.na(data$focal_start_time) == FALSE,]
		firstFocalAnimal	<- unique(firstFocalData$focal_individual_id)
		secondFocalAnimal	<- unique(secondFocalData$focal_individual_id)	
		if(firstFocalAnimal != secondFocalAnimal){ #two unique focal animals
			firstParticipants	<- c(firstFocalData$actor, firstFocalData$subject)
			firstPartners	<- unique(firstParticipants)
			firstPartners	<- firstPartners[firstPartners != firstFocalAnimal]
			secondParticipants<- c(secondFocalData$actor, secondFocalData$subject)
			secondPartners	<- unique(secondParticipants)
			secondPartners	<- secondPartners[secondPartners!= secondFocalAnimal]
			firstToSwap		<- sample(firstPartners, 1)
			secondToSwap	<- sample(secondPartners, 1)
			if(firstToSwap != secondToSwap){
				if(firstToSwap != secondFocalAnimal){
					if(secondToSwap != firstFocalAnimal){ ### 4 unique animals, do the swap
						firstFocalData$actor	<- gsub(firstToSwap, secondToSwap, firstFocalData$actor)
						firstFocalData$subject	<- gsub(firstToSwap, secondToSwap, firstFocalData$subject)
						secondFocalData$actor	<- gsub(secondToSwap, firstToSwap, secondFocalData$actor)
						secondFocalData$subject	<- gsub(secondToSwap, firstToSwap, secondFocalData$subject)
						
						### Replace new data into the old dataset
						oldData1	<- data[data$focal_start_time != firstFocalTime,] # get rid of 1st focal data
						oldData	<- oldData1[oldData1$focal_start_time != secondFocalTime,] #get rid of 2nd focal data
						data	<- rbind(oldData, firstFocalData, secondFocalData)
						
						count	<- count + 1 #successful swap
						
						#calculate the net
						tempNet	<- createNet(data$actor, data$subject, data$behavior, data[1,]$behavior, 
							subjects = sort(unique(data$focal_individual_id)), type = 'count')
						
						print(tempNet)
						#print(dim(tempNet))

						#adjust for obs time
						focalAnimals	<- sort(unique(data$focal_individual_id))
						#print(focalAnimals)
						#print(listFocalsID)
						focalTable		<- data.frame(table(listFocalsID))
						#print(focalTable)
						obsMat		<- createObsMatrix(focalTable)
						print(obsMat)
						tempNetAdj		<- tempNet/obsMat
						
						##Calculate test stat
						listEdgeWeights	<- sort(as.numeric(tempNetAdj))[(dim(tempNetAdj)[1]+1):dim(tempNetAdj)[1]^2]
						testStatTemp	<- var(listEdgeWeights)/mean(listEdgeWeights)
						testStats		<- c(testStats, testStatTemp)
					}
				}
			}
		}
	}
	return(testStats)
}
	
permCVD3	<-permuteData(grmD3, 1000, focalListD3)
obsNetD3	<- createNet(grmD3$actor, grmD3$subject, grmD3$category, grmD3[1,]$behavior, 
							subjects = sort(unique(grmD3$focal_individual_id)), type = 'count')	
focalAnimalsD3	<- sort(unique(grmD3$focal_individual_id))
focalTableD3	<- data.frame(table(focalListD3))
obsMatD3		<- createObsMatrix(focalTableD3)
obsNetAdjD3		<- obsNetD3/obsMatD3
listEdgeWeightsD3	<- sort(as.numeric(obsNetAdjD3))[(dim(obsNetAdjD3)[1]+1):dim(obsNetAdjD3)[1]^2]
obsTestStatD3	<- var(listEdgeWeightsD3)/mean(listEdgeWeightsD3)

permCVD2	<-permuteData(grmD2, 1000, focalListD2)
obsNetD2	<- createNet(grmD2$actor, grmD2$subject, grmD2$category, grmD2[1,]$behavior, 
							subjects = sort(unique(grmD2$focal_individual_id)), type = 'count')	
focalAnimalsD2	<- sort(unique(grmD2$focal_individual_id))
focalTableD2	<- data.frame(table(focalListD2))
obsMatD2		<- createObsMatrix(focalTableD2)
obsNetAdjD2		<- obsNet/obsMat
listEdgeWeightsD2	<- sort(as.numeric(obsNetAdjD2))[(dim(obsNetAdjD2)[1]+1):dim(obsNetAdjD2)[1]^2]
obsTestStatD2	<- var(listEdgeWeightsD2)/mean(listEdgeWeightsD2)

permCVF3	<-permuteData(grmF3, 1000, focalListF3)
obsNetF3	<- createNet(grmF3$actor, grmF3$subject, grmF3$category, grmF3[1,]$behavior, 
							subjects = sort(unique(grmF3$focal_individual_id)), type = 'count')	
focalAnimalsF3	<- sort(unique(grmF3$focal_individual_id))
focalTableF3	<- data.frame(table(focalListF3))
obsMatF3		<- createObsMatrix(focalTableF3)
obsNetAdjF3		<- obsNetF3/obsMatF3
listEdgeWeightsF3	<- sort(as.numeric(obsNetAdjF3))[(dim(obsNetAdjF3)[1]+1):dim(obsNetAdjF3)[1]^2]
obsTestStatF3	<- var(listEdgeWeightsF3)/mean(listEdgeWeightsF3)

permCVF2	<-permuteData(grmF2, 1000, focalListF2)
obsNetF2	<- createNet(grmF2$actor, grmF2$subject, grmF2$category, grmF2[1,]$behavior, 
							subjects = sort(unique(grmF2$focal_individual_id)), type = 'count')	
focalAnimalsF2	<- sort(unique(grmF2$focal_individual_id))
focalTableF2	<- data.frame(table(focalListF2))
obsMatF2		<- createObsMatrix(focalTableF2)
obsNetAdjF2		<- obsNetF2/obsMatF2
listEdgeWeightsF2	<- sort(as.numeric(obsNetAdjF2))[(dim(obsNetAdjF2)[1]+1):dim(obsNetAdjF2)[1]^2]
obsTestStatF2	<- var(listEdgeWeightsF2)/mean(listEdgeWeightsF2)

par(mfrow = c(2, 2))
hist(permCVD2, main = 'Diadema 2', freq = FALSE, xlab = 'CV of Edge Weights for Permuted Networks')
abline(v = obsTestStatD2, col = 'red', lwd = 2)
text(.2, 20, 'p < .001')

hist(permCVD3, main = 'Diadema 3', freq = FALSE, xlab = 'CV of Edge Weights for Permuted Networks')
abline(v = obsTestStatD3, col = 'red', lwd = 2)
text(.25, 14, 'p = .038')

hist(permCVF2, main = 'Fulvus 2', freq = FALSE, xlab = 'CV of Edge Weights for Permuted Networks')
abline(v = obsTestStatF2, col = 'red', lwd = 2)
text(.25, 15, 'p = .004')

hist(permCVF3, main = 'Fulvus 3', freq = FALSE, xlab = 'CV of Edge Weights for Permuted Networks')
abline(v = obsTestStatF3, col = 'red', lwd = 2)
text(.14, 21, 'p = .067')

sum(permCVD2 < obsTestStatD2)/1000
sum(permCVD3 < obsTestStatD3)/1000
sum(permCVF2 < obsTestStatF2)/1000
sum(permCVF3 < obsTestStatF3)/1000