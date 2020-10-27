#####################################
##### Temporal Feeding Analyses #####
#####################################

setwd('G:/My Drive/Graduate School/Research/Projects/TemporalNets')

library(chron)
library(stringr)

source('G:/My Drive/Graduate School/Research/AO/CleanAOData/CleanAOData/CleanFeedingData.R')

feedingCleaned	<- feedingCleaned[[1]][,c(1:2, 4:8, 18:19, 23:27, 36:38, 40:42)]

feedingCleaned$cycle_number	<- 0
feedingCleaned[feedingCleaned$focal_start_time >= '2019-09-09' & feedingCleaned$focal_start_time <= '2019-10-04',]$cycle_number	<- 1
feedingCleaned[feedingCleaned$focal_start_time >= '2019-10-07' & feedingCleaned$focal_start_time <= '2019-11-01',]$cycle_number	<- 2
feedingCleaned[feedingCleaned$focal_start_time >= '2019-11-04' & feedingCleaned$focal_start_time <= '2019-11-29',]$cycle_number	<- 3
feedingCleaned[feedingCleaned$focal_start_time >= '2019-12-02' & feedingCleaned$focal_start_time <= '2020-01-17',]$cycle_number	<- 4
feedingCleaned[feedingCleaned$focal_start_time >= '2020-01-20' & feedingCleaned$focal_start_time <= '2020-02-14',]$cycle_number	<- 5
feedingCleaned[feedingCleaned$focal_start_time >= '2020-02-17' & feedingCleaned$focal_start_time <= '2020-03-13',]$cycle_number	<- 6

###########################################################
##### Calculate Diversity Indices per cycle per group #####
###########################################################

shannonWeinerD	<- function(data, period, group){
	subset		<- data[data$cycle_number == period & data$group_id == group, ]
	plants		<- unique(paste(subset$tree_number, subset$part_eaten))
	plantComponents	<- data.frame(str_split_fixed(as.character(plants), ' ', n = 3))
	treeNumber		<- paste(plantComponents[,1], plantComponents[,2])
	plantsFull		<- data.frame(plants = plants, treeNum = treeNumber, partEaten = as.character(plantComponents$X3))
	print(plantsFull)
	durations		<- NA
	for(i in 1:dim(plantsFull)[1]){
		if(substr(plantsFull[i,]$plants, 1, 1) == 'N'){ #No tree number
			plantsFull[i,]$treeNum	<- NA
			plantsFull[i,]$partEaten	<- paste(plantComponents[i,2], plantComponents[i,3])
		}
		plantData		<- subset[as.character(subset$tree_number) == plantsFull[i, 2] & as.character(subset$part_eaten) == plantsFull[i, 3],]
		totalDuration	<- sum(plantData$duration, na.rm = TRUE)
		durations		<- c(durations, as.numeric(totalDuration))
	}
	durationsNoNA	<- durations[2:length(durations)]
	plantsFull$duration	<- durationsNoNA
	plantsFull		<- plantsFull[plantsFull$duration != 0,]
	totalFeeding	<- sum(plantsFull$duration)
	plantsFull$pilnpi	<- (plantsFull$duration/totalFeeding) * log(plantsFull$duration/totalFeeding)
	return(-1 * sum(plantsFull$pilnpi))
}

periods	<- rep(0:6, 4)
groups	<- c(rep('Diadema 2', 7), rep('Diadema 3', 7), rep('Fulvus 2', 7), rep('Fulvus 3', 7))
diversity	<- data.frame(groups, periods, d = rep(NA, 28))
for(i in 1:dim(diversity)[1]){
	print(diversity[i,]$periods)
	print(diversity[i,]$groups)
	diversity[i, ]$d	<- shannonWeinerD(feedingCleaned, diversity[i,]$periods, diversity[i,]$groups)
}

plot(diversity[2:7,]$periods, diversity[2:7,]$d, col = 'midnightblue', pch = 16, cex = 1.5, type = 'o', ylim = c(2, 5), lwd = 1, xlab = 'Sampling Period', ylab = 'Shannon Weiner Diversity Index')
rect(4, 2, 6, 5, col = rgb(.13, .545, .13, alpha = 0.25))
lines(diversity[2:7,]$periods, diversity[2:7,]$d, col = 'midnightblue', pch = 16, cex = 1.5, type = 'o', ylim = c(2, 5), lwd = 1)
lines(diversity[9:14,]$periods, diversity[9:14,]$d, col = 'deepskyblue2', pch = 16, cex = 1.5, type = 'o', ylim = c(1, 5), lwd = 1)
lines(diversity[16:21,]$periods, diversity[16:21,]$d, col = 'goldenrod3', pch = 16, cex = 1.5, type = 'o', ylim = c(1, 5), lwd = 1)
lines(diversity[23:28,]$periods, diversity[23:28,]$d, col = 'khaki', pch = 16, cex = 1.5, type = 'o', ylim = c(1, 5), lwd = 1)
legend(1, 2.75, legend = c('Diadema 2', 'Diadema 3', 'Fulvus 2', 'Fulvus 3'), col = c('midnightblue', 'deepskyblue2', 'goldenrod3', 'khaki'), pch = 16)

##########################
##### Diet Over Time #####
##########################
partSummary	<- aggregate(feedingCleaned$duration, by = list(group = feedingCleaned$group_id, cycle = feedingCleaned$cycle_number, plant_part = feedingCleaned$part_eaten), FUN=sum)
