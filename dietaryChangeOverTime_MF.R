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
#Add cycle numbers
feedingCleaned$cycle_number	<- 0
feedingCleaned[feedingCleaned$focal_start_time >= '2019-09-09' & feedingCleaned$focal_start_time <= '2019-10-04',]$cycle_number	<- 1
feedingCleaned[feedingCleaned$focal_start_time >= '2019-10-07' & feedingCleaned$focal_start_time <= '2019-11-01',]$cycle_number	<- 2
feedingCleaned[feedingCleaned$focal_start_time >= '2019-11-04' & feedingCleaned$focal_start_time <= '2019-11-29',]$cycle_number	<- 3
feedingCleaned[feedingCleaned$focal_start_time >= '2019-12-02' & feedingCleaned$focal_start_time <= '2020-01-17',]$cycle_number	<- 4
feedingCleaned[feedingCleaned$focal_start_time >= '2020-01-20' & feedingCleaned$focal_start_time <= '2020-02-14',]$cycle_number	<- 5
feedingCleaned[feedingCleaned$focal_start_time >= '2020-02-17' & feedingCleaned$focal_start_time <= '2020-03-13',]$cycle_number	<- 6

#Summarize data
partSummary	<- aggregate(feedingCleaned$duration, by = list(group = feedingCleaned$group_id, cycle = feedingCleaned$cycle_number, plant_part = feedingCleaned$part_eaten), FUN=sum)
partSummary$feedingHours	<- partSummary$x/3600
colnames(partSummary)	<- c('group', 'cycle', 'plant_part', 'feedingSec', 'feedingHours')
feedingTimeSummary	<- aggregate(partSummary$feedingHours, by = list(group = partSummary$group, cycle = partSummary$cycle), FUN=sum)

feedingRateSummary			<- merge(partSummary, feedingTimeSummary)
colnames(feedingRateSummary)		<- c('group', 'cycle', 'plant_part', 'feedingSec', 'feedingHoursPlantPart', 'totalFeedingHours')
feedingRateSummary$feedingPercent	<- feedingRateSummary$feedingHoursPlantPart/feedingRateSummary$totalFeedingHours

#Add categories and summarize
feedingRateSummary[feedingRateSummary$plant_part == 'Unfruit fruit', 'plant_part']	<- 'Unripe fruit'
feedingRateSummary$partCat	<- NA
feedingRateSummary[feedingRateSummary$plant_part == 'Stems', 'partCat']		<- 'otherPlant'
feedingRateSummary[feedingRateSummary$plant_part == 'Soil', 'partCat']		<- 'otherNonPlant'
feedingRateSummary[feedingRateSummary$plant_part == 'Unripe fruit', 'partCat']<- 'plantRepro'
feedingRateSummary[feedingRateSummary$plant_part == 'Flower', 'partCat']	<- 'plantRepro'
feedingRateSummary[feedingRateSummary$plant_part == 'Mature leaves', 'partCat']<- 'leaves'
feedingRateSummary[feedingRateSummary$plant_part == 'Young leaves', 'partCat']<- 'leaves'
feedingRateSummary[feedingRateSummary$plant_part == 'Ripe fruit', 'partCat']	<- 'plantRepro'
feedingRateSummary[feedingRateSummary$plant_part == 'Bark', 'partCat']		<- 'otherPlant'
feedingRateSummary[feedingRateSummary$plant_part == 'Seeds', 'partCat']		<- 'plantRepro'
feedingRateSummary[feedingRateSummary$plant_part == 'Buds', 'partCat']		<- 'plantRepro'
feedingRateSummary[feedingRateSummary$plant_part == 'Water', 'partCat']		<- 'otherNonPlant'
feedingRateSummary[feedingRateSummary$plant_part == 'Insect', 'partCat']	<- 'otherNonPlant'
feedingRateSummary[feedingRateSummary$plant_part == 'Fungi', 'partCat']		<- 'otherNonPlant'

dietSummary	<- aggregate(feedingRateSummary$feedingPercent, by = list(group = feedingRateSummary$group, cycle = feedingRateSummary$cycle, part = feedingRateSummary$partCat), FUN=sum)

##Large dietary categories
par(mfrow = c(2, 2))
#Diadema 2
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Diadema 2')
lines(dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'plantRepro',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'plantRepro',]$x, 
	col = 'blue', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'leaves',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'leaves',]$x, 
	col = 'green', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'otherNonPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'otherNonPlant',]$x, 
	col = 'yellow', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'otherPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 2' & dietSummary$part == 'otherPlant',]$x, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 100, legend = c('Plant Reproductive Parts', 'Leaves', 'Other Plant Parts', 'Non-plant'),
	col = c('blue', 'green', 'black', 'yellow'), pch = 16)

#Diadema 3
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Diadema 3')
lines(dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'plantRepro',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'plantRepro',]$x, 
	col = 'blue', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'leaves',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'leaves',]$x, 
	col = 'green', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'otherNonPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'otherNonPlant',]$x, 
	col = 'yellow', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'otherPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Diadema 3' & dietSummary$part == 'otherPlant',]$x, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 100, legend = c('Plant Reproductive Parts', 'Leaves', 'Other Plant Parts', 'Non-plant'),
	col = c('blue', 'green', 'black', 'yellow'), pch = 16)

#Fulvus 2
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Fulvus 2')
lines(dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'plantRepro',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'plantRepro',]$x, 
	col = 'blue', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'leaves',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'leaves',]$x, 
	col = 'green', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'otherNonPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'otherNonPlant',]$x, 
	col = 'yellow', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'otherPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 2' & dietSummary$part == 'otherPlant',]$x, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 60, legend = c('Plant Reproductive Parts', 'Leaves', 'Other Plant Parts', 'Non-plant'),
	col = c('blue', 'green', 'black', 'yellow'), pch = 16)

#Fulvus 3
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Fulvus 3')
lines(dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'plantRepro',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'plantRepro',]$x, 
	col = 'blue', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'leaves',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'leaves',]$x, 
	col = 'green', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'otherNonPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'otherNonPlant',]$x, 
	col = 'yellow', pch = 16, type = 'o')
lines(dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'otherPlant',]$cycle, 
	100*dietSummary[dietSummary$group == 'Fulvus 3' & dietSummary$part == 'otherPlant',]$x, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 60, legend = c('Plant Reproductive Parts', 'Leaves', 'Other Plant Parts', 'Non-plant'),
	col = c('blue', 'green', 'black', 'yellow'), pch = 16)

##Smaller diet categories
par(mfrow = c(2, 2))
#Diadema 2
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Diadema 2')
#lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Young leaves',]$cycle, 
#	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Young leaves',]$feedingPercent, 
#	col = 'blue', pch = 16, type = 'o')
#lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Mature leaves',]$cycle, 
#	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Mature leaves',]$feedingPercent, 
#	col = 'yellow', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Unripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Unripe fruit',]$feedingPercent, 
	col = 'green', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Ripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 2' & feedingRateSummary$plant_part == 'Ripe fruit',]$feedingPercent, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 100, legend = c('Young leaves', 'Mature leaves', 'Unripe fruit', 'Ripe fruit'),
	col = c('blue', 'yellow', 'green', 'black'), pch = 16)

#Diadema 3
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Diadema 3')
#lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Young leaves',]$cycle, 
#	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Young leaves',]$feedingPercent, 
#	col = 'blue', pch = 16, type = 'o')
#lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Mature leaves',]$cycle, 
#	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Mature leaves',]$feedingPercent, 
#	col = 'yellow', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Unripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Unripe fruit',]$feedingPercent, 
	col = 'green', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Ripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Diadema 3' & feedingRateSummary$plant_part == 'Ripe fruit',]$feedingPercent, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 100, legend = c('Young leaves', 'Mature leaves', 'Unripe fruit', 'Ripe fruit'),
	col = c('blue', 'yellow', 'green', 'black'), pch = 16)

#Fulvus 2
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Fulvus 2')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Young leaves',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Young leaves',]$feedingPercent, 
	col = 'blue', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Mature leaves',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Mature leaves',]$feedingPercent, 
	col = 'yellow', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Unripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Unripe fruit',]$feedingPercent, 
	col = 'green', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Ripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 2' & feedingRateSummary$plant_part == 'Ripe fruit',]$feedingPercent, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 100, legend = c('Young leaves', 'Mature leaves', 'Unripe fruit', 'Ripe fruit'),
	col = c('blue', 'yellow', 'green', 'black'), pch = 16)

#Fulvus 3
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
	xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Fulvus 3')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Young leaves',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Young leaves',]$feedingPercent, 
	col = 'blue', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Mature leaves',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Mature leaves',]$feedingPercent, 
	col = 'yellow', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Unripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Unripe fruit',]$feedingPercent, 
	col = 'green', pch = 16, type = 'o')
lines(feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Ripe fruit',]$cycle, 
	100*feedingRateSummary[feedingRateSummary$group == 'Fulvus 3' & feedingRateSummary$plant_part == 'Ripe fruit',]$feedingPercent, 
	col = 'black', pch = 16, type = 'o')
legend(0.5, 100, legend = c('Young leaves', 'Mature leaves', 'Unripe fruit', 'Ripe fruit'),
	col = c('blue', 'yellow', 'green', 'black'), pch = 16)


## start code that Alice added: plot large + small diet categories by species

############################################################################################
## Large plant part categories
############################################################################################

feedingRateSummary$species <- ifelse(feedingRateSummary$group %in% c("Diadema 2","Diadema 3"),
                                     "Diadema","Fulvus")

dietSum1    <- aggregate(feedingRateSummary$feedingHoursPlantPart, by = list(species = feedingRateSummary$species, cycle = feedingRateSummary$cycle, part = feedingRateSummary$partCat), FUN=sum)
names(dietSum1) <- c("species","cycle","part","feedingHoursPlantPart")
dietSum1$spcyc <- paste(dietSum1$species,dietSum1$cycle)

hrs    <- aggregate(dietSum1$feedingHoursPlantPart, by = list(species = dietSum1$species, cycle = dietSum1$cycle), FUN=sum)
hrs$spcyc <- paste(hrs$species,hrs$cycle)
dietSum.sp <- merge(dietSum1, hrs[,c(3,4)], by="spcyc")[,-1]
names(dietSum.sp) <- c("species","cycle","part","feedingHoursPlantPart","totalFeedingHours")

dietSum.sp$x <- dietSum.sp$feedingHoursPlantPart/dietSum.sp$totalFeedingHours

############################################################################################

##Large dietary categories
par(mfrow = c(1, 2))
#Diadema
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
     xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Diadema')
lines(dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'plantRepro',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'plantRepro',]$x,
      col = 'blue', pch = 16, type = 'o')
lines(dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'leaves',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'leaves',]$x,
      col = 'green', pch = 16, type = 'o')
lines(dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'otherNonPlant',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'otherNonPlant',]$x,
      col = 'yellow', pch = 16, type = 'o')
lines(dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'otherPlant',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Diadema' & dietSum.sp$part == 'otherPlant',]$x,
      col = 'black', pch = 16, type = 'o')
#legend(0.5, 100, legend = c('Plant Reproductive Parts', 'Leaves', 'Other Plant Parts', 'Non-plant'),
#       col = c('blue', 'green', 'black', 'yellow'), pch = 16)

#Fulvus
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
     xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Fulvus')
lines(dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'plantRepro',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'plantRepro',]$x,
      col = 'blue', pch = 16, type = 'o')
lines(dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'leaves',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'leaves',]$x,
      col = 'green', pch = 16, type = 'o')
lines(dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'otherNonPlant',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'otherNonPlant',]$x,
      col = 'yellow', pch = 16, type = 'o')
lines(dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'otherPlant',]$cycle,
      100*dietSum.sp[dietSum.sp$species == 'Fulvus' & dietSum.sp$part == 'otherPlant',]$x,
      col = 'black', pch = 16, type = 'o')
legend(0.5, 60, legend = c('Plant Reproductive Parts', 'Leaves', 'Other Plant Parts', 'Non-plant'),
       col = c('blue', 'green', 'black', 'yellow'), pch = 16, bty = "n")


############################################################################################
## Small plant part categories
############################################################################################

feedingSum1    <- aggregate(feedingRateSummary$feedingHoursPlantPart, by = list(species = feedingRateSummary$species, cycle = feedingRateSummary$cycle, part = feedingRateSummary$plant_part), FUN=sum)
names(feedingSum1) <- c("species","cycle","part","feedingHoursPlantPart")
feedingSum1$spcyc <- paste(feedingSum1$species,feedingSum1$cycle)

feedingSum.sp <- merge(feedingSum1, hrs[,c(3,4)], by="spcyc")[,-1]
names(feedingSum.sp) <- c("species","cycle","plant_part","feedingHoursPlantPart","totalFeedingHours")

feedingSum.sp$feedingPercent <- feedingSum.sp$feedingHoursPlantPart/feedingSum.sp$totalFeedingHours


############################################################################################

##Small dietary categories
par(mfrow = c(1, 2))
#Diadema
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
     xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Diadema')
#lines(feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Young leaves',]$cycle,
#      100*feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Young leaves',]$feedingPercent,
#      col = 'blue', pch = 16, type = 'o')
#lines(feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Mature leaves',]$cycle,
#      100*feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Mature leaves',]$feedingPercent,
#      col = 'yellow', pch = 16, type = 'o')
lines(feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Unripe fruit',]$cycle,
      100*feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Unripe fruit',]$feedingPercent,
      col = 'green', pch = 16, type = 'o')
lines(feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Ripe fruit',]$cycle,
      100*feedingSum.sp[feedingSum.sp$species == 'Diadema' & feedingSum.sp$plant_part == 'Ripe fruit',]$feedingPercent,
      col = 'black', pch = 16, type = 'o')
legend(0.5, 100, legend = c('Young leaves', 'Mature leaves', 'Unripe fruit', 'Ripe fruit'),
       col = c('blue', 'yellow', 'green', 'black'), pch = 16, bty = "n")

#Fulvus
plot(x = 0, y = 1, xlim = c(0,6), ylim = c(0, 100), pch = 16, type = 'n',
     xlab = 'Cycle', ylab = 'Diet Percentage', main = 'Fulvus')
#lines(feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Young leaves',]$cycle,
#      100*feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Young leaves',]$feedingPercent,
#      col = 'blue', pch = 16, type = 'o')
#lines(feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Mature leaves',]$cycle,
#      100*feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Mature leaves',]$feedingPercent,
#      col = 'yellow', pch = 16, type = 'o')
lines(feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Unripe fruit',]$cycle,
      100*feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Unripe fruit',]$feedingPercent,
      col = 'green', pch = 16, type = 'o')
lines(feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Ripe fruit',]$cycle,
      100*feedingSum.sp[feedingSum.sp$species == 'Fulvus' & feedingSum.sp$plant_part == 'Ripe fruit',]$feedingPercent,
      col = 'black', pch = 16, type = 'o')
#legend(0.5, 100, legend = c('Young leaves', 'Mature leaves', 'Unripe fruit', 'Ripe fruit'),
#       col = c('blue', 'yellow', 'green', 'black'), pch = 16, bty = "n")

## end code that Alice added


######################
##### Focal Time #####
######################

drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'diadema_fulvus', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'postgres')
focalList	<- dbGetQuery(con, 'select main_tables.list_focals.session_start_time, 
				main_tables.list_focals.focal_start_time, main_tables.list_focals.focal_end_time,
				main_tables.list_focals.focal_individual_id, main_tables.list_sessions.group_id,
				main_tables.list_sessions.pin_code_name from main_tables.list_focals
				left join main_tables.list_sessions
				on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')

focalList	<- focalList[focalList$focal_individual_id != 'Ml',]

focalList$cycle_number	<- 0
focalList[focalList$focal_start_time >= '2019-09-09' & focalList$focal_start_time <= '2019-10-04',]$cycle_number	<- 1
focalList[focalList$focal_start_time >= '2019-10-07' & focalList$focal_start_time <= '2019-11-01',]$cycle_number	<- 2
focalList[focalList$focal_start_time >= '2019-11-04' & focalList$focal_start_time <= '2019-11-29',]$cycle_number	<- 3
focalList[focalList$focal_start_time >= '2019-12-02' & focalList$focal_start_time <= '2020-01-17',]$cycle_number	<- 4
focalList[focalList$focal_start_time >= '2020-01-20' & focalList$focal_start_time <= '2020-02-14',]$cycle_number	<- 5
focalList[focalList$focal_start_time >= '2020-02-17' & focalList$focal_start_time <= '2020-03-13',]$cycle_number	<- 6

focalListNoVo				<- focalList[!(focalList$focal_individual_id == 'Vo' & focalList$cycle_number >= 2),]

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

durationSummary	<- aggregate(focalListNoOdilon$focalDuration, by = list(group = focalListNoOdilon$group_id, cycle = focalListNoOdilon$cycle_number), FUN=sum)

