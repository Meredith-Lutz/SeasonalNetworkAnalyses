#####################################
##### Seasonal Graphs for DDRIG #####
#####################################

############
### KMNP ###
############
setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data')

kmnp		<- read.csv('Temperature and rain fall.csv')
kmnpScan	<- read.csv('Focal_Behavior_data_combined_for_analysis_4-27-20_MLCorrected.csv')

kmnpScan$monthNum	<- ifelse(kmnpScan$Month == 'Jan', 1, 
				ifelse(kmnpScan$Month == 'Feb', 2,
				ifelse(kmnpScan$Month == 'Mar', 3,
				ifelse(kmnpScan$Month == 'Apr', 4,
				ifelse(kmnpScan$Month == 'May', 5,
				ifelse(kmnpScan$Month == 'Jun', 6,
				ifelse(kmnpScan$Month == 'Jul', 7,
				ifelse(kmnpScan$Month == 'Aug', 8,
				ifelse(kmnpScan$Month == 'Sep', 9,
				ifelse(kmnpScan$Month == 'Oct', 10,
				ifelse(kmnpScan$Month == 'Nov', 11, 12)))))))))))

kmnpFeedScans	<- kmnpScan[kmnpScan$Focal_Activity %in% c('Feed Buds', 'Feed Flowers', 'Feed Fruit', 'Feed Seeds', 'Feed Bark',
				'Feed Branch', 'Feed Branches', 'Feed Caterpillar', 'Feed Dirt', 'Feed Mature Leaves', 'Feed Petiole',
				'Feed Stems', 'Feed Young Leaves'),]
kmnpFeedScans$plantRepro	<- ifelse(kmnpFeedScans$Focal_Activity %in% c('Feed Buds', 'Feed Flowers', 'Feed Fruit', 'Feed Seeds'), 1, 0)
monthDietKMNP			<- aggregate(x = kmnpFeedScans$plantRepro, by = list(month = kmnpFeedScans$monthNum, year = kmnpFeedScans$Year), FUN = mean)
avgMonthDietKMNP			<- aggregate(monthDietKMNP[,3], by = list(month = monthDietKMNP$month), FUN = mean)

monthAvgHighKMNP	<- aggregate(x = kmnp$LF.Maximum, by = list(month = kmnp$Month, year = kmnp$Year), FUN = mean, na.rm = TRUE)
monthTotalRFKMNP	<- aggregate(x = kmnp$Rain.fall, by = list(month = kmnp$Month, year = kmnp$Year), FUN = sum, na.rm = TRUE)

monthlyClimateKMNP	<- cbind(monthAvgHighKMNP, monthTotalRFKMNP[,3])
colnames(monthlyClimateKMNP)	<- c('month', 'year', 'avgHigh', 'totalRain')

avgClimateKMNP	<- aggregate(monthlyClimateKMNP[,3:4], by = list(month = monthlyClimateKMNP$month), FUN = mean, na.rm = TRUE)

png('KMNPSeasonalityNetsBottom.png', res = 300, width = 12, height = 6, units = 'in')
par(mar = c(5, 5, 3, 5), bty = 'u')
plot(avgMonthDietKMNP$month, 100*avgMonthDietKMNP$x, pch = 16, lwd = 2, type = 'l', yaxt = 'n', xaxt = 'n', col = 'deepskyblue4',
	 xlab = 'Month', ylab = '', ylim = c(-50, 112.5))
axis(side = 2, labels = c(0, 25, 50, 75, 100), at = c(0, 25, 50, 75, 100))
text(-0.25, 50, "% plant reproductive parts in diet", srt = 90,  xpd = TRUE, adj = c(NA, 0.5))
par(new = TRUE)
plot(avgClimateKMNP$month, avgClimateKMNP$totalRain, pch = 16, type = 'l', lwd = 2, lty = 5, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', ylim = c(-150, 450))
axis(1, at = 1:12, labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))
axis(side = 4, labels = NA, at = c(0, 100, 200, 300, 400))
text(13.25, 200, "Monthly rainfall (mm)", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 200, "200", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 0, "0", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 100, "100", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 300, "300", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 400, "400", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
rect(1, 425, 3, 450, col = 'deepskyblue4', border = 'deepskyblue4', lwd = 2)
rect(3, 425, 6.5, 450, col = 'lightblue', border = 'deepskyblue4', lwd = 2)
rect(6.5, 425, 8.5, 450, col = 'deepskyblue4', border = 'deepskyblue4', lwd = 2)
rect(8.5, 425, 12, 450, col = 'lightblue', border = 'deepskyblue4', lwd = 2)
text(2, 437.5, "Mating", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(4.75, 437.5, "Gestation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
text(7.5, 437.5, "Birthing", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(10.25, 437.5, "Lactation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
dev.off()

png('KMNPSeasonalityNetsTop.png', res = 300, width = 12, height = 6, units = 'in')
par(mar = c(5, 5, 3, 5), bty = 'u')
plot(avgMonthDietKMNP$month, 100*avgMonthDietKMNP$x, pch = 16, lwd = 2, type = 'l', yaxt = 'n', xaxt = 'n', col = 'deepskyblue4',
	 xlab = 'Month', ylab = '', ylim = c(0, 150))
axis(side = 2, labels = c(0, 25, 50, 75, 100), at = c(0, 25, 50, 75, 100))
text(-0.25, 50, "% plant reproductive parts in diet", srt = 90,  xpd = TRUE, adj = c(NA, 0.5))
par(new = TRUE)
plot(avgClimateKMNP$month, avgClimateKMNP$totalRain, pch = 16, type = 'l', lwd = 2, lty = 5, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', ylim = c(0, 600))
axis(1, at = 1:12, labels = c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))
axis(side = 4, labels = NA, at = c(0, 100, 200, 300, 400))
text(13.25, 200, "Monthly rainfall (mm)", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 200, "200", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 0, "0", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 100, "100", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 300, "300", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(12.85, 400, "400", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
rect(1, 575, 3, 600, col = 'deepskyblue4', border = 'deepskyblue4', lwd = 2)
rect(3, 575, 6.5, 600, col = 'lightblue', border = 'deepskyblue4', lwd = 2)
rect(6.5, 575, 8.5, 600, col = 'deepskyblue4', border = 'deepskyblue4', lwd = 2)
rect(8.5, 575, 12, 600, col = 'lightblue', border = 'deepskyblue4', lwd = 2)
text(2, 587.5, "Mating", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(4.75, 587.5, "Gestation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
text(7.5, 587.5, "Birthing", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(10.25, 587.5, "Lactation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
dev.off()

##################
### Maromizaha ###
##################
setwd('G:/My Drive/Graduate School/Research/FieldSeason2019MF/Climate Data')
MF	<- read.csv('MZ_PBV_Climate_Data.csv')
colnames(MF)	<- c('x', 'date', 'month', 'year', 'time', 'rain', 'temp', 'humidity')

dailyMFTemp	<- aggregate(x = MF[,c('month', 'year', 'temp')], by = list(date = MF$date), FUN = max)
dailyMFRain	<- aggregate(x = MF$rain, by = list(date = MF$date), FUN = sum, na.rm = TRUE)
dailyMF	<- cbind(dailyMFTemp, dailyMFRain[,2])
colnames(dailyMF)	<- c('date', 'month', 'year', 'temp', 'rain')

monthAvgHighMF	<- aggregate(x = dailyMF$temp, by = list(month = dailyMF$month, year = dailyMF$year), FUN = mean, na.rm = TRUE)
monthTotalRFMF	<- aggregate(x = dailyMF$rain, by = list(month = dailyMF$month, year = dailyMF$year), FUN = sum, na.rm = TRUE)

#Exclude partial months
monthlyClimateMF	<- cbind(monthAvgHighMF[2:17,], monthTotalRFMF[2:17,3])
colnames(monthlyClimateMF)	<- c('month', 'year', 'avgHigh', 'totalRain')

avgClimateMF	<- aggregate(monthlyClimateMF[,3:4], by = list(month = monthlyClimateMF$month), FUN = mean, na.rm = TRUE)
avgClimateMF$adjMonth	<- c(6:12, 1:5)
avgClimateShort	<- avgClimateMF[avgClimateMF$adjMonth < 9,]
avgClimateSort	<- avgClimateShort[order(avgClimateShort$adjMonth),]
avgClimateSort$andasibeRain	<- c(89, 44.5, 62.5, 118.3, 250.8, 342.6, 296.8, 261.7)

#Get scan data
library(RPostgreSQL)
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'diadema_fulvus', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'postgres')

allScan	<- dbGetQuery(con, 'select group_id, pin_code_name, focal_start_time, 
	focal_individual_id, scanned_individual_id, activity, part_eaten from main_tables.all_scan_data_view;')

scanFocalAnimalOnly		<- allScan[allScan$pin_code_name != 'Odilon' & allScan$focal_individual_id == allScan$scanned_individual_id,]
scanFocalAnimalOnly$species	<- ifelse(scanFocalAnimalOnly$group_id %in% c('Diadema 2', 'Diadema 3'), 'diadema', 'fulvus')

library(lubridate)
scanFocalAnimalOnly$month	<- month(scanFocalAnimalOnly$focal_start_time)
scanFocalAnimalOnly		<- scanFocalAnimalOnly[is.na(scanFocalAnimalOnly$month) == FALSE,]
feedingScansMF			<- scanFocalAnimalOnly[scanFocalAnimalOnly$activity == 'Feeding',]
feedingScansMF$plantRepro	<- ifelse(feedingScansMF$part_eaten %in% c('Buds', 'Flower', 'Ripe fruit', 'Unfruit fruit', 'Seeds'), 1, 0)

monthDietMF			<- aggregate(x = feedingScansMF$plantRepro, by = list(month = feedingScansMF$month, species = feedingScansMF$species), FUN = mean)
monthDietMF$adjMonth	<- rep(c(6:8, 1:5), 2)
diadema	<- monthDietMF[monthDietMF$species == 'diadema',]
diadema	<- diadema[order(diadema$adjMonth),]
fulvus	<- monthDietMF[monthDietMF$species == 'fulvus',]
fulvus	<- fulvus[order(fulvus$adjMonth),]

png('MFDiademaSeasonalityNetsBottom.png', res = 300, width = 12, height = 6, units = 'in')
par(mar = c(5, 5, 3, 5), bty = 'u')
plot(diadema$adjMonth, 100*diadema$x, pch = 16, lwd = 2, type = 'l', yaxt = 'n', xaxt = 'n', col = 'midnightblue',
	 xlab = 'Month', ylab = '', ylim = c(-50, 112.5))
axis(side = 2, labels = c(0, 25, 50, 75, 100), at = c(0, 25, 50, 75, 100))
text(0.25, 50, "% plant reproductive parts in diet", srt = 90,  xpd = TRUE, adj = c(NA, 0.5))
par(new = TRUE)
plot(avgClimateSort$adjMonth, avgClimateSort$andasibeRain, pch = 16, type = 'l', lwd = 2, lty = 5, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', ylim = c(-150, 450))
axis(1, at = 1:8, labels = c('A', 'S', 'O', 'N', 'D', 'J', 'F', 'M'))
axis(side = 4, labels = NA, at = c(0, 100, 200, 300, 400))
text(8.75, 200, "Monthly rainfall (mm)", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 200, "200", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 0, "0", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 100, "100", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 300, "300", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 400, "400", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
rect(1, 425, 1.75, 450, col = 'midnightblue', border = 'midnightblue', lwd = 2)
rect(1.75, 425, 4.5, 450, col = 'steelblue1', border = 'midnightblue', lwd = 2)
rect(4.5, 425, 6.5, 450, col = 'midnightblue', border = 'midnightblue', lwd = 2)
rect(6.5, 425, 8, 450, col = 'steelblue1', border = 'midnightblue', lwd = 2)
text(1.375, 437.5, "Birthing", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(3.125, 437.5, "Lactation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
text(5.5, 437.5, "Mating", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(7.25, 437.5, "Gestation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
dev.off()

png('MFDiademaSeasonalityNetsTop.png', res = 300, width = 12, height = 6, units = 'in')
par(mar = c(5, 5, 3, 5), bty = 'u')
plot(diadema$adjMonth, 100*diadema$x, pch = 16, lwd = 2, type = 'l', yaxt = 'n', xaxt = 'n', col = 'midnightblue',
	 xlab = 'Month', ylab = '', ylim = c(0, 150))
axis(side = 2, labels = c(0, 25, 50, 75, 100), at = c(0, 25, 50, 75, 100))
text(0.25, 50, "% plant reproductive parts in diet", srt = 90,  xpd = TRUE, adj = c(NA, 0.5))
par(new = TRUE)
plot(avgClimateSort$adjMonth, avgClimateSort$andasibeRain, pch = 16, type = 'l', lwd = 2, lty = 5, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', ylim = c(0, 600))
axis(1, at = 1:8, labels = c('A', 'S', 'O', 'N', 'D', 'J', 'F', 'M'))
axis(side = 4, labels = NA, at = c(0, 100, 200, 300, 400))
text(8.75, 200, "Monthly rainfall (mm)", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 200, "200", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 0, "0", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 100, "100", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 300, "300", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 400, "400", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
rect(1, 575, 1.75, 600, col = 'midnightblue', border = 'midnightblue', lwd = 2)
rect(1.75, 575, 4.5, 600, col = 'steelblue1', border = 'midnightblue', lwd = 2)
rect(4.5, 575, 6.5, 600, col = 'midnightblue', border = 'midnightblue', lwd = 2)
rect(6.5, 575, 8, 600, col = 'steelblue1', border = 'midnightblue', lwd = 2)
text(1.375, 587.5, "Birthing", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(3.125, 587.5, "Lactation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
text(5.5, 587.5, "Mating", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(7.25, 587.5, "Gestation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
dev.off()

png('MFFulvusSeasonalityNetsBottom.png', res = 300, width = 12, height = 6, units = 'in')
par(mar = c(5, 5, 3, 5), bty = 'u')
plot(fulvus$adjMonth, 100*fulvus$x, pch = 16, lwd = 2, type = 'l', yaxt = 'n', xaxt = 'n', col = 'orange4',
	ylab = '', xlab = 'Month', ylim = c(0-50, 112.5))
axis(side = 2, labels = c(0, 25, 50, 75, 100), at = c(0, 25, 50, 75, 100))
text(0.25, 50, "% plant reproductive parts in diet", srt = 90,  xpd = TRUE, adj = c(NA, 0.5))
par(new = TRUE)
plot(avgClimateSort$adjMonth, avgClimateSort$andasibeRain, pch = 16, type = 'l', lwd = 2, lty = 5, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', ylim = c(-150, 450))
axis(1, at = 1:8, labels = c('A', 'S', 'O', 'N', 'D', 'J', 'F', 'M'))
axis(side = 4, labels = NA, at = c(0, 100, 200, 300, 400))
text(8.75, 200, "Monthly rainfall (mm)", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 200, "200", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 0, "0", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 100, "100", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 300, "300", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 400, "400", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
rect(1, 425, 3, 450, col = 'orange4', border = 'orange4', lwd = 2)
rect(3, 425, 6, 450, col = 'gold2', border = 'orange4', lwd = 2)
text(2, 437.5, "Birthing", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(4.5, 437.5, "Lactation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
dev.off()

png('MFFulvusSeasonalityNetsTop.png', res = 300, width = 12, height = 6, units = 'in')
par(mar = c(5, 5, 3, 5), bty = 'u')
plot(fulvus$adjMonth, 100*fulvus$x, pch = 16, lwd = 2, type = 'l', yaxt = 'n', xaxt = 'n', col = 'orange4',
	ylab = '', xlab = 'Month', ylim = c(0, 150))
axis(side = 2, labels = c(0, 25, 50, 75, 100), at = c(0, 25, 50, 75, 100))
text(0.25, 50, "% plant reproductive parts in diet", srt = 90,  xpd = TRUE, adj = c(NA, 0.5))
par(new = TRUE)
plot(avgClimateSort$adjMonth, avgClimateSort$andasibeRain, pch = 16, type = 'l', lwd = 2, lty = 5, xaxt = 'n', yaxt = 'n', ylab = '', xlab = '', ylim = c(0, 600))
axis(1, at = 1:8, labels = c('A', 'S', 'O', 'N', 'D', 'J', 'F', 'M'))
axis(side = 4, labels = NA, at = c(0, 100, 200, 300, 400))
text(8.75, 200, "Monthly rainfall (mm)", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 200, "200", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 0, "0", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 100, "100", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 300, "300", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
text(8.55, 400, "400", srt = -90, xpd = TRUE, adj = c(NA, 0.5))
rect(1, 575, 3, 600, col = 'orange4', border = 'orange4', lwd = 2)
rect(3, 575, 6, 600, col = 'gold2', border = 'orange4', lwd = 2)
text(2, 587.5, "Birthing", xpd = TRUE, adj = c(0.5, 0.5), col = 'white', font = 2)
text(4.5, 587.5, "Lactation", xpd = TRUE, adj = c(0.5, 0.5), col = 'black', font = 2)
dev.off()

