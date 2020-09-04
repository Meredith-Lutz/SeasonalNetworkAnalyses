######################################################
##### Trying to find periods to analyze for KMNP #####
######################################################

setwd('G:/My Drive/Graduate School/Research/Projects/TemporalNets/KMNP Data')

groups	<- read.csv('Groups (5).csv', stringsAsFactors = FALSE)
colnames(groups)	<- c('month', 'year', 'censusCount', 'censusDate', 'indiv', 'grp', 'sex', 'age',
		'ageGuess', 'unmarked', 'source', 'resident', 'notes')

groups$month	<- ifelse(groups$month == 'January', '01', ifelse(groups$month == 'February', '02', ifelse(groups$month == 'March', '03', ifelse(
		groups$month == 'April', '04', ifelse(groups$month == 'May', '05', ifelse(groups$month == 'June', '06', ifelse(
		groups$month == 'July', '07', ifelse(groups$month == 'August', '08', ifelse(groups$month == 'September', '09', ifelse(
		groups$month == 'October', '10', ifelse(groups$month == 'November', '11', '12')))))))))))

groups$censusDate	<- paste(groups$year, groups$month, sep = '-')

gp1	<- groups[groups$grp == 'I',]
gp2	<- groups[groups$grp == 'II',]
gp3	<- groups[groups$grp == 'III',]
gp4	<- groups[groups$grp == 'IV',]
gp5	<- groups[groups$grp == 'V',]
gp6	<- groups[groups$grp == 'VI',]
gp7	<- groups[groups$grp == 'VII',]
gp8	<- groups[groups$grp == 'VIII',]
gp9	<- groups[groups$grp == 'IX',]
gp10	<- groups[groups$grp == 'X',]
gp11	<- groups[groups$grp == 'XI',]
gp12	<- groups[groups$grp == 'XII',]

#to generate table of which animals were in the group each month:
table(gp6$censusDate, gp6$indiv)

