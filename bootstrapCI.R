setwd('G:/My Drive/Graduate School/Research/Projects/DiademaPilot')
source('createNetworkFunction.R')

##Eventually this should pull from SQL, but clearly that isn't hooked up on laptop
setwd('G:/My Drive/Bucknell/Abstracts_Papers/Thesis/StatisticalAnalysis/Sifakas')

focalData	<- read.csv('SifakaEdited6.csv')
sifaka	<- c('Zo', 'Mv', 'Ir', 'Mr', 'Ta', 'Ne', 'Gs', 'Tk', 'Vn')

prxMat	<- createNet(actor = focalData$actor, recipient = focalData$recip,
			behavior = 'prx', subjects = sifaka, directional = TRUE, 
			type = 'duration', durs = focalData$duration)

#Probably need to add behav cat to data file, cause I don't think they are there
grmMat	<- createNet(actor = focalData$actor, recipient = focalData$recip,
			behavior = 'grm', subjects = sifaka, directional = TRUE, 
			type = 'duration', durs = focalData$duration)

aggMat	<- createNet(actor = focalData$actor, recipient = focalData$recip,
			behavior = 'agg', subjects = sifaka, directional = TRUE, 
			type = 'duration', durs = focalData$duration)

cntMat	<- createNet(actor = focalData$actor, recipient = focalData$recip,
			behavior = 'cnt', subjects = sifaka, directional = TRUE, 
			type = 'duration', durs = focalData$duration)

#Want to create bootstrapped confidence intervals for the edgeweights
	

#Want to compare to a random graph
	##Do some randomization procedure 1000ish times and compare