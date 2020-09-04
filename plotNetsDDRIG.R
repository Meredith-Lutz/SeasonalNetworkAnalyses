##################################################
##### Plot networks month by month for DDRIG #####
##################################################
library(RPostgreSQL)
library(igraph)

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
focalList	<- dbGetQuery(con, 'select main_tables.list_focals.session_start_time, 
				main_tables.list_focals.focal_start_time, main_tables.list_focals.focal_end_time,
				main_tables.list_focals.focal_individual_id, main_tables.list_sessions.group_id,
				main_tables.list_sessions.pin_code_name from main_tables.list_focals
				left join main_tables.list_sessions
				on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')

## Add rotation #'s to data
allData$rotation	<- 0
allData[allData$focal_start_time >= '2019-09-09' & allData$focal_start_time <= '2019-10-04',]$rotation	<- 1
allData[allData$focal_start_time >= '2019-10-07' & allData$focal_start_time <= '2019-11-01',]$rotation	<- 2
allData[allData$focal_start_time >= '2019-11-04' & allData$focal_start_time <= '2019-11-29',]$rotation	<- 3

rot1		<- allData[allData$rotation == 1,]
rot2		<- allData[allData$rotation == 2,]
rot3		<- allData[allData$rotation == 3,]

grm		<- allData[allData$behavior == 'Groom' & allData$start_stop == 'Start',]
ply		<- allData[allData$behavior == 'Play' & allData$start_stop == 'Start',]
cnt		<- allData[allData$behavior == 'Contact' & allData$start_stop == 'Start',]

diadema2	<- sort(c('On', 'Ta', 'Zr', 'Kr', 'Gg', 'Jb', 'Tk', 'Kt', 'Vo', 'Gr', 'Bl'))
diadema3	<- sort(c('Pk', 'Mk', 'An', 'Bk', 'Sm', 'Or'))
fulvus2	<- sort(c('Af', 'Kl', 'Sf', 'Fz', 'Ps', 'Pr', 'Mn', 'Ld'))
fulvus3	<- sort(c('Zk', 'Rk', 'Vt', 'So', 'Kd', 'Pm', 'Gy'))

listGroups	<- c('Diadema 2', 'Diadema 3', 'Fulvus 2', 'Fulvus 3')
listFocalByGroup	<- list(focalList[focalList$group_id == listGroups[1],], focalList[focalList$group_id == listGroups[2],],
				focalList[focalList$group_id == listGroups[3],], focalList[focalList$group_id == listGroups[4],])
allFocalCounts	<- list()
for(i in 1:4){
	listFocalAnimals	<- data.frame(listFocalByGroup[i])$focal_individual_id
	focalAnimals	<- sort(unique(listFocalAnimals))
	focalTable		<- data.frame(table(listFocalAnimals))
	allFocalCounts[[i]]	<- focalTable
}

## Drop Mantiloha
allFocalCounts[[2]]	<- allFocalCounts[[2]][c(1:3, 5:7),]

## Add Sex as a demographic factor (Male/Female/Infant)
allFocalCounts[[1]]$Sex	<- c('M', 'M', 'F', 'M', 'I', 'F', 'F', 'F', 'M', 'F', 'I')
allFocalCounts[[2]]$Sex	<- c('M', 'M', 'M', 'F', 'M', 'F')
allFocalCounts[[3]]$Sex	<- c('I', 'F', 'F', 'M', 'M', 'M', 'M', 'I')
allFocalCounts[[4]]$Sex	<- c('F', 'M', 'F', 'F', 'I', 'F', 'M')
for(i in 1:4){
	allFocalCounts[[i]]$nodeShape	<- gsub('M', 'square', allFocalCounts[[i]]$Sex)
	allFocalCounts[[i]]$nodeShape	<- gsub('F', 'circle', allFocalCounts[[i]]$nodeShape)
	allFocalCounts[[i]]$nodeShape	<- gsub('I', 'rectangle', allFocalCounts[[i]]$nodeShape)
	allFocalCounts[[i]]$nodeColor	<- gsub('M', 'midnightblue', allFocalCounts[[i]]$Sex)
	allFocalCounts[[i]]$nodeColor	<- gsub('F', 'goldenrod', allFocalCounts[[i]]$nodeColor)
	allFocalCounts[[i]]$nodeColor	<- gsub('I', 'forestgreen', allFocalCounts[[i]]$nodeColor)
}

################################
##### Obs mat construction #####
################################
obsMatList	<- lapply(allFocalCounts, createObsMatrix)

############################
##### Network creation #####
############################
d2grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = diadema2, type = 'count')
d3grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = diadema3, type = 'count')
f2grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = fulvus2, type = 'count')
f3grm1	<- createNet(rot1$actor, rot1$subject, rot1$behavior, 'Groom', subjects = fulvus3, type = 'count')

d2grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = diadema2, type = 'count')
d3grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = diadema3, type = 'count')
f2grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = fulvus2, type = 'count')
f3grm2	<- createNet(rot2$actor, rot2$subject, rot2$behavior, 'Groom', subjects = fulvus3, type = 'count')

d2grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = diadema2, type = 'count')
d3grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = diadema3, type = 'count')
f2grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = fulvus2, type = 'count')
f3grm3	<- createNet(rot3$actor, rot3$subject, rot3$behavior, 'Groom', subjects = fulvus3, type = 'count')

mats	<- list(d2grm1, d2grm2, d2grm3, d3grm1, d3grm2, d3grm3, f2grm1, f2grm2, f2grm3, f3grm1, f3grm2, f3grm3)
matsObsAdj	<- list()
for(i in 1:12){
	obsMat		<- obsMatList[[ceiling(i/3)]]
	matsObsAdj[[i]]	<- mats[[i]]/obsMat
}

nets	<- lapply(matsObsAdj, graph_from_adjacency_matrix, mode = "directed", weighted = TRUE, add.rownames = TRUE)
layd2	<- layout.fruchterman.reingold(nets[[1]])
layd3	<- layout.fruchterman.reingold(nets[[4]])
layf2	<- layout.fruchterman.reingold(nets[[7]])
layf3	<- layout.fruchterman.reingold(nets[[10]])
lays	<- list(layd2, layd3, layf2, layf3)

par(mfrow = c(4, 3), mar = c(0, 0, 0, 0))
for(i in 1:12){
	plot.igraph(nets[[i]], layout = lays[[ceiling(i/3)]], vertex.label = NA, 
		edge.color = 'black', vertex.color = allFocalCounts[[ceiling(i/3)]]$nodeColor,
		mark.expand = 20, mark.groups = communities(cluster_optimal(nets[[i]])),
		edge.width = E(nets[[1]])$weight*5, vertex.label.cex = 1.5, vertex.size = 40, edge.arrow.size = 0.25)
}

pdf(
commStrx	<- list()
par(mfrow = c(4, 3), mar = c(0, 0, 0, 0))
for(i in 1:12){
	commStrx[[i]]	<- data.frame(comm = matrix(membership(cluster_optimal(nets[[i]])), dimnames = list(allFocalCounts[[ceiling(i/3)]]$listFocalAnimals)))
	commStrx[[i]]$nodeColor	<- gsub('1', 'forestgreen', commStrx[[i]]$comm)
	commStrx[[i]]$nodeColor	<- gsub('2', 'midnightblue', commStrx[[i]]$nodeColor)
	if(max(commStrx[[i]]$comm == 3)){
		commStrx[[i]]$nodeColor	<- gsub('3', 'goldenrod', commStrx[[i]]$nodeColor)
	}

	plot.igraph(nets[[i]], layout = lays[[ceiling(i/3)]], 
		edge.color = 'black', vertex.shape = 'circle',
		vertex.color = commStrx[[i]]$nodeColor, vertex.label = NA,
		edge.width = E(nets[[1]])$weight*5, vertex.label.cex = 1.5, vertex.size = 40, edge.arrow.size = 0.25)
}