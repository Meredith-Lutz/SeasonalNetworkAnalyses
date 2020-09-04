createObsMatrix	<- function(focalCounts){
	## createObsMatrix takes a matrix n x 2 matrix (where n is the # of animals) called focalCounts
	## Column 1 is the names of the animals
	## Column 2 is the number of focals completed of that animal
	## Function creates a matrix that represents the joint number of observation hours for a dyad
	## Does not account for days when one of the pair was missing
	## Assumes every focal is 1 hr long
	n	<- dim(focalCounts)[1]
	dyadTime	<- matrix(,n,n, dimnames = list(focalCounts[,1], focalCounts[,1]))
	for(i in 1:n){
		for(j in 1:n){
			dyadTime[i, j]	<- (focalCounts[i, 2] + focalCounts[j, 2])
		}
	}
	return(dyadTime)	
}