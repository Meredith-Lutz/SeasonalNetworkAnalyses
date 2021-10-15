createNet	<- function(actor, recipient, behavior, behav1, 
	behav2 = FALSE, behav3 = FALSE, subjects, directional = TRUE, 
	type, durs = NA){
	# actor is a variable in a data frame giving the actors of the interactions
	# recipient is a variable in a data frame giving the recipients of interactions
	# behavior is a variable in a data frame giving the behaviors that occurred
	# behav1 is a string of the first behavior that you want to run the analysis on
	# behav2 is a string of the 2nd behavior that you want to analyze (not necessary)
	# behav3 is a string of the 3rd behavior that you want to analyze (not necessary)
	# subjects is a vector of all of the subject names
	# directional is TRUE or FALSE depending on whether the analysis is directional or not
	# type is either "count" or "duration" to represent the type of analysis
	# durs is a vector of all of the duration of behaviors
	# Last updated 5/21/2018 by ML

	if(is.na(durs[1]) == FALSE){
		dataFull	<- data.frame(actor, behavior, recipient, durs)
	}
	if(is.na(durs[1]) == TRUE){
		dataFull	<- data.frame(actor, behavior, recipient)
	}
	n		<- length(subjects)

	data	<- subset(dataFull, actor %in% subjects & recipient %in% subjects)
	
	#print(dim(data))

	##Setting behavior-appropriate datasets
	data1	<- data[which(data$behavior == behav1),]
	mat1	<- matrix(NA, n, n, dimnames = list(subjects, subjects))
	if(behav2 != FALSE){
		data2	<- subset(data, behavior == behav2)
		mat2	<- matrix(NA, n, n, dimnames = list(subjects, subjects))
	}
	if(behav3 != FALSE){
		data3	<- subset(data, behavior == behav3)
		mat3	<- matrix(NA, n, n, dimnames = list(subjects, subjects))
	}

	##Calulate interction matrices based on counts or seconds interacted
	for(i in 1:n){
		for(j in 1:n){
			act		<- as.character(subjects[i])
			rec		<- as.character(subjects[j])
			if(type == 'count'){
				subset1	<- subset(data1, actor == act & recipient == rec & behavior == behav1)
				mat1[i, j]	<- dim(subset1)[1]
				if(behav2 != FALSE){
					subset2	<- subset(data2, actor == act & recipient == rec & behavior == behav2)
					mat2[i, j]	<- dim(subset2)[1]	
				}
				if(behav3 != FALSE){
					subset3	<- subset(data3, actor == act & recipient == rec & behavior == behav3)
					mat3[i, j]	<- dim(subset3)[1]	
				}
			}
			if(type == 'duration'){
				subset1	<- subset(data1, actor == act & recipient == rec & behavior == behav1)
				mat1[i, j]	<- sum(as.numeric(subset1$durs), na.rm = TRUE)
				if(behav2 != FALSE){
					subset2	<- subset(data2, actor == act & recipient == rec & behavior == behav2)
					mat2[i, j]	<- sum(as.numeric(subset2$durs), na.rm = TRUE)
				}
				if(behav3 != FALSE){
					subset3	<- subset(data3, actor == act & recipient == rec & behavior == behav3)
					mat3[i, j]	<- sum(as.numeric(subset3$durs), na.rm = TRUE)	
				}
			}
		}
	}
	

	#Combine across diagonal if directionality is FASLE
	if(directional == FALSE){
		mat1undir	<- matrix(NA, n, n, dimnames = list(subjects, subjects))
		mat2undir	<- matrix(NA, n, n, dimnames = list(subjects, subjects))
		mat3undir	<- matrix(NA, n, n, dimnames = list(subjects, subjects))
		for(i in 1:n){
			for(j in 1:n){
				mat1undir[i, j]	<- mat1[i, j] + mat1[j, i]
				if(behav2 != FALSE){
					mat2undir[i, j]	<- mat2[i, j] + mat2[j, i]
				}
				if(behav3 != FALSE){
					mat3undir[i, j]	<- mat3[i, j] + mat3[j, i]
				}
			}
		}
		mat1	<- mat1undir
		if(behav2 != FALSE){
			mat2	<- mat2undir
		}
		if(behav3 != FALSE){
			mat3	<- mat3undir
		}
	}

	#Average across behaviors if applicable
	if(behav2 == FALSE){
		if(behav3 == FALSE){
			simat	<- mat1
		}
		if(behav3 != FALSE){
			simat	<- (mat1 + mat3)/2
		}
	}
	if(behav2 != FALSE){
		if(behav3 == FALSE){
			simat	<- (mat1 + mat2)/2
		}
		if(behav3 != FALSE){
			simat	<- (mat1 + mat2 + mat3)/3
		}
	}

	return(simat)
}