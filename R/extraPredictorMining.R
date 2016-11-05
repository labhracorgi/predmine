#Extra functions to predmine.R
#Simplifying function
#Function to retrieve all AIC values in order to make a histogram.
plot.aic = function(predmine.obj,nclass = 30){
	
	if(missing(predmine.obj)){
		warning("Please provide the predmine object to find AIC values from.")
		print("-1")
	}
	this.plot = hist(predmine.obj[[4]],
					nclass=nclass,
					main = "Histogram: Distribution of AIC",
					xlab = "AIC values")
	return(this.plot)
}

#Simplyfying function
#Number of models in predmine object
num.models = function(predmine.obj){
	n = length(predmine.obj[[3]])
	
	print(paste("Total number of models is ",predmine.obj[[3]][1]))
	
	for(i in 2:n){
		print(paste(paste(paste("Number of models with",i-1),"predictor(s) are:"),predmine.obj[[3]][i]))
	}
	
	return(predmine.obj[[3]])
}

#Simplifying function
#Function to retrieve all models of given specifications, if none specified ALL should be chosen.
look.at.predmine = function(predmine.obj,upper.aic.limit,summaries,parameter.count){
	
	#Initialize.
	sub.obj = c()
	look.at.aic = FALSE
	sub.lists = FALSE
	n = c()
	m = c()
	
	if(missing(predmine.obj)){
		warning("Please provide the predmine object which contains the models")
		return(-1)
	}
	
	if(missing(upper.aic.limit)){
		look.at.aic = FALSE	
	}
	else{
		look.at.aic = TRUE
	}
	
	if(missing(summaries)){
		summaries = FALSE
	}
	
	if(!missing(parameter.count)){
		sub.obj = predmine.obj[[2]][[parameter.count]]
		n = length(sub.obj)
	}
	else{
		sub.obj = predmine.obj[[2]]
		n = length(sub.obj)
		sub.lists = TRUE
	}
	
	if(look.at.aic == TRUE){
		
		if(sub.lists == TRUE){
			
			for(i in 1:n){
				
				m = length(sub.obj[[i]])
				
				for(j in 1:m){
					
					aic.check = (sub.obj[[i]][[j]]$aic > upper.aic.limit)
					if(aic.check){
						sub.obj[[i]][[j]] = "Removed due to AIC criterion specified"
					}
					
				}
				
			}
			
		}
		else{

			for(i in 1:n){
				
				aic.check = (sub.obj[[i]]$aic > upper.aic.limit)
				if(aic.check){
					sub.obj[[i]] = "Removed due to AIC criterion specified"
				}
				
			}
			
		}
		
	}
	
	if(summaries == TRUE){
		
		if(sub.lists == TRUE){
			
			for(i in 1:n){
				
				m = length(sub.obj[[i]])
				
				for(j in 1:m){
					
					sub.obj[[i]][[j]] = summary(sub.obj[[i]][[j]])
					
				}
				
			}
			
		}
		else{
			
			for(i in 1:n){
				
				sub.obj[[i]] = summary(sub.obj[[i]])
			
			}
		
		}
	}
	return(sub.obj)
}