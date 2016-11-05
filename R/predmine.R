#Predictor Mining function package:

#Regression supported:
#GLM

#GLM - distributions supported:
#Poisson
#Binomial/Logistic
#Gaussian

##Return values:
#Something went wrong initially: "-1"
#Something went wrong later on: "-2"
##If nothing went wrong then
#A list will be returned with the following properties:
#list[[1]] 				is the GBM object
#list[[2]] 				is the GLM models fit.
#list[[2]][[X]] 			is the GLM models fit with X+1 parameters.
#list[[2]][[X]][[Y]] 			is the Y'th GLM model with X+1 parameters.
#list[[3]]				contains [number of models fit, number of models with 1 predictor, etc]
#list[[4]]				contains all the AIC values - from which it is possible to find mean or median

##Input that need to be specified by the user:
#gbm.x vector of coloumns	- for predictors to mine from
#gbm.y number of coloumn 	- for response
#family 			- a string that characterizes the distribution, check "Distributions supported". 
#data 				- the desired dataframe
#max.predict 			- the maximum number of predictors in the model.
#max.models 			- the maximum number of models that can be fit.
#influence.lim 			- the standardized value of the least satisfactory influence [0,1]
#				- "0" will simply plot all predictors in the model...
#				- "1" will simply plot only the predictor with the most influence...

##Input that can be specified by the user:
#regression 			- Specify to change regression type, "glm" default, no other options yet.
#learning.rate			-
#bag.fraction			-
#tree.complexity 		-
#max.trees			-
#n.trees			-
#n.folds			-

#This package requires the following libraries:
library(gbm)
library(dismo)

###Predictor mine function.
predmine = function(
	influence.lim,
	max.predict,
	max.models,
	gbm.x,
	gbm.y,
	data,
	family,
	regression = "glm",
	learning.rate = 0.01,
	bag.fraction = 1,
	tree.complexity = 1,
	max.trees = 10000,
	n.trees = 50,
	n.folds = 10){
	
	#If missing: Terminate
	if(missing(influence.lim)){
		warning("Specify the influence lower limit.")
		return(-1)
	}
	
	if(missing(max.predict)){
		warning("Please specify the maximum number of predictors in a model.")
		return(-1)
	}
	
	if(missing(max.models)){
		warning("Please specify the maximum number of models to fit.")
	}
	
	if(missing(gbm.x)){
		warning("Specify atleast one predictor.")
		return(-1)
	}
	if(missing(gbm.y)){
		warning("Specify one response.")
		return(-1)
	}
	if(missing(data)){
		warning("Provide the data you want to work with.")
		return(-1)
	}
	
	#Finding response name from the data frame to be used later.
	if(dim(test.data)[2] == gbm.y){
		response.name = names(data[,(gbm.y-1):gbm.y])[2]
	}
	else{
		repsonse.name = names(data[,gbm.y:(gbm.y+1)])[1]
	}
	
	#Starting GBM.STEP:
	gbm.object = gbm.step(data = data,
						 gbm.x = gbm.x,
						 gbm.y = gbm.y,
						 tree.complexity = tree.complexity,
						 learning.rate = learning.rate,
						 bag.fraction = bag.fraction,
						 n.trees = n.trees,
						 max.trees = max.trees)
	
	print("GBM finished (...)")
	
	##Evaluate the returned GBM object for predictors.
	
	#Taking the influence.
	influence = gbm.object$contributions$rel.inf
	
	#Standardizing the influence (i.e. [0,1] range).
	influence = as.numeric(influence/max(influence))
	print("Relative influence of the given predictors:")
	print(influence)
	
	#Taking the names.
	name.pred = as.character(gbm.object$contributions$var)
	print("Name of all the predictors:")
	print(name.pred)
	
	#Indexing which elements that had satisfactory influence.
	actual.influence.index = (influence > influence.lim)
	
	#Those with higher than 1 in influence
	total.predict = sum(as.numeric(actual.influence.index))
	
	#Name of those with higher than 1 in influence
	actual.influence = name.pred[actual.influence.index]
	print("Predictors with satisfactory influence:")
	print(actual.influence)
	
	#Indexing again.
	idx = 1:total.predict
	
	#Counting model fit(s).
	q = 0
	
	#Making sure the "combn(...)" works.
	if(total.predict < max.predict){
		max.predict = total.predict
	}
	
	#Calculating the number of models that will be fit.
	sum.ret = sumcomb(idx,max.predict,max.models)
	
	#Checking if too many models and if it is then terminate.
	if(sum.ret == -1){
		warning("Models fit exceeds the limit.")
		return(-2)
	}
	else{
		print("The number of models that will be fit is:")
		print(sum.ret)
	}
	
	#Making an object to store number of models. [total, sub_1,sub_2,etc...]
	lengths.vector = c(sum.ret)
	
	#Making a list such that each sub-list in this list will have the same amount of parameters in it.
	regression.list = vector("list",max.predict)
	
	#Vector to save model property AIC, DIC etc...
	property.save = c()
	
	#Performing all (reasonable) model combinations:
	for(i in 1:max.predict){
		this.combination = combn(idx,i)
		
		#Finding the number of different models at each combination
		m = dim(this.combination)[2]
		
		#Storing number of models
		lengths.vector = c(lengths.vector,m)
		
		#i parameter model list
		i.parameter.list = vector("list",m)
		
		for(j in 1:m){
			
			#Locating the current combination of predictors.
			using.predictors = as.character(actual.influence[this.combination[,j]])
			
			if(regression == "glm"){
				#Running GLM
				this.regression = perform.glm(data = data,
											response.name = response.name,
											using.predictors = using.predictors,
											family = family)
				
				property.save = c(aic.save,this.regression$aic)
			}
			
			#Saving the current regression model
			i.parameter.list[[j]] = this.regression
		}
		
		#Saving the list of models with "i" predictors in another list
		regression.list[[i]] = i.parameter.list
	}
	
	#Creating a finalized "object"/list containing everything
	finalizing.list = list(gbm.object,regression.list,lengths.vector,property.save)
	
	return(finalizing.list)
} 

#Not for end user function
#Function to have GLM in its own "space".
perform.glm = function(data,response.name,using.predictors,family){
	almost.formula = paste(paste(response.name,"~"),paste(using.predictors,collapse="+"))
	this.formula = as.formula(almost.formula)
	this.regression = glm(this.formula, family = family, data = data)
	return(this.regression)
}

#Not for end user function
#Sum of combinations function - used to check if too many models are being fit.
sumcomb = function(n,m,limit){
	summering = 0
	for(x in 1:m){
		antall = dim(combn(n,x))[2]
		summering = summering + antall
		if(summering > limit){
			return(-1)
		}
	}
	return(summering)
}
