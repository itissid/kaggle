rfe.5fold = getDefaultRFEControl.xg(folds=2)

params = setNames(as.list(as.numeric(xgbLinear.grid.default)), names(xgbLinear.grid.default))

rfProfile = rfe(x, y, sizes = subsets, rfeControl=rfe.5fold, params=params)


# independent_vars = c(
#     "num_bathroom",
#     "num_bedroom" , 
#     "latitude",
#     "longitude" , 
#     "region_county",
#     "num_room" , 
#     "tax_year",
#     "tax_delinquency_year", 
#     "tax_delinquency",
#     "date" , 
#     "tax_land",
#     "tax_assd_perroom" , 
#     "tax_prop_perroom",
#     "region_zip" , 
#     "tax_building",
#     "build_year" , 
#     "num_bathroom_calc",
#     "num_bath" , 
#     "region_city",
#     "tax_assd_persqfeet_living", 
#     "tax_prop_persqfeet_living"
# )

independentVars = colnames(transr4.r1imputeoff.fe.no.logxfrm.na.omit %>% select(-logerror))



list[XTrain, YTrain, XHoldout, YHoldout] = splitTrainingWrapper(transr4.r1imputeoff.fe.no.logxfrm.na.omit, split_percent=0.90, YName="logerror") 

XYTrain.h2o = as.h2o(x = cbind(XTrain, logerror=YTrain), destination_frame="XTrain.h2o")
XYTest.h2o = as.h2o(x = cbind(XHoldout, logerror=YHoldout), destination_frame="XTest.h2o")

XPredict = as.h2o(propsr4.r1imputeoff.fe.no.logxfrm.na.omit, destination_frame="XPredict")

gbm.model = h2o.gbm(
	x = independentVars,
	y = "logerror",
	training_frame=XYTrain.h2o,
	validation_frame=XYTest.h2o,
	## more trees is better if the learning rate is small enough 
  	## here, use "more than enough" trees - we have early stopping
  	ntrees = 10000,          
	
	max_depth=5,
 	
 	## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
 	learn_rate=0.01,                                                         
  	
  	## sample 80% of rows per tree
  	sample_rate = 0.8,                                                   

  	## sample 80% of columns per split
  	col_sample_rate = 0.8,                                                   

 	## fix a random number generator seed for reproducibility
 	seed = 1234,                                                             
  
 	## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
 	score_tree_interval = 10,
	
	## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "RMSE",
)

#> h2o.performance(gbm.model, valid = TRUE)
H2ORegressionMetrics: gbm
** Reported on validation data. **

#MSE:  0.00580662
#RMSE:  0.07620118
#MAE:  0.04911084
#RMSLE:  0.07704034
#Mean Residual Deviance :  0.00580662

getDateCode = function(d, rlist) {rlist$date %>% dplyr::filter(date==as.Date(d)) %>% pull(date_coded)}

rmseFromModeIds = function(model_ids){
	models <- lapply(model_ids, function(id) { h2o.getModel(id)})
	return(list(validation_rmse=sapply(models, FUN=function(model) model@model$validation_metrics@metrics$RMSE),
	training_rmse=sapply(models, FUN=function(model) model@model$training_metrics@metrics$RMSE)))
}

########### save all models to a given file ##############

saveModelsById = function(model_ids, directory) {
	models <- lapply(model_ids, function(id) { h2o.getModel(id)})
	for(m in models){
		h2o.saveModel(m, directory)
	}
}

varImp.glm = function(glm.fit, tol=1e-4){
	glm.fit@model$coefficients_table %>% data.frame %>% filter(coefficients >tol) %>% arrange(coefficients)
}

###################################
### NOW DOING THE PREDICTIONS ####
###################################
createPredictionsFromModel = function(gbm.model, X, rlist) {
	getDateCode = function(d, rlist) {rlist$date %>% dplyr::filter(date==as.Date(d)) %>% pull(date_coded)}
	dates = vapply(FUN=getDateCode, X=c("2016-10-01", "2016-11-01", "2016-12-01"), rlist, FUN.VALUE=1, USE.NAMES=F)
	XPredict.1 = as.h2o(X  %>% mutate(date=dates[1]), destination_frame="XPredict.1")
	print(".P")
	XPredict.2 = as.h2o(X  %>% mutate(date=dates[2]), destination_frame="XPredict.2")
	print("..P")
	XPredict.3 = as.h2o(X  %>% mutate(date=dates[3]), destination_frame="XPredict.3")
	print("...P")
	p.1 = as.data.frame(h2o.predict(gbm.model,  XPredict.1))
	print("...P")
	p.2 = as.data.frame(h2o.predict(gbm.model,  XPredict.2))
	print("....P")
	p.3 = as.data.frame(h2o.predict(gbm.model,  XPredict.3))
	predictions = cbind(p.1, p.2, p.3, p.1, p.2, p.3)
	colnames(predictions) = c("201610", "201611", "201612", "201710", "201711", "201712")
	predictions$parcelid = X$id_parcel
	print("....P")
	return(predictions)
}

createPredictionsFromModel(gbm.model, propsr4.r1imputeoff.fe.no.logxfrm.na.omit)
d = format(Sys.time(), "%Y%m%d_%H-%M-%S")
writePredictions(predictions, filename.suffix=paste("gbm_", d, sep=""))
## The prediction distribution seems promising. Lets see how it does on the LB ##
## what i can do next is use cross validation to get the best parameters for this model ##
## I want to also ensemble this model with the linear model quickly ##

###################################
# LB score is the best yet: 0.06455
####################################
hyper_params = list(max_depth = seq(1,10,2))
grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid.2",
  
  ## standard model parameters
  x = independentVars, 
  y = "logerror", 
  training_frame = XYTrain.h2o, 
  validation_frame = XYTest.h2o,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees  we have early stopping
  ntrees = 1000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.3,                                                         

  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.95,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       

  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "RMSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)
