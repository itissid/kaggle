list[trans.r8, props.r8, rlist.r8, testVTreatFn.r8, tplan.r8] =  prepareDataWrapper.regression(do.vtreat=T, outlier.range=c(-0.8, 0.8), do.factor.conversion=T, vtreat.opts=list(scale.features=T, usecached.plan=F, pruneSig=NULL))
list[fit.lm.vtreat.r8, predictions.vtreat.r8]  = trainingAndPredictionWrapper.regression(trans.r8, props.r8, rlist.r8, testVTreatFn.r8)

