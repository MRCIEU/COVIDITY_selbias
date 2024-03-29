### Louise AC Millard 2021
###



printStats <- function(simres, trueeffect, strata, outfile) {

	simres = simres[which(simres$strata==strata),]


	##
	## mean bias

	# calculate bias for each sim iteration
	simres$bias = simres$estimate - trueeffect

	meanbias = mean(simres$bias)


	##
	## calculate bias monte carlo standard error
	nsim = nrow(simres)

	biasMCSE = sqrt((1/(nsim*(nsim-1))) * sum((simres$bias-meanbias)^2))


	#print(paste0(strata, ": ", meanbias, ", ", mcSE))
	#cat(paste0(meanbias, " (", mcSE, ")"), outfile, append=TRUE)


	##
	## coverage

	simres$covered = simres$lower <= trueeffect & simres$upper >= trueeffect
	cov = length(which(simres$covered==1))/nsim


	##
	## coverage MCSE

	covMCSE = sqrt(cov*(1-cov)/nsim)


	##
	## bias eliminated coverage

	biasedeffect = trueeffect + meanbias
	simres$elimcovered = simres$lower <= biasedeffect & simres$upper >= biasedeffect
        elimcov = length(which(simres$elimcovered==1))/nsim
	

	##
	## bias eliminated coverage MCSE

	elimcovMCSE = sqrt(elimcov*(1-elimcov)/nsim)

	return(list(biasMean=meanbias, biasMCSE=biasMCSE, coverage=cov, coverageMCSE=covMCSE, biasElimCoverage=elimcov, biasElimCoverageMCSE=elimcovMCSE))

}

formatStat <- function(stat, dp=4) {

	return(sprintf(paste0("%.",dp,"f"), stat))

}



##
## get settings from args

args = commandArgs(trailingOnly=TRUE)

# either effect or null for simulations where bmi has an effect or no effect on covid risk (the hypothesis we are testing)
bmiEffect = args[1]
print(bmiEffect)

large = args[2]
print(large)

estimatesForSim <- function(bmiEffect, selInteractEffect, large) {

	# load in the simulation results


	for (i in 1:20) {
		if (i == 1) {
			simres = read.table(paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-", i,".csv"), header=1, sep=",")
		}
		else {
			simresthis = read.table(paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-", i,".csv"), header=1, sep=",")
			simres = rbind(simres,simresthis)
		}
	}
	write.table(simres, paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "all.csv"), row.names=FALSE, sep=',')

	# columns: iter,strata,estimate,lower,upper


	# remove any iterations where at least 1 regression did not converge

	if ('conv' %in% colnames(simres)) {
		iterRemove = unique(simres$iter[which(simres$conv==0)])

		print(paste0('Number of iterations: ', length(unique(simres$iter)), '. Number of iterations containing a regression that did not converge: ', length(iterRemove)))
		if (length(iterRemove)>0) {
			ix = which(simres$iter %in% iterRemove)
			simres = simres[-ix,]
			print(paste0('Those iterations have been removed. Number of iterations included: ', length(unique(simres$iter))))
		}
	}	
	else {
		# no converge indicator column so all iterations should be included so stop if there aren't 5000 rows (1000 iters with 5 tests each) 
		stopifnot(nrow(simres)==5000)
	}

	# set true beta in log odds, assoc of bmi with SARS-CoV-2 infection or COVID-19 severity

	trueeffect = log(1)
	if (bmiEffect == "effect") {
		trueeffect = log(3)
	}
	
	# calculate average bias of simulation iterations for each version

	outfile=paste0("out/sim-",bmiEffect, ".csv")

	#print("strata: meanbias, mcSE")


	res = printStats(simres, trueeffect, "all")
	resstr = paste0(formatStat(res$biasMean), " (", formatStat(res$biasMCSE), ")")
	resstrCov = paste0(formatStat(res$coverage, 3), " (", formatStat(res$coverageMCSE), ")")
	resstrElimCov = paste0(formatStat(res$biasElimCoverage, 3), " (", formatStat(res$biasElimCoverageMCSE), ")")

	res = printStats(simres, trueeffect, "all-confadj")
	resstr = paste(resstr, paste0(formatStat(res$biasMean), " (", formatStat(res$biasMCSE), ")"), sep='\t')
	resstrCov = paste(resstrCov, paste0(formatStat(res$coverage, 3), " (", formatStat(res$coverageMCSE), ")"), sep='\t')
	resstrElimCov = paste(resstrElimCov, paste0(formatStat(res$biasElimCoverage, 3), " (", formatStat(res$biasElimCoverageMCSE), ")"), sep='\t')

	res = printStats(simres, trueeffect, "selected")
	resstr = paste(resstr, paste0(formatStat(res$biasMean), " (", formatStat(res$biasMCSE), ")"), sep='\t')
	resstrCov = paste(resstrCov, paste0(formatStat(res$coverage, 3), " (", formatStat(res$coverageMCSE), ")"), sep='\t')
	resstrElimCov = paste(resstrElimCov, paste0(formatStat(res$biasElimCoverage, 3), " (", formatStat(res$biasElimCoverageMCSE), ")"), sep='\t')

	res = printStats(simres, trueeffect, "selected-confadj")
	resstr = paste(resstr, paste0(formatStat(res$biasMean), " (", formatStat(res$biasMCSE), ")"), sep='\t')
	resstrCov = paste(resstrCov, paste0(formatStat(res$coverage, 3), " (", formatStat(res$coverageMCSE), ")"), sep='\t')
	resstrElimCov = paste(resstrElimCov, paste0(formatStat(res$biasElimCoverage, 3), " (", formatStat(res$biasElimCoverageMCSE), ")"), sep='\t')

	res = printStats(simres, trueeffect, "control-everyone")
	resstr = paste(resstr, paste0(formatStat(res$biasMean), " (", formatStat(res$biasMCSE), ")"), sep='\t')
	resstrCov = paste(resstrCov, paste0(formatStat(res$coverage, 3), " (", formatStat(res$coverageMCSE), ")"), sep='\t')
	resstrElimCov = paste(resstrElimCov, paste0(formatStat(res$biasElimCoverage, 3), " (", formatStat(res$biasElimCoverageMCSE), ")"), sep='\t')


	#cat(resstr, outfile, append=TRUE)
	cat('bias,', resstr, '\n')
	cat('coverage,', resstrCov, '\n')
	cat('biasElimCoverage,', resstrElimCov, '\n')
}



cat("No interact \n")
estimatesForSim(bmiEffect, "nointeract", large)

cat("Plausible \n")
estimatesForSim(bmiEffect, "plausible", large)

cat("Extreme \n")
estimatesForSim(bmiEffect, "extreme", large)













