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
	## calculate monte carlo standard error
	nsim = nrow(simres)

	mcSE = sqrt((1/(nsim*(nsim-1))) * sum((simres$bias-meanbias)^2))


	#print(paste0(strata, ": ", meanbias, ", ", mcSE))
	#cat(paste0(meanbias, " (", mcSE, ")"), outfile, append=TRUE)

	return(list(meanbias=meanbias, mcse=mcSE))

}

formatStat <- function(stat) {

	return(sprintf("%.4f", stat))

}



##
## get settings from args

args = commandArgs(trailingOnly=TRUE)

# either effect or null for simulations where bmi has an effect or no effect on covid risk (the hypothesis we are testing)
bmi_assoc = args[1]
print(bmi_assoc)



estimatesForSim <- function(bmi_assoc, setup, covidSelectOR) {

	# load in the simulation results

	simres = read.table(paste0("out/sim-main-",bmi_assoc,"-", setup, "-", covidSelectOR, ".csv"), header=1, sep=",")
	# columns: iter,strata,estimate,lower,upper


	# set true beta in log odds, assoc of bmi with SARS-CoV-2 infection or COVID-19 severity

	trueeffect = log(1)
	if (bmi_assoc == "effect") {
		trueeffect = log(3)
	}
	
	# calculate average bias of simulation iterations for each version

	outfile=paste0("out/sim-main-",bmi_assoc, ".csv")

	#print("strata: meanbias, mcSE")


	res = printStats(simres, trueeffect, "all")
	resstr = paste0(formatStat(res$meanbias), " (", formatStat(res$mcse), ")")

	res = printStats(simres, trueeffect, "all-confadj")
	resstr = paste(resstr, paste0(formatStat(res$meanbias), " (", formatStat(res$mcse), ")"), sep='\t')

	res = printStats(simres, trueeffect, "selected")
	resstr = paste(resstr, paste0(formatStat(res$meanbias), " (", formatStat(res$mcse), ")"), sep='\t')

	res = printStats(simres, trueeffect, "selected-confadj")
	resstr = paste(resstr, paste0(formatStat(res$meanbias), " (", formatStat(res$mcse), ")"), sep='\t')

	res = printStats(simres, trueeffect, "control-everyone")
	resstr = paste(resstr, paste0(formatStat(res$meanbias), " (", formatStat(res$mcse), ")"), sep='\t')

	#cat(resstr, outfile, append=TRUE)
	cat(resstr, '\n')

}



cat("OR=2 \n")
estimatesForSim(bmi_assoc, "all", 2)
estimatesForSim(bmi_assoc, "bmi", "")
estimatesForSim(bmi_assoc, "covars", "")
estimatesForSim(bmi_assoc, "covid", "")
estimatesForSim(bmi_assoc, "bmi_covars", "")
estimatesForSim(bmi_assoc, "bmi_covid", "")
estimatesForSim(bmi_assoc, "covars_covid", "")

cat("OR=5 \n")
estimatesForSim(bmi_assoc, "all", 5)

cat("OR=10 \n")
estimatesForSim(bmi_assoc, "all", 10)








