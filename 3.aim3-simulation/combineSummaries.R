### Louise AC Millard 2021
###



##
## get settings from args

args = commandArgs(trailingOnly=TRUE)

# either effect or null for simulations where bmi has an effect or no effect on covid risk (the hypothesis we are testing)
bmiEffect = args[1]
print(bmiEffect)

large = args[2]
print(large)

combineSummaries <- function(bmiEffect, selInteractEffect, large) {

	# load in the simulation results

	# summary data
	for (i in 1:20) {
		if (i == 1) {
			simres = read.table(paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-summaries-", i,".csv"), header=1, sep=",")
		}
		else {
			simresthis = read.table(paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-summaries-", i,".csv"), header=1, sep=",")
			simres = rbind(simres,simresthis)
		}
	}
	write.table(simres, paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-summaries-all.csv"), row.names=FALSE, sep=',')


	# checking data
	for (i in 1:20) {
	        if (i == 1) {
                        simres = read.table(paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-checking-", i,".csv"), header=1, sep=",")
                }
                else {
                      	simresthis = read.table(paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-checking-", i,".csv"), header=1, sep=",")
                        simres = rbind(simres,simresthis)
                }
        }
	write.table(simres, paste0("out/sim-",bmiEffect,"-", selInteractEffect, large, "-checking-all.csv"), row.names=FALSE, sep=',')
	

}



cat("No interact \n")
combineSummaries(bmiEffect, "nointeract", large)

cat("Plausible \n")
combineSummaries(bmiEffect, "plausible", large)

cat("Extreme \n")
combineSummaries(bmiEffect, "extreme", large)



warnings()
