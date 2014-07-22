







shinyServer(function(input, output, session) {

	## source shared functions
	## source('../base/radiant.R', local = TRUE)
	source('radiant.R', local = TRUE)

	## source data & analysis tools
	R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
	R.utils::sourceDirectory('tools/data', recursive = TRUE)

	## R.utils::sourceDirectory('tools/indic', recursive = TRUE)
        source("tools/indic/icioFddva.R", local = TRUE)
        ## source("tools/indic/stanIndic.R", local = TRUE)
        ## source("tools/indic/stanRnd.R", local = TRUE)
        source("tools/indic/stani3Estimate.R", local = TRUE)
        source("tools/indic/lfsShare.R", local = TRUE)

        R.utils::sourceDirectory('tools/disclaimer', recursive = TRUE)
	R.utils::sourceDirectory('tools/app', recursive = TRUE)
        ## R.utils::sourceDirectory('../base/tools/data', recursive = TRUE)
	## R.utils::sourceDirectory('../base/tools/app', recursive = TRUE)

        loginData <- list(LoggedIn = TRUE)

})
