







shinyServer(function(input, output, session) {

	## source shared functions
	## source('../base/radiant.R', local = TRUE)
	source('radiant.R', local = TRUE)

	## source data & analysis tools
	R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
	R.utils::sourceDirectory('tools/data', recursive = TRUE)
	R.utils::sourceDirectory('tools/app', recursive = TRUE)
 
	## R.utils::sourceDirectory('tools/indic', recursive = TRUE)
        if (active.icioFddva==TRUE) {
            source("tools/indic/icioFddva.R", local = TRUE)
        }
        if (active.stanIndic==TRUE) {
            source("tools/indic/stanIndic.R", local = TRUE)
        }
        if (active.stanRnd==TRUE) {
            source("tools/indic/stanRnd.R", local = TRUE)
        }
        if (active.stani3Estimate==TRUE) {
            source("tools/indic/stani3Estimate.R", local = TRUE)
        }
        if (active.lfsShare==TRUE) {
            source("tools/indic/lfsShare.R", local = TRUE)
        }
        if (active.sdmxBrowser==TRUE) {
            source("tools/indic/sdmxBrowser.R", local = TRUE)
        }

        ## R.utils::sourceDirectory('tools/disclaimer', recursive = TRUE)
       ## R.utils::sourceDirectory('../base/tools/data', recursive = TRUE)
	## R.utils::sourceDirectory('../base/tools/app', recursive = TRUE)

        loginData <- list(LoggedIn = TRUE)

})
