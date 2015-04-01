







shinyServer(function(input, output, session) {

	## source shared functions
	## source('../base/radiant.R', local = TRUE)
	source('radiant.R', local = TRUE)

	## source data & analysis tools
	R.utils::sourceDirectory('tools/analysis', recursive = TRUE)
	R.utils::sourceDirectory('tools/data', recursive = TRUE)
	R.utils::sourceDirectory('tools/app', recursive = TRUE)

	## R.utils::sourceDirectory('tools/indic', recursive = TRUE)
        if (active.icioFddva2013==TRUE) {
            source("tools/indic/icioFddva2013.R", local = TRUE)
        }
        if (active.icioFddva==TRUE) {
            source("tools/indic/icioFddva.R", local = TRUE)
        }
        if (active.icioIndic==TRUE) {
            source("tools/indic/icioIndic.R", local = TRUE)
        }
        if (active.icioNet==TRUE) {
            source("tools/indic/icioNet.R", local = TRUE)
        }
        if (active.icioDash==TRUE) {
            source("tools/indic/icioDash.R", local = TRUE)
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
        if (active.stani4Estimate==TRUE) {
            source("tools/indic/stani4Estimate.R", local = TRUE)
        }
        if (active.lfsShare==TRUE) {
            source("tools/indic/lfsShare.R", local = TRUE)
        }
        if (active.sdmxBrowser==TRUE) {
            source("tools/indic/sdmxBrowser.R", local = TRUE)
        }
        if (active.apiBEA==TRUE) {
            source("tools/indic/apiBEA.R", local = TRUE)
        }
        if (active.fameBrowser==TRUE) {
            source("tools/indic/fameBrowser.R", local = TRUE)
        }

        ## R.utils::sourceDirectory('tools/disclaimer', recursive = TRUE)
       ## R.utils::sourceDirectory('../base/tools/data', recursive = TRUE)
	## R.utils::sourceDirectory('../base/tools/app', recursive = TRUE)

        loginData <- list(LoggedIn = TRUE)

})
