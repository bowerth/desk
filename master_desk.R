## source(file.path(dbpath, "GitHub", "desk", "master_desk.R"))

###################################
##  Master File                  ##
##                               ##
##    Desk                       ##
###################################

## library(httr)
## set_config(use_proxy("wsg-proxy.oecd.org:80"))
## set_config(use_proxy(url = "wsg-proxy.oecd.org", port = 80))

## sudo yum install cairo-devel libXt-devel
## install.packages("httpuv")
## install.packages("devtools")
## install.packages("Cairo")
## install.packages("RJSONIO")
# detach('package:shiny', unload=TRUE)
# library(devtools); install_github("shiny", username = "rstudio")
# library(devtools); load_all(file.path(dbpath, "GitHub", "shiny"))
## require(shiny)
## install.packages("shiny")

## library(devtools)
## install_github("R-Websockets", username = "rstudio")
## install_github("ShinySky", username = "AnalytixWare")
## install_github("shinyExt", username = "marcionicolau")
## detach("packages:devtools", unload = TRUE)

## library(markdown)
## setwd(file.path(dbpath, "GitHub", "stan", "inst", "doc"))
## setwd(file.path(dbpath, "GitHub", "desk", "inst", "industry", "tools", "help"))

## test <- markdownToHTML(file = "stanIndic.md",
##                ## output = ".html",
##                ## options = c("toc"),
##                options = c(""),
##                stylesheet="www/empty.css")

## q('no')

library(shiny)
path <- file.path(dbpath, "GitHub", "desk", "inst", "industry")
setwd(path)
testingRadiant <- FALSE
source(file.path(path, "global.R"))
input <- NULL
output <- NULL
source(file.path(path, "tools", "app", "state.R"))
source(file.path(path, "tools", "app", "tab_ui.R"))
source(file.path(path, "tools", "indic", "stanIndic.R"))
## source(file.path(path, "tools", "indic", "stani3Estimate.R"))
## source(file.path(path, "tools", "indic", "icioFddva.R"))
## source(file.path(path, "tools", "indic", "lfsShare.R"))
source(file.path(path, "radiant.R"))
testingRadiant <- TRUE

## source(file.path(path, "tools", "indic", "sdmxBrowser.R"))
runApp(path)


## ## https://github.com/ramnathv/rChartsShiny
## library(shiny)
## path <- "~/Downloads/rChartsShiny-gh-pages/rChartsShiny-gh-pages"
## runApp(file.path(path, "rChartOECD"))

## DEBUG:
## global.R : testingRadiant <- TRUE
## ui.R : comment "Disclaimer" and "Login"
## server.R : loginData <- list(LoggedIn = TRUE)
## copy to Linux server: radiant.R + "tools" folder
## shiny::runGitHub('radyant','mostly-harmless', subdir = 'inst/marketing')
## runApp(paste0(path, 'radiant\\inst\\marketing'))


## install.packages(file.path(dbpath, "CRAN_Data", "src", "contrib", "icioData_0.1.tar.gz"), repos = NULL, type = "source")
## packageData(list = c("ICIO5837VB"),
##             isic = 3,
##             nameyear=c(1995:2009),
##             file = paste0(PATH.REPO, "icioData\\data\\ICIO5837APP.rda"),
##             replace = TRUE)
## install.packages(file.path(dbpath, "CRAN_Data", "src", "contrib", "stanData_0.1.tar.gz"), repos = NULL, type = "source")

## #################
## ## ISIC Rev. 3 ##
## #################
## sourcesSTANNAi3 <- c("BTD", "EUNAIOR1", "ICIO052013", "INDSTAT32", "NSONAPATCH", "OECDSUT112013", "STAN", "UNDATA203100", "UNDATA203CON", "UNSDSNA2013", "WIOT042012", "WIOT112013")
## namecou <- STAN.COU[["ICIO"]] # excluding LKA
## ## namecou <- unique(c(union(STAN.COU, STAN.COUKPC), "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM"))
## namevar <- c("VALU", "PROD", "EMPN", "EMPE", "GFCF", "LABR", "NOPS", "OTXS", "EXPO", "IMPO", "FDDE", "FGGE", "FHHE", "GCFI", "INVC", "GDPR")
## nameyear <- c(1995:2012) # nameyear only used for ICIO extraction

## packageData(list=sourcesSTANNAi3, # c("UNSDSNA2013")
##             namecou = namecou,
##             namevar = namevar,
##             nameind = STANi3.INDA60All,
##             isic = 3,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"),
##             replace = TRUE)
## ##
## packageData(list = c("NSONAPATCH"),
##             namecou = namecou,
##             namevar = namevar,
##             nameind = STANi3.INDA60All,
##             isic = 3,
##             file = paste0(PATH.REPO, "stanData\\data\\STANNAi3.rda"),
##             replace = FALSE)
## ##
## load(file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"))

## ## "C65T99"%in%STANi3.INDA60All
## ## "C65T99"%in%DATA.UNSDSNA2013$ind
## ## unique(DATA.UNSDSNA2013$ind)

## #################
## ## ISIC Rev. 4 ##
## #################
## sourcesSTANNAi4 <- c("STAN", "BTD", "ANBERD", "XRATES")
## packageData(list=sourcesSTANNAi4,
##             namecou = namecou <- STAN.COU[["ICIO"]], # unique(union(STAN.COU, STAN.COUKPC))
##             namevar = c("EMPN", "EMPE", "GFCF", "HRSE", "HRSN", "INTI", "LABR", "PROD", "VALU", "VALK", "EXPO", "IMPO", "RDNC", "EXCH", "PPPS"), # MA, PF
##             isic = 4,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda"),
##             replace = TRUE)

## sourcesSTANNAi4 <- c("XRATES")
## packageData(list=sourcesSTANNAi4,
##             namecou = STAN.COU[["ICIO"]]
##             , # unique(union(STAN.COU, STAN.COUKPC))
##             namevar = c("EXCH", "PPPS")
##             ,
##             isic = 4,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda"),
##             replace = FALSE)


## sourcesSTANNAi4 <- c("ANBERD")
## packageData(
##     list=sourcesSTANNAi4
##     ,
##     namecou = STAN.COU[["ICIO"]]
##     , # unique(union(STAN.COU, STAN.COUKPC))
##     namevar = c("RDNC")
##     ,
##     isic = 4
##     ,
##     file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda")
##     ,
##     replace = FALSE
##     )

## load(file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda"))
## sort(unique(DATA.STAN$var))

## library(devtools); load_all(file.path(dbpath, "GitHub", "stan"))
## detach("package:stanData", unload=TRUE)
## install.packages("stanData", repos="file:///C:/Users/werth_b/LocalData/Dropbox/CRAN_Data/")

## unique(DATA.XRATES$value[is.na(DATA.XRATES$value)])
## library(stanData)
## data(STANNAi4)
## data(STANNAi3)
## h(DATA.STAN)
## h(DATA.BTD)
## h(DATA.ANBERD)
## h(DATA.XRATES)
## unique(DATA.XRATES$cou)
## unique(DATA.XRATES$var)



## cou <- "FRA"
## ind <- "CTOTAL"
## year <- 2012
## ## DATA.STAN[DATA.STAN$cou=="FRA" & DATA.STAN$ind=="DTOTAL" & DATA.STAN$year==2012,]

## test <- stani3Estimate.data.all
## test[test$cou==cou & test$ind==ind & test$year==year,]

## DATA.XRATES[DATA.XRATES$cou=="FRA" & DATA.XRATES$year==2012,]

