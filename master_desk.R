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
## install.packages("dplyr")
## devtools::install(file.path(dbpath, "GitHub", "ggvis"))

## devtools::install(file.path(dbpath, "GitHub", "shiny-incubator"))
## devtools::install(file.path(dbpath, "GitHub", "d3Network"))

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

## library(devtools); load_all(file.path(dbpath, "GitHub", "stan"))

input <- NULL
output <- NULL
source(file.path(path, "tools", "app", "state.R"))
## source(file.path(path, "tools", "app", "tab_ui.R"))
## source(file.path(path, "tools", "indic", "stanIndic.R"))
## source(file.path(path, "tools", "indic", "stani3Estimate.R"))
## source(file.path(path, "tools", "indic", "icioFddva.R"))
source(file.path(path, "tools", "indic", "icioIndic.R"))
## source(file.path(path, "tools", "indic", "lfsShare.R"))
source(file.path(path, "radiant.R"))
testingRadiant <- TRUE
## source(file.path(path, "tools", "indic", "sdmxBrowser.R"))
## library(devtools); load_all(file.path(dbpath, "GitHub", "stan"))
## library(devtools); load_all(file.path(dbpath, "GitHub", "stanData"))

runApp(path)


## system('g++ -v')
## system('where make')

## ## https://github.com/ramnathv/rChartsShiny
## library(shiny)
## path <- "~/Downloads/rChartsShiny-gh-pages/rChartsShiny-gh-pages"
## runApp(file.path(path, "rChartOECD"))
## devtools::install(file.path(dbpath, "GitHub", "rCharts"))

## DEBUG:
## global.R : testingRadiant <- TRUE
## ui.R : comment "Disclaimer" and "Login"
## server.R : loginData <- list(LoggedIn = TRUE)
## copy to Linux server: radiant.R + "tools" folder
## shiny::runGitHub('radyant','mostly-harmless', subdir = 'inst/marketing')
## runApp(paste0(path, 'radiant\\inst\\marketing'))


## install.packages(file.path(dbpath, "CRAN_Data", "src", "contrib", "stanData_0.1.tar.gz"), repos = NULL, type = "source")

## library(devtools); load_all(file.path(dbpath, "GitHub", "stan"))

## detach("package:stanData", unload=TRUE)
## install.packages(file.path(dbpath, "CRAN_Data", "src", "contrib", "stanData_0.1.tar.gz"), repos=NULL, type = "source")

## install.packages(file.path(dbpath, "CRAN_Data", "src", "contrib", "icioData_0.1.tar.gz"), repos=NULL, type = "source")
## install.packages(file.path(dbpath, "CRAN_Data", "src", "contrib", "skillData_0.1.tar.gz"), repos=NULL, type = "source")


## ####################
## ICIO
## ####################
## install.packages(file.path(dbpath, "CRAN_Data", "src", "contrib", "icioData_0.1.tar.gz"), repos = NULL, type = "source")
## packageData(list = c("ICIO5837VB"),
##             isic = 3,
##             nameyear=c(1995:2009),
##             file = paste0(PATH.REPO, "icioData\\data\\ICIO5837APP.rda"),
##             replace = TRUE)


## #################
## ## All ISIC    ##
## #################

## packageData(list=c("CONVCUR"),
##             namecou = STAN.COU[["ICIO"]],
##             namevar = c("EXCH", "PPPS"),
##             ## nameind = STANi3.INDA60All,
##             isic = 0,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi0.rda"),
##             replace = TRUE)

## env <- new.env()
## load(file.path(PATH.REPO, "stanData", "data", "STANNAi0.rda"), envir = env)
## ls(env)
## load(file.path(PATH.REPO, "stanData", "data", "STANNA.rda"))

## #################
## ## ISIC Rev. 3 ##
## #################

## ## sourcesSTANNAi3 <- c("BTDi3", "EUNAIOR1", "ICIO052013", "INDSTAT32", "NSONAPATCH", "OECDSUT112013", "STANi3", "UNDATA203100", "UNDATA203CON", "UNSDSNA2013", "WIOT042012", "WIOT112013")
## ## namecou <- STAN.COU[["ICIO"]] # excluding LKA
## ## ## namecou <- unique(c(union(STAN.COU, STAN.COUKPC), "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM"))
## namevar <- c("VALU", "PROD", "EMPN", "EMPE", "GFCF", "LABR", "NOPS", "OTXS", "EXPO", "IMPO", "FDDE", "FGGE", "FHHE", "GCFI", "INVC", "GDPR")
## ## nameyear <- c(1995:2012) # nameyear only used for ICIO extraction

## packageData(list=sourcesSTANNAi3, # c("UNSDSNA2013")
##             namecou = STAN.COU[["ICIO"]], # namecou,
##             namevar = namevar,
##             nameind = STANi3.INDA60All,
##             isic = 3,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"),
##             replace = TRUE)

## packageData(list = c("STANi3"),
##             namecou = STAN.COU[["ICIO"]]
##             , # namecou,
##             namevar = namevar,
##             nameind = STANi3.INDA60All,
##             isic = 3,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"),
##             replace = FALSE)

## packageData(list=c("STDSNAi3")
##             ,
##             namecou = STAN.COU[["ICIO"]]
##             ,
##             namevar = namevar
##             ,
##             isic = 3
##             ,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda")
##             ,
##             replace = FALSE
##             )

## packageData(list = c("EUNAMAR1"),
##             namecou = STAN.COU[["ICIO"]]
##             , # namecou,
##             namevar = namevar,
##             nameind = STANi3.INDA60All,
##             isic = 3,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"),
##             replace = FALSE)

## packageData(list = c("INDSTAT32"),
##             namecou = STAN.COU[["ICIO"]]
##             , # namecou,
##             namevar = namevar,
##             nameind = STANi3.INDA60All,
##             isic = 3,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"),
##             replace = FALSE)

## packageData(list = c("NSONAPATCHi3"),
##             namecou = STAN.COU[["ICIO"]]
##             , # namecou,
##             namevar = namevar,
##             nameind = STANi3.INDA60All,
##             isic = 3,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"),
##             replace = FALSE)

## load(file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"))

## renameData(file = file.path(dbpath, "GitHub", "stanData", "data", "STANNAi3.rda"),
## renameData(file = file.path(PATH.SASi3, "DATA_out", "NSONAPATCHi3.rda"),
##            from = c("DATA.NSONAPATCH"),
##            to = c("DATA.NSONAPATCHi3"))

## env <- new.env()
## load(file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"), envir = env)
## load(file.path(PATH.SASi3, "DATA_in", "SNA", "STDSNAi3.rda"), envir = env)
## load(file.path(PATH.SASi4, "DATA_in", "SNA", "STDSNAi4.rda"), envir = env)
## load(file.path(PATH.SASi4, "DATA_out", "NSONAPATCHi4.rda"), envir = env)

## list <- ls(env)
## rm(DATA.NSONAPATCH, envir = env)
## save(list = list, file = file.path(PATH.REPO, "stanData", "data", "STANNAi3.rda"), envir = env)


## #################
## ## ISIC Rev. 4 ##
## #################
## sourcesSTANNAi4 <- c("STAN", "BTD", "ANBERD", "XRATES")
## packageData(list=sourcesSTANNAi4,
##             namecou = STAN.COU[["ICIO"]], # unique(union(STAN.COU, STAN.COUKPC))
##             namevar = c("EMPN", "EMPE", "GFCF", "HRSE", "HRSN", "INTI", "LABR", "PROD", "VALU", "VALK", "EXPO", "IMPO", "RDNC", "EXCH", "PPPS")
##             , # MA, PF
##             isic = 4,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda"),
##             replace = TRUE)

## packageData(list=c("STANi4")
##             ,
##             namecou = STAN.COU[["ICIO"]]
##             ,
##             namevar = c("EMPN", "EMPE", "GFCF", "HRSE", "HRSN", "INTI", "LABR", "PROD", "VALU", "VALK", "EXPO", "IMPO", "RDNC", "EXCH", "PPPS")
##             ,
##             isic = 4
##             ,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda")
##             ,
##             replace = FALSE
##             )

## packageData(list=c("STDSNAi4")
##             ,
##             namecou = STAN.COU[["ICIO"]]
##             ,
##             namevar = c("EMPN", "EMPE", "GFCF", "HRSE", "HRSN", "INTI", "LABR", "PROD", "VALU", "VALK", "EXPO", "IMPO", "RDNC", "EXCH", "PPPS")
##             ,
##             isic = 4
##             ,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda")
##             ,
##             replace = FALSE
##             )

## packageData(list=c("EUNAMAR2")
##             ,
##             namecou = STAN.COU[["ICIO"]]
##             ,
##             namevar = c("EMPN", "EMPE", "GFCF", "HRSE", "HRSN", "INTI", "LABR", "PROD", "VALU", "VALK", "EXPO", "IMPO", "RDNC", "EXCH", "PPPS")
##             ,
##             isic = 4
##             ,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda")
##             ,
##             replace = FALSE
##             )

## data <- DATA.STAN
## data <- DATA.EUNAMAR2
## unique(data[duplicated(data[,colnames(data)%in%c("cou", "var", "ind", "year")]),]$cou)

## packageData(list = c("NSONAPATCHi4"),
##             namecou = STAN.COU[["ICIO"]]
##             , # namecou,
##             namevar = c("EMPN", "EMPE", "GFCF", "HRSE", "HRSN", "INTI", "LABR", "PROD", "VALU", "VALK", "EXPO", "IMPO", "RDNC", "EXCH", "PPPS"),
##             ## nameind = STANi3.INDA60All,
##             isic = 4,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda"),
##             replace = FALSE)


## sourcesSTANNAi4 <- c("XRATES")
## packageData(list=sourcesSTANNAi4
##             ,
##             namecou = STAN.COU[["ICIO"]]
##             , # unique(union(STAN.COU, STAN.COUKPC))
##             namevar = c("EXCH", "PPPS")
##             ,
##             isic = 4
##             ,
##             file = file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda")
##             ,
##             replace = FALSE
##             )

## load(file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda"))


## renameData(file = file.path(dbpath, "GitHub", "stanData", "data", "STANNAi4.rda"),
##            from = c("DATA.ANBERD"),
##            to = c("DATA.ANBERDi4"))
## env <- new.env()
## load(file.path(PATH.REPO, "stanData", "data", "STANNAi4.rda"), envir = env)
## ls(env)

## #################################
## Skill data
## #################################

## ## namecou <- unique(c(union(STAN.COU, STAN.COUKPC), "BGR", "BRN", "CYP", "HKG", "KHM", "LKA", "LTU", "LVA", "MLT", "MYS", "ROU", "SAU", "SGP", "THA", "VNM"))
## namevar <- c("EMPN", "EMPE", "SELF")
## ## sou <- c("LFSAUS", "LFSCAN", "LFSEU", "LFSUSA", "LFSILO")
## sou <- "LFSEU"

## ## source("~/LocalData/Dropbox/GitHub/stan/R/packageData.R")
## packageData(list = sou,
##             namecou = STAN.COU[["ICIO"]],
##             namevar = namevar,
##             ## nameind = STANi4.INDALL,
##             isic = 4,
##             file = file.path(PATH.REPO, "skillData", "data", "LFSi4.rda"),
##             replace = FALSE)

## env <- new.env()
## load(file.path(PATH.REPO, "skillData", "data", "LFSi4.rda"), envir = env)
## ls(envir = env)

