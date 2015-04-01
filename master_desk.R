## source(file.path(dbpath, "GitHub", "desk", "master_desk.R"))
## source(file.path("C:", "Users", "werth_b", "LocalData", "Dropbox", "GitHub", "desk", "master_desk.R"))


###################################
##  Master File                  ##
##                               ##
##    Desk                       ##
###################################

## library(httr)
## set_config(use_proxy("wsg-proxy.oecd.org:80"))
## set_config(use_proxy(url = "wsg-proxy.oecd.org", port = 80))

## sudo yum install cairo-devel libXt-devel
## install.packages("devtools")
## install.packages("Cairo")
## install.packages("RJSONIO")
## devtools::install(file.path(dbpath, "GitHub", "httpuv"))
## devtools::install(file.path(dbpath, "GitHub", "shiny"))

## library(devtools)
## install_github("R-Websockets", username = "rstudio")
## install_github("ShinySky", username = "AnalytixWare")
## install_github("shinyExt", username = "marcionicolau")
## detach("packages:devtools", unload = TRUE)
## install.packages("dplyr")
## devtools::install(file.path(dbpath, "GitHub", "ggvis"))

## devtools::install(file.path(dbpath, "GitHub", "ggthemes"))
## devtools::install(file.path(dbpath, "GitHub", "rCharts"))
## devtools::install(file.path(dbpath, "GitHub", "rMaps"))

## devtools::install(file.path(dbpath, "GitHub", "httpuv"))
## devtools::install(file.path(dbpath, "GitHub", "shinyExt"))
## devtools::install(file.path(dbpath, "GitHub", "shinysky"))

## devtools::install(file.path(dbpath, "GitHub", "icioData"))


## devtools::install(file.path(dbpath, "GitHub", "htmltools"))
## require(htmltools)
## devtools::install(file.path(dbpath, "GitHub", "htmlwidgets"))
## require(htmlwidgets)

## devtools::install(file.path(dbpath, "GitHub", "rjson", "rjson"))
## devtools::install(file.path(dbpath, "GitHub", "networkD3"))

## devtools::install(file.path(dbpath, "GitHub", "xts"))
## devtools::install(file.path(dbpath, "GitHub", "dygraphs"))
## devtools::install(file.path(dbpath, "GitHub", "stanApi"))


## devtools::install(file.path(dbpath, "GitHub", "shiny-incubator"))


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
## path <- file.path(dbpath.server, "GitHub", "desk", "inst", "industry")
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
## source(file.path(path, "tools", "indic", "stani4Estimate.R"))
## source(file.path(path, "tools", "indic", "icioFddva.R"))
## source(file.path(path, "tools", "indic", "icioIndic.R"))
## source(file.path(path, "tools", "indic", "icioNet.R"))
## source(file.path(path, "tools", "indic", "lfsShare.R"))
## source(file.path(path, "tools", "indic", "apiBEA.R"))
source(file.path(path, "radiant.R"))
testingRadiant <- TRUE
## source(file.path(path, "tools", "indic", "sdmxBrowser.R"))
## library(devtools); load_all(file.path(dbpath, "GitHub", "stan"))
## library(devtools); load_all(file.path(dbpath, "GitHub", "stanData"))

# runApp(path)
runApp(path, launch.browser = TRUE)

## ############## ##
## Data Packaging ##
## ############## ##

## ~/LocalData/Dropbox/GitHub/stanData/data-raw/
##   - package_STANNAi4.R




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

## packageData(list = c("WIOT042012", "WIOT112013"),
##             namecou = STAN.COU[["ICIO"]]
##             , # namecou,
##             namevar = namevar,
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

