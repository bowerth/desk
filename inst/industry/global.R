
if (exists("testingRadiant")==FALSE) testingRadiant <- FALSE

app.menu <- list()

app.menu$disclaimer <- TRUE
app.menu$about <- TRUE

app.menu$panel.df <- rbind.data.frame(
    c("active", "ICIO", "Foreign Demand Domestic Value Added 2013", "icioFddva2013"),
    c("inactive", "ICIO", "Foreign Demand Domestic Value Added", "icioFddva"),
    c("active", "ICIO", "TiVA Indicators", "icioIndic"),
    c("active", "ICIO", "ICIO Networks", "icioNet"),
    c("active", "ICIO", "ICIO Dashboards", "icioDash"),
    ## c("active", "ICIO", "ICIO BSCI", "icioBsci"),
    c("active", "STAN", "STAN ISIC3 Estimate", "stani3Estimate"),
    c("active", "STAN", "STAN ISIC4 Estimate", "stani4Estimate"),
    c("active", "STAN", "STAN Indicators", "stanIndic"),
    c("active", "STAN", "R&D Intensity", "stanRnd"),
    c("active", "SKILL", "LFS Share", "lfsShare"),
    c("active", "API", "SDMX Browser", "sdmxBrowser"),
    c("active", "API", "API BEA", "apiBEA")
   ,
    c("inactive", "API", "FAME Browser", "fameBrowser")
    )
names(app.menu$panel.df) <- c("status", "menuTitle", "panelTitle", "outputID")

## app.menu$panel.df$status <- "active"


for (outputID in app.menu$panel.df$outputID) {
    if (app.menu$panel.df$status[app.menu$panel.df$outputID==outputID]=="active") {
        eval(parse(text = paste0('active.', outputID, ' <- TRUE')))
    } else {
        eval(parse(text = paste0('active.', outputID, ' <- FALSE')))
    }
}

command.ui <- NULL
menuTitle <- "STAN"
for (menuTitle in unique(app.menu$panel.df$menuTitle[app.menu$panel.df$status=="active"])) {
    command.ui <- paste0(command.ui, 'navbarMenu("', menuTitle, '",\n')
    for (outputID in app.menu$panel.df$outputID[app.menu$panel.df$menuTitle==menuTitle & app.menu$panel.df$status == "active"]) {
        panelTitle <- app.menu$panel.df$panelTitle[app.menu$panel.df$outputID==outputID]
        command.ui <- paste0(command.ui, '\ttabPanel("', panelTitle, '", uiOutput("', outputID, '")),\n')
    }
    command.ui <- paste0(command.ui, '),\n')
}
command.ui <- gsub(',\n\\)', '\n\\)', command.ui)


## only write if running on developer computer
if(file.exists("~\\LocalData\\Dropbox\\GitHub\\desk"))
  {
    ## ## list of all radiant files with time-stamps
    ## dbox_remote <- file.info(list.files(recursive = TRUE, include.dirs = TRUE))
    ## save(dbox_remote, file = "dbox_remote.rda")
    ## options(shiny.reactlog=TRUE)
    ## options(error = recover)
    ## ## shiny.trace shows JSON packets transferred over websockets
    options(shiny.trace = TRUE)
    vimKeyBinding <- TRUE
  } else {
    vimKeyBinding <- FALSE
  }

options(digits = 3)

## allowing anyfile size when run locally
if(Sys.getenv('SHINY_PORT') == "") {
    ## no limit to filesize locally
    options(shiny.maxRequestSize=-1)
    running_local <<- TRUE
  } else {
    running_local <<- FALSE
  }

setInitValues <- function() {
    ## initialize state list and reactive values
    if(testingRadiant) {
        ## load previous state for testing
    } else {
        state_list <<- list()
        values <<- reactiveValues()

        ## initial plot height and width
        values$plotHeight <- 650
        values$plotWidth <- 650
        values$colors <- c("#4F81BD", "#C0504D", "#9BBB59", "#8064A2", "#4BACC6", "#F79646")

        ## ## Datasets can change over time (i.e. the changedata function). Therefore,
        ## ## the data need to be a reactive value so the other reactive functions
        ## ## and outputs that depend on these datasets will know when they are changed.
        ## robj <- load("../base/data/data_init/diamonds.rda")
        ## robj <- load("data/data_init/diamonds.rda")
        ## df <- mget(robj)
        ## df <- get(robj)

        ## test
        values$datasetlist <- NULL
        ##

        if (active.icioFddva2013==TRUE) {
            env <- new.env()
            data("ICIO5837APP", package = "icioData", envir = env)
            df <- mget(ls(envir = env), envir = env)
            values[["ICIO5837APP"]] <- df # rbind(df$DATA.STAN, df$DATA.BTD)
            values[["ICIO5837APP_descr"]] <- attr(df,"description")
            ## values$datasetlist <- c("ICIO5837APP")
            values$datasetlist <- c(isolate(values$datasetlist), "ICIO5837APP")
        }

        if (active.icioFddva==TRUE | active.icioDash==TRUE) {
        ## if (active.icioFddva==TRUE | active.icioDash==TRUE | active.icioBsci==TRUE) {
        ## if (active.icioDash==TRUE) {
            env <- new.env()
            data("ICIO6234APP", package = "icioData", envir = env)
            df <- mget(ls(envir = env), envir = env)
            values[["ICIO6234APP"]] <- df # rbind(df$DATA.STAN, df$DATA.BTD)
            values[["ICIO6234APP_descr"]] <- attr(df,"description")
            ## values$datasetlist <- c("ICIO6234APP")
            values$datasetlist <- c(isolate(values$datasetlist), "ICIO6234APP")
        }

        if (active.stani4Estimate==TRUE | active.stani3Estimate==TRUE | active.stanIndic==TRUE | active.stanRnd==TRUE) {
            env <- new.env()
            data("STANNAi4", package = "stanData", envir = env)
            df <- mget(ls(envir = env), envir = env)
            ##
            ## check for duplicates in type: MA and PF
            ## row <- df$DATA.ANBERD[1,]
            ## row$type <- "PF"; row$value <- 10
            ## df$DATA.ANBERD <- rbind(row, df$DATA.ANBERD)
            ## test <- df$DATA.ANBERD[duplicated(df$DATA.ANBERD[,!colnames(df$DATA.ANBERD)%in%c("value", "type")]),]
            df$DATA.ANBERD <- df$DATA.ANBERD[,!colnames(df$DATA.ANBERD)%in%c("type")]
            df$DATA.BTD$ind <- sub("D31T32", "D31T33", df$DATA.BTD$ind)
            require(reshape2)
            require(stan)
            data(stanDim)
            df$DATA.BTD <- dcast(df$DATA.BTD, cou + var + year ~ ind, value.var = "value")
            df$DATA.BTD <- indAggregate(df$DATA.BTD, isic = 4)
            df$DATA.BTD <- melt(df$DATA.BTD, id.vars = c("cou", "var", "year"), variable.name = "ind")
            ##
            values[["STANNAi4"]] <- df
            values[["STANNAi4_descr"]] <- attr(df,"description")
            ## values$datasetlist <- c("STANNAi4")
            values$datasetlist <- c(isolate(values$datasetlist), "STANNAi4")

            if (active.stani3Estimate==TRUE) {
                env <- new.env()
                data("STANNAi3", package = "stanData", envir = env)
                df <- mget(ls(envir = env), envir = env)
                values[["STANNAi3"]] <- df # rbind(df$DATA.STAN, df$DATA.BTD)
                values[["STANNAi3_descr"]] <- attr(df,"description")
                ## values$datasetlist <- c("STANNAi3")
                values$datasetlist <- c(isolate(values$datasetlist), "STANNAi3")
            }

            ## Exchange rates and other data without industry classification
            if (active.stani4Estimate==TRUE | active.stani3Estimate==TRUE | active.stanRnd==TRUE) {
                env <- new.env()
                data("STANNAi0", package = "stanData", envir = env)
                df <- mget(ls(envir = env), envir = env)
                values[["STANNAi0"]] <- df
                values[["STANNAi0_descr"]] <- attr(df,"description")
                ## values$datasetlist <- c("STANNAi0")
                values$datasetlist <- c(isolate(values$datasetlist), "STANNAi0")
            }

        }

        if (active.lfsShare==TRUE) {
            env <- new.env()
            data("LFSi4", package = "skillData", envir = env)
            ##
            ## load(file.path(PATH.REPO, "skillData", "data", "LFSi4.rda"), envir = env)
            ##
            df <- mget(ls(envir = env), envir = env)
            df.rbind <- NULL
            ## unique(df.rbind$ocu)
            ## sou <- "LFSUSA"
            ## namesou <- setdiff(names(df), paste0('DATA.', sou))
            ## for (sou in sub("DATA.", "", namesou))
            for (lfssou in sub("DATA.", "", names(df)))
                {
                    if (!lfssou%in%c("LFSEU", "LFSILO")) sou <- "LFSNSO" else sou <- lfssou
                    eval(parse(text = paste0('DATA.', sou, ' <- df$DATA.', lfssou)))
                    eval(parse(text = paste0('DATA.', sou, '$sou <- "', sou, '"')))
                    eval(parse(text = paste0('DATA.', sou, ' <- subset(DATA.', sou, ', select = c("sou", "cou", "var", "ind", "ocu", "year", "value"))')))
                    eval(parse(text = paste0('df.rbind <- rbind(df.rbind, DATA.', sou, ')')))
                }
            values[["LFSi4"]] <- df.rbind
            values[["LFSi4_descr"]] <- attr(df,"description")
            ## values$datasetlist <- c("LFSi4")
            values$datasetlist <- c(isolate(values$datasetlist), "LFSi4")
        }

    }
}

setInitValues() # using a function here so it can also be called from state.R to reset the app


## main install happens through update.R
options(repos = c(CRAN = "http://cran.rstudio.com"))
## libs <- c("shiny", "knitr", "shinyAce", "car", "tools", "gridExtra", "markdown", "R.utils", "psych",
##   "arm", "plyr", "reshape2", "vegan", "ggplot2", "lubridate", "wordcloud", "AlgDesign")

## #################### ##
## create miniCRAN repo ##
## #################### ##
## ## ~/Rinitfunctions.R
## ## current_repo["CRAN"]
## pkgList <- pkgDep(pkg = libs, repos = current_repo, type = "source", suggests = FALSE)
## makeRepo(pkgList, path=file.path(dbpath, "miniCRAN"), repos=current_repo, type="source")

## ## dir.create(pth <- file.path(tempdir(), "miniCRAN"))
## ## list.files(pth, recursive = TRUE, full.names = FALSE)
## ##
## ## installation test
## install.packages(pkgs = "dichromat", repos = file.path("file://", dbpath, "miniCRAN"), type = "source")
## install.packages(pkgs = "data.table", repos = file.path("file://", dbpath, "miniCRAN"), type = "source")


libs <- c(## "stan",
          "AlgDesign",
          "car",
          ## "d3Network",
          ## "networkD3", # using various render* functions - cumbersome with radiant...
          "htmlwidgets",
          "data.table",
          "dplyr",
          "digest",
          "dygraphs",
          "fame",
          "ggplot2",
          ## "ggvis", # need dependencies
          "grDevices",
          "gridExtra",
          "knitr",
          "lubridate",
          "markdown",
          "MASS",
          ## "pander",
          "plyr",
          "psych",
          "R.utils",
          "RJSDMX",
          ## "rCharts", # github
          "RColorBrewer",
          "RCurl",
          "reshape2",
          "rjson",
          "shiny",
          "shinyAce",
          ## "shinyExt", # github
          ## "stanApi",
          "stringr",
          "vegan",
          "wordcloud",
          "XLConnect",
          "xtable",
          "xts"
          ## ,
          ## "websockets"
          )

## ## use miniCRAN to download all package sources
## pkgList <- pkgDep(pkg = libs, repos = current_repo, type = "source", suggests = FALSE)
## makeRepo(pkgList, path=file.path(dbpath, "miniCRAN"), repos=current_repo, type="source")

## install.packages(file.path(dbpath, "CRAN", "src", "contrib", "RJSDMX_0.1.tar.gz"), repos = NULL, type = "source")
libs.dev.remote <- c(
    "ggthemes"
    ,
    "rCharts"
    ,
  "rMaps"
  ,
  "networkD3"
    ## ,
    ## "shinyExt"
    ,
    ## "shinysky"
    "shinysky"
    ,
    "stan"
   ,
   "stanApi"
    )                                  # rCharts, RJSDMX, R-Websockets
available.dev <- suppressWarnings(sapply(libs.dev.remote, require, character.only=TRUE))
inst.libs.dev.remote <- libs.dev.remote[available.dev == FALSE]
if(length(inst.libs.dev.remote) != 0) {
    ## if (length(libs.dev.remote) > 0)
    require(devtools)
    require(httr)
    set_config(config(ssl.verifypeer = 0L))
    for (lib in inst.libs.dev.remote) {
        ## if (lib == "ggthemes") install_github("ggthemes", username = "jrnold")
        if (lib == "ggthemes") install_github("bowerth/ggthemes")
        if (lib == "networkD3") install_github("bowerth/networkD3")
        if (lib == "rCharts") install_github("ramnathv/rCharts")
        if (lib == "rMaps") install_github("bowerth/rMaps")
        ## if (lib == "shinyExt") install_github("shinyExt", username = "marcionicolau")
        if (lib == "shinysky") install_github("AnalytixWare/ShinySky")
        if (lib == "stan") install_github("bowerth/stan")
        if (lib == "stanApi") install_github("bowerth/stanApi")
        ## if (lib == "rCharts") install_github("rCharts", username = "ramnathv")
        ## if (lib == "RJSDMX") install_github("RJSDMX", username = "bowerth")
        ## if (lib == "R-Websockets") install_github("R-Websockets", username = "rstudio")
    }
    ## suppressWarnings(sapply("stan", "rMaps"), install_github, username = "bowerth"))
    ## suppressWarnings(sapply(file.path(dbpath, "GitHub", libs.dev.remote), load_all))
    detach("package:devtools", unload = TRUE)
}


## libs.dev.local <- c(
##     ## "rCharts",
##     "rMaps"
##     )
## require(devtools)
## ## set_config( config( ssl.verifypeer = 0L ) )
## suppressWarnings(sapply(file.path(dbpath, "GitHub", libs.dev.local), load_all))
## detach("package:devtools", unload = TRUE)

if (Sys.info()["sysname"]=="Linux") {
    ## in desk/configuration.properties: comment "http.proxy.name0 = wsg-proxy.oecd.org:80"
    ## ## install.packages(file.path(dbpath, "CRAN", "src", "contrib", "RJSDMX_0.1.tar.gz"), repos = NULL, type = "source")
    require(RJSDMX)
    ## .jaddClassPath(file.path(dbpath, "GitHub", "RJSDMX", "inst", "java", "SDMX.jar"))
    ## source(file.path(dbpath, "GitHub", "RJSDMX", "R", "SdmxClient.R"))
    ## source(file.path(dbpath, "GitHub", "RJSDMX", "R", "onLoad.R"))
    ## source(file.path(dbpath, "GitHub", "RJSDMX", "R", "TSConverter.R"))
}

## would prefer to use importFrom but ...
## detach("package:R.utils", unload=TRUE)
##
## problem with ggplot2::fortify
##
## pander for pretty printing of data and analysis output
## panderOptions('digits',3)

libs.data <- c("icioData",
               "stanData",
               "skillData")
available.data <- suppressWarnings(sapply(libs.data, require, character.only=TRUE))
inst.libs.data <- libs.data[available.data == FALSE]
if(length(inst.libs.data) != 0) {
  require(devtools)
  sapply(file.path(dbpath, "GitHub", inst.libs.data), FUN = "install") 
  detach("package:devtools", unload = TRUE)
}

## check if all packages in libs are available
available <- suppressWarnings(sapply(libs, require, character.only=TRUE))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
  install.packages(inst.libs, dependencies = TRUE)
  ## build from source files in miniCRAN
  ## install.packages(pkgs = inst.libs, repos = file.path("file://", dbpath, "miniCRAN"), type = "source", dependencies = TRUE) # dbpath.server

  ## suppressWarnings(sapply(inst.libs, require, character.only=TRUE))
  sapply(inst.libs, require, character.only=TRUE)
}

## binding for a text input that updates when the return key is pressed
returnTextInput <- function(inputId, label, value = "") {
    tagList(
        singleton(tags$head(tags$script(src = "js/returnTextInputBinding.js"))),
        tags$label(label, `for` = inputId),
        tags$input(id = inputId, type = "text", value = value, class = "returnTextInput")
        )
}

## binding for a sortable list of variables or factor levels
html_list <- function(vars, id) {
    hl <- paste0("<ul id=\'",id,"\' class='stab'>")
    for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
    paste0(hl, "</ul>")
}

## binding for a sortable list of variables or factor levels
returnOrder <- function(inputId, vars) {
    tagList(
        singleton(tags$head(tags$script(src = 'js/sort.js'))),
        singleton(includeCSS("www/sort.css")),
        HTML(html_list(vars, inputId)),
        tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
        )
}

## function to render .Rmd files to html on-the-fly
includeRmd <- function(path) {
    ## shiny:::dependsOnFile(path)
    contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
    ## do not embed image or add css
    html <- knit2html(text = contents, fragment.only = TRUE, options = "", stylesheet = "www/empty.css")
    Encoding(html) <- 'UTF-8'
    HTML(html)
}

## binding to a bootstrap popover, function by Joe Cheng https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
    tagList(
        singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
        tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
               `data-placement` = match.arg(placement, several.ok=TRUE)[1],
               `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
        )
}

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", "tools/help/figures/")
## addResourcePath("www", "../base/www/")
## addResourcePath("tools", "../base/tools/")

## binding to a bootstrap modal
helpModal <- function(title, link, content) {
    html <- sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <a title='Help' data-toggle='modal' href='#%s' class='icon-question-sign'></a>", link, title, content, link)
    Encoding(html) <- 'UTF-8'
    HTML(html)
}

helpAndReport <- function(title, link, content) {
    html <- sprintf("<div id='%sHelp' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <div>
                     <a title='Help' data-toggle='modal' href='#%sHelp' class='icon-question-sign alignleft'></a>
                     <a title='Report results' class='icon-book action-button shiny-bound-input alignright' href='#%sReport' id='%sReport'></a>
                   </div>
                   <div style='clear: both;'></div>
                   ", link, title, content, link, link, link)
    Encoding(html) <- 'UTF-8'
    HTML(html)
}

## inclMD <- function(file) return(includeHTML(file))
inclMD <- function(file) return(markdownToHTML(file, options = c(""), stylesheet="www/empty.css"))

## }



