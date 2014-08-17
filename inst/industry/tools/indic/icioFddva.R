## R.utils::sourceDirectory('tools/app', recursive = TRUE)

###############################
## icioFddva
## icioSelect
###############################
##
## add converter plot
## add table output
## add map plot
##
## output <- NULL
##
## setwd('C:\\Users\\werth_b\\LocalData\\Dropbox\\GitHub\\desk\\inst\\industry')
##
## menu_name = "IcioFddva"                      # menu_name
## fun_name = "IcioFddva"                       # fun_name
## rfun_label = ".icioFddva"                    # rfun_label
## fun_label = "icioFddva"                      # fun_label
##
## ## output elements
## output$uiRnd_var # dynamic UI element
## output$uiRnd_block # dynamic UI element
## output$ui_random # UI: inputs
## output$random # UI: statTabPanel
## .random # reactive
## random # function
## summary_random # output for summary tab
## plots_random # output for plots tab

ui.icioFddva.ind <- list(
    "C01T99 TOT" = 137,
    "C15T37 MANUF" = 320,
    ## "C30T33 ICT" = 1417,
    "C50T99 SERV" = 2337,
    "C50T74 MKSERV" = 2332,
    "C10T74 NAGMK" = 232,
    "......" = 9001,
    "C01T05 AGR" = 1,
    "C10T14 MIN" = 2,
    "C15T16 FOD" = 3,
    "C17T19 TEX" = 4,
    "C20T22 WPP" = 506,
    ## "C20 WOD" = 5,
    ## "C21T22 PAP" = 6,
    "C23T26 CNM" = 710,
    ## "C23 PET" = 7,
    ## "C24 CHM" = 8,
    ## "C25 RBP" = 9,
    ## "C26 NMM" = 10,
    "C27T28 MFM" = 1112,
    ## "C27 MET" = 11,
    ## "C28 FBM" = 12,
    "C29 MEQ" = 13,
    "C30T33 ICT" = 1417,
    ## "C30 ITQ" = 14,
    ## "C31 ELQ" = 15,
    ## "C32 CMQ" = 16,
    ## "C33 SCQ" = 17,
    "C34T35 MTQ" = 1819,
    "C34 MTR" = 18,
    "C35 TRQ" = 19,
    "C36T37 OTM" = 20,
    "C40T41 EGW" = 21,
    "C45 CON" = 22,
    "C50T55 THR" = 2324,
    ## "C50T52 WRT" = 23,
    ## "C55 HTR" = 24,
    "C60T64 TRT" = 2526,
    ## "C60T63 TRN" = 25,
    ## "C64 PTL" = 26,
    "C65T67 FIN" = 27,
    "C70T74 BZS" = 2832,
    ## "C70 REA" = 28,
    ## "C71 RMQ" = 29,
    ## "C72 ITS" = 30,
    ## "C73 RDS" = 31,
    ## "C74 BZS" = 32,
    "C75T95 OTS" = 3337
    ## "C75 GOV" = 33,
    ## "C80 EDU" = 34,
    ## "C85 HTH" = 35,
    ## "C90T93 OTS" = 36,
    ## "C95 PVH" = 37,
    ## "......" = 9002,
    ## "C29T30 OTH" = 1314
    )
##
ui.icioFddva.cou <- list(
  "WOR: Total World" = 65,
  "Total OECD" = 64,
  "ASEAN" = 63,
  "East Asia" = 62,
  "NAFTA" = 61,
  "EU27" = 60,
  "EU15" = 59,
  "..." = 100,
  "AUS: Australia" = 1,
  "AUT: Austria" = 2,
  "BEL: Belgium" = 3,
  "CAN: Canada" = 4,
  "CHL: Chile" = 5,
  "CZE: Czech Republic" = 6,
  "DNK: Denmark" = 7,
  "EST: Estonia" = 8,
  "FIN: Finland" = 9,
  "FRA: France" = 10,
  "DEU: Germany" = 11,
  "GRC: Greece" = 12,
  "HUN: Hungary" = 13,
  "ISL: Iceland" = 14,
  "IRL: Ireland" = 15,
  "ISR: Israel" = 16,
  "ITA: Italy" = 17,
  "JPN: Japan" = 18,
  "KOR: Korea" = 19,
  "LUX: Luxembourg" = 20,
  "MEX: Mexico" = 21,
  "NLD: Netherlands" = 22,
  "NZL: New Zealand" = 23,
  "NOR: Norway" = 24,
  "POL: Poland" = 25,
  "PRT: Portugal" = 26,
  "SVK: Slovak Republic" = 27,
  "SVN: Slovenia" = 28,
  "ESP: Spain" = 29,
  "SWE: Sweden" = 30,
  "CHE: Switzerland" = 31,
  "TUR: Turkey" = 32,
  "GBR: United Kingdom" = 33,
  "USA: United States" = 34,
  "ARG: Argentina" = 35,
  "BRA: Brazil" = 36,
  "CHN: China" = 37,
  "TWN: Chinese Taipei" = 38,
  "IND: India" = 39,
  "IDN: Indonesia" = 40,
  "RUS: Russian Federation" = 41,
  "SGP: Singapore" = 42,
  "ZAF: South Africa" = 43,
  "HKG: Hong Kong, China" = 44,
  "MYS: Malaysia" = 45,
  "PHL: Philippines" = 46,
  "THA: Thailand" = 47,
  "ROU: Romania" = 48,
  "VNM: Viet Nam" = 49,
  "SAU: Saudi Arabia" = 50,
  "BRN: Brunei" = 51,
  "BGR: Bulgaria" = 52,
  "CYP: Cyprus" = 53,
  "LVA: Latvia" = 54,
  "LTU: Lithuania" = 55,
  "MLT: Malta" = 56,
  "KHM: Cambodia" = 57,
  "ROW: Rest of the World" = 58
  ## "CHN2" = 59,
  ## "CHN3" = 60,
  ## "CHN4" = 61
  )
##
ui.icioFddva.year <- c(1995, 2000, 2005, 2008, 2009)
##
ui.icioFddva.coef <- rbind.data.frame(
  ## c("eB", "Employment"),
  c("vB", "Value added")
  )
names(ui.icioFddva.coef) <- c("coef", "label")
##
ui.icioFddva.demand <- rbind.data.frame(
  c("FDTTLWITHDISC", "Final demand"),
  c("GFCF", "Capital formation"),
  c("GRTR", "Gross trade"),
  c("HHCP", "Household consumption")
  )
names(ui.icioFddva.demand) <- c("demand", "label")
##
ui.icioFddva.aggind <- rbind.data.frame(
  c("C01T05 AGR", "C01T05 AGR"),
  c("C10T14 MIN", "C10T14 MIN"),
  c("C15T16 FOD", "C15T16 FOD"),
  c("C17T19 TEX", "C17T19 TEX"),
  c("C20T22 WPP", "C20 WOD"),
  c("C20T22 WPP", "C21T22 PAP"),
  c("C23T26 CNM", "C23 PET"),
  c("C23T26 CNM", "C24 CHM"),
  c("C23T26 CNM", "C25 RBP"),
  c("C23T26 CNM", "C26 NMM"),
  c("C27T28 MFM", "C27 MET"),
  c("C27T28 MFM", "C28 FBM"),
  c("C29 MEQ", "C29 MEQ"),
  c("C30T33 ICT", "C30 ITQ"),
  c("C30T33 ICT", "C31 ELQ"),
  c("C30T33 ICT", "C32 CMQ"),
  c("C30T33 ICT", "C33 SCQ"),
  c("C34T35 MTQ", "C34 MTR"),
  c("C34T35 MTQ", "C35 TRQ"),
  c("C36T37 OTM", "C36T37 OTM"),
  c("C40T41 EGW", "C40T41 EGW"),
  c("C45 CON", "C45 CON"),
  c("C50T55 THR", "C50T52 WRT"),
  c("C50T55 THR", "C55 HTR"),
  c("C60T64 TRT", "C60T63 TRN"),
  c("C60T64 TRT", "C64 PTL"),
  c("C65T67 FIN", "C65T67 FIN"),
  c("C70T74 BZS", "C70 REA"),
  c("C70T74 BZS", "C71 RMQ"),
  c("C70T74 BZS", "C72 ITS"),
  c("C70T74 BZS", "C73 RDS"),
  c("C70T74 BZS", "C74 BZS"),
  c("C75T95 OTS", "C75 GOV"),
  c("C75T95 OTS", "C80 EDU"),
  c("C75T95 OTS", "C85 HTH"),
  c("C75T95 OTS", "C90T93 OTS"),
  c("C75T95 OTS", "C95 PVH")
  )
names(ui.icioFddva.aggind) <- c("ind.icio18", "ind.icio37")
##
ui.icioFddva.secagg <- rbind.data.frame(
    c(137, "C01T99 TOT", 1, 37),
    c(320, "C15T37 MANUF", 3, 20),
    c(506, "C20T22 WPP", 5, 6),
    c(710, "C23T26 CNM", 7, 10),
    c(1112, "C27T28 MFM", 11, 12),
    c(1314, "C29T30 OTH", 13, 14),
    c(1417, "C30T33 ICT", 14, 17),
    c(1819, "C34T35 MTQ", 18, 19),
    c(2324, "C50T55 THR", 23, 24),
    c(2526, "C60T64 TRT", 25, 26),
    c(2832, "C70T74 BZS", 28, 32),
    c(3337, "C75T95 OTS", 33, 37),
    c(2337, "C50T99 SERV", 23, 37),
    c(2332, "C50T74 MKSERV", 23, 32),
    c(232, "C10T74 NAGMK", 2, 32)
    )
##
names(ui.icioFddva.secagg) <- c("id", "agg", "start", "end")
ui.icioFddva.secagg$id <- as.numeric(as.character(ui.icioFddva.secagg$id))
ui.icioFddva.secagg$start <- as.numeric(as.character(ui.icioFddva.secagg$start))
ui.icioFddva.secagg$end <- as.numeric(as.character(ui.icioFddva.secagg$end))

ui.icioFddva.namesec <- as.character(ui.icioFddva.aggind[,"ind.icio37"])
ui.icioFddva.namesec.agg <- union(ui.icioFddva.namesec, ui.icioFddva.secagg$agg)

##
ui.icioFddva.seclabel <- rbind.data.frame(
  c("C01T99 TOT","C01T99","Total","total",0,0,1),
  c("C01T05 AGR","C01T05","Agriculture","agriculture",1,1,0),
  c("C10T74 NAGMK","C10T74","Non-agriculture _ business sector","non-agriculture business sector",0,0,1),
  c("C10T14 MIN","C10T14","Mining","mining",1,1,0),
  c("C15T37 MANUF","C15T37","Manufacturing","manufacturing",0,0,1),
  c("C15T16 FOD","C15T16","Food _ products","food products ",1,1,0),
  c("C17T19 TEX","C17T19","Textile &_ apparel","textile and apparel",1,1,0),
  c("C20T22 WPP","C20T22","Wood &_ paper","wood and paper",0,1,0),
  c("C20 WOD","C20","Wood","wood",1,0,0),
  c("C21T22 PAP","C21T22","Pulp &_ paper","pulp and paper",1,0,0),
  c("C23T26 CNM","C23T26","Chemicals &_ minerals","chemicals and minerals",0,1,0),
  c("C23 PET","C23","Coke &_ petroleum","coke and petroleum",1,0,0),
  c("C24 CHM","C24","Chemical _ products","chemical products",1,0,0),
  c("C25 RBP","C25","Rubber &_ plastics","rubber and plastics",1,0,0),
  c("C26 NMM","C26","Non-metallic _ mineral","non-metallic mineral",1,0,0),
  c("C27T28 MFM","C27T28","Basic &_ fabricated metals","basic and febricated metal",0,1,0),
  c("C27 MET","C27","Basic _ metal","basic metal",1,0,0),
  c("C28 FBM","C28","Fabricated _ metal","fabricated metal",1,0,0),
  c("C29T30 OTH","C29T30","Other &_ office machinery","Other and office machinery",0,0,1),
  c("C29 MEQ","C29","Other _ machinery","other machinery",1,1,0),
  c("C30T33 ICT","C30T33","Electrical _ equipment","electrical equipment",0,0,1),
  c("C30 ITQ","C30","Office _ machinery","office machinery",1,0,0),
  c("C31 ELQ","C31","Electrical _ machinery","electrical machinery",1,0,0),
  c("C32 CMQ","C32","Communication _ equipment","communication equipment",1,0,0),
  c("C33 SCQ","C33","Medical &_ precision","medical and precision equipment",1,0,0),
  c("C34T35 MTQ","C34T35","Transport _ equipment","transport equipment",0,1,0),
  c("C34 MTR","C34","Motor vehicle &_ trailers","motor vehicle and trailer",1,0,0),
  c("C35 TRQ","C35","Other transport _ equipment","other transport equipment",1,0,0),
  c("C36T37 OTM","C36T37","Other _ manufactures","other manufactures",1,1,0),
  c("C40T41 EGW","C40T41","Utilities","utilities",1,1,0),
  c("C45 CON","C45","Construction","construction",1,1,0),
  c("C50T99 SERV","C50T99","Total _ services","total services",0,0,1),
  c("C50T74 MKSERV","C50T74","Business sector _ services","business sector services",0,0,1),
  c("C50T55 THR","C50T55","Trade &_ restaurants","trade and restaurants",0,1,0),
  c("C50T52 WRT","C50T52","Wholesale &_ retail","wholesale and retail",1,0,0),
  c("C55 HTR","C55","Hotels &_ restaurants","hotels and restaurants",1,0,0),
  c("C60T64 TRT","C60T64","Transport &_ telecoms","transport and telecommunications",0,1,0),
  c("C60T63 TRN","C60T63","Transport &_ storage","transport and storage",1,0,0),
  c("C64 PTL","C64","Post &_ telecom","post and telecom",1,0,0),
  c("C65T67 FIN","C65T67","Finance &_ insurance","finance and insurance",1,1,0),
  c("C70T74 BZS","C70T74","Business _ services","business services",0,1,0),
  c("C70 REA","C70","Real _estate","real estate",1,0,0),
  c("C71 RMQ","C71","Renting","renting",1,0,0),
  c("C72 ITS","C72","Computer _ services","computer services",1,0,0),
  c("C73 RDS","C73","Research &_ development","research and development",1,0,0),
  c("C74 BZS","C74","Other _ business","other business",1,0,0),
  c("C75T95 OTS","C75T95","Other services","other services",0,1,0),
  c("C75 GOV","C75","Public _ administration","public administration",1,0,0),
  c("C80 EDU","C80","Education","education",1,0,0),
  c("C85 HTH","C85","Health & social","health and social",1,0,0),
  c("C90T93 OTS","C90T93","Community &_ personal","community and personal",1,0,0),
  c("C95 PVH","C95","Private _ households","private households",1,0,0)
  )
names(ui.icioFddva.seclabel) <- c("ind", "code", "industry", "indlabel", "ind.icio37", "ind.icio18", "ind.icioagg")
##
## order and number of items matters - compare with ui
ui.icioFddva.reglabel <- rbind.data.frame(
  c("AUS","Australia","Australia","Australian",1,0,0,0,0,0,1),
  c("AUT","Austria","Austria","Austrian",1,1,1,0,0,0,1),
  c("BEL","Belgium","Belgium","Belgian",1,1,1,0,0,0,1),
  c("CAN","Canada","Canada","Canadian",1,0,0,1,0,0,1),
  c("CHL","Chile","Chile","Chilean",1,0,0,0,0,0,1),
  c("CZE","Czech Republic","the Czech Republic","Czech",1,0,1,0,0,0,1),
  c("DNK","Denmark","Denmark","Danish",1,1,1,0,0,0,1),
  c("EST","Estonia","Estonia","Estonian",1,0,1,0,0,0,1),
  c("FIN","Finland","Finland","Finnish",1,1,1,0,0,0,1),
  c("FRA","France","France","French",1,1,1,0,0,0,1),
  c("DEU","Germany","Germany","German",1,1,1,0,0,0,1),
  c("GRC","Greece","Greece","Greek",1,1,1,0,0,0,1),
  c("HUN","Hungary","Hungary","Hungarian",1,0,1,0,0,0,1),
  c("ISL","Iceland","Iceland","Icelandish",1,0,0,0,0,0,1),
  c("IRL","Ireland","Ireland","Irish",1,1,1,0,0,0,1),
  c("ISR","Israel","Israel","Israeli",1,0,0,0,0,0,1),
  c("ITA","Italy","Italy","Italian",1,1,1,0,0,0,1),
  c("JPN","Japan","Japan","Japanese",1,0,0,0,1,0,1),
  c("KOR","Korea","Korea","Korean",1,0,0,0,1,0,1),
  c("LUX","Luxembourg","Luxembourg","Luxembourgish",1,1,1,0,0,0,1),
  c("MEX","Mexico","Mexico","Mexican",1,0,0,1,0,0,1),
  c("NLD","Netherlands","the Netherlands","Dutch",1,1,1,0,0,0,1),
  c("NZL","New Zealand","New Zealand","New Zealand's",1,0,0,0,0,0,1),
  c("NOR","Norway","Norway","Norwegian",1,0,0,0,0,0,1),
  c("POL","Poland","Poland","Polish",1,0,1,0,0,0,1),
  c("PRT","Portugal","Portugal","Portuguese",1,1,1,0,0,0,1),
  c("SVK","Slovak Republic","the Slovak Republic","Slovak",1,0,1,0,0,0,1),
  c("SVN","Slovenia","Slovenia","Slovenian",1,0,1,0,0,0,1),
  c("ESP","Spain","Spain","Spainish",1,1,1,0,0,0,1),
  c("SWE","Sweden","Sweden","Swedish",1,1,1,0,0,0,1),
  c("CHE","Switzerland","Switzerland","Swiss",1,0,0,0,0,0,1),
  c("TUR","Turkey","Turkey","Turkish",1,0,0,0,0,0,1),
  c("GBR","United Kingdom","the United Kingdom","UK",1,1,1,0,0,0,1),
  c("USA","United States","the United States","US",1,0,0,1,0,0,1),
  c("ARG","Argentina","Argentina","Argentinian",1,0,0,0,0,0,0),
  c("BRA","Brazil","Brazil","Brazilian",1,0,0,0,0,0,0),
  c("CHN","China","China","Chinese",1,0,0,0,1,0,0),
  c("TWN","Chinese Taipei","Chinese Taipei","Chinese Taipei",1,0,0,0,1,0,0),
  c("IND","India","India","Indian",1,0,0,0,0,0,0),
  c("IDN","Indonesia","Indonesia","Indonesian",1,0,0,0,0,1,0),
  c("RUS","Russian Fed.","the Russian Federation","Russian",1,0,0,0,0,0,0),
  c("SGP","Singapore","Singapore","Singaporean",1,0,0,0,0,1,0),
  c("ZAF","South Africa","South Africa","South African",1,0,0,0,0,0,0),
  c("HKG","Hong Kong","Hong Kong (SAR China)","Hong Kong",1,0,0,0,1,0,0),
  c("MYS","Malaysia","Malaysia","Malaysian",1,0,0,0,0,1,0),
  c("PHL","Philippines","Philippines","Filipino",1,0,0,0,0,1,0),
  c("THA","Thailand","Thailand","Thai",1,0,0,0,0,1,0),
  c("ROU","Romania","Romania","Romanian",1,0,1,0,0,0,0),
  c("VNM","Viet Nam","Viet Nam","Vietnamese",1,0,0,0,0,1,0),
  c("SAU","Saudi Arabia","Saudi Arabia","Saudi Arabian",1,0,0,0,0,0,0),
  c("BRN","Brunei Darussalam","Brunei Darussalam","Bruneian",1,0,0,0,0,1,0),
  c("BGR","Bulgaria","Bulgaria","Bulgarian",1,0,1,0,0,0,0),
  c("CYP","Cyprus","Cyprus","Cypriot",1,0,1,0,0,0,0),
  c("LVA","Latvia","Latvia","Latvian",1,0,1,0,0,0,0),
  c("LTU","Lithuania","Lithuania","Lithuanian",1,0,1,0,0,0,0),
  c("MLT","Malta","Malta","Maltese",1,0,1,0,0,0,0),
  c("KHM","Cambodia","Cambodia","Cambodian",1,0,0,0,0,1,0),
  c("ROW","Rest_of the_World","the Rest of the World","Rest of the World's",1,0,0,0,0,0,0),
  c("EU15","EU 15","EU 15","EU 15",0,0,0,0,0,0,0),
  c("EU27","EU 27","EU 27","EU 27",0,0,0,0,0,0,0),
  c("NAFTA","NAFTA","NAFTA","NAFTA",0,0,0,0,0,0,0),
  c("E_ASIA","East Asia","East Asia","East Asia",0,0,0,0,0,0,0),
  c("ASEAN","ASEAN","ASEAN","ASEAN",0,0,0,0,0,0,0),
  c("OECD","Total OECD","Total OECD","Total OECD",0,0,0,0,0,0,0),
  c("WOR","Total World","Total World","Total World",0,0,0,0,0,0,0)
  )
names(ui.icioFddva.reglabel) <- c("cou","country", "coulabel", "coupron", "reg.WOR", "reg.EU15", "reg.EU27", "reg.NAFTA", "reg.E_ASIA", "reg.ASEAN", "reg.OECD")
##
ui.icioFddva.region <- sub("reg.", "", names(ui.icioFddva.reglabel)[substr(names(ui.icioFddva.reglabel), 1, 4)=="reg."])
##
## create character vectors "EU15", "EU27" etc. with member country codes
ui.icioFddva.namereg <- ui.icioFddva.reglabel$cou[ui.icioFddva.reglabel$reg.WOR==1]
for (reg in ui.icioFddva.region) # c("WOR", "EU27", "NAFTA", "E_ASIA", "ASEAN", "OECD")
{
  eval(parse(text = paste0(reg, ' <- ui.icioFddva.reglabel$cou[ui.icioFddva.reglabel$reg.', reg, '==1]')))
}
##
ui.icioFddva.namereg.list <- NULL
for (j in seq(along=ui.icioFddva.namereg))
{
  ui.icioFddva.namereg.list <- c(ui.icioFddva.namereg.list, list(j))
  names(ui.icioFddva.namereg.list)[j] <- as.character(ui.icioFddva.namereg[j])
}
##
##
ui.icioFddva.namereg.agg <- list(EU15 = match(EU15, ui.icioFddva.namereg),
                                 EU27 = match(EU27, ui.icioFddva.namereg),
                                 NAFTA = match(NAFTA, ui.icioFddva.namereg),
                                 E_ASIA = match(E_ASIA, ui.icioFddva.namereg),
                                 ASEAN = match(ASEAN, ui.icioFddva.namereg),
                                 OECD = match(OECD, ui.icioFddva.namereg),
                                 WOR = match(WOR, ui.icioFddva.namereg))
##
ui.icioFddva.namereg.agg <- c(ui.icioFddva.namereg.list, ui.icioFddva.namereg.agg)
##
ui.icioFddva.rownames <- merge(ui.icioFddva.namesec, ui.icioFddva.namereg)
ui.icioFddva.rownames<- paste(ui.icioFddva.rownames[,2], ui.icioFddva.rownames[,1])
ui.icioFddva.colnames <- ui.icioFddva.namereg
ui.icioFddva.dimnames=list(ui.icioFddva.rownames, ui.icioFddva.colnames)
##
dat <- isolate(values[["ICIO5837APP"]])
##
ui.icioFddva.nocou <- dim(dat$DATA.ICIO5837GRTR)[2]
ui.icioFddva.noind <- dim(dat$DATA.ICIO5837GRTR)[1] / ui.icioFddva.nocou
##
ui.icioFddva.dim_conv <- list(row=c(ui.icioFddva.nocou, ui.icioFddva.noind), col=c(ui.icioFddva.nocou))

## ########################
## icioFddva ui script
## ########################

ui.icioFddva.dimS = c("Industry" = "ind", "Country" = "cou")
ui.icioFddva.sortdata = c("ICIO industry order" = "indicio", "Decending value" = "desc", "Ascending value" = "asc")

output$ui_icioFddva <- renderUI({

  ## doLogin()
  ## if (loginData$LoggedIn) {

    list(

      conditionalPanel(condition = "input.tabs_icioFddva == 'Plots' | input.tabs_icioFddva == 'Maps'",
                       wellPanel(
                         checkboxInput("fddva_viz_plot_controls", "Plot options", FALSE),
                         conditionalPanel(condition = "input.fddva_viz_plot_controls == true",
                                          ## htmlOutput("ui_plot_options"),
                                          sliderInput(inputId = "icioFddva_viz_plot_width", label = "Width:", min = 900, max = 2400, value = 900, step = 50),
                                          conditionalPanel(condition = "input.tabs_icioFddva == 'Plots'",
                                                           sliderInput(inputId = "icioFddva_viz_plot_height", label = "Height:", min = 400, max = 1200, value = 600, step = 50)
                                                           ),
                                          conditionalPanel(condition = "input.tabs_icioFddva == 'Maps'",
                                                           ## sliderInput(inputId = "fddva_viz_plot_ncut", label = "Number of colours:", min = 1, max = 20, value = 10, step = 1)
                                                           numericInput(inputId = "fddva_viz_plot_ncut", label = "Number of colours:", 5)
                                                           )
                                          )
                         )
                       ),
      wellPanel(
        selectInput("fddva_coef", "Coefficients:",
                    ui.icioFddva.coef$coef
                    ## list("eB: Employment" = "eB", "vB: Value-added" = "vB")
                    ),
        selectInput("fddva_time", "Year:",
                    list(
                      "2009" = 5,
                      "2008" = 4,
                      "2005" = 3,
                      "2000" = 2,
                      "1995" = 1
                      )
                    ),
        ## conditionalPanel(condition="input.datatabs==3 | input.datatabs==4 | input.datatabs==5 | input.datatabs==6" ,
        selectInput("fddva_demand", "Demand concept: (demand)",
                    list(
                      "GRTR: Gross Trade" = "GRTR",
                      "FD: Final Demand" = "FDTTLWITHDISC",
                      "GFCF: Gross Fixed Capital Formation" = "GFCF",
                      "HHCP: Household Consumption" = "HHCP"
                      ),
                    selected = "FDTTLWITHDISC"
                    )
        ## )
        ,

        wellPanel(
          h5("Demand data"),
          selectInput("fddva_couX", "Export Country: (couX)", ui.icioFddva.cou,
                      selected = 62), # 62: East Asia; 65: WOR: Total World"
          selectInput("fddva_indX", "Demand or Final Expenditure Industry: (indX)", ui.icioFddva.ind,
                      selected = 320), # 39: C15T37 MANUF; 41: "C30T33 ICT"
          selectInput("fddva_couD", "Demand Country: (couD)", ui.icioFddva.cou,
                      selected = 60),
          checkboxInput("fddva_conv1", "Plot dimension selection", FALSE),
          conditionalPanel(condition = "input.tabs_icioFddva == 'Plots'",
                           conditionalPanel(condition = "input.fddva_dimS == 'ind'",
                                            checkboxInput("fddva_noindX", "Remove 'indX' from chart", FALSE)
                                            ),
                           conditionalPanel(condition = "input.fddva_dimS == 'cou'",
                                            checkboxInput("fddva_nocouX", "Remove 'couX' from chart", FALSE)
                                            )
                           )
          ), # EU27

        conditionalPanel(condition = "input.tabs_icioFddva == 'Plots' | input.tabs_icioFddva == 'Tables'",
                         wellPanel(
                           h5("Aggregate results"),
                           radioButtons(inputId = "fddva_dimS", label = "Data by Source Dimension:", fddva_dimS,
                                        ## selected = state_init_list("fddva_dimS","ind", fddva_dimS)),
                                        selected = ui.icioFddva.dimS[[1]],
                           conditionalPanel(condition = "input.fddva_dimS == 'ind'",
                                            selectInput("fddva_couS", "Source Country: (couS)", ui.icioFddva.cou,
                                                        selected = 64) # "Total OECD"
                                            )
                           ,
                           conditionalPanel(condition = "input.fddva_dimS == 'cou'",
                                            selectInput("fddva_indS", "Source Industry: (indS)", ui.icioFddva.ind,
                                                        selected = 137) # "C01T99 TOT"
                                            )
                           )
                         )
        ,

        wellPanel(
          h5("Result options"),
          checkboxInput("fddva_calcshare", "Calculate share in total", FALSE),
          numericInput("fddva_rounddec", "Round to number of decimals:", 4),
          numericInput("fddva_topN", "Display first N values in legend:", 5),
          selectInput("fddva_sortdata", "Order or sorting (legend, table):", fddva_sortdata,
                      ## selected = state_init_list("fddva_sortdata", "", ui.icioFddva.sortdata),
                      selected = ui.icioFddva.sortdata[[1]],
                      multiple = FALSE),
          conditionalPanel(condition = "input.tabs_icioFddva == 'Tables'",
                           ## conditionalPanel(condition="input.datatabs==4" ,
                           checkboxInput("fddva_aggindS", "Add ICIO 18 industry aggregates", FALSE)
                           )
          )
        ## ,
        ## helpText("Download Data:"),
        ## downloadButton("fddva_downloadData", "")
        ## , h6("Explanation Flow Diagram", a("Source-Export-Demand",
        ##                                    href="https://www.dropbox.com/s/duzwe197fr844h9/flow_diagram.png",
        ##                                    ## href="https://www.dropbox.com/s/mj7kv7254d2pwjt/flow_diagram.pdf",
        ##                                    target="_blank"))

        ),
      helpAndReport("Foreign Demand Domestic Value Added","icioFddva",inclMD("tools/help/icioFddva.md"))
      ) # list(...

  ## } else
  ##   {
  ##     h3("Please log in")
  ##   }

})

icioFddva_widthSize <- reactive({
    ifelse(is.null(input$icioFddva_viz_plot_width), return(values$plotWidth), return(input$icioFddva_viz_plot_width))
})
icioFddva_heightSize <- reactive({
    ifelse(is.null(input$icioFddva_viz_plot_height), return(values$plotHeight), return(input$icioFddva_viz_plot_height))
})

output$icioFddva <- renderUI({
  ## for input-output
  statTabPanel(menu_name = "ICIO", # menu_name: for side bar - coincide with navbarMenu
               fun_name = "Foreign Demand Domestic Value Added",           # fun_name
               rfun_label = ".icioFddva",         # rfun_label
               fun_label = "icioFddva"           # fun_label
               ,fun_tabs = c("Plots", "Tables", "Maps")
               ,widthFun = "icioFddva_widthSize"
               ,heightFun = "icioFddva_heightSize"
               )
})

## ########################
## old server script
## ########################

## input <- list(fddva_aggindS=FALSE,
##               fddva_calcshare=TRUE,
##               fddva_coef="eB",
##               fddva_couS=64,
##               fddva_couD=60,
##               fddva_couX=65,
##               fddva_datasets="ICIO5837APP",
##               fddva_datatabs=3,
##               fddva_demand="GRTR",
##               fddva_dimS="ind",
##               fddva_indS=137,
##               fddva_indX=320,
##               nav_radiant="Icio",
##               fddva_nocouX=FALSE,
##               fddva_noindX=FALSE,
##               fddva_rounddec=4,
##               fddva_sortdata="desc",
##               fddva_time=4,
##               fddva_topN=5
##               )
## input
## fddva_coef = input$fddva_coef
## fddva_time = input$fddva_time
## fddva_demand = input$fddva_demand
## fddva_indX = input$fddva_indX
## fddva_dimS = input$fddva_dimS
## fddva_indS = input$fddva_indS
## fddva_couS = input$fddva_couS
## fddva_couX = input$fddva_couX
## fddva_couD = input$fddva_couD
## fddva_couD = input$fddva_couD
## fddva_noindX = input$fddva_noindX
## fddva_calcshare = input$fddva_calcshare
## fddva_rounddec = input$fddva_rounddec
## fddva_sortdata = input$fddva_sortdata
## fddva_topN = input$fddva_topN
## fddva_conv1 = input$fddva_conv1

.icioFddva <- reactive({
  ## reactive that calls the function for main analysis
  ## . used to indicate this is an 'internal' function

  if (length(input$fddva_dimS) == 0) return ()

  icioFddva(fddva_coef = input$fddva_coef,
            ## year = ui.icioFddva.year[as.numeric(input$time)],
            fddva_time = input$fddva_time,
            fddva_demand = input$fddva_demand,
            ## indX = ui.icioFddva.namesec.agg[as.numeric(input$indX)],
            fddva_indX = input$fddva_indX,
            fddva_dimS = input$fddva_dimS,
            ## indS = ui.icioFddva.namesec.agg[as.numeric(input$indS)],
            fddva_indS = input$fddva_indS,
            ## couS = names(ui.icioFddva.namereg.agg)[as.numeric(input$couS)],
            fddva_couS = input$fddva_couS,
            ## couX = names(ui.icioFddva.namereg.agg)[as.numeric(input$couX)],
            fddva_couX = input$fddva_couX,
            ## couD = names(ui.icioFddva.namereg.agg)[as.numeric(input$couD)]
            fddva_couD = input$fddva_couD,
            fddva_noindX = input$fddva_noindX,
            fddva_nocouX = input$fddva_nocouX,
            fddva_calcshare = input$fddva_calcshare,
            fddva_rounddec = input$fddva_rounddec,
            fddva_sortdata = input$fddva_sortdata,
            fddva_topN = input$fddva_topN,
            fddva_conv1 = input$fddva_conv1,
            fddva_aggindS = input$fddva_aggindS,
            fddva_viz_plot_ncut = input$fddva_viz_plot_ncut,
            icioFddva_viz_plot_height = input$icioFddva_viz_plot_height,
            icioFddva_viz_plot_width = input$icioFddva_viz_plot_width
            ## fddva_map = input$fddva_map
            )
})

## isolate(.icioFddva())

observe({
  if(is.null(input$icioFddvaReport) || input$icioFddvaReport == 0) return()
  isolate({
    inp <- list(
      input$datasets,

      input$fddva_coef,
      ui.icioFddva.year[as.numeric(input$fddva_time)],
      input$fddva_demand,
      ui.icioFddva.namesec.agg[as.numeric(input$fddva_indX)],
      ## indS = ui.icioFddva.namesec.agg[as.numeric(input$indS)],
      names(ui.icioFddva.namereg.agg)[as.numeric(input$fddva_couS)],
      names(ui.icioFddva.namereg.agg)[as.numeric(input$fddva_couX)],
      names(ui.icioFddva.namereg.agg)[as.numeric(input$fddva_couD)]
      )

    updateReport(inp,"icioFddva")
  })
})

icioFddva <- function(fddva_coef = fddva_coef,
                      fddva_time = fddva_time,
                      fddva_demand = fddva_demand,
                      fddva_indX = fddva_indX,
                      fddva_dimS = fddva_dimS,
                      fddva_indS = fddva_indS,
                      fddva_couS = fddva_couS,
                      fddva_couX = fddva_couX,
                      fddva_couD = fddva_couD,
                      fddva_nocouX = fddva_nocouX,
                      fddva_noindX = fddva_noindX,
                      fddva_calcshare = fddva_calcshare,
                      fddva_rounddec = fddva_rounddec,
                      fddva_sortdata = fddva_sortdata,
                      fddva_topN = fddva_topN,
                      fddva_conv1 = fddva_conv1,
                      fddva_aggindS = fddva_aggindS,
                      fddva_viz_plot_ncut = fddva_viz_plot_ncut,
                      icioFddva_viz_plot_height = icioFddva_viz_plot_height,
                      icioFddva_viz_plot_width = icioFddva_viz_plot_width
                      ## fddva_map = fddva_map
                      )
{

    ## Part I: Text strings
  ## Chart title
  title <- list(coef = fddva_coef,
                year = ui.icioFddva.year[as.numeric(fddva_time)],
                ## time = input$time,
                demand = fddva_demand,
                ## indX = ui.icioFddva.namesec.agg[as.numeric(fddva_indX)],
                indX = names(ui.icioFddva.ind)[match(as.numeric(fddva_indX), unlist(ui.icioFddva.ind))],
                ## indX = input$indX,
                ## fddva_dimS = fddva_dimS,
                ## indS = ui.icioFddva.namesec.agg[as.numeric(fddva_indS)],
                indS = names(ui.icioFddva.ind)[match(as.numeric(fddva_indS), unlist(ui.icioFddva.ind))],
                ## indS = input$indS,
                couS = names(ui.icioFddva.namereg.agg)[as.numeric(fddva_couS)],
                ## couS = input$couS,
                couX = names(ui.icioFddva.namereg.agg)[as.numeric(fddva_couX)],
                ## couX = input$couX,
                couD = names(ui.icioFddva.namereg.agg)[as.numeric(fddva_couD)]
                ## couD = input$couD
                )
  if (fddva_dimS == "ind")
    {
      title <- title[!names(title)%in%c("indS")]
    } else if (fddva_dimS == "cou")
      {
        title <- title[!names(title)%in%c("couS")]
      }
  title.string <- NULL
  for (i in seq(along=title))
    {
      if (is.null(title.string))
        {
          title.string <- paste(toString(names(title[i])), toString(title[[i]]), sep = ": ")
        }
      else
        {
          title.string <- paste(title.string, paste(toString(names(title[i])), toString(title[[i]]), sep = ": "), sep = ", ")
        }
    }
  if (fddva_calcshare==TRUE) unit.string <- "share in total" else unit.string <- "in USD"
  title.string <- paste(title.string, unit.string, sep = ", ")
  ## Result interpretation
  blurb <- list(
    coef = ui.icioFddva.coef$label[ui.icioFddva.coef$coef==title$coef],
    year = title$year,
    demand = ui.icioFddva.demand$label[ui.icioFddva.demand$demand==title$demand],
    indX = paste0(ui.icioFddva.seclabel$indlabel[ui.icioFddva.seclabel$ind==title$indX],
      " industries (", ui.icioFddva.seclabel$code[ui.icioFddva.seclabel$ind==title$indX], ")"),
    couX1 = ui.icioFddva.reglabel$coupron[ui.icioFddva.reglabel$cou==title$couX],
    couX2 = ui.icioFddva.reglabel$country[ui.icioFddva.reglabel$cou==title$couX],
    couD = ui.icioFddva.reglabel$coupron[ui.icioFddva.reglabel$cou==title$couD]
    )
  if (fddva_dimS == "ind")
    {
      blurb <- c(blurb, couS = as.character(ui.icioFddva.reglabel$coupron[ui.icioFddva.reglabel$cou==title$couS]))
    } else if (fddva_dimS == "cou")
      {
        blurb <- c(blurb, indS = as.character(paste0(ui.icioFddva.seclabel$indlabel[ui.icioFddva.seclabel$ind==title$indS]," industries (", ui.icioFddva.seclabel$code[ui.icioFddva.seclabel$ind==title$indS], ")")))
      }
  if (fddva_dimS == "ind")
    {
      blurb.string <- paste0(
        as.character(blurb$couS), ' ', tolower(blurb$coef), ', in source industry, \n', # \t\t\t\t <coef>, in source industry\n',
        'generated by ', blurb$couD, ' ', tolower(blurb$demand), ' of \n',
        blurb$couX1, ' exports of ', blurb$indX, ', \n',
        blurb$year, '.'
        )
      blurb.string.template <- paste0('<couS><coef>, in source industry,\n',
                                      'generated by <couD><demand> of \n',
                                      '<couX> exports of <indX>, \n',
                                      '<year>.')
      ## blurb.string <- paste0(blurb.string, '\n\n', blurb.string.template)
    } else if (fddva_dimS == "cou")
      {
        blurb.string <- paste0(
          blurb$coef, ' in ', blurb$indS, ' within each country,\n',
          'supported by ', blurb$couD, ' ', tolower(blurb$demand), ' of\n',
          blurb$couX1, ' exports of ', blurb$indX, ',\n',
          blurb$year, '\n',
          '(includes ', tolower(blurb$coef), ' in ', blurb$couX2, ').')
        blurb.string.template <- paste0('<coef> in <indS>, within each country,\n',
                                        'supported by <couD><demand> of\n',
                                        '<couX> exports of <indX>,\n',
                                        '<year>\n',
                                        '(includes <coef> in <couX>).')
      }
  blurb.string <- paste0(blurb.string, '\n\n', blurb.string.template)
  ## Country labels for region selection
  string.label.couX <- NULL
  string.label.couD <- NULL
  string.label.couS <- NULL
  ##
  if (title$couX%in%ui.icioFddva.region)
    {
      label.couX <- isolate(eval(parse(text = title$couX)))
      string.label.couX <- paste0('couX: ', title$couX, '\n\t', toString(sort(label.couX)), '\n')
    }
  if (title$couD%in%ui.icioFddva.region)
    {
      label.couD <- isolate(eval(parse(text = title$couD)))
      string.label.couD <- paste0('couD: ', title$couD, '\n\t', toString(sort(label.couD)), '\n')
    }
  if (fddva_dimS == "ind")
    {
      if (title$couS%in%ui.icioFddva.region)
        {
          label.couS <- isolate(eval(parse(text = title$couS)))
          string.label.couS <- paste0('couS: ', title$couS, '\n\t', toString(sort(label.couS)), '\n')
        }
    }
  label_cou <- paste0(string.label.couX,
                      string.label.couD,
                      string.label.couS)
  ## Part II: Calculation
  ## Data selection

  dat <- values[["ICIO5837APP"]]
  ## dat <- isolate(values[["ICIO5837APP"]])

  ## Coefficient matrix
  if (fddva_coef=="eB")
    {
      xB <- array(dat$DATA.ICIO5837EB[,,as.numeric(fddva_time)],
                  dim = dim(dat$DATA.ICIO5837EB[,,as.numeric(fddva_time)])[1:2])
    } else if (fddva_coef=="vB")
      {
        xB <- array(dat$DATA.ICIO5837VB[,,as.numeric(fddva_time)],
                    dim = dim(dat$DATA.ICIO5837VB[,,as.numeric(fddva_time)])[1:2])
      }
  ## Demand matrix
  data.demand <- eval(parse(text = paste0('array(dat$DATA.ICIO5837', fddva_demand, '[,,as.numeric(', fddva_time, ')], dim = dim(dat$DATA.ICIO5837', fddva_demand, '[,,as.numeric(', fddva_time, ')])[1:2])')))
  ## Binary selection matrix
  if (as.numeric(fddva_couX) <= ui.icioFddva.nocou) agg.couX <- as.numeric(fddva_couX) else if (as.numeric(fddva_couX) > ui.icioFddva.nocou) agg.couX <- ui.icioFddva.namereg.agg[[as.numeric(fddva_couX)]]
  if (as.numeric(fddva_couD) <= ui.icioFddva.nocou) agg.couD <- as.numeric(fddva_couD) else if (as.numeric(fddva_couD) > ui.icioFddva.nocou) agg.couD <- ui.icioFddva.namereg.agg[[as.numeric(fddva_couD)]]
  ##
  if (as.numeric(fddva_indX) <= ui.icioFddva.noind)
    {
      agg.indX <- c(as.numeric(fddva_indX), as.numeric(fddva_indX))
    } else if (as.numeric(fddva_indX) > ui.icioFddva.noind)
      {
        agg.indX <- ui.icioFddva.secagg[ui.icioFddva.secagg$id==fddva_indX,3:4]
        agg.indX <- c(agg.indX[[1]] : agg.indX[[2]])
      }
  data.conv1 <- convCreate(dim = ui.icioFddva.dim_conv,
                           agg.row1 = agg.couD,
                           agg.row2 = agg.indX,
                           agg.col1 = agg.couX,
                           horiz = TRUE,
                           ## horiz = FALSE, # originally used for Final Demand / vertical layout
                           dimnames = ui.icioFddva.dimnames)
  ## Calculation
  data.couX.indX <- data.conv1 * data.demand
  aaa <- xB %*% data.couX.indX
  aaa <- apply(aaa, 1, sum)
  ## Results by source industry
  ## if (fddva_dimS == "ind")
  ##   {
  conv.cou <- array(0, dim = c(ui.icioFddva.noind, ui.icioFddva.nocou * ui.icioFddva.noind)) # 37 x 2146
  for (i in c(1:ui.icioFddva.noind))
    {
      if (as.numeric(fddva_couS) <= ui.icioFddva.nocou)
        {
          conv.cou[i, (as.numeric(fddva_couS)-1)*ui.icioFddva.noind + i] <- 1
        } else if (as.numeric(fddva_couS) >= 59)
          {
            ## agg.ind <- ui.icioFddva.secagg[ui.icioFddva.secagg$id==fddva_indS,3:4]
            agg.couS <- ui.icioFddva.namereg.agg[[as.numeric(fddva_couS)]]
            for (j in seq(along=agg.couS))
              {
                conv.cou[i, (agg.couS[j]-1)*ui.icioFddva.noind + i] <- 1
              }
          }
    }
  ## data.by.dimS <- conv.cou %*% aaa
  ## if (fddva_calcshare==TRUE) data.by.dimS <- data.by.dimS / sum(data.by.dimS)
  data.by.ind <- conv.cou %*% aaa
  if (fddva_calcshare==TRUE) data.by.ind <- data.by.ind / sum(data.by.ind)
  ## } else if (fddva_dimS == "cou")
  ##   {
  conv.ind <- array(0, dim = c(ui.icioFddva.nocou, ui.icioFddva.nocou * ui.icioFddva.noind)) # 58 x 2146
  for (j in c(1:ui.icioFddva.nocou))
    {
      if (as.numeric(fddva_indS) <= ui.icioFddva.noind)
        {
          conv.ind[j, (j-1)*ui.icioFddva.noind + as.numeric(fddva_indS)] <- 1
        } else if (as.numeric(fddva_indS) > ui.icioFddva.noind)
          {
            agg.ind <- ui.icioFddva.secagg[ui.icioFddva.secagg$id==fddva_indS,3:4]
            conv.ind[j, ((j-1)*ui.icioFddva.noind + as.numeric(agg.ind[1])) : ((j-1)*ui.icioFddva.noind + as.numeric(agg.ind[2]))] <- 1
          }
    }
  ## structure: [0 0 1 1 1 0 : 0 0 0 0 0 0 : 0 0 0 0 0 0 : ... ]
  ##            [0 0 0 0 0 0 : 0 0 1 1 1 0 : 0 0 0 0 0 0 : ... ]
  ##            [0 0 0 0 0 0 : 0 0 0 0 0 0 : 0 0 1 1 1 0 : ... ]
  ## data.by.dimS <- conv.ind %*% aaa
  ## if (fddva_calcshare==TRUE) data.by.dimS <- data.by.dimS / sum(data.by.dimS)
  data.by.cou <- conv.ind %*% aaa
  if (fddva_calcshare==TRUE) data.by.cou <- data.by.cou / sum(data.by.cou)
  ## }

  return(list(unit.string = unit.string,
              title.string = title.string,
              title = title,
              blurb.string = blurb.string,
              label_cou = label_cou,
              data.conv1 = data.conv1,
              ## data.by.dimS = data.by.dimS,
              data.by.ind = data.by.ind,
              data.by.cou = data.by.cou,
              ## data.plot = data.plot,
              ## data.sort = data.sort,
              ## names = names,
              fddva_conv1 = fddva_conv1,
              ## fddva_map = fddva_map,
              ## rChartMap = rChartMap,
              ## data.plot = data.by.dimS,
              ## fddva_calcshare = fddva_calcshare,
              fddva_dimS = fddva_dimS,
              fddva_noindX = fddva_noindX,
              fddva_nocouX = fddva_nocouX,
              fddva_sortdata = fddva_sortdata,
              fddva_topN = fddva_topN,
              fddva_rounddec = fddva_rounddec,
              fddva_aggindS = fddva_aggindS,
              fddva_indX = fddva_indX,
              fddva_couX = fddva_couX,
              fddva_viz_plot_ncut = fddva_viz_plot_ncut,
              icioFddva_viz_plot_height = icioFddva_viz_plot_height,
              icioFddva_viz_plot_width = icioFddva_viz_plot_width
              )
         )
}

summary_icioFddva <- function(result = .icioFddva())
{ if (length(result) > 0) {

    label_cou <- result$label_cou
    blurb.string <- result$blurb.string
  ## cat("Title:\n", result$title.string)

  blurb <- paste0(label_cou)
  if (result$fddva_conv1 == FALSE)
    {
      blurb <- paste(blurb.string, blurb, sep = "\n\n")
    }
  cat(blurb)

}}

tables_icioFddva <- function(result = .icioFddva())
{ if (length(result) > 0) {

  fddva_dimS <- result$fddva_dimS
  ## data.by.dimS <- result$data.by.dimS
  data.by.cou <- result$data.by.cou
  data.by.ind <- result$data.by.ind
  fddva_rounddec <- result$fddva_rounddec
  fddva_sortdata <- result$fddva_sortdata
  fddva_aggindS <- result$fddva_aggindS
  title.string <- result$title.string
  ## fddva_calcshare <- result$fddva_calcshare


  if (fddva_dimS == "ind")
    {
      data <- data.by.ind
      data.table <- cbind.data.frame(Industry = ui.icioFddva.namesec,
                                     Value = data)
      if (fddva_sortdata=="desc")
        {
          data.table <- data.table[order(-data),]
        } else if (fddva_sortdata=="asc")
          {
            data.table <- data.table[order(data),]
          }
      ## if (fddva_aggindS==TRUE)
      ##   {
          data.ind18 <- data.by.ind
          data.ind18.1 <- cbind.data.frame(Industry = as.character(ui.icioFddva.aggind[,"ind.icio18"]),
                                           Value = data.ind18)
          data.ind18.2 <- aggregate(data.ind18.1[,"Value"],
                                    by = list(data.ind18.1$Industry),
                                    FUN = "sum")
          names(data.ind18.2) <- c("Industry", "Value")
      ## data.table <- rbind(data.ind18.2, data.table)
      ## show only 18 industries
      data.table <- data.ind18.2
      ## }

      data.sum <- cbind.data.frame(Industry = "Total", Value = sum(data)) # calculate Total

    } else if (fddva_dimS == "cou")
      {
        data <- data.by.cou
        data.table <- cbind.data.frame(Country = ui.icioFddva.namereg,
                                       Value = data)
        if (fddva_sortdata=="desc")
          {
            data.table <- data.table[order(-data),]
          } else if (fddva_sortdata=="asc")
            {
              data.table <- data.table[order(data),]
            }
        data.sum <- cbind.data.frame(Country = "Total", Value = sum(data))
      }

  data.table <- rbind(data.sum, data.table)
  fddva_rounddec <- 5
  data.table$Value <- as.character(round(data.table$Value, fddva_rounddec))

  ## if (fddva_calcshare==TRUE) title.string <- paste0(title.string, ', share in total') else title.string <- paste0(title.string, ', in USD')
  names(data.table) <- sub("Value", title.string, names(data.table))
  data.table

}}

plots_icioFddva <- function(result = .icioFddva())
{ if (length(result) > 0) {

    data.conv1 <- result$data.conv1

  if (result$fddva_conv1 == TRUE)
    {
      ## converter plot
      x <- data.conv1
      x <- x[, rev(seq_len(ncol(x)))]
      xLabels <- rownames(x)
      yLabels <- colnames(x)
      mid <- (ui.icioFddva.noind-1)/2
      posxLabels <- seq(from = mid+1, to = ui.icioFddva.noind*ui.icioFddva.nocou-mid, by = ui.icioFddva.noind)
      ## posxLabels <- seq(from = 1, to = 2146, by = 37)
      i <- image(1:length(xLabels), 1:length(yLabels), x,
                 col = c("white", "green"),
                 xlab = "Export Country + Demand or Final Expenditure Industry (couX + indX)",
                 ylab = "Demand Country (couD)",
                 axes = FALSE)
      axis(BOTTOM<-1, at=posxLabels, labels=rev(yLabels), las = VERTICAL<-2, cex.axis=0.7)
      axis(TOP<-3, at=posxLabels, labels=rev(yLabels), las = VERTICAL<-2, cex.axis=0.7)
      axis(LEFT<-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1, cex.axis=0.6)
      axis(RIGHT<-4, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1, cex.axis=0.6)
      return(i)
      ## }
    } else # default: plot column charts
      {
        fddva_dimS <- result$fddva_dimS
        ## data.by.dimS <- result$data.by.dimS
        data.by.cou <- result$data.by.cou
        data.by.ind <- result$data.by.ind
        ## data.plot <- data.by.dimS
        unit.string <- result$unit.string
        title.string <- result$title.string
        title <- result$title
        fddva_rounddec <- result$fddva_rounddec
        fddva_sortdata <- result$fddva_sortdata
        fddva_topN <- result$fddva_topN
        fddva_nocouX <- result$fddva_nocouX
        fddva_noindX <- result$fddva_noindX
        fddva_aggindS <- result$fddva_aggindS
        fddva_indX <- result$fddva_indX
        fddva_couX <- result$fddva_couX

        if (fddva_dimS == "ind")
          {
            ## data.plot <- data.by.ind
            ## data.plot <- cbind.data.frame(ui.icioFddva.namesec, data.by.ind)
            data.plot <- cbind.data.frame(Industry = as.character(ui.icioFddva.aggind[,"ind.icio18"]),
                                             Value = data.by.ind)
            data.plot <- aggregate(data.plot[,"Value"],
                                      by = list(data.plot$Industry),
                                      FUN = "sum")
            names(data.plot) <- c("names", "value")
            if (fddva_noindX==TRUE)
            {
                data.plot <- data.plot[!data.plot$names==title$indX,]
            }
            ##
            data.sort <- data.plot
            ## data.sort <- cbind.data.frame(ui.icioFddva.namesec, data.by.ind)
            if (fddva_sortdata=="desc")
              {
                ## data.sort <- data.sort[order(-data.by.ind),]
                data.sort <- data.sort[order(-data.plot$value),]
              } else if (fddva_sortdata=="asc")
                {
                  ## data.sort <- data.sort[order(data.by.ind),]
                  data.sort <- data.sort[order(data.plot$value),]
                }
            ## data.sort$data.by.ind <- round(data.sort$data.by.ind, fddva_rounddec)
            data.sort$value <- round(data.sort$value, fddva_rounddec)
            data.sort <- paste0(as.character(data.sort[1:fddva_topN,1]), ": ", data.sort[1:fddva_topN,2])

          } else if (fddva_dimS == "cou")
            {
              ## data.plot <- data.by.cou
              ## names <- ui.icioFddva.namereg
                data.plot <- cbind.data.frame(Country = as.character(ui.icioFddva.namereg),
                                              Value = data.by.cou)
              ## data.plot <- aggregate(data.plot[,"Value"],
              ##                        by = list(data.plot$Country),
              ##                        FUN = "sum")
              names(data.plot) <- c("names", "value")

              if (fddva_nocouX==TRUE)
                {
                  ## data.plot <- as.matrix(data.plot[-as.numeric(fddva_couX),])
                  ## names <- names[-as.numeric(fddva_couX)]
                  data.plot <- data.plot[-as.numeric(fddva_couX),]
                }
              ## legend values
              ## data.sort <- cbind.data.frame(ui.icioFddva.namereg, data.by.cou)
                data.sort <- data.plot
                if (fddva_sortdata=="desc")
                {
                  ## data.sort <- data.sort[order(-data.by.cou),]
                    data.sort <- data.sort[order(-data.plot$value),]
                } else if (fddva_sortdata=="asc")
                {
                    ## data.sort <- data.sort[order(data.by.cou),]
                    data.sort <- data.sort[order(data.plot$value),]
                }
              ## data.sort$data.by.cou <- round(data.sort$data.by.cou, fddva_rounddec)
              data.sort$value <- round(data.sort$value, fddva_rounddec)
              data.sort <- paste0(as.character(data.sort[1:fddva_topN,1]), ": ", data.sort[1:fddva_topN,2])
            }
        ##
        op <- suppressWarnings(par(oma=c(2,1,2,0) # bottom, left, top, right
                  ## ,mgp = c(0,3,2)
                  ,mgp = c(-1,1,0) # title, labels, line
                  )) # Room for the title and legend
        barplot(data.plot$value,
                beside = TRUE,
                names = data.plot$names, las = 2, cex.names = .8,
                ylab = unit.string,
                ## main = title.string,
                col = "#4F81BD")
        ##
        par(op) # Leave the last plot
        mtext(title.string, line = 2, font = 2, cex = 1.2)
        op <- par(usr=c(0,0.1,0,1.05), # Reset the coordinates
                  xpd=NA)         # Allow plotting outside the plot region
        legend(0, 1.1, legend=data.sort, cex=0.8, box.col = NA, horiz = TRUE)

      }

}}
## plots_icioFddva(result = isolate(.icioFddva()))

maps_icioFddva <- function(result = .icioFddva())
{ if (length(result) > 0) {

  icioFddva_viz_plot_height <- result$icioFddva_viz_plot_height
  icioFddva_viz_plot_width <- result$icioFddva_viz_plot_width
  fddva_viz_plot_ncut <- result$fddva_viz_plot_ncut
  data.by.cou <- result$data.by.cou
  data.plot <- data.frame(cou = ui.icioFddva.namereg,
                          value = round(data.by.cou, 0))
  data.plot <- data.plot[!data.plot$value==0,]
  ## d <- choropleth(value ~ cou, data = data.plot[1:9,], map = "world", pal = "PuRd")
  ## m1 <- ichoropleth(value ~ cou, data = data.plot, map = "world", ncuts = 9)
  m1 <- ichoropleth(value ~ cou, data = data.plot, map = "world", ncuts = fddva_viz_plot_ncut)
  m1$set(width = icioFddva_viz_plot_width,
         height = .5 * icioFddva_viz_plot_width,
         ## height = "100%",
         slider = TRUE
         )
  return(m1)
  ## library(rMaps)
  ## library(shiny)
  ## library(rCharts)
  ## renderChart2

}}

## saveTreatmentAssign <- function(result = .icioFddva()) {
## ## radiant.R
##     changedata(data.frame(as.factor(result$dat$treatment)),
##                "treatment") # addColName: return(values[[input$datasets]][,addColName] <- addCol)
## }

## observe({
##   ## rnd_save_treatment: actionButton
##     if(is.null(input$rnd_save_treatment) || input$rnd_save_treatment == 0) return()
##     isolate({
##         result <- .icioFddva()
##         if(is.character(result)) return()
##         saveTreatmentAssign(result)
##     })
## })
