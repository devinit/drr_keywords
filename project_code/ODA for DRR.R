required.packages <- c("data.table", "rstudioapi", "rvest")
lapply(required.packages, require, character.only = T)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

load_crs <- function(dataname="crs", path="project_data"){
  require("data.table")
  files.bz <- list.files(path, pattern=paste0(dataname, "_part.+[.]bz"))
  files.csv <- list.files(path, pattern=paste0(dataname, "_part.+[.]csv"))
  if(length(files.bz) > 0 & length(files.csv) > 0){
    files <- files.csv
    read.crs <- function(x){return(fread(x))}
  } else {
    if(length(files.bz) > 0){
      files <- files.bz
      read.crs <- function(x){return(read.csv(x))}
    } else {
      files <- files.csv
      read.crs <- function(x){return(fread(x))}
    }
  }
  crs <- list()
  for(i in 1:length(files)){
    print(paste0("Loading part ", i, " of ", length(files)))
    filepath <- paste0(path, "/", files[i])
    crs[[i]] <- read.crs(filepath)
  }
  crs <- rbindlist(crs)
  return(crs)
}

crs <- load_crs(path = "project_data")

keep <- c(
  "CrsID",
  "Year",
  "FlowName",
  "DonorName",
  "RecipientName",
  "USD_Disbursement_Defl",
  "PurposeName",
  "ProjectTitle",
  "ShortDescription",
  "LongDescription",
  "DRR"
)

crs <- crs[, ..keep]

crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  | 
    FlowName == "Private Development Finance"
  ]

crs <- crs[Year >= 2019]

major.keywords <- c(
  "anti-seismic adaption",
  "cbdrm",
  "climate protection",
  "climate resilience",
  "climate vulnerability",
  "coastal protection",
  "cyclone preparedness",
  "disaster management",
  "disaster reduction",
  "disaster resilience",
  "disaster risk mitigation",
  "disaster risk reduction",
  "disaster vulnerability reduction",
  "disaster risk management",
  "drm",
  "drr",
  "early warning",
  "earthquake-resistant",
  "earthquake resistant",
  "earthquake resistance",
  "embankment",
  "flood control",
  "flood mitigation plan",
  "flood prevention",
  "flood protection",
  "flood risk management",
  "fpi",
  "gfdrr",
  "hyogo framework of action",
  "lutte contre les inondations",
  "r.duction des risques de catastrophes",
  "resilience to disasters",
  "resilience to natural",
  "resilience to shock",
  "shock resilience",
  "storm warning",
  "vulnerability and capacity assessment",
  "disaster risk assessment",
  "multi-hazard risk mapping",
  "resilient infrastructure",
  "disaster insurance",
  "disaster risk insurance",
  "disaster risk analysis",
  "flood risk",
  "resilience to earthquakes",
  "seismically safe standards",
  "disaster preparedness plan",
  "disaster preparedness policy",
  "disaster preparedness",
  "disaster resistant construction",
  "disaster resilient building",
  "vulnerability to natural hazards",
  "disaster-resilient",
  "forest fire prevention",
  "hazard monitoring",
  "katastrophenvorsorge",
  "vorhersagebasiert",
  "fr.hwarnsystem",
  "klimaanpassung",
  "katastrophenrisik",
  "katastrophenvorbeugung",
  "evakuierungsplan",
  "r.duction des risques de catastrophe",
  "changement climatique",
  "r.silience climatique",
  "pr.paration aux catastrophes",
  "pr.vention des catastrophes",
  "r.sistante aux catastrophes",
  "cadre de sendai",
  "r.silience aux risques naturels",
  "vuln.rabilit. aux risques naturels",
  "construction r.sistantes aux catastrophes",
  "alerte pr.coce",
  "preparaci.n y prevenci.n desastre",
  "prevenci.n y preparaci.n en caso de desastre",
  "cambio clim.tico",
  "resiliencia a amenazas naturales",
  "reducci.n del riesgo de desastres",
  "resiliencia a los desastres",
  "vulnerabilidad frente a los desastres",
  "marco de sendai",
  "variabilidad clim.tica",
  "risk financing",
  "sendai framework"
)

#minor.keywords <- c(
  #"keyword"
 #)

disqualifying.keywords <- c(
"serendipity",
"domestic revenue mobilisation"
)

disqualifying.sectors <- c(
  "Domestic revenue mobilisation"
)

#Assign relevance based on keywords
crs[, relevance := "None"]
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$LongDescription)), relevance := "Minor"]
crs[grepl(paste(major.keywords, collapse = "|"), tolower(paste(crs$ShortDescription, crs$ProjectTitle))), relevance := "Major"]

#Create check column based on disqualifying factors
crs[, check := "No"]
#crs[relevance == "Minor"]$check <- "potential false positive"
#crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription))), check := "potential false negative"]

#Remove relevance based on disqualifying factors
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription))), relevance := "None"]
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

#Create Primary DRR identifier based on keyword relevance, DRR marker, and Purpose Name
crs[, Primary_DRR := ifelse(relevance == "Major" | DRR == 2 | PurposeName == "Disaster Risk Reduction", "Primary", NA_character_)]

#Load in INFORM index Natural Hazard Risk
inform <- data.table(html_table(read_html(POST('https://drmkc.jrc.ec.europa.eu/Inform-Index/DesktopModules/MVC/InformMVC/Admin/_ResultsTable', config = add_headers(c("content-type"="application/x-www-form-urlencoded; charset=UTF-8","moduleid"="1782","tabid"="1195")), body = "id=433&indicatorsToShow=HA.NAT&countriesToShow=")))[[1]])
inform <- inform[,.(iso3 = Iso3, IndicatorScore = HA.NAT)]

#Merge Countrynames/ISOs
countrynames <- fread("project_data/isos.csv", encoding = "UTF-8")
crs <- merge(countrynames[,c("iso3", "countryname_oecd")], crs, by.x = "countryname_oecd", by.y = "RecipientName", all.y = T)

#Merge INFORM index
crs <- merge(crs, inform[,c("iso3", "IndicatorScore")], by = "iso3", all.x = T)

#Assign Hazard Class based on INFORM
crs[, hazard_class := ifelse(IndicatorScore >= 6.9, "Very High", ifelse(IndicatorScore >= 4.7, "High", ifelse(IndicatorScore >= 2.8, "Medium", "Low")))]

#Calculate total country DRR and ODA and write
crs_total <- crs[, .(drr_oda = sum(USD_Disbursement_Defl[Primary_DRR == "Primary"], na.rm = T), total_oda = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, iso3, hazard_class, IndicatorScore)]
fwrite(crs_total, "output/crs_total.csv")

#Write out list of DRR relevant projects
fwrite(crs[Primary_DRR == "Primary"], "output/all_crs_ddr.csv")