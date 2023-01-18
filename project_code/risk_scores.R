require("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

required.packages <- c("data.table", "readxl")
lapply(required.packages, require, character.only = T)

gain <- fread("GAIN/gain.csv", encoding = "UTF-8", header = T)
inform <- data.table(read_excel("INFORM/inform.xlsx"))

inform_ph <- dcast(inform[IndicatorId %in% c("HA.NAT.FL", "HA.NAT.TC", "HA.NAT.DR", "CC")], INFORMYear + Iso3 ~ IndicatorId, value.var = "IndicatorScore")

inform_ph[, PH := round((10-((10-HA.NAT.FL)/10*9+1)^(1/3)*((10-HA.NAT.TC)/10*9+1)^(1/3)*((10-HA.NAT.DR)/10*9+1)^(1/3))/9*10, 1)][PH == 0, PH := 0.1]
inform_ph[, inform := round(PH^(1/2)*CC^(1/2),1)*10]

gain_ph <- melt(gain, id.vars = c("ISO3", "Name"), value.name = "gain", variable.factor = F)

risk <- merge(inform_ph[, .(iso3 = Iso3, Year = as.character(INFORMYear), inform)], gain_ph[, .(iso3 = ISO3, Year = variable, gain)], by = c("iso3", "Year"), all = T)

fwrite(risk[!is.na(inform)], "risk_scores.csv")
