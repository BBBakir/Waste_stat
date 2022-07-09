rm(list = ls(all.names = TRUE)) 

library(readxl)


companyA <- read_excel("sw_data.xlsx", sheet = "Company A")

companyB <- read_excel("sw_data.xlsx", sheet = "Company B")


companyA[,"Plastic Waste"] <- companyA$`total waste from plastic recycling bins` - companyA$`non-recyclable waste from plastic recycling bins`

companyA[,"Glass Waste"] <- companyA$`total waste from glass recycling bins` - companyA$`non-recyclable waste from glass recycling bins`

companyA[,"Aliminum Waste"] <- companyA$`total waste from aluminum recycling bins` - companyA$`non-recyclable waste from aluminum recycling bins`

companyB[,"Plastic Waste"] <- companyB$`total waste from plastic recycling bins` - companyB$`non-recyclable waste from plastic recycling bins`

companyB[,"Glass Waste"] <- companyB$`total waste from glass recycling bins` - companyB$`non-recyclable waste from glass recycling bins`

companyB[,"Aliminum Waste"] <- companyB$`total waste from aluminum recycling bins` - companyB$`non-recyclable waste from aluminum recycling bins`



pairs.panels(companyA[, c("Plastic Waste", "Glass Waste", "Aliminum Waste")],main = "Weekly Plastic, Glass and Aliminum Recyclable Waste Correlations for Company A")
pairs.panels(companyB[, c("Plastic Waste", "Glass Waste", "Aliminum Waste")],main = "Weekly Plastic, Glass and Aliminum Recyclable Waste Correlations for Company B")
