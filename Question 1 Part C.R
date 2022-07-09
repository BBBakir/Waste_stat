rm(list = ls(all.names = TRUE)) 


library(readxl)

library(ggplot2)

library(reshape2)

library(psych)

library(ggpubr)


library(rcompanion)

companyA <- read_excel("sw_data.xlsx", sheet = "Company A")

companyB <- read_excel("sw_data.xlsx", sheet = "Company B")


companyA$row_sum <- rowSums(companyA[,c(3:5)])

companyB$row_sum <- rowSums(companyB[,c(3:5)])

companyA$nonrecycling_sum <- rowSums(companyA[,c(6:8)])			##total non recyclable waste weekly

companyB$nonrecycling_sum <- rowSums(companyB[,c(6:8)])

companyA[,"Plastic Waste"] <- companyA$`total waste from plastic recycling bins` - companyA$`non-recyclable waste from plastic recycling bins`

companyA[,"Glass Waste"] <- companyA$`total waste from glass recycling bins` - companyA$`non-recyclable waste from glass recycling bins`

companyA[,"Aliminum Waste"] <- companyA$`total waste from aluminum recycling bins` - companyA$`non-recyclable waste from aluminum recycling bins`

companyB[,"Plastic Waste"] <- companyB$`total waste from plastic recycling bins` - companyB$`non-recyclable waste from plastic recycling bins`

companyB[,"Glass Waste"] <- companyB$`total waste from glass recycling bins` - companyB$`non-recyclable waste from glass recycling bins`

companyB[,"Aliminum Waste"] <- companyB$`total waste from aluminum recycling bins` - companyB$`non-recyclable waste from aluminum recycling bins`

B_sum_weekly = aggregate(x = companyB$`row_sum`,
                         by = list(companyB$Week),
                         FUN = sum)


# 1.C.2
region_1_weekly_total <- data.frame(companyA[companyA$Region==1,c(1)])
region_1_weekly_total["Weekly Total Waste of Region 1"] <- rowSums(companyA[companyA$Region==1,c(3:5)])




region_17_weekly_total <- data.frame(companyB[companyB$Region==17,c(1)])
region_17_weekly_total["Weekly Total Waste of Region 17"] <- rowSums(companyB[companyB$Region==17,c(3:5)])

region_17_weekly_plastic <- data.frame(companyB[companyB$Region==17,c(1)])
region_17_weekly_plastic["Weekly Plastic Waste of Region 17"] <- rowSums(companyB[companyB$Region==17,c(3)]-companyB[companyB$Region==17,c(6)])




plotNormalHistogram(region_17_weekly_total[,"Weekly Total Waste of Region 17"],
                    main="Weekly Total Waste of Region 17",
                    xlab="Weekly Total Waste Amount",
                    ylab="Number of Occurrences",
                    col="#EEA47FFF",
                    adj = 0.5,
                    linecol="#085499"
)





plotNormalHistogram(region_17_weekly_plastic[,"Weekly Plastic Waste of Region 17"],
                    main="Weekly Plastic Waste of Region 17",
                    xlab="Weekly Plastic Waste Amount",
                    ylab="Number of Occurrences",
                    col="#EEA47FFF",
                    adj = 0.5,
                    linecol="#085499"
)



plotNormalHistogram(B_sum_weekly[,"x"],
                    main="Weekly Total Waste of Company B",
                    xlab="Weekly Total Waste Amount",
                    ylab="Number of Occurrences",
                    col="#EEA47FFF",
                    adj = 0.5,
                    linecol="#085499"
)
