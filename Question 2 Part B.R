rm(list = ls(all.names = TRUE))


library(readxl)

library(ggplot2)

library(reshape2)

library(psych)

library(ggpubr)


library(rcompanion)

companyA <- read_excel("C:/GAMS/266/sw_data.xlsx", sheet = "Company A")

companyB <- read_excel("C:/GAMS/266/sw_data.xlsx", sheet = "Company B")


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






alpha = 0.05









cA_region_by_region_nonrecy = data.frame(seq(1,16))


for (k in 2:17)
{
  
  cA_region_by_region_nonrecy[k-1,2] = (sum(companyA[companyA$Region==k-1,c(10)]))/sum((companyA[companyA$Region==k-1,c(9)]))
  
}



cB_region_by_region_nonrecy = data.frame(seq(17,36))


for (k in 18:37)
{
  cB_region_by_region_nonrecy[k-17,2] = (sum(companyB[companyB$Region==k-1,c(10)]))/sum((companyB[companyB$Region==k-1,c(9)]))
}
length_B_region <- length(cA_region_by_region_nonrecy[,1])
length_A_region <- length(cA_region_by_region_nonrecy[,1])
z_score_camp <- qnorm(alpha, lower.tail=FALSE)


for (k in 2:17)
{
  sd_av_regionA = sd(cB_region_by_region_nonrecy[,2])
  
  
  cA_region_by_region_nonrecy[k-1,"LB"] = cA_region_by_region_nonrecy[k-1,2] - z_score_camp*sd_av_regionA/sqrt(length_A_region)
  
  
}



for (k in 2:21)
{
  sd_av_regionB = sd(cB_region_by_region_nonrecy[,2])
  
  
  cB_region_by_region_nonrecy[k-1,"LB"] = cB_region_by_region_nonrecy[k-1,2] - z_score_camp*sd_av_regionB/sqrt(length_B_region)
  
  
}

names(cA_region_by_region_nonrecy) = c("Region","AverageRatio","LB")
names(cB_region_by_region_nonrecy) = c("Region","AverageRatio","LB")



ggplot(cA_region_by_region_nonrecy, aes(Region, AverageRatio)) + 
  labs(title = "Average nonrecyclable Lower bounds for regions", x = "Regions of company A", y = "Confidence intervals with significance level 0.05")+
  geom_point() +
  geom_errorbar(aes(ymin=AverageRatio,ymax = LB))+ geom_hline(yintercept=0.18)+
  coord_flip()+
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen" ,hjust = 0.5))


ggplot(cB_region_by_region_nonrecy, aes(Region, AverageRatio)) + 
  labs(title = "Average nonrecyclable Lower bounds for regions", x = "Regions of company B", y = "Confidence intervals with significance level 0.05")+# ggplot2 plot with confidence intervals# ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin=AverageRatio,ymax= LB))+ geom_hline(yintercept=0.18)+
  coord_flip() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen" ,hjust = 0.5))


whole_city = setNames(data.frame(matrix(ncol = 1, nrow = 35)), c("Nonrecyclable ratio over all regions"))
z_for_whole = qnorm(alpha,lower.tail = FALSE)

for (k in 1:35)
{
  whole_city[k,1] <- ((sum(companyA[companyA$Week==k,c(10)])+sum(companyB[companyB$Week==k,c(10)]))/(sum(companyB[companyB$Week==k,c(9)])+sum(companyA[companyA$Week==k,c(9)])))
  
}

sd_whole = sd(whole_city[,1])

L_b_whole = mean(whole_city[,1])-z_for_whole*sd_whole/sqrt(35)




whole_city_lb = setNames(data.frame(L_b_whole),c("LB for whole city"))