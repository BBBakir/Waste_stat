rm(list = ls(all.names = TRUE)) 

library(readxl)

library(ggplot2)

library(reshape2)

library(psych)

library(ggpubr)


library(rcompanion)

companyA <- read_excel("sw_data.xlsx", sheet = "Company A")

companyB <- read_excel("sw_data.xlsx", sheet = "Company B")


companyA[,"Plastic Waste"] <- companyA$`total waste from plastic recycling bins` - companyA$`non-recyclable waste from plastic recycling bins`

companyA[,"Glass Waste"] <- companyA$`total waste from glass recycling bins` - companyA$`non-recyclable waste from glass recycling bins`

companyA[,"Aliminum Waste"] <- companyA$`total waste from aluminum recycling bins` - companyA$`non-recyclable waste from aluminum recycling bins`

companyB[,"Plastic Waste"] <- companyB$`total waste from plastic recycling bins` - companyB$`non-recyclable waste from plastic recycling bins`

companyB[,"Glass Waste"] <- companyB$`total waste from glass recycling bins` - companyB$`non-recyclable waste from glass recycling bins`

companyB[,"Aliminum Waste"] <- companyB$`total waste from aluminum recycling bins` - companyB$`non-recyclable waste from aluminum recycling bins`



color_s <- c("#085499","#EEA47FFF")



alpha = 0.05


##  For Question 2 part C

lost_week_comA <- setNames(data.frame(matrix(ncol = 6, nrow = 35)), c("Nonrecyclable ratio on Plastic", "Nonrecyclable ratio on Glass", "Nonrecyclable ratio on Aliminum","Plastic loss", "Glass loss", "Aliminum loss"))
lost_week_comB <- setNames(data.frame(matrix(ncol = 6, nrow = 35)), c("Nonrecyclable ratio on Plastic", "Nonrecyclable ratio on Glass", "Nonrecyclable ratio on Aliminum","Plastic loss", "Glass loss", "Aliminum loss"))

for (k in 1:35)
{
  lost_week_comA[k,1] = (sum(companyA[companyA$Week == k,6])/sum(companyA[companyA$Week == k,3]))
  a = sum(companyA[companyA$Week == k,3])
  lost_week_comA[k,2] = (sum(companyA[companyA$Week == k,7])/sum(companyA[companyA$Week == k,4]))
  lost_week_comA[k,3] = (sum(companyA[companyA$Week == k,8])/sum(companyA[companyA$Week == k,5]))
  if (lost_week_comA[k,1] >= 0.2)
  {
    lost_week_comA[k,4] = "Lost Week"
  }else {
    lost_week_comA[k,4] = "Not Lost Week"
  }
  if (lost_week_comA[k,2] >= 0.13)
  {
    lost_week_comA[k,5] = "Lost Week"
  }else {
    lost_week_comA[k,5] = "Not Lost Week"
  }
  if (lost_week_comA[k,3] >= 0.25)
  {
    lost_week_comA[k,6] = "Lost Week"
  }else {
    lost_week_comA[k,6] = "Not Lost Week"
  }
  
  
  ###########For company B############
  lost_week_comB[k,1] = (sum(companyB[companyB$Week == k,6])/sum(companyB[companyB$Week == k,3]))
  
  lost_week_comB[k,2] = (sum(companyB[companyB$Week == k,7])/sum(companyB[companyB$Week == k,4]))
  lost_week_comB[k,3] = (sum(companyB[companyB$Week == k,8])/sum(companyB[companyB$Week == k,5]))
  if (lost_week_comB[k,1] >= 0.2)
  {
    lost_week_comB[k,4] = "Lost Week"
  }else {
    lost_week_comB[k,4] = "Not Lost Week"
  }
  if (lost_week_comB[k,2] >= 0.13)
  {
    lost_week_comB[k,5] = "Lost Week"
  }else {
    lost_week_comB[k,5] = "Not Lost Week"
  }
  if (lost_week_comB[k,3] >= 0.25)
  {
    lost_week_comB[k,6] = "Lost Week"
  }else {
    lost_week_comB[k,6] = "Not Lost Week"
  }
}

count_lost_weekA <- setNames(data.frame(matrix(ncol = 3, nrow = 3)), c("Plastic count","Glass count","Aliminum count"))
count_lost_weekA[,] = 0
rownames(count_lost_weekA) <- c("Lost week","Not lost week","Ratio")
count_lost_weekA[1:2,"Plastic count"] = table(lost_week_comA['Plastic loss'])
count_lost_weekA[2,"Glass count"] = table(lost_week_comA['Glass loss'])
count_lost_weekA[2,"Aliminum count"] = table(lost_week_comA['Aliminum loss'])
count_lost_weekA[3,] = count_lost_weekA[1,1:3]/ (count_lost_weekA[1,1:3]+count_lost_weekA[2,1:3])

count_lost_weekB <- setNames(data.frame(matrix(ncol = 3, nrow = 3)), c("Plastic count","Glass count","Aliminum count"))
count_lost_weekB[,] = 0
rownames(count_lost_weekB) <-  c("Lost week","Not lost week","Ratio")
count_lost_weekB[1:2,"Plastic count"] = table(lost_week_comB['Plastic loss'])
count_lost_weekB[2,"Glass count"] = table(lost_week_comB['Glass loss'])
count_lost_weekB[2,"Aliminum count"] = table(lost_week_comB['Aliminum loss'])
count_lost_weekB[3,] = count_lost_weekB[1,1:3]/ (count_lost_weekB[1,1:3]+count_lost_weekB[2,1:3])




#test statictics based on results



####CI test
z_score_dif_in_pro <- qnorm(alpha/2, lower.tail=FALSE)

prop_plasA = count_lost_weekA[3,1]
prop_plasB = count_lost_weekB[3,1]

z_UB_dif_in_pro_plas  = prop_plasA - prop_plasB + z_score_dif_in_pro*sqrt(((prop_plasA*(1-prop_plasA))/35)+((prop_plasB*(1-prop_plasB))/35))
z_LB_dif_in_pro_plas  = prop_plasA - prop_plasB - z_score_dif_in_pro*sqrt(((prop_plasA*(1-prop_plasA))/35)+((prop_plasB*(1-prop_plasB))/35))



prop_GlasSA = count_lost_weekA[3,2]
prop_GlasSB = count_lost_weekB[3,2]

z_UB_dif_in_pro_Glass  = prop_GlasSA - prop_GlasSB + z_score_dif_in_pro*sqrt(((prop_GlasSA*(1-prop_GlasSA))/35)+((prop_GlasSB*(1-prop_GlasSB))/35))
z_LB_dif_in_pro_Glass  = prop_GlasSA - prop_GlasSB - z_score_dif_in_pro*sqrt(((prop_GlasSA*(1-prop_GlasSA))/35)+((prop_GlasSB*(1-prop_GlasSB))/35))



prop_aliminumA = count_lost_weekA[3,3]
prop_aliminumB = count_lost_weekB[3,3]

z_UB_dif_in_pro_aliminum  = prop_aliminumA - prop_aliminumB + z_score_dif_in_pro*sqrt(((prop_aliminumA*(1-prop_aliminumA))/35)+((prop_aliminumB*(1-prop_aliminumB))/35))
z_LB_dif_in_pro_aliminum  = prop_aliminumA - prop_aliminumB - z_score_dif_in_pro*sqrt(((prop_aliminumA*(1-prop_aliminumA))/35)+((prop_aliminumB*(1-prop_aliminumB))/35))

bounds_propotions = setNames(data.frame(matrix(ncol =3,nrow=2)),c("Plastic","Glass","Aliminum"))
bounds_propotions[1,1] = z_UB_dif_in_pro_plas
bounds_propotions[2,1] = z_LB_dif_in_pro_plas
bounds_propotions[1,2] = z_UB_dif_in_pro_Glass
bounds_propotions[2,2] = z_LB_dif_in_pro_Glass
bounds_propotions[1,3] = z_UB_dif_in_pro_aliminum
bounds_propotions[2,3] = z_LB_dif_in_pro_aliminum
rownames(bounds_propotions) <-  c("UB","LB")
