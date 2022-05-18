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




#Plastic##Plastic#Plastic#Plastic#Plastic#Plastic#Plastic#Plastic#Plastic#Plastic#Plastic#
A_recyclable_plastic_sum_weekly = aggregate(x = companyA$`Plastic Waste`,
                                            by = list(companyA$Week),
                                            FUN = sum)

A_recyclable_plastic_sum_weekly$avarage = A_recyclable_plastic_sum_weekly$x / 16

A_recyclable_plastic_avarage_weekly_mean = mean(A_recyclable_plastic_sum_weekly$avarage)


B_recyclable_plastic_sum_weekly = aggregate(x = companyB$`Plastic Waste`,
                                            by = list(companyB$Week),
                                            FUN = sum)

B_recyclable_plastic_sum_weekly$avarage = B_recyclable_plastic_sum_weekly$x / 20

B_recyclable_plastic_avarage_weekly_mean = mean(B_recyclable_plastic_sum_weekly$avarage)


na_plastic <- nrow(A_recyclable_plastic_sum_weekly)
nb_plastic <- nrow(B_recyclable_plastic_sum_weekly)
alpha <- 0.05


sa_plastic=sd(A_recyclable_plastic_sum_weekly[,3])
sb_plastic=sd(B_recyclable_plastic_sum_weekly[,3])


f_score_plastic <- qf(alpha/2,nb_plastic-1,na_plastic-1,lower.tail = FALSE)

f_LB_plastic = (sa_plastic^2/sb_plastic^2)/f_score_plastic
f_UB_plastic = (sa_plastic^2/sb_plastic^2)*f_score_plastic


t_score_plastic <- qt(alpha/2, na_plastic+nb_plastic-2, lower.tail=FALSE)

sp_plastic=sqrt((na_plastic-1)*sa_plastic^2/(na_plastic+nb_plastic-2)+(nb_plastic-1)*sb_plastic^2/(na_plastic+nb_plastic-2))


t_UB_plastic = A_recyclable_plastic_avarage_weekly_mean - B_recyclable_plastic_avarage_weekly_mean + t_score_plastic*sp_plastic*sqrt(1/na_plastic+1/nb_plastic)
t_LB_plastic = A_recyclable_plastic_avarage_weekly_mean - B_recyclable_plastic_avarage_weekly_mean - t_score_plastic*sp_plastic*sqrt(1/na_plastic+1/nb_plastic)

plastic_diff_bounds = setNames(data.frame(matrix(ncol =3, nrow = 2)), c("Bounds","Confidance Interval F","Confidance Interval T"))
plastic_diff_bounds$Bounds = c("UB","LB")
plastic_diff_bounds[,"Confidance Interval F"] = c(f_UB_plastic,f_LB_plastic)
plastic_diff_bounds[,"Confidance Interval T"] = c(t_UB_plastic,t_LB_plastic)
#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#Glass#
A_recyclable_glass_sum_weekly = aggregate(x = companyA$`Glass Waste`,
                                          by = list(companyA$Week),
                                          FUN = sum)

A_recyclable_glass_sum_weekly$avarage = A_recyclable_glass_sum_weekly$x / 16

A_recyclable_glass_avarage_weekly_mean = mean(A_recyclable_glass_sum_weekly$avarage)

B_recyclable_glass_sum_weekly = aggregate(x = companyB$`Glass Waste`,
                                          by = list(companyB$Week),
                                          FUN = sum)

B_recyclable_glass_sum_weekly$avarage = B_recyclable_glass_sum_weekly$x / 20

B_recyclable_glass_avarage_weekly_mean = mean(B_recyclable_glass_sum_weekly$avarage)

na_glass <- nrow(A_recyclable_glass_sum_weekly)
nb_glass <- nrow(B_recyclable_glass_sum_weekly)
alpha <- 0.05


sa_glass=sd(A_recyclable_glass_sum_weekly[,3])
sb_glass=sd(B_recyclable_glass_sum_weekly[,3])


f_score_glass <- qf(alpha/2,nb_glass-1,na_glass-1,lower.tail = FALSE)

f_LB_glass = (sa_glass^2/sb_glass^2)/f_score_glass
f_UB_glass = (sa_glass^2/sb_glass^2)*f_score_glass


t_score_glass <- qt(alpha/2, na_glass+nb_glass-2, lower.tail=FALSE)

sp_glass=sqrt((na_glass-1)*sa_glass^2/(na_glass+nb_glass-2)+(nb_glass-1)*sb_glass^2/(na_glass+nb_glass-2))

t_UB_glass = A_recyclable_glass_avarage_weekly_mean - B_recyclable_glass_avarage_weekly_mean + t_score_glass*sp_glass*sqrt(1/na_glass+1/nb_glass)
t_LB_glass = A_recyclable_glass_avarage_weekly_mean - B_recyclable_glass_avarage_weekly_mean - t_score_glass*sp_glass*sqrt(1/na_glass+1/nb_glass)

glass_diff_bounds = setNames(data.frame(matrix(ncol =3, nrow = 2)), c("Bounds","Confidance Interval F","Confidance Interval T"))
glass_diff_bounds$Bounds = c("UB","LB")
glass_diff_bounds[,"Confidance Interval F"] = c(f_UB_glass,f_LB_glass)
glass_diff_bounds[,"Confidance Interval T"] = c(t_UB_glass,t_LB_glass)
#########################################################Glassssssssss gLASSSSSSSSSSSSSSSSSSSSS GLASSSS###################################################


###############################################################    PLASTIC AND GLASSS    ##################################################
f_score_glass_and_plastic <- qf(alpha/2,na_plastic-1,na_glass-1,lower.tail = FALSE)

f_LB_glass_and_plastic = (sa_glass^2/sa_plastic^2)/f_score_glass_and_plastic
f_UB_glass_and_plastic = (sa_glass^2/sa_plastic^2)*f_score_glass_and_plastic


t_score_plas_glass <- qt(alpha/2, na_plastic+na_glass-2, lower.tail=FALSE)




t_UB_plas_glass = A_recyclable_plastic_avarage_weekly_mean - A_recyclable_glass_avarage_weekly_mean + t_score_plas_glass*sqrt(sa_plastic^2/na_plastic+sa_glass^2/na_glass)
t_LB_plas_glass = A_recyclable_plastic_avarage_weekly_mean - A_recyclable_glass_avarage_weekly_mean - t_score_plas_glass*sqrt(sa_plastic^2/na_plastic+sa_glass^2/na_glass)

plas_glass = setNames(data.frame(matrix(ncol =3, nrow = 2)), c("Bounds","Confidance Interval F","Confidance Interval T"))
plas_glass$Bounds = c("UB","LB")
plas_glass[,"Confidance Interval F"] = c(f_UB_glass_and_plastic,f_LB_glass_and_plastic)
plas_glass[,"Confidance Interval T"] = c(t_UB_plas_glass,t_LB_plas_glass)

###############################################################   PLASTIC AND GLASSS   ##################################################