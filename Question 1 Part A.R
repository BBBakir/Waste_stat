rm(list = ls(all.names = TRUE)) 

library(readxl)

library(ggplot2)

library(reshape2)



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


A_sum_weekly = aggregate(x = companyA$`row_sum`,
                         by = list(companyA$Week),
                         FUN = sum)

B_sum_weekly = aggregate(x = companyB$`row_sum`,
                         by = list(companyB$Week),
                         FUN = sum)

A_nonrecycling_sum_weekly = aggregate(x = companyA$`nonrecycling_sum`,
                                      by = list(companyA$Week),
                                      FUN = sum)

B_nonrecycling_sum_weekly = aggregate(x = companyB$`nonrecycling_sum`,
                                      by = list(companyB$Week),
                                      FUN = sum)    

total_sum_weekly <- A_sum_weekly

total_sum_weekly$y <- B_sum_weekly$x

nonrecycled_weekly_sum <- A_nonrecycling_sum_weekly				###the data weekly non recyclable waste of regions for a given company

nonrecycled_weekly_sum$y <- B_nonrecycling_sum_weekly$x

recycled_weekly_sum = total_sum_weekly									###the data weekly recyclable waste of regions for a given company

recycled_weekly_sum$x = recycled_weekly_sum$x - nonrecycled_weekly_sum$x	

recycled_weekly_sum$y = recycled_weekly_sum$y - nonrecycled_weekly_sum$y

color_s <- c("#085499","#EEA47FFF")

total_sum_ggplot <- melt(total_sum_weekly, id.vars = "Group.1")                 # 1.A  Weekly Waste 

recycled_sum_ggplot <- melt(recycled_weekly_sum, id.vars = "Group.1")  

ratio_recycled_total_waste <- recycled_weekly_sum                               
ratio_recycled_total_waste$x <- recycled_weekly_sum$x/total_sum_weekly$x
ratio_recycled_total_waste$y <- recycled_weekly_sum$y/total_sum_weekly$y
ratio_recycled_total_waste_ggplot <- melt(ratio_recycled_total_waste, id.vars = "Group.1")





ggplot(companyA, aes(Week, row_sum, fill=row_sum))+ 
  labs(title = "Weekly Total Waste of regions for Company A", x = "Weeks", y = "Total Waste")+#	Y
  facet_wrap(companyA$Region)+																			# E
  geom_bar(stat = "identity", fill= companyA$Region)+
  theme_minimal()																									# I

ggplot(companyB, aes(Week, row_sum))+  
  labs(title = "Weekly Total Waste of regions for Company B", x = "Weeks", y = "Total Waste", color = "Company\n")+#	Y
  facet_wrap(companyB$Region)+																			# E
  geom_bar(stat = "identity", fill= companyB$Region)+             # N
  theme_minimal()		


ggplot(aes(x = Group.1, y = value, group= variable, fill=variable), data = recycled_sum_ggplot)+
  labs(title = "Weekly Total Recyclable Waste For Company A vs Company B\n", x = "Week", y = "Total Recyclable Waste", color = "Company\n")+
  geom_bar(position = "dodge", stat = "identity")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0)) + 
  scale_fill_manual(values = color_s,labels = c("Company A", "Company B"))


total_rec_sum_companyA <- sum(companyA$row_sum)
total_nonrec_sum_companyA <- sum(companyA$nonrecycling_sum)

total_rec_sum_companyB <- sum(companyB$row_sum)
total_nonrec_sum_companyB <- sum(companyB$nonrecycling_sum)

total_sum <- data.frame(total_rec_sum_companyA)
total_sum[3,1] = total_rec_sum_companyB
total_sum[2,1] = total_nonrec_sum_companyA
total_sum[4,1] = total_nonrec_sum_companyB

percentage_nonrec <- data.frame(total_sum[2,1]/(total_sum[1,1]))*100
percentage_nonrec[2,1] <- (total_sum[4,1]/(total_sum[3,1]))*100
names(percentage_nonrec) = c("ratio")
percentage_nonrec$company = list("Company A","Company B")
percentage_nonrec$company <- unlist(percentage_nonrec$company)

ggplot(aes(x = company, y = ratio, fill=company), data = percentage_nonrec)+
  labs(title = "Ratio of Nonrecyclable to Total Waste For Company A and Company B", x = "Company", y = "Ratio of Nonrecyclable to Total Waste in percentage")+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_manual(values = color_s) + 
  geom_text(aes(label = ratio), vjust = -0.2)
