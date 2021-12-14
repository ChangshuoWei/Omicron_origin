library(ggplot2)
library(tidyverse)
date_mutation <- read.table('/Dell/Dell4/weics/Omicron/S_enrich/count_100_seq.txt',header = T)
date_S_mutation <- read.table('/Dell/Dell4/weics/Omicron/S_enrich/select100_s_nS.txt',header = T)
date_mutation_dnds <- read.table('/Dell/Dell4/weics/Omicron/S_enrich/select100_S_region_NSvsS.txt',header = T)
date_mutation <- merge(date_mutation,date_S_mutation, by.x='begin',by.y='strain')
date_mutation <- merge(date_mutation,date_mutation_dnds, by.x='begin',by.y='strain')
date_mutation <- separate(date_mutation,begin,into = c('strain','collect_date','submit_date'),sep = '\\|')
date_mutation[767,2] <- '2021-10-20'
date_mutation[767,3] <- '2021-10-20'
date_mutation[767,4] <- 52
date_mutation[767,5] <- 28
date_mutation[767,6] <- 24
date_mutation[767,7] <- 27
patient_detail <- read.table('patein_detail.txt')
Voc_detail <- read.table('VoC.txt')
colnames(Voc_detail) <- colnames(date_mutation)
colnames(patient_detail) <- colnames(date_mutation)
date_mutation <- rbind(date_mutation,patient_detail)
date_mutation <- rbind(date_mutation,Voc_detail)
date_mutation[c(1:765),9] <- 'Variant'
date_mutation[c(766,768,769),9] <- 'Patient'
date_mutation[c(767),9] <- 'Omicron'
date_mutation[c(770:773),9] <- 'VOC'
date_mutation <- date_mutation[date_mutation$X0 < 100,]
colnames(date_mutation)[4] <- 'mutation_count'

date_mutation$collect_date <-as.Date(date_mutation$collect_date,"%Y-%m-%d")
library(ggplot2)
library(lubridate)

date_mutation$day <- c(1:766)
date_mutation$dn <- date_mutation$NS/3018
date_mutation$ds <- date_mutation$S/771
date_mutation$dnds <- date_mutation$dn/date_mutation$ds
date_mutation$V9 <- factor(date_mutation$V9,levels = rev(c('Variant','VOC','Patient','Omicron')))
g1<- ggplot(date_mutation,aes(ymd(collect_date),mutation_count,fill = V9,size =V9))+
  geom_point(aes(color= V9))+
  scale_color_manual(values = rev(c('grey','cyan','blue','red')))+
  scale_size_manual(values=rev(c(0.2,1,1,1)))+
  my_theme2
g1
g2<- ggplot(date_mutation,aes(ymd(collect_date),S_region,fill = V9,size = V9))+
  geom_point(aes(color= V9))+
  geom_smooth(data=date_mutation[date_mutation$V9 =='Variant',],method = 'lm',se=F)+
  scale_color_manual(values = rev(c('grey','cyan','blue','red')))+
  scale_size_manual(values=rev(c(0.2,1,1,1)))+
  my_theme2
g2
g3 <- ggplot(date_mutation,aes(S,NS,fill = V9,size = V9))+
  geom_jitter(aes(color= V9),width = 0.4)+
  #geom_smooth(method = 'lm')+
  scale_color_manual(values = rev(c('grey','cyan','blue','red')))+
  scale_size_manual(values=rev(c(0.2,1,1,1)))+
  my_theme2
g3

library(grid)
pdf("fig1_date_v3.pdf",useDingbats = F,width = 9.6,height = 2.4)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(g1,vp=vplayout(1,1))
print(g2,vp=vplayout(1,2))
print(g3,vp=vplayout(1,3))
dev.off()

row.names(date_mutation) <- NULL
date_mutation <- date_mutation[order(date_mutation$collect_date),]
date_mutation <- date_mutation[1:741,]
date_mutation$day <- as.numeric((date_mutation$collect_date -date_mutation[4,2]))
date_mutation <- date_mutation[date_mutation$day >0,]
d <- lm(S_region ~ day,data = date_mutation[date_mutation$V9 != 'Omicron',])

(26/3018)/(1/771)
fisher.test(matrix(c(26,1,3018,771),nrow=2))
