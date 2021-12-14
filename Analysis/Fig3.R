library(tidyverse)
library(gridExtra)

dog <- Count[Count$Species == 'Dog',]
dog$SNP<-factor(dog$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
plot_list <- list()
for (i in unique(dog$Branch)) {
  plot<-filter(dog,Branch==i)
  Sum<-sum(plot$Count)
  plot$Frac<-plot$Count/Sum
  plot_list[[i]] <-ggplot(data=plot, aes(x=factor(SNP),y=Frac),fill=SNP) + 
    geom_bar(position="dodge", stat="identity",color="black",width = 0.8)+
    #scale_fill_manual(values = Self1)+#facet_grid(~Branch)+
    labs(x='',y='SNV count',title = paste0(i,"\t",Sum))+
    scale_y_continuous(breaks = seq(0,0.5,0.1),limits = c(0,.5))+
    my_theme2#2+guides(fill=F)
}
do.call(grid.arrange, plot_list)



Count<-read.table("Fig7.txt",sep="\t",header = T,stringsAsFactors = F)

#Count <- filter(Count,!grepl( "Bat_",Species))
#Count <- filter(Count,!grepl( "Human_",Species))
#Count <- filter(Count,!grepl( "^m",Branch))
other_virus <- read.table('other_virus_new.txt',sep="\t",header = T,stringsAsFactors = F)
#other_virus$Branch <- paste0(other_virus$Branch,other_virus$Species)
judge_frame <- data.frame()
j <- 1
for (i in as.list(unique(other_virus$Branch))) {
  judge_frame[j,1] <- i
  judge_frame[j,2] <- sum(other_virus[other_virus$Branch == i,]$Count)
  judge_frame[j,3] <- substring(i,1,1)
  j<- j+1
}
judge_frame
judge_frame <- judge_frame[order(judge_frame$V2,decreasing = T),]
judge_frame <- judge_frame[order(judge_frame$V3),]
judge_frame <-  judge_frame %>% group_by(V3) %>% mutate(no=1:length(V1)) %>% as.data.frame()
judge_frame <- judge_frame[!(judge_frame$no >5),]
other_virus <- other_virus[other_virus$Branch %in% as.list(judge_frame$V1),]


#pateint_mutation <- read.table('patient_mutation.txt',header = F,stringsAsFactors = F)
#colnames(pateint_mutation) <- colnames(Count)
#pateint_mutation$Species <- 'Human'
#Count <- Count[-c(433:456,433:456,397:420),] #不包含omicron 分区域
#Count <- Count[-c(433:456),] #不包含after break S non S
Count <- Count[-c(313:348),] #不包含mSCV2、mEbola、mPV
Count <- filter(Count,!grepl( "Bat_",Species))
Count <- filter(Count,!grepl( "Human_",Species))
#Count <- Count[Count$Species != 'Camel',]
Count <- Count[Count$Branch != 'B0',]
#Count <- Count[Count$Branch != 'pMERS',]
colnames(other_virus) <- colnames(Count)
#other_virus <- other_virus[-c(1:108,109:168,469:588),]
variant_spectrum_stat_spectrum$Branch <- as.character(variant_spectrum_stat_spectrum$Branch)
variant_spectrum_stat_spectrum[c(1:12),]$Branch <- 'Alpha'
variant_spectrum_stat_spectrum[c(13:24),]$Branch <- 'Delta'
variant_spectrum_stat_spectrum[25:36,]$Branch <- 'Gamma'
variant_spectrum_stat_spectrum[37:48,]$Branch <- 'Beta'
Count <- rbind(Count,other_virus,variant_spectrum_stat_spectrum[-c(49:60),])

#orthopox <- read.table('orthopox.txt',sep="\t",header = F,stringsAsFactors = F)
#colnames(orthopox) <- colnames(Count)
#Count <- rbind(Count,orthopox)
#Count <- rbind(Count,pateint_mutation)

#Count <- Count[Count$Branch != 'BV3' & Count$Branch != 'M4',]
#Count <- Count[Count$Species != "Cattle",]
head(Count)
Count <- Count[Count$Branch != 'pateint_russia',]
rownames(Count) <- NULL

head(Count)
#Count <- Count[Count$Branch != 'B10',]

Count$SNP<-factor(Count$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
other_virus$SNP<-factor(other_virus$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
plot_list <- list()
for (i in unique(other_virus$Branch)) {
  plot<-filter(other_virus,Branch==i)
  Sum<-sum(plot$Count)
  plot$Frac<-plot$Count/Sum
  plot_list[[i]] <-ggplot(data=plot, aes(x=factor(SNP),y=Frac),fill=SNP) + 
    geom_bar(position="dodge", stat="identity",color="black",width = 0.8)+
    #scale_fill_manual(values = Self1)+#facet_grid(~Branch)+
    labs(x='',y='SNV count',title = paste0(i,"\t",Sum))+
    scale_y_continuous(breaks = seq(0,0.5,0.1),limits = c(0,.5))+#
    my_theme2#+guides(fill=F)
}
#do.call(grid.arrange, plot_list)
################################################################################
#Fig7B
Count <- Count[Count$Species != "Turkey",]
library(reshape2)
mid<-reshape2::dcast(Count[,1:3],Branch~SNP,value.var = "Count")
head(mid)
mid[,-1]<- mid[,-1]/rowSums(mid[,-1])
row.names(mid) <- mid$Branch
nrow(mid)
sum(mid[1,-1])
pca <- prcomp(mid[,2:13],scale = T)
head(pca)
summary(pca)
aa <- data.frame(Branch=rownames(mid),pca$x)
head(aa)
head(Count)
name<-unique(Count[,c(3,4)])
a<-merge(aa,name,by="Branch")
a

Self3<-c("#548235",
         "green",
         "pink",
         "#4EA0EA",
         "#2564BF",
         "#9966FF",
         "cyan",
         "black",
         "orange",
         'grey','red','black',
         'orange','green','red','grey','yellow','red')
fig2b <- ggplot()+
  geom_point(data=filter(a,!Branch %in% c("B21","B22","B23","B24","B25")), mapping=aes(x=PC1,y=PC2,color=Species))+
  #geom_point(data=filter(a,grepl("Human",Species)), mapping=aes(x=PC1,y=PC2),color="blue")+ 
  #geom_point(filter(a,Species=="Bat",!Branch %in% c("B20","B21","B22","B23","B24","B25")), mapping =aes(PC1,PC2),color="green")+  #,color=Species
  #geom_point(data=filter(a,Species=="Human"), 
  #           mapping=aes(x=PC1,y=PC2),color="red")+
  # geom_point(data=filter(a,Species=="Bat"), mapping=aes(x=PC1,y=PC2),color="purple")+ 
  ggrepel::geom_text_repel(data =a[a$Species != 'Unknown_compute',], aes(x=PC1,y=PC2,label = Branch,color="#000000"))+
  #stat_ellipse(filter(a,Species=="Bat",!Branch %in% c("B20","B21","B22","B23","B24","B25")), mapping =aes(PC1,PC2),color="orange")+
  stat_ellipse(filter(a,grepl("Human",Species)), level=0.95, mapping =aes(PC1,PC2),color="cyan")+
  stat_ellipse(filter(a,Species=="Bat"), level=0.95,mapping =aes(PC1,PC2),color="green")+
  stat_ellipse(filter(a,Species=="Cat"),level=0.95, mapping =aes(PC1,PC2),color="#4EA0EA")+
  stat_ellipse(filter(a,Species=="Cattle"), level=0.95,mapping =aes(PC1,PC2),color="#2564BF")+
  stat_ellipse(filter(a,Species=="Dog"), level=0.95,mapping =aes(PC1,PC2),color="#9966FF")+
  stat_ellipse(filter(a,Species=="Pig"), level=0.95,mapping =aes(PC1,PC2),color="orange")+
  stat_ellipse(filter(a,Species=="Turkey"), level=0.95,mapping =aes(PC1,PC2),color="grey")+
  stat_ellipse(filter(a,Species=="mouse"),level=0.95, mapping =aes(PC1,PC2),color="black")+
  #geom_point(data=filter(a,Species=="Bat",!Branch %in% c("B20","B21","B22","B23","B24","B25")), mapping=aes(x=PC1,y=PC2,color=Species))+
  geom_point(data=filter(a,Branch=="B21"), mapping=aes(x=PC1,y=PC2),color="#00553A",shape=3,size=3)+
  geom_point(data=filter(a,Branch=="B23"), mapping=aes(x=PC1,y=PC2),color="#04845A",shape=4,size=3)+
  geom_point(data=filter(a,Branch=="B25"), mapping=aes(x=PC1,y=PC2),color="grey")+
  geom_point(data=filter(a,Branch %in% c("B22","B24")), mapping=aes(x=PC1,y=PC2),color="#08FFAD",shape=2,size=3)+
  #scale_x_continuous(limits = c(-8.5,5)) +
  #scale_y_continuous(limits = c(-4,4))+
  #my_theme2+guides(colour=F)+
  scale_color_manual(values = Self3)+
  theme_classic()
fig2b

omicron_mutation <- read.table('omicron_befor_after_mutation.txt')
colnames(omicron_mutation) <- colnames(Count)
omicron_mutation[c(13:24),] <- Omicron_variant_table
#omicron_mutation[4,2] <- 4
#omicron_mutation[6,2] <- 11
pSCV2_combine <- omicron_mutation
#pSCV2_combine[c(1:12,37:72),]$Species <- 'Human' #认为pateint是人
med<-reshape2::dcast(pSCV2_combine[,1:3],Branch~SNP,value.var = "Count")
head(med)
med[is.na(med)] <- 0
med[,-1]<- med[,-1]/rowSums(med[,-1])
row.names(med) <- med$Branch

head(med[,-1])
predicted <- predict(pca, newdata = med[,-1])
#str(predicted)

predicted<-as.data.frame(unlist(predicted))
predicted$Branch<-rownames(predicted)
head(a)

head(pca)
summary(pca)
aa <- data.frame(Branch=rownames(mid),pca$x)
head(aa)
head(Count)
name<-unique(Count[,c(3,4)])
a<-merge(aa,name,by="Branch")
predicted_name <- unique(pSCV2_combine[,c(3,4)])
predicted <- merge(predicted,predicted_name,by="Branch")
a <- rbind(a,predicted)
Self3<-c("#548235",
         "green",
         "pink",
         "#4EA0EA",
         "#2564BF",
         "#9966FF",
         "cyan",
         "black",
         "orange",
         'red','red','black',
         'orange','green','red','red','yellow','red')
fig3 <- ggplot()+
  geom_point(data=a, mapping=aes(x=PC1,y=PC2,color=Species))+
  #geom_point(data=filter(a,grepl("Human",Species)), mapping=aes(x=PC1,y=PC2),color="blue")+ 
  #geom_point(filter(a,Species=="Bat",!Branch %in% c("B20","B21","B22","B23","B24","B25")), mapping =aes(PC1,PC2),color="green")+  #,color=Species
  #geom_point(data=filter(a,Species=="Human"), 
  #           mapping=aes(x=PC1,y=PC2),color="red")+
  # geom_point(data=filter(a,Species=="Bat"), mapping=aes(x=PC1,y=PC2),color="purple")+ 
  ggrepel::geom_text_repel(data =a[a$Species != 'Unknown_compute',], aes(x=PC1,y=PC2,label = Branch,color="#000000"))+
  #stat_ellipse(filter(a,Species=="Bat",!Branch %in% c("B20","B21","B22","B23","B24","B25")), mapping =aes(PC1,PC2),color="orange")+
  stat_ellipse(filter(a,grepl("Human",Species)), level=0.95, mapping =aes(PC1,PC2),color="cyan")+
  stat_ellipse(filter(a,Species=="Bat"), level=0.95,mapping =aes(PC1,PC2),color="green")+
  stat_ellipse(filter(a,Species=="Cat"),level=0.95, mapping =aes(PC1,PC2),color="#4EA0EA")+
  stat_ellipse(filter(a,Species=="Cattle"), level=0.95,mapping =aes(PC1,PC2),color="#2564BF")+
  stat_ellipse(filter(a,Species=="Dog"), level=0.95,mapping =aes(PC1,PC2),color="#9966FF")+
  stat_ellipse(filter(a,Species=="Pig"), level=0.95,mapping =aes(PC1,PC2),color="orange")+
  #stat_ellipse(filter(a,Species=="Turkey"), level=0.95,mapping =aes(PC1,PC2),color="grey")+
  stat_ellipse(filter(a,Species=="mouse"),level=0.95, mapping =aes(PC1,PC2),color="black")+
  #geom_point(data=filter(a,Species=="Bat",!Branch %in% c("B20","B21","B22","B23","B24","B25")), mapping=aes(x=PC1,y=PC2,color=Species))+
  #geom_point(data=filter(a,Branch=="B21"), mapping=aes(x=PC1,y=PC2),color="#00553A",shape=3,size=3)+
  #geom_point(data=filter(a,Branch=="B23"), mapping=aes(x=PC1,y=PC2),color="#04845A",shape=4,size=3)+
  #geom_point(data=filter(a,Branch=="B25"), mapping=aes(x=PC1,y=PC2),color="grey")+
  #geom_point(data=filter(a,Branch %in% c("B22","B24")), mapping=aes(x=PC1,y=PC2),color="#08FFAD",shape=2,size=3)+
  #scale_x_continuous(limits = c(-8.5,5)) +
  #scale_y_continuous(limits = c(-4,4))+
  #my_theme2+guides(colour=F)+
  scale_color_manual(values = Self3)+
  theme_classic()
fig3

pdf("fig3.pdf",useDingbats = F,width = 5.8,height = 5.8)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(fig2b,vp=vplayout(1,1))

dev.off()
