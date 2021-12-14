chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "omicron_fastml",],pSCV2)
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "omicron_fastml",],Count[Count$Branch == 'M3',])
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "omicron_after_break",],pSCV2)
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "pateint_russia",],pSCV2)
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data <- rbind(pateint_mutaion[pateint_mutaion$Branch == "pateint2",],pSCV2)
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data <- rbind(pateint_mutaion[pateint_mutaion$Branch == "pateint1",],pSCV2)
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data <- rbind(pateint_mutaion[pateint_mutaion$Branch == "pateint_nature",],pSCV2)
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))




pSCV2 <- Count[Count$Branch == 'pSCV2',]
sum(pSCV2[c(1),]$Count)
pSCV2_temp <- pSCV2
pSCV2_temp$Count <- 0
pSCV2_temp$Branch <- 'pSCV2_28'
pSCV2_temp$Species <- 'Unknown_compute'
sample(1:2,1)
pSCV2_combine <- pSCV2
for (i in 1:100) {
  pSCV2_temp$Count <- 0
  pSCV2_temp$Branch <- paste('pSCV2',i)
  for (j in 1:45) {
    random <- sample(1:sum(pSCV2$Count),1)
    if(random >0 & random <= sum(pSCV2[0:1]$Count)){
      pSCV2_temp[1,2] <- 1 +  pSCV2_temp[1,2]
    }else if(random >sum(pSCV2[0:1,]$Count) & random <= sum(pSCV2[0:2,]$Count)){
      pSCV2_temp[2,2] <- 1 +  pSCV2_temp[2,2]
    }else if(random >sum(pSCV2[0:2,]$Count) & random <= sum(pSCV2[0:3,]$Count)){
      pSCV2_temp[3,2] <- 1 +  pSCV2_temp[3,2]
    }else if(random >sum(pSCV2[0:3,]$Count) & random <= sum(pSCV2[0:4,]$Count)){
      pSCV2_temp[4,2] <- 1 +  pSCV2_temp[4,2]
    }else if(random >sum(pSCV2[0:4,]$Count) & random <= sum(pSCV2[0:5,]$Count)){
      pSCV2_temp[5,2] <- 1 +  pSCV2_temp[5,2]
    }else if(random >sum(pSCV2[0:5,]$Count) & random <= sum(pSCV2[0:6,]$Count)){
      pSCV2_temp[6,2] <- 1 +  pSCV2_temp[6,2]
    }else if(random >sum(pSCV2[0:6,]$Count) & random <= sum(pSCV2[0:7,]$Count)){
      pSCV2_temp[7,2] <- 1 +  pSCV2_temp[7,2]
    }else if(random >sum(pSCV2[0:7,]$Count) & random <= sum(pSCV2[0:8,]$Count)){
      pSCV2_temp[8,2] <- 1 +  pSCV2_temp[8,2]
    }else if(random >sum(pSCV2[0:8,]$Count) & random <= sum(pSCV2[0:9,]$Count)){
      pSCV2_temp[9,2] <- 1 +  pSCV2_temp[9,2]
    }else if(random >sum(pSCV2[0:9,]$Count) & random <= sum(pSCV2[0:10,]$Count)){
      pSCV2_temp[10,2] <- 1 +  pSCV2_temp[10,2]
    }else if(random >sum(pSCV2[0:10,]$Count) & random <= sum(pSCV2[0:11,]$Count)){
      pSCV2_temp[11,2] <- 1 +  pSCV2_temp[11,2]
    }else if(random >sum(pSCV2[0:11,]$Count) & random <= sum(pSCV2[0:12,]$Count)){
      pSCV2_temp[12,2] <- 1 +  pSCV2_temp[12,2]
    }
  }
  pSCV2_combine <- rbind(pSCV2_combine,pSCV2_temp)
}
pSCV2_combine <- pSCV2_combine[13:1212,]


pSCV2_Gtest <- data_frame()
j <- 1
for (i in as.list(unique(pSCV2_combine$Branch))) {
  chisq_data <- rbind(pSCV2_combine[pSCV2_combine$Branch == i,],pSCV2)
  chisq_data <- chisq_data[order(chisq_data$SNP),]
  chisq_data <- chisq_data[order(chisq_data$Branch),]
  b <- GTest(chisq_data[13:24,]$Count,p = chisq_data[1:12,]$Count/sum(chisq_data[1:12,]$Count))
  pSCV2_Gtest[j,1] <- i
  pSCV2_Gtest[j,2] <- b$p.value
  j <- j+1
  
}
colnames(pSCV2_Gtest) <- c('sample','G_test_p')
plot(density(-log10(pSCV2_Gtest$G_test_p)))
fig1c <- ggplot(pSCV2_Gtest,aes(-log10(G_test_p)))
fig1c <- fig1c + 
  geom_histogram(bins = 50,color = "#1368ac",fill = '#1368ac')+
  geom_vline(xintercept = -log10(0.003562),color="red")+
  geom_vline(xintercept = -log10(0.5266),color="yellow")+
  geom_vline(xintercept = -log10(0.2705),color="yellow")+
  geom_vline(xintercept = -log10(0.07762),color="yellow")+
  geom_vline(xintercept = -log10(0.6397),color="pink")+
  my_theme2
fig1c
sum(pSCV2_Gtest$G_test_p  < 0.003562)
pdf("fig1c_v3.pdf",useDingbats = F,width = 4.8,height = 3.6)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(fig1c,vp=vplayout(1,1))
dev.off()

Self1<-c("#1F78B4","#A6CEE3","#33A02C","#B2DF8A","#E31A1C","#FB9A99","#FF7F00","#FDBF6F","#6A3D9A","#CAB2D6","#B15928","#FFFF99" )
plot <- pSCV2
plot$SNP <- as.factor(plot$SNP)
plot$SNP<-factor(plot$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
fig1b1 <- ggplot(data=plot, aes(x=factor(SNP),y=Count,fill=SNP)) + 
  geom_bar(position="dodge", stat="identity",color="black",width = 0.8)+
  scale_fill_manual(values = Self1)+
  labs(x='pSCV2',y='Mutation count')+my_theme2
Self1<-c("#1F78B4","#A6CEE3","#33A02C","#B2DF8A","#E31A1C","#FB9A99","#FF7F00","#FDBF6F","#6A3D9A","#CAB2D6","#B15928","#FFFF99" )
plot <- omicron_mutation[omicron_mutation$Branch == 'omicron_fastml',]
plot$SNP <- as.factor(plot$SNP)
plot$SNP<-factor(plot$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
fig1b2 <- ggplot(data=plot, aes(x=factor(SNP),y=Count,fill=SNP)) + 
  geom_bar(position="dodge", stat="identity",color="black",width = 0.8)+
  scale_fill_manual(values = Self1)+
  labs(x='Pre-outbreak omicron',y='Mutation count')+my_theme2
fig1b2

plot <- omicron_mutation[omicron_mutation$Branch == 'omicron_after_break',]
plot$SNP <- as.factor(plot$SNP)
plot$SNP<-factor(plot$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
fig1b3 <- ggplot(data=plot, aes(x=factor(SNP),y=Count,fill=SNP)) + 
  geom_bar(position="dodge", stat="identity",color="black",width = 0.8)+
  scale_fill_manual(values = Self1)+
  labs(x='Post-outbreak omicron',y='Mutation count')+my_theme2
fig1b3

pdf("fig1b_v3.pdf",useDingbats = F,width = 13.2,height = 3.6)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(fig1b1,vp=vplayout(1,1))
print(fig1b2,vp=vplayout(1,2))
print(fig1b3,vp=vplayout(1,3))
dev.off()




plot <- Count[Count$Branch == 'M3',]
plot$SNP <- as.factor(plot$SNP)
plot$SNP<-factor(plot$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
fig2c <- ggplot(data=plot, aes(x=factor(SNP),y=Count,fill=SNP)) + 
  geom_bar(position="dodge", stat="identity",color="black",width = 0.8)+
  scale_fill_manual(values = Self1)+
  labs(x='',y='Mutation count')+my_theme2
fig2c

pdf("fig2c.pdf",useDingbats = F,width = 13.2,height = 3.6)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(fig2c,vp=vplayout(1,1))
print(fig1b2,vp=vplayout(1,2))

dev.off()

chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "omicron_fastml",],Count[Count$Branch == 'M3',])
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "omicron_after_break",],Count[Count$Branch == 'M3',])
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
chisq.test(matrix(c(chisq_data$Count),nrow = 2,byrow = T))
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

chisq_data[1:12,]$Count <- chisq_data[1:12,]$Count/sum(chisq_data[1:12,]$Count)
chisq_data[13:24,]$Count <- chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count)

ks.test(chisq_data[1:12,2],chisq_data[13:24,2])
plot(ecdf(chisq_data[1:12,2]), do.points =F,verticals =T,xlim =c(1,12))
lines(ecdf(chisq_data[13:24,2]),lty = 3,do.points =F,verticals =T)





Count <- Count[order(Count$SNP),]
Count <- Count[order(Count$Branch),]
G_test_animals <- data.frame()
plot_animals_combine <- omicron_mutation[1:12,]
j <- 1
for(k in as.list(unique(Count$Species))){
  Turkey <- Count[Count$Species ==k ,]
  Turkey <- Turkey[order(Turkey$SNP),]
  snp_type <- as.data.frame(table(Turkey$SNP))
  for (i in as.list(snp_type$Var1)) {
    snp_type[snp_type$Var1 == i,2] = sum(Turkey[Turkey$SNP ==i,2])
  }
  snp_type$Branch <- k
  snp_type$species <- k
  colnames(snp_type) <- colnames(plot_animals_combine)
  chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "omicron_fastml",],snp_type)
  chisq_data$Branch <- as.character(chisq_data$Branch)
  chisq_data[chisq_data$Branch == 'omicron_fastml',]$Branch <- 'a'
  chisq_data <- chisq_data[order(chisq_data$SNP),]
  chisq_data <- chisq_data[order(chisq_data$Branch),]
  G_test_animals[j,1]  <- k
  G_test_animals[j,2] <- GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))$statistic
  G_test_animals[j,3] <- GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))$p.value
  j <- j+1
  plot_animals_combine <- rbind(plot_animals_combine,snp_type)
}

plot_animals_combine$SNP<-factor(plot_animals_combine$SNP,levels = c("CT","GA","AG","TC","GT","CA","GC","CG","AC","TG","AT","TA"))
plot_list <- list()
for (i in unique(plot_animals_combine$Branch)) {
  plot<-filter(plot_animals_combine,Branch==i)
  Sum<-sum(plot$Count)
  plot$Frac<-plot$Count/Sum
  plot_list[[i]] <-ggplot(data=plot, aes(x=factor(SNP),y=Frac),fill=SNP) + 
    geom_bar(position="dodge", stat="identity",color="black",width = 0.8)+
    #scale_fill_manual(values = Self1)+#facet_grid(~Branch)+
    labs(x='',y='SNV count',title = paste0(i,"\t",Sum))+
    scale_y_continuous(breaks = seq(0,0.5,0.1),limits = c(0,.5))#+my_theme#2+guides(fill=F)
}
do.call(grid.arrange, plot_list)

Turkey <- Count[Count$Species == 'Camel' ,]
Turkey <- Turkey[order(Turkey$SNP),]
snp_type <- as.data.frame(table(Turkey$SNP))
for (i in as.list(snp_type$Var1)) {
  snp_type[snp_type$Var1 == i,2] = sum(Turkey[Turkey$SNP ==i,2])
}
snp_type$Branch <- 'camel'
snp_type$species <- 'camel'
colnames(snp_type) <- colnames(plot_animals_combine)
chisq_data <- rbind(omicron_mutation[omicron_mutation$Branch == "omicron_fastml",],snp_type)
chisq_data <- chisq_data[order(chisq_data$SNP),]
chisq_data <- chisq_data[order(chisq_data$Branch),]
GTest(chisq_data[1:12,]$Count,p = chisq_data[13:24,]$Count/sum(chisq_data[13:24,]$Count))

