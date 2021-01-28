# BEE VISITATION DATASET FROM MINDI SUMMERS, 2020
# WRITTEN BY SAM ROBINSON, JAN 2021

# Load everything ---------------------------------------------------------

setwd("~/Projects/Stats projects/Mindi Summers")

library(tidyverse)
theme_set(theme_classic())
library(ggpubr)
library(vegan)

source('helperFuns.R') #Load helper functions

#Data from Mindi (bees + flowers)
dat <- read.csv('./Data/MSSampling_2020.csv',na.strings = '',stringsAsFactors = FALSE) %>% 
  filter(!is.na(Family)) %>% 
  mutate(genSpp=paste(Genus,gsub('.+ sp\\.','spp.',Species))) %>% #Remove Subgenus and sp.
  mutate(FlowerGen=gsub('\\s.+','',FlowerSpp)) %>%  
  mutate(Method=gsub('sweep net','Hand Netting',Method)) %>% 
  mutate(across(c('StartMonth','EndMonth'),~as.numeric(as.roman(.)))) %>% 
  dmy2date(StartDay,StartMonth,StartYear,'StartDay') %>% 
  dmy2date(EndDay,EndMonth,EndYear,'EndDay')

#Data from Linc and Ron (bees only)
datLB <- read.csv('./Data/LRBestSampling_2017_2018.csv',na.strings = '',stringsAsFactors = FALSE) %>% 
  mutate(across(c('StartMonth','EndMonth'),~as.numeric(as.roman(.)))) %>% 
  dmy2date(StartDay,StartMonth,StartYear,'StartDay') %>% 
  dmy2date(EndDay,EndMonth,EndYear,'EndDay') %>% 
  transmute(ID,Lat,Lon,Method=Trap,StartDay,EndDay,Family,Genus,Species,Sex) %>% 
  mutate(Method=case_when(Method=='pan trap' ~ 'Pan Trap', 
                          Method=='vane trap' ~ 'Blue Vane',
                          Method=='net' ~ 'Hand Netting')) %>% 
  mutate(Species=ifelse(is.na(Species)|grepl('sp{1,2}\\.',Species),'spp.',Species)) #Strip out spp.

datRM <- read.csv('./Data/RMSampling_2019.csv',na.strings = c('','NA'),stringsAsFactors = FALSE) %>% 
  mutate(Sex=case_when(Sex=='F' ~ 'female',Sex=='M' ~ 'male')) %>% mutate(Year2=Year) %>% 
  dmy2date(StartDay,StartMonth,Year,'StartDay','%d_%b_%Y') %>% 
  dmy2date(EndDay,EndMonth,Year2,'EndDay','%d_%b_%Y') %>% 
  transmute(ID,Lat=Latitude,Lon=Longitude,Method,StartDay,EndDay,Family,Genus,Species,Sex) %>% 
  mutate(Species=ifelse(is.na(Species)|grepl('(sp\\.|sp\\.*\\d)',Species)|Species=='sp',
                        'spp.',Species))
  

datMS <- dat %>% transmute(ID=Specimen_ID,Lat,Lon,Method,StartDay,EndDay,Family,Genus,Species,Sex)

dat2 <- bind_rows(datLB,datRM,datMS) %>% 
  mutate(genSpp=paste(Genus,gsub('.+ sp\\.','spp.',Species))) #%>% #Remove Subgenus and sp.


rm(datLB,datRM,datMS)

# Basic richness plots (MS data) ----------------------------------------------------------

#What bees were caught?
p1 <- abundPlots(dat,fam=Family,gen=Genus,spp=Species) #Lots of Apidae, mainly Bombus
ggsave('./Figures/beeRichness.png',p1,width=6,height=9)

#What flowers were visited the most/had the most species visit them?
# Solidago is rather popular, but hard to gauge relative attractiveness without estimates of floral density
p2a <- dat %>% filter(!is.na(FlowerSpp)) %>% group_by(FlowerSpp) %>% summarize(N=n(),Nspp=length(unique(genSpp))) %>% 
  arrange(desc(N)) %>% mutate(FlowerSpp=factor(FlowerSpp,levels=rev(FlowerSpp))) %>%   
  pivot_longer(N:Nspp,names_to='Ntype') %>% 
  mutate(Ntype=factor(Ntype,labels=c('Number of Specimens','Number of Species'))) %>% 
  ggplot(aes(x=FlowerSpp,y=value))+geom_col()+facet_wrap(~Ntype,scales='free_x')+
    coord_flip()+labs(y='Number of Bees Collected',x='Flower Species')

p2b <- dat %>% filter(!is.na(FlowerGen)) %>% group_by(FlowerGen) %>% summarize(N=n(),Nspp=length(unique(genSpp))) %>% 
  arrange(desc(N)) %>% mutate(FlowerGen=factor(FlowerGen,levels=rev(FlowerGen))) %>%   
  pivot_longer(N:Nspp,names_to='Ntype') %>% 
  mutate(Ntype=factor(Ntype,labels=c('Number of Specimens','Number of Species'))) %>% 
  ggplot(aes(x=FlowerGen,y=value))+geom_col()+facet_wrap(~Ntype,scales='free_x')+
  coord_flip()+labs(y='Number of Bees Collected',x='Flower Genus')

ggsave('./Figures/flwSppBeeCounts.png',p2a,width=6,height=9)
ggsave('./Figures/flwGenBeeCounts.png',p2b,width=6,height=9)

#Number of species and specimens
dat %>% group_by(Family,Genus) %>% summarize(nSpecimens=n(),nSpp=length(unique(genSpp)))
dat %>% group_by(Family) %>% summarize(nSpecimens=n(),nSpp=length(unique(genSpp)))
dat %>% summarize(nSpecimens=n(),nSpp=length(unique(genSpp)))

# Rarefied richness plots (MS data) -------------------------------------------------

#12 Most-visited flowers
topFlw <- dat %>% filter(!is.na(FlowerSpp)) %>% count(FlowerSpp) %>% 
  arrange(desc(n)) %>% slice(1:12) %>% pull(FlowerSpp)

#Pivot into a matrix
datMat <- dat %>% filter(FlowerSpp %in% topFlw) %>% 
  count(FlowerSpp,genSpp) %>% 
  pivot_wider(names_from=genSpp,values_from=n,values_fill=0) %>% 
  mutate(FlowerSpp=factor(FlowerSpp,levels=topFlw)) %>% arrange(FlowerSpp) %>% 
  column_to_rownames('FlowerSpp') %>% 
  as.matrix(.)

p3 <- siteRarePlots(datMat,Ncol=4,Nrow=3,measType='Chao1',textRange=c(rep(c(0.5,0.6),12)),rowOrder='Ndiv')
ggsave('./Figures/visitorRichness.png',p3,width=8,height=9)

#Bombus only
p3a <- datMat[,grepl('Bombus',colnames(datMat))] %>% 
  siteRarePlots(.,Ncol=4,Nrow=3,measType='Chao1',textRange=c(rep(c(0.5,0.6),12)),rowOrder='Ndiv')+
  labs(title='Bombus only')

#Non-Bombus
p3b <- datMat[,!grepl('Bombus',colnames(datMat))] %>% 
  siteRarePlots(.,Ncol=4,Nrow=3,measType='Chao1',textRange=c(rep(c(0.5,0.6),12)),rowOrder='Ndiv')+
  labs(title='Non-Bombus only')

ggsave('./Figures/visitorRichness_bombus.png',p3a,width=8,height=9)
ggsave('./Figures/visitorRichness_nonbombus.png',p3b,width=8,height=9)

# Who caught the most bees? (MS data) -----------------------------------------------

collDat <- dat %>% group_by(Collector) %>% summarize(N=n(),Nspp=length(unique(genSpp))) 

p4 <- collDat %>% arrange(desc(N)) %>% mutate(Collector=factor(Collector,levels=rev(Collector))) %>% 
  pivot_longer(N:Nspp,names_to='Ntype') %>% 
  mutate(Ntype=factor(Ntype,labels=c('Number of Specimens','Number of Species'))) %>% 
  ggplot(aes(x=Collector,y=value))+geom_col()+facet_wrap(~Ntype,scales='free_x')+coord_flip()+labs(y='Count')

# p4a <- collDat %>% arrange(desc(N)) %>% mutate(Collector=factor(Collector,levels=rev(Collector))) %>% 
#   ggplot(aes(x=Collector,y=N))+geom_col()+coord_flip()+labs(y='Number of bees caught')
# 
# p4b <- collDat %>% arrange(desc(Nspp)) %>% mutate(Collector=factor(Collector,levels=rev(Collector))) %>% 
#   ggplot(aes(x=Collector,y=Nspp))+geom_col()+coord_flip()+labs(y='Number of species caught')
# 
# p4 <- ggarrange(p4a,p4b,ncol=2)

ggsave('./Figures/collectors.png',p4,width=8,height=6)




# Basic richness plots (LB+RM+MS data - bees only) ---------------------------------------

p1 <- dat2 %>% 
  abundPlots(fam=Family,gen=Genus,spp=Species,scaleYtext=c(0.6,1,1)) #Lots of Apidae, mainly Bombus
ggsave('./Figures/beeRichness_all.png',p1,width=6,height=9)
