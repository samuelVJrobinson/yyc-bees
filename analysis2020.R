# BEE VISITATION DATASET FROM MINDI SUMMERS, 2020
# WRITTEN BY SAM ROBINSON, JAN 2021

# Load everything ---------------------------------------------------------
setwd("~/Documents/yyc-bees")

library(tidyverse)
theme_set(theme_classic())
library(ggpubr)
library(vegan)

source('helperFuns.R') #Load helper functions

#Data from Mindi (bees + flowers)
dat <- read.csv('./Data/MSSampling_2020.csv',na.strings = '',stringsAsFactors = FALSE) %>% 
  filter(!is.na(Family)) %>% 
  mutate(Species=gsub('.*sp+\\.\\s*(\\d)','spp. \\1',Species)) %>% 
  mutate(genSpp=paste(Genus,ifelse(grepl('spp\\.',Species),'spp.',Species))) %>% #Remove Subgenus and sp.
  mutate(FlowerSpp=gsub('sp\\.','spp.',FlowerSpp)) %>% 
  mutate(FlowerGen=gsub('\\s.+','',FlowerSpp)) %>%  
  mutate(Method='Hand Netting') %>% 
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
  mutate(Species=ifelse(is.na(Species),'spp.',Species)) %>% 
  mutate(Species=gsub('^sp+\\.\\s*(\\d)','spp. \\1',Species)) %>% 
  mutate(genSpp=paste(Genus,gsub('.*sp{1,2}\\.\\s\\d','spp.',Species))) %>% 
  mutate(dataset='LBest')

datRM <- read.csv('./Data/RMSampling_2019.csv',na.strings = c('','NA'),stringsAsFactors = FALSE) %>% 
  mutate(Sex=case_when(Sex=='F' ~ 'female',Sex=='M' ~ 'male')) %>% mutate(Year2=Year) %>% 
  dmy2date(StartDay,StartMonth,Year,'StartDay','%d_%b_%Y') %>% 
  dmy2date(EndDay,EndMonth,Year2,'EndDay','%d_%b_%Y') %>% 
  transmute(ID,Lat=Latitude,Lon=Longitude,Method,StartDay,EndDay,Family,Genus,Species,Sex) %>% 
  mutate(Species=gsub('sp{1,2}\\.*(\\d)','spp. \\1',Species)) %>% 
  mutate(Species=gsub('sp+\\.*','spp.',Species)) %>% 
  mutate(genSpp=paste(Genus,gsub('.*sp{1,2}\\.\\s\\d','spp.',Species))) %>% 
  mutate(dataset='RMiksha')

datMS <- dat %>% transmute(ID=Specimen_ID,Lat,Lon,Method,StartDay,EndDay,Family,Genus,Species,Sex,genSpp) %>% 
  mutate(dataset='MSummers')

dat2 <- bind_rows(datLB,datRM,datMS) %>% 
  #Better labels for facets
  mutate(dataset=factor(dataset,levels=c('LBest','RMiksha','MSummers'),
                        labels=c('L.Best, 2017-2018 (N=2043)','R.Miksha, 2019 (N=1260)','M.Summers, 2020 (N=1058)')))

rm(datLB,datRM,datMS)

# Bee checklist (LB+RM+MS data) ------------------------------------------------

dat2 %>% count(Family,genSpp,dataset) %>% 
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>% 
  write.csv(.,file='./Bee_Checklist.csv',row.names=FALSE)


# Basic richness plots (MS data) ----------------------------------------------------------

#What bees were caught?
p1 <- abundPlots(dat,fam=Family,gen=Genus,spp=Species,showSpp=TRUE) #Lots of Apidae, mainly Bombus
ggsave('./Figures/beeRichness.png',p1,width=6,height=9)

#What flowers were visited the most/had the most species visit them?
# Solidago is rather popular, but hard to gauge relative attractiveness without estimates of floral density
p2a <- dat %>% filter(!is.na(FlowerSpp)) %>% group_by(FlowerSpp) %>% summarize(N=n(),Nspp=length(unique(genSpp))) %>% 
  arrange(desc(N)) %>% mutate(FlowerSpp=factor(FlowerSpp,levels=rev(FlowerSpp))) %>%   
  pivot_longer(N:Nspp,names_to='Ntype') %>% 
  mutate(Ntype=factor(Ntype,labels=c('Number of Specimens','Number of Bee Species'))) %>% 
  ggplot(aes(x=FlowerSpp,y=value))+geom_col()+facet_wrap(~Ntype,scales='free_x')+
    coord_flip()+labs(y='Number of Bees Collected',x='Flower Species')

p2b <- dat %>% filter(!is.na(FlowerGen)) %>% group_by(FlowerGen) %>% summarize(N=n(),Nspp=length(unique(genSpp))) %>% 
  arrange(desc(N)) %>% mutate(FlowerGen=factor(FlowerGen,levels=rev(FlowerGen))) %>%   
  pivot_longer(N:Nspp,names_to='Ntype') %>% 
  mutate(Ntype=factor(Ntype,labels=c('Number of Specimens','Number of Bee Species'))) %>% 
  ggplot(aes(x=FlowerGen,y=value))+geom_col()+facet_wrap(~Ntype,scales='free_x')+
  coord_flip()+labs(y='Number of Bees Collected',x='Flower Genus')

ggsave('./Figures/flwSppBeeCounts.png',p2a,width=6,height=9)
ggsave('./Figures/flwGenBeeCounts.png',p2b,width=6,height=9)

#What bees were the most common visitors/ had the most floral hosts

p2c <- dat %>% filter(!is.na(genSpp)) %>% group_by(genSpp) %>% summarize(N=n(),Nspp=length(unique(FlowerSpp))) %>% 
  arrange(desc(N)) %>% mutate(genSpp=factor(genSpp,levels=rev(genSpp))) %>%   
  pivot_longer(N:Nspp,names_to='Ntype') %>% 
  mutate(Ntype=factor(Ntype,labels=c('Number of Specimens','Number of Flower Species'))) %>% 
  ggplot(aes(x=genSpp,y=value))+geom_col()+facet_wrap(~Ntype,scales='free_x')+
  coord_flip()+labs(y='Number of Bees Collected',x='Bee Species')

p2d <- dat %>% filter(!is.na(Genus)) %>% group_by(Genus) %>% summarize(N=n(),Nspp=length(unique(FlowerSpp))) %>% 
  arrange(desc(N)) %>% mutate(Genus=factor(Genus,levels=rev(Genus))) %>%   
  pivot_longer(N:Nspp,names_to='Ntype') %>% 
  mutate(Ntype=factor(Ntype,labels=c('Number of Specimens','Number of Flower Species'))) %>% 
  ggplot(aes(x=Genus,y=value))+geom_col()+facet_wrap(~Ntype,scales='free_x')+
  coord_flip()+labs(y='Number of Bees Collected',x='Bee Genus')

ggsave('./Figures/beeSppFlwCounts.png',p2c,width=6,height=9)
ggsave('./Figures/beeGenFlwCounts.png',p2d,width=6,height=9)


#Number of species and specimens
dat %>% group_by(Family,Genus) %>% summarize(nSpecimens=n(),nSpp=length(unique(genSpp)))
dat %>% group_by(Family) %>% summarize(nSpecimens=n(),nSpp=length(unique(genSpp)))
dat %>% summarize(nSpecimens=n(),nSpp=length(unique(genSpp)))

# Rarefied richness plots (MS data) -------------------------------------------------

#12 Most-visited flowers
topFlw <- dat %>% filter(!is.na(FlowerSpp)) %>% count(FlowerSpp) %>% 
  arrange(desc(n)) %>% slice(1:12) %>% pull(FlowerSpp)

#Pivot into a matrix
datMat <- dat %>% 
  count(FlowerSpp,genSpp) %>% 
  pivot_wider(names_from=genSpp,values_from=n,values_fill=0) %>% 
  filter(FlowerSpp %in% topFlw) %>%  mutate(FlowerSpp=factor(FlowerSpp,levels=topFlw)) %>% arrange(FlowerSpp) %>% 
  column_to_rownames('FlowerSpp') %>% 
  as.matrix(.)

tr <- c(rep(c(0.15,0.3),6),rep(c(0.75,0.9),6))
p3 <- siteRarePlots(datMat,Ncol=4,Nrow=3,measType='Chao1',textRange=tr,rowOrder='Ndiv')
ggsave('./Figures/visitorRichness.png',p3,width=8,height=9)

#Bombus only
topFlw <- dat %>% filter(!is.na(FlowerSpp),Genus=='Bombus') %>% count(FlowerSpp) %>% 
  arrange(desc(n)) %>% slice(1:12) %>% pull(FlowerSpp)

datMat <- dat %>% filter(Genus=='Bombus') %>% 
  count(FlowerSpp,genSpp) %>% 
  pivot_wider(names_from=genSpp,values_from=n,values_fill=0) %>% 
  filter(FlowerSpp %in% topFlw) %>%  mutate(FlowerSpp=factor(FlowerSpp,levels=topFlw)) %>% arrange(FlowerSpp) %>% 
  column_to_rownames('FlowerSpp') %>% 
  as.matrix(.)

tr <- c(rep(c(0.15,0.3),4),rep(c(0.75,0.9),8))
p3a <- datMat %>% 
  siteRarePlots(.,Ncol=4,Nrow=3,measType='Chao1',textRange=tr,rowOrder='Ndiv')+
  labs(title='Bombus only')

#Non-Bombus
topFlw <- dat %>% filter(!is.na(FlowerSpp),Genus!='Bombus') %>% count(FlowerSpp) %>% 
  arrange(desc(n)) %>% slice(1:12) %>% pull(FlowerSpp)

datMat <- dat %>% filter(Genus!='Bombus') %>% 
  count(FlowerSpp,genSpp) %>% 
  pivot_wider(names_from=genSpp,values_from=n,values_fill=0) %>% 
  filter(FlowerSpp %in% topFlw) %>%  mutate(FlowerSpp=factor(FlowerSpp,levels=topFlw)) %>% arrange(FlowerSpp) %>% 
  column_to_rownames('FlowerSpp') %>% 
  as.matrix(.)

tr <- c(rep(c(0.15,0.3),2),rep(c(0.75,0.9),10))
p3b <- datMat %>% 
  siteRarePlots(.,Ncol=4,Nrow=3,measType='Chao1',textRange=tr,rowOrder='Ndiv')+
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

p1 <- dat2 %>% #filter(!grepl('spp.',genSpp)) %>% 
  abundPlots(fam=Family,gen=Genus,spp=Species,scaleYtext=c(0.6,1,1),showSpp=FALSE) #Lots of Apidae, mainly Bombus
ggsave('./Figures/beeRichness_all.png',p1,width=6,height=9)


# Sampling map ------------------------------------------------------------

library(sf)
library(shadowtext)
library(ggsn)
library(ggrepel)

maptheme <- theme_bw()+theme(axis.text=element_blank(),
                             axis.ticks = element_blank(),
                             axis.title=element_blank())

#Read in community shapefile
yycComm <- st_read("./Shapefiles/yycCommunities/yycCommunities.shp") %>% 
  select(class,name,sector) %>% st_set_crs(4326) %>% 
  st_transform(3401) %>% #AB 10-TM
  mutate(isPark=grepl('Park',class))

#Read in water shapefile
yycWater <- st_read("./Shapefiles/yycHydrology/yycHydrology_poly.shp") %>% 
  st_set_crs(4326) %>% st_transform(3401) #AB 10-TM

yycMap <- ggplot(data=yycComm)+
  geom_sf(aes(fill=isPark),show.legend=FALSE)+
  geom_sf(data=yycWater,col='lightskyblue',fill='lightskyblue')+
  scale_fill_manual(values=c('white','palegreen3'))+
  scalebar(data=yycComm,location='bottomleft',dist=5,dist_unit='km',transform=FALSE)+
  north(data=yycComm,location='topleft',symbol=4)+
  maptheme

#Assign CRS to dat and dat2
dat <- dat %>% st_as_sf(coords=c('Lon','Lat')) %>% 
  st_set_crs(4326) %>% st_transform(3401)

dat2 <- dat2 %>% filter(!is.na(Lat),!is.na(Lon)) %>% 
  # filter(Genus=='Bombus') %>% #Bumblebee-only
  st_as_sf(coords=c('Lon','Lat')) %>% 
  st_set_crs(4326) %>% st_transform(3401) 

#Grouped version of dat2 (grouped by location)
gDat2 <- dat2 %>%
  mutate(lon=st_coordinates(.)[,1],lat=st_coordinates(.)[,2]) %>%
  unite(loc,lat,lon,sep='_') %>% group_by(loc,Method) %>%
  summarize(StartDay=first(StartDay),EndDay=first(EndDay),dataset=first(dataset),
            nSamp=n(),nGen=length(unique(Genus)),nSpp=length(unique(genSpp))) %>% 
  ungroup() %>% separate(loc,c('lat','lon'),sep='_',convert=TRUE) %>% 
  mutate(Method=factor(Method,levels=c('Pan Trap','Blue Vane','Hand Netting')))

#Where did sampling occur (points)?
methodCols <- c('darkorange','blue','black')

# Update: this package is super fragile. It breaks if geometry column is present, and is very fiddly about names.
#   Also, pie charts aren't projected as vector objects, and warp if the map aspect changes.
#   Might be able to steal the guy's code and jerry-rig something, but it's probably not worth it.
# library(scatterpie)

p1 <- yycMap+
  geom_sf(data=gDat2,aes(size=nSamp,col=Method))+
  # coord_sf(datum=st_crs(3401))+
  # geom_scatterpie(data=gDat2,aes(x=lon,y=lat),cols='Method',long_format=TRUE)+
  facet_wrap(~dataset)+
  scale_colour_manual(values=methodCols)+
  labs(size='Number of Specimens')+
  maptheme+
  theme(legend.position='bottom')+
  theme(legend.box='vertical')
ggsave('./Figures/Maps/sampleMap1.png',p1,width=10.5,height=8)

p2 <- yycComm %>% 
  mutate(nRecords=sapply(st_contains(.,dat2),length)) %>%  #Number of bee specimens per community
  mutate(nRecords=ifelse(nRecords==0,NA,nRecords)) %>% 
  ggplot()+geom_sf(aes(fill=nRecords),show.legend = FALSE)+
  geom_sf(data=yycWater,col='deepskyblue',fill='deepskyblue')+
  geom_shadowtext(aes(x=st_coordinates(st_centroid(yycComm))[,1],y=st_coordinates(st_centroid(yycComm))[,2],
                      label=ifelse(nRecords==0,NA,nRecords)),size=3)+
  # geom_text_repel(aes(x=st_coordinates(st_centroid(yycComm))[,1],y=st_coordinates(st_centroid(yycComm))[,2],
  #                                         label=ifelse(nRecords==0,NA,nRecords)),
  #                 force_pull=10,force=0.1,color='white',bg.color='black',bg.r=0.1,size=3)+
  scalebar(data=yycComm,location='bottomleft',dist=5,dist_unit='km',transform=FALSE,height=0.01,st.size=3) +
  north(data=yycComm,location='topleft',symbol=3)+
  labs(x=NULL,y=NULL,title='Number of Specimens per Community')+
  scale_fill_distiller(palette="OrRd",direction=1,na.value="white")+
  maptheme
ggsave('./Figures/Maps/sampleMap2.png',p2,width=8,height=10.5)

#2020 map only       
p3 <- yycMap+
  geom_sf(data=filter(gDat2,grepl('2020',dataset)),aes(size=nSamp))+
  scale_colour_manual(values=methodCols)+
  labs(size='Number of Specimens')+
  maptheme+
  theme(legend.position='bottom')+
  theme(legend.box='vertical')
ggsave('./Figures/Maps/sampleMap3.png',p3,width=10.5,height=8)
