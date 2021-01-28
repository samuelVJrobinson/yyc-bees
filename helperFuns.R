#Helper functions

#Function to make bee abundance plots from data frame
# fam,gen,spp = names of family, genus, species
# colSet = colour set to use
# scaleYtext = scale for y text on species, genus, family plots (reverse order)
# vj = vertical adjustment for y-axis labels
abundPlots <- function(d,excludeSpp='spp.',fam,gen,spp,
                       colSet='Set1',scaleYtext=c(1,1,1),vj=c(0.3,0.4,0.4)){
  require(RColorBrewer)
  require(tidyverse)
  require(ggpubr)
  options(dplyr.summarise.inform=FALSE)
  
  
  #Converts fam, gen, spp to characters
  d <- d %>% mutate(across(c({{fam}},{{gen}},{{spp}}),as.character)) %>% 
    #Is spp properly listed?
    mutate(noSpp=is.na({{spp}}) | grepl(gsub('\\.','\\\\.',excludeSpp),{{spp}}))
  
  #Deparsed family string
  famStr <- deparse(substitute(fam))
  
  #Creates genSpp column from Genus and Spp
  d <- d %>% mutate(genSpp=case_when(
    is.na({{gen}}) ~ paste({{fam}},' spp.'), #If no genus, use "Family spp."
    #If no species, append "spp." (also deals with escaped periods)
    noSpp ~ paste({{gen}},' spp.'),
    #If genus and spp !NA, paste together
    !noSpp ~ paste({{gen}},{{spp}},sep=' ')
  ))
  
  #Colours for individual families - could also have custom colours fed in
  famCols <- tibble({{fam}} := c('Andrenidae','Apidae','Colletidae','Halictidae','Megachilidae')) %>% 
    mutate(cols=brewer.pal(5,colSet)) #Colour scheme for families
  
  #Data for Species abundance plot
  plotDat <- d %>% filter({{spp}}!=excludeSpp) %>%  #Data for histograms
    count({{fam}},genSpp) %>% #Count Family and genSpp occurrences
    arrange(desc({{fam}}),n) %>% ungroup() %>% #Arrange by Family
    mutate(genSpp=factor(genSpp,level=genSpp))
  
  #Data for coloured background rectangles
  rectDat <- d %>% filter({{spp}}!='spp.') %>%  
    count({{fam}},genSpp) %>%  #Count Family and genSpp occurrences
    group_by({{fam}}) %>% summarize(nSpp=n()) %>% ungroup() %>% 
    arrange(desc({{fam}})) %>% #Arrange by Family
    mutate(ymax=cumsum(nSpp)+0.5,ymin=lag(ymax,default=0.5)) %>% 
    rowwise() %>% mutate(ymid=mean(c(ymax,ymin))) %>%
    mutate(xmin=0,xmax=max(plotDat$n)) %>% 
    left_join(famCols,by=famStr)
  
  #Species plot
  sppPlot <- ggplot()+ geom_col(data=plotDat,aes(n,genSpp))+ #Make columns
    geom_rect(data=rectDat, #Make background rectangles
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill={{fam}}),
              alpha=0.3,show.legend = FALSE)+
    geom_col(data=plotDat,aes(n,genSpp,fill={{fam}}),show.legend = FALSE)+ #Columns (again)
    geom_text(data=rectDat,aes(x=xmax*0.9,y=ymid,label={{fam}}),hjust=1)+ #Add text
    theme(axis.text.y=element_text(vjust=vj[1],size=8*scaleYtext[1]))+ #Change theme
    labs(y=NULL,x='Number of specimens',title='Species')+
    scale_fill_manual(values=rev(as.character(rectDat$cols)))
  
  #Data for Genus abundance plots
  plotDat <- d %>%  #Data for histograms
    count({{fam}},{{gen}}) %>% #Count Family and Genus occurrences 
    arrange(desc({{fam}}),n) %>% ungroup() %>% 
    mutate({{gen}}:=factor({{gen}},levels={{gen}})) #Re-order Genus
  
  #Data for background rectangles
  rectDat <- d %>%  
    group_by({{fam}},{{gen}}) %>% summarize(n=n()) %>% 
    group_by({{fam}}) %>% summarize(nSpp=n()) %>% ungroup() %>% 
    arrange(desc({{fam}})) %>% 
    mutate(ymax=cumsum(nSpp)+0.5,ymin=lag(ymax,default=0.5)) %>% 
    rowwise() %>% mutate(ymid=mean(c(ymax,ymin))) %>% 
    mutate(xmin=0,xmax=max(plotDat$n)) %>% 
    left_join(famCols,by=famStr)
  
  #Genus plot
  genPlot <- ggplot()+ geom_col(data=plotDat,aes(n,{{gen}}))+ 
    geom_rect(data=rectDat,
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill={{fam}}),
              alpha=0.3,show.legend = FALSE)+
    geom_col(data=plotDat,aes(n,{{gen}},fill={{fam}}),show.legend = FALSE)+
    geom_text(data=rectDat,aes(x=xmax*0.9,y=ymid,label={{fam}}),hjust=1)+
    theme(axis.text.y=element_text(vjust=vj[2],size=8*scaleYtext[2]))+
    labs(y=NULL,x='Number of specimens',title='Genera')+
    scale_fill_manual(values=rev(as.character(rectDat$cols)))
  
  #Family abundance plots
  plotDat <- d %>%  #Data for histograms
    group_by({{fam}}) %>% summarize(n=n()) %>% ungroup() %>% 
    left_join(famCols,by=famStr) %>% 
    arrange(desc({{fam}}),n) %>%
    mutate({{fam}}:=factor({{fam}},level={{fam}}))
  
  #Make Family plot
  famPlot <- ggplot()+ geom_col(data=plotDat,aes(n,{{fam}}))+ #Make plot
    geom_col(data=plotDat,aes(n,{{fam}},fill={{fam}}),show.legend = FALSE)+
    theme(axis.text.y=element_text(vjust=vj[3],size=8*scaleYtext[3]))+
    labs(y=NULL,x='Number of specimens',title='Families')+
    scale_fill_manual(values=as.character(plotDat$cols))
  
  #Put all plots together into a single plot
  a <- ggarrange(sppPlot,ggarrange(genPlot,famPlot,nrow=2),ncol=2)
  return(a)
}

# #Test data
# dat <- data.frame(f=c('Apidae','Apidae','Apidae','Colletidae','Andrenidae'),
#                   g=c('Bombus','Bombus','Apis','Hylaeus','Panurgus'),
#                   s=c('rufocinctus','rufocinctus','mellifera','latifrons','badia'))
# abundPlots(dat,fam=f,gen=g,spp=s)
debugonce(abundPlots)

#Function to make "Linc-style" rarefaction plots using Vegan
# Takes matrix of spp abundance, with named sites for each row, and named spp for each column
# Ncol,Nrow = number of columns/rows for facets
# measType = 'both','Chao1','ACE','none' - Type of diversity predictor
# textRange = proportion upper/lower bounds for text display. 
#       Can be 2 values overall, or 2 x site (min,max,min,max...)
# seMult = multiplier for SE
# rowOrder = should sites in facets be ordered 'asis', by diversity ('estDiv'), or # of samples ('Nsamp')?

#TO DO: 
# 1) How does function respond to empty rows in matrix? 
# 2) Label location adjustments are tied to initial row order, not sorted row order

siteRarePlots <- function(d,Ncol=NA,Nrow=NA,measType='both',textRange=c(0.1,0.4),seMult=1,rowOrder='asis'){
  require(vegan)
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  if(nrow(d)>1 & any(is.na(c(Ncol,Nrow)))){ #If columns not specified, and Nsites>1
    Ncol <- nrow(d); Nrow <- 1 
  }
  
  #Get rarefaction data from 1:N samples
  rareDat <- data.frame(Species=unlist(lapply(1:nrow(d),
                                              function(x) as.vector(rarefy(d[x,],1:sum(d[x,]))))),
                        Sample=unlist(lapply(1:nrow(d),function(x) 1:sum(d[x,]))),
                        Site=unlist(lapply(rownames(d),function(x) rep(x,sum(d[x,]))))) %>% 
    mutate(Site=factor(Site,levels=rownames(d)))
  
  graphText <- data.frame(Site=rownames(d),t(estimateR(d))) %>% #Data for Chao/ACE lines
    mutate(across(S.obs:se.ACE,~ifelse(is.nan(.),NA,.))) %>%
    select(-S.obs) %>% unite(chao,S.chao1,se.chao1,sep='_') %>%
    unite(ACE,S.ACE,se.ACE,sep='_') %>% pivot_longer(chao:ACE,names_to='index') %>%
    separate(value,c('meas','se'),sep='_',convert=T) %>%
    mutate(se=se*seMult) %>% #Multiply by se multiplier
    mutate(index=factor(index,labels=c('ACE','Chao1'))) %>%
    mutate(Site=factor(Site,levels=rownames(d)))

  graphText <- data.frame(graphText[rep(1:nrow(graphText),each=max(rareDat$Sample)),],
                          Sample=rep(1:max(rareDat$Sample),nrow(graphText)))
  
  graphText2 <- data.frame(N=rowSums(d),t(estimateR(d))) %>%
    rownames_to_column(var='Site') %>% mutate(Site=factor(Site,levels=Site)) %>% 
    mutate(across(N:se.ACE,~ifelse(is.nan(.),NA,.))) %>%
    mutate(xpos=max(N)*0.95) %>% #X position
    mutate(ymax=switch(measType,
                       both=max(c(S.chao1+se.chao1,S.ACE+se.ACE,na.rm=TRUE)),
                       Chao1=max(S.chao1+se.chao1,na.rm=TRUE),
                       ACE=max(S.ACE+se.ACE,na.rm=TRUE))) %>%  #Maximum y value
    mutate(ymax=ifelse(is.na(ymax),S.obs,ymax)) #If NAs made it through
  
  if(length(textRange)==2){ #If 2 range values provided
    graphText2 <- graphText2 %>% 
      mutate(ylwr=ymax*min(textRange),yupr=ymax*max(textRange))
    } else if(length(textRange)==nrow(d)*2){ #If nrow(d)*2 range values provided (min,max,min,max,...)
      graphText2 <- graphText2 %>% 
        mutate(ylwr=textRange[seq(1,nrow(d)*2-1,2)],
               yupr=textRange[seq(2,nrow(d)*2,2)]) %>% 
        mutate(yupr=yupr*ymax,ylwr=ylwr*ymax)
    } else {
      stop('textRange must be of length 2 or nrow(d)*2')
    }
  
  graphText2 <- graphText2 %>% #Trim display variables
    mutate_at(vars(S.chao1:se.ACE),function(x) trimws(format(round(x,2),nsmall=2)))
  
  #Re-order sites as necessary
  newOrder <- switch(rowOrder,
         Ndiv=order(apply(as.matrix(as.numeric(graphText2[,switch(measType,both=c(4,6),Chao1=c(4),ACE=c(6))])),1,max),
                    decreasing=TRUE),
         Nsamp=order(graphText2$N,decreasing=TRUE),
         asis=1:nrow(d))
  rareDat <- rareDat %>% mutate(Site=factor(Site,levels=levels(Site)[newOrder]))

  p1 <- ggplot(data=rareDat)+
    facet_wrap(~Site,nrow=Nrow,ncol=Ncol) +
    geom_line(aes(Sample,Species),data=rareDat,size=1)+
    geom_point(data=graphText2,aes(x=N,y=S.obs),size=2)+
    geom_segment(data=graphText2,aes(x=N,y=S.obs,xend=N,yend=0),linetype='dashed')+
    geom_segment(data=graphText2,aes(x=0,y=S.obs,xend=N,yend=S.obs),linetype='dashed')+
    geom_text(data=graphText2,aes(x=xpos,y=yupr,label=paste('N =',N)),hjust=1,size=3)+
    geom_text(data=graphText2,aes(x=xpos,y=yupr-(yupr-ylwr)*ifelse(measType=='both',0.33,0.5),
                                  label=paste('S.obs =',S.obs)),hjust=1,size=3)
  
  if(measType=='both'){ #If using both richness indices
    p1 <- p1 +
      geom_line(data=graphText,aes(x=Sample,y=meas,col=index),size=1)+
      geom_ribbon(data=graphText,aes(x=Sample,ymax=meas+se,ymin=meas-se,fill=index),alpha=0.2)+
      geom_text(data=graphText2,aes(x=xpos,y=yupr-(yupr-ylwr)*0.66,
                                    label=paste('S.chao1 =',S.chao1,'\u00B1',se.chao1)),hjust=1,size=3)+
      geom_text(data=graphText2,aes(x=xpos,y=ylwr,label=paste('S.ACE =',S.ACE,'\u00B1',se.ACE)),hjust=1,size=3)+
      labs(x='Number of Specimens',y='Richness',col='Estimated\nSpecies\nRichness',
           fill='Estimated\nSpecies\nRichness')+
      scale_colour_manual(values=c('red','blue'))+
      scale_fill_manual(values=c('red','blue'))
    
  } else if(measType=='Chao1'|measType=='ACE') {
    
    p1 <- p1 +
      geom_line(data=filter(graphText,index==measType),aes(x=Sample,y=meas,col=index),size=1)+
      geom_ribbon(data=filter(graphText,index==measType),aes(x=Sample,ymax=meas+se,ymin=meas-se,fill=index),alpha=0.2)+
      labs(x='Number of Specimens',y='Richness',col='Estimated\nSpecies\nRichness',
           fill='Estimated\nSpecies\nRichness')+
      scale_colour_manual(values=c('black','black'))+
      scale_fill_manual(values=c('black','black'))
    
    if(measType=='Chao1'){
      p1 <- p1 + geom_text(data=graphText2,aes(x=xpos,y=ylwr,
                                               label=paste('S.chao1 =',S.chao1,'\u00B1',se.chao1)),hjust=1,size=3)
    } else {
      p1 <- p1 + geom_text(data=graphText2,aes(x=xpos,y=ylwr*0.66,
                                               label=paste('S.ACE =',S.ACE,'\u00B1',se.ACE)),hjust=1,size=3)
    }
  } 
  return(p1)
}

#Converts day,month,year to Date object in dplyr
dmy2date <- function(data,day,month,year,newName='date',dateFormat='%d_%m_%Y'){
  require(dplyr)
  day <- enquo(day); month <- enquo(month); year <- enquo(year)
  newName <- sym(newName)
  
  data %>% 
    unite(!!newName,c({{day}},{{month}},{{year}}),sep='_') %>% 
    mutate(!!newName := as.Date(!!newName,format=dateFormat))
}

# #Test data
# dat <- data.frame(a=letters[1:2],d=c(10,12),m=c(6,12),y=c(1988,2012))
# dmy2date(dat,d,m,y,'newDate')

