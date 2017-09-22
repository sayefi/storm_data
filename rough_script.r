
## reading input file

inputURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

temp<-tempfile()
download.file(inputURL,temp)

stormData <- read.csv(bzfile(temp))

unlink(temp)

library(dplyr)
library(ggplot2)

head(stormData)

str(stormData)

# Across the United States, which types of events (as indicated in the EVTYPE 
# variable) are most harmful with respect to population health?

len1<-nrow(stormData)
stormDataHealth<-subset(stormData,INJURIES+FATALITIES>0)
len2<-nrow(stormDataHealth)

hist(log10(stormDataHealth$INJURIES+stormDataHealth$FATALITIES),breaks = 30)

g<-ggplot(stormDataHealth,aes(x=log2(INJURIES+FATALITIES)))
g<-g+geom_histogram(binwidth = 1,col="white")
g<-g+geom_vline(xintercept = 1,lty=5,col="blue")
g<-g+geom_text(aes(x=2.5,y=7500,label="Line indicates 2 fatalities/injuries"))
g<-g+ggtitle(label="Most of the storms didn't cause Injuries/fatalities",
             subtitle = paste("Histogram of",len2,
                              "stroms which caused injuries or fatalities",
                              "(out of", len1,"storms)"))
print(g)

as.character()

boxplot(y=stormDataHealth$INJURIES,x=as.character(stormDataHealth$EVTYPE))

InjuriesByEvent<-group_by(select(stormDataHealth,EVTYPE,INJURIES,FATALITIES),
                          EVTYPE)

InjuriesByEvent<-summarise_all(InjuriesByEvent,sum)

InjuriesByEvent<-arrange(InjuriesByEvent,desc(INJURIES+FATALITIES))

InjuriesByEventTop50<-head(InjuriesByEvent,50)

# ggplot(mydata100, aes(gender, fill=workshop) ) +
#      geom_bar(position="fill")
# 
# temp<-InjuriesByEventTop50
# InjuriesByEventTop50<-cbind(temp$EVTYPE,"Injuries",temp$INJURIES)
# InjuriesByEventTop50<-cbind(temp$EVTYPE,"Fatalities",temp$FATALITIES)
# 
# 
# g<-ggplot(data=InjuriesByEventTop50,aes(x=EVTYPE))
# g<-g+geom_point(aes(y=log10(FATALITIES),col="red"))
# g<-g+geom_point(aes(y=log10(INJURIES),col="blue"))
# g<-g+coord_flip()
# print(g)
# 
# g<-ggplot(data=InjuriesByEventTop50,aes(x=EVTYPE,y=log10(INJURIES+FATALITIES)))
# 
InjuriesByEventTop50t<-transform(InjuriesByEventTop50,type="Injury",occur=INJURIES)
InjuriesByEventTop50t<-rbind(InjuriesByEventTop50t,
                             transform(InjuriesByEventTop50,
                                       type="Fatality",occur=FATALITIES))

## highest number of casualities are caused by Tornado

g<-ggplot(InjuriesByEventTop50t,aes(reorder(EVTYPE,(INJURIES+FATALITIES)),
                                   log10(occur),fill=type))
g<-g+geom_bar(stat="Identity")
# g<-g+geom_bar(aes(x=EVTYPE,y=FATALITIES),stat="Identity")
g<-g+coord_flip()
# g<-g+geom_vline(xintercept = 1,lty=5,col="blue")
# g<-g+geom_text(aes(x=2.5,y=7500,label="Line indicates 2 fatalities/injuries"))
# g<-g+ggtitle(label="Most of the storms didn't cause Injuries/fatalities",
#              subtitle = paste("Histogram of",len2,
#                               "stroms which caused injuries or fatalities",
#                               "(out of", len1,"storms)"))
print(g)


InjuriesByEventTop10<-head(InjuriesByEvent,10)

InjuriesByEventTop10t<-transform(InjuriesByEventTop10,type="Injuries",
                                 occurances=INJURIES)
InjuriesByEventTop10t<-rbind(InjuriesByEventTop10t,
                             transform(InjuriesByEventTop10,
                                       type="Fatalities",occurances=FATALITIES))
g<-NULL

##----------------------------------------------------------------------------

g<-ggplot(InjuriesByEventTop10t,aes(reorder(EVTYPE,(INJURIES)),
                                    occurances,fill=type))
g<-g+geom_bar(stat="Identity")
# g<-g+geom_bar(aes(x=EVTYPE,y=FATALITIES),stat="Identity")
g<-g+coord_flip()
g<-g+ scale_fill_discrete(name="Type of Casualities")
g<-g+theme( legend.position=c(.8,.5))
# g<-g+geom_vline(xintercept = 1,lty=5,col="blue")
# g<-g+geom_text(aes(x=2.5,y=7500,label="Line indicates 2 fatalities/injuries"))
# g<-g+ggtitle(label="Most of the storms didn't cause Injuries/fatalities",
#              subtitle = paste("Histogram of",len2,
#                               "stroms which caused injuries or fatalities",
#                               "(out of", len1,"storms)"))
print(g)


##---------------------------------------------------------------------------

totalFatalities<-sum(InjuriesByEvent$FATALITIES)
totalInjuries<-sum(InjuriesByEvent$INJURIES)

InjuriesByEvent<-mutate(InjuriesByEvent,
                        pctInjuries<-round(INJURIES/totalInjuries*100,2),
                        pctFatalities<-round(FATALITIES/totalFatalities*100,2))


names(InjuriesByEvent)[4]<-"pctInjuries"
names(InjuriesByEvent)[5]<-"pctFatalities"


InjuriesByEventTop10<-head(InjuriesByEvent,10)

InjuriesByEventTop10<-select(InjuriesByEventTop10,-INJURIES,-FATALITIES)
                            
InjuriesByEventTop10Matrix<-as.matrix(t(InjuriesByEventTop10))

barplot(as.matrix(t(InjuriesByEventTop10)),col=topo.colors(2))


totalFatalities

InjuriesByEventTop50

dplyr::summarise_all()

log10(2)
exp(.25)

hist((stormData$INJURIES+stormData$FATALITIES),breaks = 30)

stormDataHealth<-subset(stormData,INJURIES+FATALITIES>10)

## Plot number of injuries and fatalities against event type

plot(y=log(stormDataHealth$INJURIES+stormDataHealth$FATALITIES),x=stormDataHealth$EVTYPE)

## Time series

plot(y=stormDataHealth$INJURIES+stormDataHealth$FATALITIES,
     x=stormDataHealth$BGN_DATE,
     col=stormDataHealth$EVTYPE)

plot(y=stormData$FATALITIES,x=stormData$EVTYPE)

plot(y=log(stormData$INJURIES+1+stormData$FATALITIES),x=stormData$EVTYPE)

# Across the United States, which types of events have the greatest economic 
# consequences?

## Subsetting to events which had property damanage 
stormDataProperty<-subset(stormData,PROPDMG>0)

## Invalid PrpoertyDMG exp value 
unique(stormDataProperty$PROPDMGEXP)

## Subsetting Invalid PrpoertyDMG exp value  
stormDataProperty<-stormDataProperty[stormDataProperty$PROPDMGEXP %in% 
                                          c("M","B","K","0"),]

## not significant - less than 0.05%
nrow(invalidExp)/nrow(stormDataProperty)*100

stormDataProperty<-select(stormDataProperty,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,
                                                       CROPDMGEXP)

calculateLoss<-function(dmg,dmgexp="0"){
     
     exps<-c("0","K","M","B")
     
     expn<-0
     
     for(n in 1:length(exps)){
          if(dmgexp[1]==exps[n])
               expn<-(n-1)*3
     }
     
     result<-dmg*10^expn
     
     result
}

calculateLoss(3)

stormDataProperty[length(stormDataProperty$PROPDMGEXP)>1,]

stormDataProperty$PROPDMGEXP[2]

stormDataProperty<-mutate(stormDataProperty,
                                  TotalDMG<-calculateLoss(PROPDMG,PROPDMGEXP) +
                                       calculateLoss(CROPDMG,CROPDMGEXP))

names(stormDataProperty)[6]<-"TotalEconomicLoss"

boxplot(log10(stormDataProperty$TotalEconomicLoss)~stormDataProperty$EVTYPE)



stormDataPropertyModified<-subset(stormDataProperty,
                                  EVTYPE %in% InjuriesByEventTop10$EVTYPE)

a<-InjuriesByEventTop10$EVTYPE


boxplot(log10(stormDataPropertyModified$TotalEconomicLoss)
        ~stormDataPropertyModified$EVTYPE)

plot(log10(stormDataPropertyModified$TotalEconomicLoss)
     ~stormDataPropertyModified$EVTYPE)

exp0<-stormDataProperty[(stormDataProperty$PROPDMGEXP %in% c("1","2","3","0")),]


plot(y=stormData$PROPDMG,x=stormData$EVTYPE)


