## reading input file

inputURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

temp<-tempfile()
download.file(inputURL,temp)

stormData <- read.csv(bzfile(temp))

unlink(temp)

library(dplyr)
library(ggplot2)

stormDataAggregate<-select(stormData, EVTYPE,INJURIES,FATALITIES,
                           PROPDMG,PROPDMGEXP, CROPDMG,CROPDMGEXP )

summary(stormDataAggregate)

unique(stormDataAggregate$PROPDMGEXP)

calculateLoss<-function(dmg,dmgexp=""){
     
     if (dmgexp[1]=="K"||dmgexp[1]=="k") result<-dmg*10^3
     else if (dmgexp[1]=="M"||dmgexp[1]=="m") result<-dmg*10^6
     else if (dmgexp[1]=="B") result<-dmg*10*9
     else if (is.numeric(dmgexp[1])) result<-dmg*10^as.numeric(dmgexp)
     else result <-dmg
     
     
     result
}

calculateLoss(5,5)


stormDataAggregate<-mutate(stormDataAggregate,
                           ECONOMICLOSS<-calculateLoss(PROPDMG,PROPDMGEXP)+
                                calculateLoss(CROPDMG,CROPDMGEXP))


names(stormDataAggregate)[8]<-"ECONOMICLOSS"

stormDataAggregate<-mutate(stormDataAggregate, EVENTCOUNT=1)


stormDataAggregate<-group_by(stormDataAggregate,EVTYPE)

stormDataAggregate<-summarise(stormDataAggregate,sum(INJURIES),sum(FATALITIES),
                         sum(ECONOMICLOSS),sum(EVENTCOUNT))

names(stormDataAggregate)<-c("EVTYPE", "INJURIES", "FATALITIES",
                         "ECONOMICLOSS", "EVENTCOUNT")

stormDataAggregate<-arrange(stormDataAggregate,desc(FATALITIES),
                            desc(ECONOMICLOSS))

## Top 10 type of storms for Fatalities
stormFatalitiesTop10<-head(stormDataAggregate,10)


totalFatalities<-sum(stormDataAggregate$FATALITIES)

stormFatalitiesTop10[1,3]

## Tornado accounts for 37% Fatalities
stormFatalitiesTop10[1,3]/totalFatalities

stormFatalitiesTop10t<-transform(stormFatalitiesTop10,type='INJURIES',
                                                  occurance=INJURIES)

stormFatalitiesTop10t<-rbind(stormFatalitiesTop10t,
                             transform(stormFatalitiesTop10,type='FATALITIES',
                                       occurance=FATALITIES))

g<-ggplot(stormFatalitiesTop10t,aes(x=reorder(EVTYPE,FATALITIES+INJURIES),
                                   y=occurance,fill=type))
g<-g+geom_bar(stat="Identity")
# g<-g+geom_bar(aes(x=EVTYPE,y=FATALITIES),stat="Identity")
g<-g+coord_flip()
g<-g+ scale_fill_discrete(name="Type of Casualities")
g<-g+theme( legend.position=c(.8,.5))
# g<-g+geom_vline(xintercept = 1,lty=5,col="blue")
# g<-g+geom_text(aes(x=2.5,y=7500,label="Line indicates 2 fatalities/injuries"))
g<-g+ggtitle(label="Top 10 storm types in US in terms of impact on population health")
print(g)

## -[]requires Injuries and fatalities in different rows to create the plot

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




## Top 10 type of storms for Economic loss
stormEconomicLossTop10<-head(arrange(stormDataAggregate,desc(ECONOMICLOSS)),10)

g<-ggplot(stormEconomicLossTop10,aes(reorder(EVTYPE,(ECONOMICLOSS)),
                                     ECONOMICLOSS,fill="topo.colors(1,alpha=.2)"))
g<-g+geom_bar(stat="Identity")
# g<-g+geom_bar(aes(x=EVTYPE,y=FATALITIES),stat="Identity")
g<-g+coord_flip()
# g<-g+ scale_fill_discrete(name="Type of Casualities")
g<-g+theme( legend.position="none")
g<-g+labs(x="Economic Loss", y="Type of Storms")
# g<-g+geom_vline(xintercept = 1,lty=5,col="blue")
# g<-g+geom_text(aes(x=2.5,y=7500,label="Line indicates 2 fatalities/injuries"))
g<-g+ggtitle(label="Top 10 storm types in US in terms of Economic impact") 
print(g)


## combined findings
stormTop20<-head(stormDataAggregate,20)

tail(stormTop20,19)


# g<-ggplot(tail(stormTop20,19),aes(x=log10(ECONOMICLOSS),y=FATALITIES+INJURIES,
#                          size=log10(EVENTCOUNT),col=factor(EVTYPE),alpha=.5))

g<-ggplot(stormTop20,aes(x=log10(ECONOMICLOSS),
                                  y=log10(FATALITIES+INJURIES),
                    size=log10(EVENTCOUNT),col=factor(EVTYPE),alpha=1))
g<-g+geom_point(pch=19)
#g<-g+scale_color_brewer(palette="Dark2")
# g<-g+geom_bar(aes(x=EVTYPE,y=FATALITIES),stat="Identity")
# g<-g+coord_flip()
# g<-g+ scale_fill_discrete(name="Type of Casualities")
g<-g+theme( legend.position="none")
# g<-g+geom_vline(xintercept = 1,lty=5,col="blue")
g<-g+geom_text(aes(x=log10(ECONOMICLOSS)+.1,
                   y=log10(FATALITIES+INJURIES)-.1,
                   label=EVTYPE,size=2.5))
g<-g+ggtitle(label="Relative comparision of top 20 storm types in US (1950-2011)",
                 subtitle = paste(stormTop20[1,]$EVTYPE, "has higest impact in terms of injuries/fatalities and Economic loss"))
g<-g+labs(x="Economic loss in Logarithmic scale (base 10)",
          y="Total injuries and Fatalities in Logarithmic scale (base 10)")
print(g)

dev.copy(pdf,"stormcomp.pdf")
dev.off()


subset(stormData,INJURIES==0 && FATALITIES==0 && PROPDMG ==0 && CORPDMG