
## reading input file

inputURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

temp<-tempfile()
download.file(inputURL,temp)

stormData <- read.csv(bzfile(temp))

unlink(temp)


head(stormData)

str(stormData)

# Across the United States, which types of events (as indicated in the EVTYPE 
# variable) are most harmful with respect to population health?

# Across the United States, which types of events have the greatest economic 
# consequences?