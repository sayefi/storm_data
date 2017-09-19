inputURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

temp<-tempfile()
download.file(inputURL,temp)

stormData <- read.csv(bzfile(temp))

unlink(temp)