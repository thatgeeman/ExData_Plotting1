readData <- function(fileToRead){
  # mem req
  mem<- 2075259*9*8/2^{20} # calculate memory needed in MB
  nskip<-(15+31)*24*60 + ((24-17)*60 +(60-24)) - 60 + 1  # minutes in 15(dec) +31(jan) days plus to adjust for starting time
  nread<-2*24*60 # minutes in 2 days
  
  # read only required data
  data<-read.table(fileToRead, header = FALSE, skip = nskip, nrows = nread,sep = ';')
  inames<-names(read.table(fileToRead, header = TRUE,nrows = 1))
  inames<-strsplit(inames,".",fixed = TRUE)
  inames<-unsplit(inames,9)
  colnames(data)<-inames
  
  # clean data
  data$Date<- format(data$Date,format="")
  data$Time<- format(data$Time,format="")
  data<-data[complete.cases(data), ] # remove na or ?
  data
}

# program begins here
fileToRead <- "../course-project-1/household_power_consumption.txt"
data <- readData(fileToRead) # function handling cleaning

# get datetime install.packages("lubridate")
library("lubridate")
data$datetime<- paste(data$Date,data$Time)
data$datetime<-dmy_hms(data$datetime)
data$datetime <- strptime(data$datetime, '%d/%m/%Y %H:%M:%S')

# plot data
png("plot2.png", width = 480, height = 480)
plot(data$datetime,data$Global_active_power, ylab = "Global Active Power (kilowatts)", xlab="", type = "l")
dev.off()

