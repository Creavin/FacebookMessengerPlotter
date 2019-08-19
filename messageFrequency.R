inputFile <- "messages.json" #path to JSON of your messenger data

library(jsonlite)#JSON convertor
library(anytime)#epoch convertor
library(plyr) #for counting
all.equal(mtcars, fromJSON(toJSON(mtcars)))

##Libraries for histograms
library(scales)
library(ggplot2)
library(zoo)
library(tsibble) # yearweek conversions
library(ISOweek)
library(ggrepel)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}    


#JSON to dataframe
messengerData <- fromJSON(inputFile)
participantsData <- messengerData["participants"]$participants
messagesData <- messengerData["messages"]$messages

#Converts timestamp to data/time
messagesData[,2]<-anytime(messagesData[,2]/1000)

messagesOf <- function(name){ #creates list of messages belonging to a user
  dataSub1 = subset(messagesData, sender_name == name)
  return(dataSub1)
}

nMessagesOf <- function(name){ #counts number of messages belonging to a user
  nMessages <- nrow(messagesOf(name))
  return(nMessages)
}

messageDates = as.Date(messagesData$timestamp_ms) #list of all message dates
messageDateFreq = count(as.Date(messagesData$timestamp_ms)) #table with list and count of all msg dates

participantMsgFreq<-NULL
for (person in participantsData$name) { #creates table of usersname, and messages sent by that user
  participantMsgFreq <- rbind(participantMsgFreq,data.frame(person,nMessagesOf(person)))
}
head(participantMsgFreq)


colnames(participantMsgFreq)[2] <- "Freq" #alias for number of messages column 
participantMsgFreq<- participantMsgFreq[order(-participantMsgFreq$Freq),]
participantMsgFreq$person<-factor(participantMsgFreq$person, levels = participantMsgFreq$person[order(-participantMsgFreq$Freq)])


##Messages by person plot
totalMessages=sum(participantMsgFreq$Freq)
personMessagesPlot <- ggplot(participantMsgFreq, aes(x=participantMsgFreq$person, y=participantMsgFreq$Freq)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text_repel(aes(
    label = paste( format( round((participantMsgFreq$Freq/totalMessages*100),1) ,nsmall=1 ), "%", sep=""), 
    y = participantMsgFreq$Freq ),
    direction = "y",
    force = 1,
    vjust = 1) +
  ggtitle("Number & percentage of messages sent to the groupchat") +
  ylab("Number of messages sent") + 
  xlab("Sender")

##Messages per day
dailyMessagePlot <- ggplot(messageDateFreq, aes(x=messageDateFreq$x, y=messageDateFreq$freq)) + geom_bar(stat = "identity") + 
  scale_x_date(breaks = pretty_breaks(16))

##Messages per week
messageDatesFreqWeek = count(ISOweek2date(paste(ISOweek(messageDates),1,sep="-")))

weeklyMessagePlot <- ggplot(messageDatesFreqWeek, aes(messageDatesFreqWeek$x, messageDatesFreqWeek$freq ))+geom_bar(stat = "identity") + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  geom_text(label=messageDatesFreqWeek$freq, position = position_dodge(.9), vjust = 0 ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Messages per month
altMessageDatesFreq = count(as.Date(as.yearmon(messageDates))) #monthly messages frequency table

monthlyMessagePlot <- ggplot(altMessageDatesFreq, aes(altMessageDatesFreq$x, altMessageDatesFreq$freq ))+geom_bar(stat = "identity") + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  geom_text(label=altMessageDatesFreq$freq, position = position_dodge(.9), vjust = 0 ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


multiplot(personMessagesPlot, dailyMessagePlot, weeklyMessagePlot, monthlyMessagePlot)

