all_departures_with_cancellation <- read.csv("~/Downloads/all_departures_with_cancellation.csv")
all_departures_with_cancellation<-subset(all_departures_with_cancellation,Carrier.Code=="AA")
years<-substr(all_departures_with_cancellation$Date..MM.DD.YYYY.,7,10)
months<-substr(all_departures_with_cancellation$Date..MM.DD.YYYY.,4,10)
months
barplot(table(years),main="Total Flight by Year",ylab="Frequency",
        ylim=c(170000,300000))
year_freq<-as.data.frame(table(years))

canceled_rows<-all_departures_with_cancellation[all_departures_with_cancellation$Cancelled==1,]
canceled_years<-substr(canceled_rows$Date..MM.DD.YYYY.,7,10)
canceled_years

canceled_freq<-as.data.frame(table(canceled_years))
barplot(table(canceled_years),main="Canceled Flights Distrbution Over Years",ylab="Frequency",
        ylim=c(2000,30000))

canceled_rate<-(canceled_freq$Freq / year_freq$Freq)*100
canceled_rate
barplot(canceled_rate,main="Percentage of Canceled Flights",ylab="Percentage",ylim=c(0,5)
        ,names = year_freq$years)


all_departures_with_cancellation$year=substr(all_departures_with_cancellation$Date..MM.DD.YYYY.,7,10)
canceled_rows$year=substr(canceled_rows$Date..MM.DD.YYYY.,7,10)

order_ind<- c("January","February","March","April","May","June","July","August","September","October","November","December")

month_names<-c("J","F","M","A","M","J","J","A","S","O","N","D")

barplot(table(canceled_rows[canceled_rows$year=="2012",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2012",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2013",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2013",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2014",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2014",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2015",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2015",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2016",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2016",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2017",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2017",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2018",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2018",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2019",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2019",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2020",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2020",ylab="Frequency",
        names=month_names)
barplot(table(canceled_rows[canceled_rows$year=="2021",]$Month)[order_ind],main="Canceled Flights Distribution Over Months in 2021",ylab="Frequency",
        names=month_names)


canceled_seasons <- canceled_rows$Season
canceled_season_freq <- as.data.frame(table(canceled_seasons))

barplot(table(canceled_seasons),main="Canceled Flight Distributed Over Seasons(2012-2021)",ylab="Frequency")

seasons<-all_departures_with_cancellation$Season
season_freq<-as.data.frame(table(seasons))

season_rate<-(canceled_season_freq$Freq/  season_freq$Freq)*100
season_rate

barplot(season_rate,main="Percentage of Canceled Flights Over the Seasons",ylab="Percentage",ylim=c(0,3),
        names = season_freq$seasons)



plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="Project Plots")
plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "Project Plots", row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("Project Plots/", 1:length(sorted.png.names), ".png")


file.rename(from=sorted.png.names, to=numbered.png.names)


x<-as.data.frame(table(all_departures_with_cancellation$Cancelled))
x
cancel_rate<-(x$Freq[2]/x$Freq[1])*100
pie(table(all_departures_with_cancellation$Cancelled),main="Percentage Of Flight Cancellation",col = rainbow(2),labels = c(round(100-cancel_rate,2),round(cancel_rate,2)))
legend("topright", c("Not Cancelled","Cancelled"), cex = 0.8,fill = rainbow(2))
