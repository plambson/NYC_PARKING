library(ggplot2)

data <- read.csv("C:/Users/paullambson/Desktop/NYC Parking Data/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", 
                 stringsAsFactors = F,
                 nrows = 1000000)

data$Issue.Date <- as.Date(data$Issue.Date, "%m/%d/%Y")

p<- as.POSIXlt(data$Issue.Date)
data$Issue.mday<- p$mday
data$Issue.mon<- p$mon
data$Issue.wday<- p$wday
rm(p)
gc()


data$Violation.Time<- gsub("A","AM",data$Violation.Time)
data$Violation.Time<- gsub("P","PM",data$Violation.Time)

data$Violation.Violation.Time <- strptime(data$Violation.Time,"%I%M%p")
data$Violation.Hour<- format(strptime(data$Violation.Time,"%I%M%p"),"%H")
data$Violation.Minute<- format(strptime(data$Violation.Time,"%I%M%p"),"%M")


#Look at enforcement by hour
a<-ggplot(data[data$Law.Section>0,], aes(Violation.Hour))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(Law.Section~., scales="free")+
  labs(title= "Enforcement by Hour and Law", x= "Violation Hour", y="Count")
#annotate(geom="rect", ymin=0,ymax=8000,xmin=7.5,xmax=18.5, alpha = .2)

#Look at enforcement by DOW
a<-ggplot(data[data$Law.Section>0,], aes(Issue.wday))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(Law.Section~., scales="free")+
  labs(title= "Enforcement by DOW and Law", x= "Weekday 6=Sunday", y="Count")

#Look at enforcement by Month
a<-ggplot(data[data$Law.Section>0,], aes(Issue.mon))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(Law.Section~., scales="free")+
  labs(title= "Enforcement by Month and Law", x= "Month", y="Count")

#Look at enforcement by Day of Day of Month
a<-ggplot(data[data$Law.Section>0,], aes(Issue.mday))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(Law.Section~., scales="free")+
  labs(title= "Enforcement by Day of Month and Law", x= "Day of Month", y="Count")

#Look at enforcement by Date and Law
a<-ggplot(data[data$Law.Section>0,], aes(Issue.Date))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(Law.Section~., scales="free")+
  labs(title= "Enforcement by Date of Month and Law", x= "Date", y="Count")



a<-ggplot(data, aes(Violation.Minute))
a+geom_bar(fill="#3E8EDE")+
  coord_flip()+
  theme_minimal()

#Look at Vehicle Year
b<-ggplot(data,aes(factor(Vehicle.Year)))
b+geom_bar(fill="#3E8EDE")+
  coord_flip()+
  theme_minimal()+
  labs(x="Vehicle Year", title="Tickets by Year")



#Look at Violations
Violation<-as.data.frame(table(data$Violation.Description))
Violation<- Violation[order(-Violation$Freq),]
c<- ggplot(Violation, aes(x=Var1, y=Freq))

c+geom_bar(stat="identity",fill="#3E8EDE")+
  coord_flip()+
  theme_minimal()+
  scale_x_discrete(limits=Violation$Var1)

#Look at Law Section
d<-as.data.frame(table(data$Law.Section, data$Violation.Description))
d<- d[d$Freq>0,]
#Law section 408 parking, 1111=Moving violation, 1180 photo enforcement, 
#maybe need look at larger data sampling




rm(list=ls(all=TRUE))
gc()