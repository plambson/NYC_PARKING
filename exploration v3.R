library(ggplot2)


PVI<-read.csv("https://data.cityofnewyork.us/resource/aagd-wyjz.csv?issue_date=2014-07-18T00:00:00.000&violation_county=K&violation_code=21",
             stringsAsFactors = F)

PVI$issue_date <- as.Date(PVI$issue_date, "%Y-%m-%dT00:00:00.000")

p<- as.POSIXlt(PVI$issue_date)
PVI$issue_mday<- p$mday
PVI$issue_mon<- p$mon
PVI$issue_wday<- p$wday
rm(p)
gc()


PVI$violation_time<- gsub("A","AM",PVI$violation_time)
PVI$violation_time<- gsub("P","PM",PVI$violation_time)
PVI$violation_time <- strptime(PVI$violation_time,"%I%M%p")
PVI$violation_time_hour<- format(PVI$violation_time,"%H")
PVI$violation_time_minute<- format(PVI$violation_time,"%M")

table(PVI$street_name)
PVI[PVI$street_name =="Vanderbilt Ave",]


#Look at enforcement by hour
a<-ggplot(PVI[PVI$law_section>0,], aes(violation_time_hour))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(law_section~., scales="free")+
  labs(title= "Enforcement by Hour and Law", x= "Violation Hour", y="Count")
#annotate(geom="rect", ymin=0,ymax=8000,xmin=7.5,xmax=18.5, alpha = .2)

#Look at enforcement by DOW
a<-ggplot(PVI[PVI$law_section>0,], aes(issue_wday))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(law_section~., scales="free")+
  labs(title= "Enforcement by DOW and Law", x= "Weekday 6=Sunday", y="Count")

#Look at enforcement by Month
a<-ggplot(PVI[PVI$law_section>0,], aes(issue_mon))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(law_section~., scales="free")+
  labs(title= "Enforcement by Month and Law", x= "Month", y="Count")

#Look at enforcement by Day of Day of Month
a<-ggplot(PVI[PVI$law_section>0,], aes(issue_mday))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(law_section~., scales="free")+
  labs(title= "Enforcement by Day of Month and Law", x= "Day of Month", y="Count")

#Look at enforcement by Date and Law
a<-ggplot(PVI[PVI$law_section>0,], aes(issue_date))
a+geom_bar(fill="#3E8EDE")+
  #coord_flip()+
  theme_minimal()+
  facet_grid(law_section~., scales="free")+
  labs(title= "Enforcement by Date of Month and Law", x= "Date", y="Count")



a<-ggplot(PVI, aes(violation_time_minute))
a+geom_bar(fill="#3E8EDE")+
  coord_flip()+
  theme_minimal()

#Look at Vehicle Year
b<-ggplot(PVI,aes(factor(vehicle_year)))
b+geom_bar(fill="#3E8EDE")+
  coord_flip()+
  theme_minimal()+
  labs(x="Vehicle Year", title="Tickets by Year")



#Look at Violations
violation<-as.data.frame(table(PVI$violation_description))
violation<- violation[order(-violation$Freq),]
c<- ggplot(violation, aes(x=Var1, y=Freq))

c+geom_bar(stat="identity",fill="#3E8EDE")+
  coord_flip()+
  theme_minimal()+
  scale_x_discrete(limits=violation$Var1)

#Look at Law Section
d<-as.data.frame(table(PVI$law_section, PVI$violation_description))
d<- d[d$Freq>0,]
#Law section 408 parking, 1111=Moving violation, 1180 photo enforcement, 
#maybe need look at larger PVI sampling

as.data.frame(table(PVI$violation_county))


rm(list=ls(all=TRUE))
gc()