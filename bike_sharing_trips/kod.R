#Wczytanie
library(readr)

bike_sharing<-read.delim("dane_surowe.txt",header=TRUE,sep=",")
head(bike_sharing)
str(bike_sharing)       

bike_sharing$start_time<-as.POSIXct(bike_sharing$start_time,format="%d.%m.%Y %H:%M")
bike_sharing$end_time<-as.POSIXct(bike_sharing$end_time,format="%d.%m.%Y %H:%M")
typeof(bike_sharing$start_time)

min(bike_sharing$member_birth_year)               #szukanie i usuwanie blednych danych
bike_sharing<-bike_sharing[!bike_sharing$member_birth_year<1910,]
min(bike_sharing$start_station_latitude)
bike_sharing<-bike_sharing[!bike_sharing$start_station_latitude==0,]

bike_sharing<-na.omit(bike_sharing)

summary(bike_sharing)

write.table(bike_sharing, "dane_przeksztalcone.txt", quote=FALSE, row.names=TRUE, sep=",")

#sortowanie, modyfikowanie, filtrowanie
library(dplyr)
library(lubridate)

bike_sharing%>%
  mutate(duration_min=round(duration_sec/60,digits=2)) %>%
  mutate(week_day=weekdays(start_time)) %>%
  mutate(month=format(start_time, "%B")) %>%
  mutate(hour=hour(start_time)) ->bike_sharing

bike_sharing$month <- ordered(bike_sharing$month, levels= c("stycze?","luty","marzec","kwiecie?","maj"))
bike_sharing$week_day <- ordered(bike_sharing$week_day, levels= c("poniedzia?ek","wtorek","?roda","czwartek","pi?tek","sobota","niedziela"))

#do pakietu ggplot2
bike_sharing %>%
  group_by(week_day,user_type) %>%
  summarize(mean_duration_min=mean(duration_min)) %>%
  arrange(desc(mean_duration_min))

bike_sharing %>%
  group_by(member_gender) %>%
  count() %>%
  arrange(desc(n))

bike_sharing %>%
  filter(hour==c(22,4)) %>%
  group_by(member_gender) %>%
  count() %>%
  arrange(desc(n))

#do pakietu lattice
bike_sharing %>%
  group_by(month) %>%
  summarize(median_duration_min=median(duration_min)) %>%
  arrange(desc(median_duration_min))

bike_sharing%>%
  group_by(month) %>%
  count() %>%
  arrange(desc(n))

#do pakietu plotly
bike_sharing %>%
  group_by(member_gender) %>%
  summarize(median_duration_min=median(duration_min)) %>%
  arrange(desc(median_duration_min))

bike_sharing %>%
  group_by(member_gender,member_birth_year) %>%
  summarize(median_duration_min=median(duration_min)) %>%
  arrange(desc(median_duration_min)) %>%
  head(10)

#do pakietu graphics
bike_sharing %>%
  group_by(bike_share_for_all_trip) %>%
  summarize(median_birth_year=median(member_birth_year)) %>%
  arrange(desc(median_birth_year))

bike_sharing %>%
  group_by(bike_share_for_all_trip) %>%
  count() %>%
  arrange(desc(n))

#Wykresy

#ggplot2
library(ggplot2)
library(ggthemes)

ggplot(data=bike_sharing, aes(x=hour, fill=user_type)) +
  xlim(0,24) +
  geom_bar() +
  facet_wrap(~week_day) +
  ggtitle("Bike sharing during hours") +
  ylab("Number of shared bikes") +
  scale_fill_manual(values=c( "#74c476","#bae4b3"))+ 
  theme(
    legend.position = c(0.5,0.17),
    legend.key = element_rect(color = NA),
    legend.title = element_text(face="bold",color="white"),
    legend.text = element_text(face="bold",color="white"),
    legend.background = element_rect(fill="#238b45"),
    plot.title = element_text(color="#238b45",size=18),
    axis.title.x = element_text(face="bold", color="#238b45",size=16),
    axis.title.y = element_text(face="bold", color="#238b45",size=16),
    axis.text = element_text(color="#238b45"),
    strip.background =element_rect(fill="#238b45"),
    strip.text = element_text(face="bold",color="white"),
    rect = element_rect(fill="ivory"),
    panel.background = element_rect(fill="#edf8e9", color="white")
  )

#lattice
library(lattice)

histogram(~bike_sharing$duration_min|bike_sharing$month,
          layout = c(1,5),
          nint = 1600,
          xlim = c(0,60),
          main = "Duration in months",
          xlab = "Duration [min]",
          ylab = "Percent of total",
          col = c("#96e5ff","#4a8fe2"))

#plotly
library(plotly)

bike_sharing %>%
  plot_ly(x = ~member_birth_year, y = ~duration_min, color=~user_type) %>%
  add_markers(colors="Dark2")%>%
layout(title = list(text="<b>Duration of trip by member birth year</b>"),
       xaxis = list(title="Member birth year"),
       yaxis = list(title="Duration [min]"),
       plot_bgcolor="azure",
       legend = list(title=list(text="<b>User type</b>",
                                font=list(color="steelblue")),
                                font=list(color="steelblue"),
                                x = 0.1, y = 0.9,
                                bgcolor="white")) 

#graphics
library(graphics)

boxplot(member_birth_year~bike_share_for_all_trip,
        data=bike_sharing,
        col = c("firebrick1","limegreen"),
        main = "Box plot",
        xlab = "Birth year",
        ylab = "'Bike share for all' trip",
        horizontal = TRUE)
abline(a=1.5, b=0, lty=1, col="grey")

#ggmap
library(ggmap)

bike_map<-get_stamenmap( bbox = c(left = -122.46, bottom =37.73 , right = -122.22, top = 37.88),
                   maptype = "terrain")
                
ggmap(bike_map, base_layer = ggplot(bike_sharing), extent="panel",maprange=TRUE) +
  geom_count(mapping = aes(start_station_longitude, start_station_latitude), color="green", shape=15, postion="identity")+
  geom_count(mapping = aes(end_station_longitude, end_station_latitude), color="blue", shape=15, alpha=0.5)+
  ggtitle("Map of bike sharing station in california") +
  xlab("longtitude") +
  ylab("latitude") +
  theme(
    legend.position = c(0.2,0.8),
    legend.background = element_rect(fill="azure"),
    legend.key = element_rect(color = NA),
    legend.title = element_text(face="bold", color="steelblue"),
    legend.text = element_text(face="bold", color="steelblue"),
    plot.title = element_text(face="bold", color="steelblue",size=18),
    axis.title.x = element_text(face="bold", color="steelblue",size=16),
    axis.title.y = element_text(face="bold", color="steelblue",size=16))

  


