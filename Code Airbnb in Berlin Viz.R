setwd("C:/Users/Monika Sienkiewicz/Documents/mgr materials/wizd/")
#DATA

#Data about Airbnb in Berlin from 
#http://insideairbnb.com/get-the-data.html

airbnb<- read.csv("C:/Users/Monika Sienkiewicz/Documents/mgr materials/wizd/listings.csv", sep=";")

#delete NA
airbnb <- na.omit(airbnb, cols=c("price", "room_type", "number_of_reviews", "minimum_nights", "dane$availability_365...","reviews_per_month"))

library(ggplot2)

#1 no of reviews
ggplot(airbnb, aes(x=number_of_reviews)) + 
  geom_histogram(colour="mintcream", fill="violetred2") + 
  theme_minimal(base_size=18) +
  xlab("No of reviews") + 
  ylab("Freq") + 
  ggtitle("") +
  scale_x_continuous()
dev.copy(png,'1noofreviews.png')
dev.off() 

summary(airbnb$number_of_reviews)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#1.00    2.00    6.00   16.42   17.00  360.00     

#2 Prices 
ggplot(airbnb, aes(x=price)) + 
  geom_histogram(colour="mintcream", fill="orchid4") + 
  theme_minimal(base_size=18) +
  xlab("Most popular price range [EUR]") + 
  ylab("Freq") + 
  ggtitle("") +
  scale_x_continuous(limits=c(0, 200), breaks=c(0, 200, 50)  )
dev.copy(png,'2prices.png')
dev.off() 

summary(airbnb$price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   32.00   47.00   57.39   69.00 1000.00

#Divide prices into categories based on above frequencies
airbnb$price2 <- cut(airbnb$price, c(0,30,57,100,1000), c("Cheap", "Average", "Pricey", "Very Expensive"), include.lowest = TRUE, right = TRUE)

grouped <- airbnb %>%
  group_by(price2) %>%
  count(price2)

#Cheap 3867
#Average 6501
#Pricey 4431
#Very Expensive 1347


#3 Neighbourhood
qplot(airbnb$neighbourhood_group, main="", data=airbnb ) +
  theme_minimal(base_size=18) +
  xlab("Neighbourhood Group") +
  ylab("") +
  geom_bar(aes(fill = airbnb$neighbourhood_group)) +
  coord_flip()+
  scale_fill_discrete(guide=FALSE) 
#most airbnbs in Friedrichshain, Mitte, Neukolln and Pankow
dev.copy(png,'neigh.png', width=700)
dev.off() 

#4 No of Reviews + minimum of nights per neighbourhood grouped by price
qplot(x=airbnb$number_of_reviews, y=minimum_nights, col=airbnb$neighbourhood_group, main="", data=airbnb) +
  facet_wrap(~ price2) +
  theme_minimal(base_size=18) +
  xlab("No of reviews") +
  ylab("Minimum no of nights") +
  scale_colour_discrete(name="Neighbourhood groups")
dev.copy(png,'noofreviewsminnights.png', width=1000)
dev.off() 

summary(airbnb$minimum_nights)
#most owners require minimum 1-2 nights, maximum is 360 > meaning almost whole year

#5 No of reviews and price per neighbourhood 
qplot(x=number_of_reviews, y=price, color = factor(neighbourhood_group),  main="", data=airbnb) +
  theme_minimal(base_size=15) +
  facet_wrap(~neighbourhood_group)+
  xlab("No of reviews") +
  ylab("Price") +
  labs(colour='Neighbourhood') +
  geom_hline(aes(yintercept = 100), colour="azure4")

dev.copy(png,'noofreviewsprice.png', width=800)
dev.off() 

#5.2 A closer look at No of reviews and price per neighbourhood, prices limited to 500, which causes 13/16k airbnbs to be excluded
qplot(x=airbnb$number_of_reviews, y=price, col=airbnb$neighbourhood_group, alpha = 0.9 , main="", data=airbnb) +
  theme_minimal(base_size=18) +
  xlab("No of reviews") +
  ylab("Price") +
  scale_colour_discrete(name="Neighbourhood groups")+
  scale_y_continuous(limits=c(0, 500), breaks=seq(0,500, by=100)) +
  geom_hline(aes(yintercept = 100), colour="azure4")
dev.copy(png,'noofreviewspricelim.png')
dev.off() 

#6 Type of airbnb and neighbourhood group
qplot(x=airbnb$room_type, main="", data=airbnb) +
  theme_minimal(base_size=18) +
  xlab("Type of airbnb") +
  ylab("Freq") +
  geom_bar(aes(fill = airbnb$neighbourhood_group)) +
  scale_fill_discrete(name="Neighbourhood groups") + coord_flip()
dev.copy(png,'type.png')
dev.off() 

#6.2 Type of airbnb and neighbourhood group grouped by price group
qplot(x=airbnb$room_type, main="", data=airbnb) +
  facet_wrap(~ price2) +
  theme_minimal(base_size=18) +
  xlab("Type of airbnb") +
  ylab("Freq") +
  geom_bar(aes(fill = airbnb$neighbourhood_group)) +
  scale_fill_discrete(name="Neighbourhood groups") + coord_flip()
dev.copy(png,'type2.png', width=1000)
dev.off() 

#7 Prices and room types - violin
  
qplot(x=airbnb$room_type, y=price, main="", data=airbnb) +
  #ylim(0, 200)+
  theme_minimal(base_size=18) +
  xlab("Airbnb type") +
  ylab("Price") +
  geom_violin( fill="#FF9999", colour="black") 

  dev.copy(png,'typepriceviolin.png')
  dev.off() 
  
  summary(airbnb$room_type)
#Entire home/apt    Private room     Shared room 
#      8301            7651             194


#9heatmap Neighbourhood groups - not very visible changes
hm <- ggplot(airbnb, aes(room_type,neighbourhood_group ))
hm + geom_raster(aes(fill = price), hjust=0.5, 
                 vjust=0.5, interpolate=FALSE)  + 
  scale_fill_continuous(name = "Neighbourhood groups") +
  theme(axis.text.x = element_text(size = rel(.8)), axis.text.y = element_text(size = rel(0.5)))
dev.copy(png,'hm.png')
dev.off() 

#10 heatmap smaller Neighbourhood
hm2 <- ggplot(airbnb, aes(room_type,neighbourhood ))
hm2 + geom_raster(aes(fill = price), hjust=0.5, 
                 vjust=0.5, interpolate=FALSE)  + 
  scale_fill_continuous(name = "Neighbourhood") +
  theme(axis.text.x = element_text(size = rel(.8)), axis.text.y = element_text(size = rel(0.6)))
dev.copy(png,'hm2.png', width=1000)
dev.off() 

#11 Are general no of reviews, monthly no of reviews and price somehow connected
ggplot(airbnb, aes(x=number_of_reviews, y= reviews_per_month))+
  geom_point(aes(colour = factor(price2)))+
  scale_x_discrete(name="No of reviews")+
  scale_y_continuous(name="Monthly no of reviews") +
  theme_bw(base_size=18) +
  labs(colour='Price') 
dev.copy(png,'noreviewsmonthly.png', width=800)
dev.off() 


#12.1 availability vs price - incl. v expensive and 3 months+ line
qplot(x=availability_365, y=price, col=neighbourhood_group, main="", data=airbnb) +
  geom_point()+
  theme_minimal(base_size=18) +
  xlab("Availability") +
  ylab("Price") +
  scale_colour_discrete(name="Neighbourhood groups")+
  scale_x_discrete(limits=c(seq(0, 365, by=60))) +
  geom_hline(aes(yintercept = 100), colour="blACK")+ #v expensive
  geom_vline(aes(xintercept = 90), colour="BLACK") #3 months+
dev.copy(png,'avail.png', width=1)
dev.off() 

#12.2 availability 
qplot(x=airbnb$availability_365,  main="", data=airbnb) +
  geom_histogram(colour="mintcream", fill="blue4")+
  theme_minimal(base_size=18) +
  xlab("Availability")+
  ylab("Freq") +
  scale_x_continuous(breaks=seq(0,365,30)) 
dev.copy(png,'avail2.png')
dev.off() 

#12.3 Close up on long availability
qplot(x=airbnb$availability_365,  main="Long availability", data=airbnb) +
  geom_histogram(colour="mintcream", fill="blue4")+
  theme_minimal(base_size=18) +
  xlab("Availability")+
  ylab("Freq") +
  coord_cartesian(xlim = c(90,365), ylim=c(0,500))
dev.copy(png,'avail3.png')
dev.off() 

# 12.4 avail per room type
qplot(x=airbnb$availability_365,  main="Long availability", data=airbnb) +
  geom_histogram(colour="mintcream", fill="navyblue")+
  theme_minimal(base_size=18) +
  xlab("Availability")+
  facet_wrap(~ room_type) +
  ylab("Freq") +
  coord_cartesian(xlim = c(90,365), ylim=c(0,500))

dev.copy(png,'availlong.png', width=1000)
dev.off() 
