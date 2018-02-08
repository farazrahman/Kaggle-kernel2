#1. Introduction- Chocolate EDA
# 1.1- Load libraries and data files

library('readr') # data input
library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('corrplot') # visualization
library('lubridate') # date and time
library("purrr")# data manipulation
library("stringr")#String manipulation


flavors_of_cacao <- read_csv("flavors_of_cacao.csv")

glimpse(flavors_of_cacao) 
summary(flavors_of_cacao)

#Observation:
#i. Small dataset with 1796 observations 
#ii. 9 variables

#We can see that there is an extra row at the top which is not required, we can get rid of that row




#2. DATA CLEANING 

#Rename the column for better readability
names(flavors_of_cacao)[1:9] <- c("Company", "Sp_origin", "Reference", "Review_date", "Cocoa_percent", "Company_location", "Rating", "Bean_type", "Broad_origin")

#Treating incorrect data types- Cocoa Percent

#Cocoa percent is in character format, we can remove the % in the end of each number 

flavors_of_cacao$Cocoa_percent <- gsub("[%]", "", flavors_of_cacao$Cocoa_percent)
flavors_of_cacao$Cocoa_percent <- as.numeric(flavors_of_cacao$Cocoa_percent)


#Missingness
flavors_of_cacao$Bean_type[flavors_of_cacao$Bean_type=="Â"] <- "NA"
flavors_of_cacao$Broad_origin[flavors_of_cacao$Broad_origin=="Â"] <- "NA"

flavors_of_cacao[] <- lapply(flavors_of_cacao, str_trim)
is.na(flavors_of_cacao) <- flavors_of_cacao==''

colMeans(is.na(flavors_of_cacao))




#49% data for bean type is missing and 4% of Broad origin is missing, we will treat it as a category as of now.

#Exploratory Analysis

#Univariate analysis
#i. Company
unique(flavors_of_cacao$Company)# Around 416 unique companies are there

#We will which are the top 10 companies, featured in this review.
top_companies <- flavors_of_cacao %>% group_by(Company) %>% summarise(Count= n())%>%
  top_n(10, wt = Count)%>%arrange(desc(Count))

ggplot(top_companies, aes(reorder(Company, -Count),  Count, fill = Count)) + 
  geom_bar(stat = "identity") +scale_fill_continuous(trans = 'reverse')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

#ii. Review Date

flavors_of_cacao %>% group_by(Review_date) %>% summarise(Count= n())%>%
  ggplot(aes(x =Review_date, y = Count, fill = Count)) + geom_bar(stat = "identity") + 
  scale_fill_continuous(trans = 'reverse')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

#We see that maximum number of reviews happened in the year 2015

#iii. Cocoa percent

  ggplot(flavors_of_cacao ,aes(x =Cocoa_percent)) + geom_histogram(bins = 60, fill = "blue") + 
  scale_fill_continuous(trans = 'reverse')+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))
#70% Cocoa is the maximum percentage of cocoa present in a chocolate bar and varies from 40-100 percent.
  
#iv. Company Location
  
  flavors_of_cacao %>% group_by(Company_location) %>% summarise(Count= n())%>% mutate(pct=Count/sum(Count)) %>%
    ggplot(aes(x =reorder(Company_location,pct), y =pct, fill = pct)) + geom_bar(stat = "identity") + 
    scale_fill_continuous(trans = 'reverse')+coord_flip()+xlab("Countries") +ylab("Percentage")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))
#Most of the Chocolate Manufacturers are based in USA, almost above 40%
  
#v. Rating
  
  ggplot(flavors_of_cacao, aes(factor(Rating))) + geom_bar(fill = "dark green")

 # We can create categories in terms of ratings such as Poor, average, good, excellent
  
  flavors_of_cacao <- mutate(flavors_of_cacao, Rating_grp = Rating)
  
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp <= 1] <- "Poor"
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp >1 & flavors_of_cacao$Rating_grp <= 2] <- 'Fair'
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp >2 & flavors_of_cacao$Rating_grp <= 3] <- 'Average'
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp >3 & flavors_of_cacao$Rating_grp <= 4] <- 'Good'
 flavors_of_cacao$Rating_grp[flavors_of_cacao$Rating_grp >4 & flavors_of_cacao$Rating_grp <= 5] <- 'Excellent'
 
 flavors_of_cacao %>% group_by(Rating_grp) %>% summarise(Count= n())%>% mutate(pct=Count/sum(Count)) %>%
   ggplot(aes(x =reorder(Rating_grp,-pct), y =pct, fill = pct)) + geom_bar(stat = "identity") + 
   scale_fill_continuous(trans = 'reverse')+xlab("Ratings") +ylab("Percentage")+
   theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

 #We see that maximum people have given a Good rating to the Chocolate bar, i.e. a rating between 3-4. Very less people have given an excellent rating.
 
#vi. Bean type(The breed of bean used)
 unique(flavors_of_cacao$Bean_type, na.rm= True)
 
 flavors_of_cacao %>% group_by(Bean_type) %>% summarise(Count= n())%>% mutate(pct=Count/sum(Count)) %>%
   ggplot(aes(x =reorder(Bean_type,-pct), y =pct, fill = pct)) + geom_bar(stat = "identity") + 
   scale_fill_continuous(trans = 'reverse')+xlab("Bean_Type") +ylab("Percentage")+
   theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))
 
 
 #vii. Broad origin
 
 unique(flavors_of_cacao$Broad_origin, na.rm= True)
 #We can see that so many unique specific origins are there, 
 #we can extract the first word from specific origin column to find the unique specific origin
 
 flavors_of_cacao %>% group_by(Broad_origin) %>% summarise(Count= n())%>% mutate(pct=Count/sum(Count)) %>%
   ggplot(aes(x =reorder(Broad_origin,-pct), y =pct, fill = pct)) + geom_bar(stat = "identity") + 
   scale_fill_continuous(trans = 'reverse')+xlab("Broad origin") +ylab("Percentage")+
   theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))
 
#Bivariate analysis
 
# What's the relationship between cocoa solids percentage and rating?
 flavors_of_cacao <- mutate(flavors_of_cacao, Cocoa.pct = Cocoa_percent)
 
 flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=40 & flavors_of_cacao$Cocoa.pct < 50] <- '40-49pct'
 flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=50 & flavors_of_cacao$Cocoa.pct < 60] <- '50-59pct'
 flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=60 & flavors_of_cacao$Cocoa.pct < 70] <- '60-69pct'
 flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=70 & flavors_of_cacao$Cocoa.pct < 80] <- '70-79pct'
 flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=80 & flavors_of_cacao$Cocoa.pct < 90] <- '80-89pct'
 flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct >=90 & flavors_of_cacao$Cocoa.pct <= 99] <- '90-99pct'
 flavors_of_cacao$Cocoa.pct[flavors_of_cacao$Cocoa.pct== 100] <- '100pct'
 
 unique(flavors_of_cacao$Cocoa.pct)

   
 ggplot(flavors_of_cacao ,aes(x =Cocoa.pct, fill = Rating_grp)) + geom_bar(position = "dodge") + 
   theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

 ggplot(flavors_of_cacao ,aes(x =Rating_grp, fill =Cocoa.pct )) + geom_bar(position = "fill") + 
   theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

 ggplot(flavors_of_cacao ,aes(x =Cocoa.pct, y = Rating_grp, fill = Rating)) + geom_tile() + 
   theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1)) 

 