cat("\014") #clear the console
rm(list=ls()) #dclearing the environment variables

library(data.table)
library(ggplot2)

# loading raw data
#dataset = read.csv("severeinjury1.csv")
dataset$Address = paste(dataset$Address1, dataset$Address2)
dataset$Address1 = NULL
dataset$Address2 = NULL
dataset = as.data.frame(dataset)
dataset = dataset[c(1,2,3,4,25,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

#loadingPre-processed data
dataset = read.csv("processed_data.csv")
summary(dataset)

#function to convert first letter as a character
first_character_uppercase = function(x) {
     substr(x,1,1) = toupper(substr(x,1,1))
  x
}

#dataset$Employer = paste(toupper(substr(dataset$Employer, 1, 1)), 
#substr(dataset$Employer, 2, nchar(dataset$Employer)), sep="")
dataset$EventDate.1 = NULL

dataset$Employer = tolower(dataset$Employer)
dataset$City = tolower(dataset$City)
dataset$State = tolower(dataset$State)
dataset$Part.of.Body.Title = tolower(dataset$Part.of.Body.Title)
dataset$Employer = first_character_uppercase(dataset$Employer)
dataset$City = first_character_uppercase(dataset$City)
dataset$State = first_character_uppercase(dataset$State)
dataset$Part.of.Body.Title = first_character_uppercase(dataset$Part.of.Body.Title)

dataset$count = rep(1, nrow(dataset))

dt = data.table(dataset)
injurycount_state = dt[,sum(count),by=State]
colnames(injurycount_state)[2] = "count"
#rm(dt)

#state_count = ggplot(injurycount_state, aes(x=State, y=Count))+geom_point()+ ggtitle("State vs count of the injury")+xlab("State")+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
state_count = ggplot(injurycount_state, aes(x=State, y=Count))+geom_bar(stat = "identity")+ ggtitle("State vs count of the injury")+xlab("State")+ylab("Count")+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
colnames(dataset)[27] = "Industry_codes"

dataset$industrynames = ifelse(dataset$Industry_codes == "11", "Agricultural, Forestry, Fishing and Hunting", 
                        ifelse(dataset$Industry_codes == "21", "Mining, Quarrying, Oil and Gas extraction",
                        ifelse(dataset$Industry_codes == "22", "Utilities", 
                        ifelse(dataset$Industry_codes == "23", "Construction", 
                        ifelse(dataset$Industry_codes == "31", "Manufacturing", 
                        ifelse(dataset$Industry_codes == "32", "Manufacturing", 
                        ifelse(dataset$Industry_codes == "33", "Manufacturing", 
                        ifelse(dataset$Industry_codes == "42", "Wholesale Trade", 
                        ifelse(dataset$Industry_codes == "44", "Retail Trade", 
                        ifelse(dataset$Industry_codes == "45", "Retail Trade", 
                        ifelse(dataset$Industry_codes == "48", "Transportation and Warehousing", 
                        ifelse(dataset$Industry_codes == "49", "Transportation and Warehousing", 
                        ifelse(dataset$Industry_codes == "51", "Information", 
                        ifelse(dataset$Industry_codes == "52", "Finance and Insurance", 
                        ifelse(dataset$Industry_codes == "53", "Real Estate and Rental and Leasing", 
                        ifelse(dataset$Industry_codes == "54", "Professional, Scientific and Technical services",
                        ifelse(dataset$Industry_codes == "55", "Management of Companies and Enterprises",
                        ifelse(dataset$Industry_codes == "56", "Administrative and Support services", 
                        ifelse(dataset$Industry_codes == "61", "Educational Service", 
                        ifelse(dataset$Industry_codes == "62", "Health Care and Social Assistance",
                        ifelse(dataset$Industry_codes == "71", "Arts, Entertainment and Recreation",
                        ifelse(dataset$Industry_codes == "72", "Accomodation and Food services",
                        ifelse(dataset$Industry_codes == "81", "Other services", "Public Administartion")))))))))))))))))))))))

dt1 = data.table(dataset)
injurycount_industry = dt1[,sum(count),by=Industrynames]
colnames(injurycount_industry)[2] = "count"
#rm(dt)

industry_count = ggplot(injurycount_industry, aes(x=Industrynames, y=count))+geom_point(color = "blue", size = 3, shape= 17)+ ggtitle("Industry vs count of the injury")+xlab("Industry")+ylab("Count")+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
colnames(dataset)[27] = "Industrycode"

#plot = ggplot()+geom_point(dataset, aes(x=dataset$))

#rmarkdown::render("dataset_script.R", "html_document")
# multiplot(state_count, industry_count)
print(state_count)
print(industry_count)
 
Injuryprone_area = dataset[dataset$State == "Texas",]

dt2 = data.table(Injuryprone_area)
injurycount_city = dt2[,sum(count),by=City]
colnames(injurycount_city)[2] = "count"
#rm(dt)

Maxinjuries_city = injurycount_city[injurycount_city$count > 30]
city_count = ggplot(Maxinjuries_city, aes(x=City, y=count))+geom_bar(stat = "identity")+ ggtitle("Areas where maximum injuries occurred in Texas")+xlab("City")+ylab("Count")+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
# colnames(dataset)[27] = "Industrycode"

print(city_count)
#rmarkdown::render("Severeinjury_Rscript.R", "html_document")

#naics = read.csv(2012_NAICS_Structure.csv")
#new = dataset %>%
#dataset = dataset[c(1,2,3,4,25,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

dataset$EventDate = as.character(dataset$EventDate)
dataset$Year = substring(dataset$EventDate,7,10)
dataset$Month = substring(dataset$EventDate,4,5)
dataset$Day = substring(dataset$EventDate,1,2)
#dt = subset(dataset,dataet$Year == "2017")

dt = data.table(dataset)
injurycount_year = dt[,sum(count),by=Year]
colnames(injurycount_year)[2] = "count"
#rm(dt)

#state_count = ggplot(injurycount_state, aes(x=State, y=Count))+geom_point()+ ggtitle("State vs count of the injury")+xlab("State")+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
state_count = ggplot(injurycount_year, aes(x=Year, y=count))+geom_bar(stat = "identity")+ ggtitle("Year vs count of the injury")+xlab("Year")+ylab("Count")+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))

Amputation_dataset = subset(dataset, dataset$Amputation == "1" | dataset$Amputation =="2" | dataset$Amputation =="3" | dataset$Amputation =="4" | dataset$Amputation == "9")
dt1 = data.table(Amputation_dataset)
injurycount_industry = dt1[,sum(count),by=Industrynames]
colnames(injurycount_industry)[2] = "count"

ggplot(injurycount_industry, aes(x=Industrynames, y=count))+geom_point(color = "blue", size = 3, shape= 17)+ ggtitle("Industry vs count of the finger injury")+xlab("Industry")+ylab("Count")+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# preprocessed_data = write.csv(dataset,"preprocessed_data.csv")
#rm(dt,dt1,dt2,naics,new,injurycount_city,injurycount_industry,injurycount_state,Injuryprone_area,injurycount_year,city_count,industry_count,state_count,Maxinjuries_city)
#y = dataset[,c("Amputation","Hospitalized")]
#x = dataset[,c("Latitude","Longitude","Zip","Industrycodes")]
# cor(x,y,use="complete.obs",method = "kendall")
# cor(x,y,use="complete.obs",method = "pearson")
# cor(x,y,use="complete.obs",method = "spearman")

#therefore in correlation we have found that the relation is not linear hence therefore we are using ridge and jackknife regression
#dt$na = is.na(dt$Primary.NAICS)


