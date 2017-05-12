#------------------------SNA PROJECT - ANALYSING BUSINESSES IN CHARLOTTE REGION-------------------------#
#-----------------------------------Arun Kumar Krishnamurthy-----------------------------#
#----------------------------------------Pranjal Gandhi---------------------------------------#
#--------------------------------------Ephraim Schoenbrun----------------------------------#
#------------------------------------Thushara Elizabeth Tom-------------------------------#

library('stringr')
library('tidyr')
library('dplyr')
library('magrittr')
library('jsonlite')
library(igraph)
library('rgexf')

#Loading the business dataset from Yelp
business <- stream_in(file("yelp_academic_dataset_business.json"))

#Subsetting for "NC" State
yelpB_NC <- data.frame(lapply(yelpB_NC, as.character), stringsAsFactors=FALSE)
write.csv(yelpB_NC,"yelp_NC.csv")

#Loading the review dataset from Yelp
review <- "yelp_academic_dataset_review.json"
con <- file(review, "r")
input_review <- readLines(con, -1L)
close(con)

#Loading the users dataset
review <- "yelp_academic_dataset_users.json"
con <- file(users, "r")
input_users <- readLines(con, -1L)
close(con)

#Loading Yelp NC business Data
yelp_NC <- read.csv('yelp_NC.csv')
#Subsetting businesses for City Charlotte 
business_charlotte <- yelp_NC[which(yelp_NC$city =='Charlotte'),]
business_charlotte <- data.frame(lapply(business_charlotte, as.character), stringsAsFactors=FALSE)
business_charlotte_names <- business_charlotte[,c("business_id","name")]
write.csv(business_charlotte_names,"business_charlotte_names.csv")

#Loading Yelp NC Reviews data
review_NC <- read.csv('review_NC.csv')
#Subsetting reviews for City Charlotte
review_NC <- data.frame(lapply(review_NC, as.character), stringsAsFactors=FALSE)
review_charlotte <- sqldf::sqldf('select b.name,a.* from review_NC as a inner join business_charlotte as b where a.business_id = b.business_id')

#Top_10_Reviewers in Charlotte
top_10_charlotte <- count(review_charlotte,user_id,sort = TRUE) %>% top_n(10)


#Retrieving user_id and business_id columns
user_business <- review_charlotte %>% select(user_id,name)
#user_business$business_id <- gsub("-", "_", paste(user_business$business_id))
#user_business$business_id <- gsub("-", "_", paste(user_business$business_id))

#Removing the node with maximum users
user_business <- sqldf::sqldf('select * from user_business where business_id !=  "gG9z6zr_49LocyCTvSFg0w" ')

#creating the connected data - between businesses by users
x3 <- x2 <- user_business[,c("user_id","name")]
names(x2) <- c('user_id', 'business1')                                                              
names(x3) <- c('user_id', 'business2')   
merged_data <- merge(x2, x3)  
test <- merged_data$business1 == merged_data$business2  
merged_data <- merged_data[!test, ] 
indx <- !duplicated(t(apply(merged_data, 1, sort)))
merged_data <- merged_data[indx,]
charlotte_gephi_yelp <- read.csv("Charlotte_Gephi_Edges.csv")


#Counting the number of users between businesses
user_business2 <- sqldf::sqldf('select business1,business2,count(user_id) as number_of_users from merged_data group by business1,business2')
#Removing businesses connected by just one user
user_business3 <- sqldf::sqldf('select business1,business2, number_of_users from user_business2 where number_of_users > 1') 

#Write into csv file
write.csv(user_business3,"user_business3.csv")

#Converting to a graph object and removing redundant edges
graph_charlotte <- graph.data.frame(charlotte_gephi_yelp)
gC <- simplify(graph_charlotte)

#Calculate basic network metrics
Cdegree <- degree(gC)
Cbetweenness <- betweenness(gC)
Ccloseness <- closeness(gC)
CevcentTemp <- evcent(gC)
Cevcent <- CevcentTemp$vector

#Creating a dataframe of the metrics above
statsC <- data.frame(V(graph_charlotte)$name,Cdegree,Cevcent,Ccloseness,Cbetweenness)
correlationC <- cor(statsC[,2:5])

#Creating a graph file to load in Gephi
gC.gexf <- igraph.to.gexf(gC)

# You have to create a file connection.
f <- file("gC.gexf")
writeLines(gC.gexf$graph, con = f)
close(f)
