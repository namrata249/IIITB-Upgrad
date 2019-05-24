
# Namrata Khatri
# 8/4/2019
# Recommendation System Assignment
rm(list=ls())
#setwd("~/Documents/Learning/IIITb/Course6/Assignment")
library(dplyr)
library(ggplot2)
library(scales)
library(recommenderlab)
#0.Data import and cleaning
beer_raw<-read.csv("beer_data.csv",header = TRUE,na.strings = c(""," "))
str(beer_raw)
head(beer_raw)
dim(beer_raw)
#check for na in beer
lapply(beer_raw,function(x){sum(is.na(x))})
# $review_profilename
# [1] 100
#Remove all NAs from review_profilename
beer<-beer_raw[which(!is.na(beer_raw$review_profilename)),]
# Some beers have same user review multiple time 
beer<-beer %>% group_by(review_profilename,beer_beerid) %>% mutate(n=n())%>%ungroup()
nrow(beer[which(beer$n>1),]) #[1] 2829
# taking first review in table as valid review, removing other instances
beer <-beer[which(beer$n==1),]

#Ratings 1 to 5 are valid ratings
#Distinct ratings..
table(beer$review_overall) #6 reviews has 0 rating
#0 is not valid ratings: Removing it
beer<-beer[which(beer$review_overall!=0),]

#1.Data preparation

#1.Choose only those beers that have at least N number of reviews

#Figure out an appropriate value of N using EDA; this may not have one 
#correct answer, but you shouldn't choose beers having extremely low number 
#of ratings

#Distinct Beers
length(unique(beer$beer_beerid))#40278
#Distinct Users
length(unique(beer$review_profilename)) #22493

#Count no of reviews per beer
beer_N_review<-beer%>%
        group_by(beer_beerid)%>%
        summarise(review_count=n())%>%
        arrange(desc(review_count))

#Again count beer based on no of reviews it got
beer_count_vs_N_review<-beer_N_review%>%
        group_by(review_count)%>%
        summarise(beer_count=n())%>%
        arrange(desc(beer_count))

ggplot(beer_count_vs_N_review,aes(x=beer_count,y=review_count))+
        geom_point(alpha=0.3,color='blue')+
        labs(title="Beer count vs Review count",x="No of Beers",y="No of Reviews")
#large no of beers got very less reviews
#Lets see log scale view of the same data to see the flow properly
ggplot(beer_count_vs_N_review,aes(x=beer_count,y=review_count))+
        geom_jitter(alpha=0.3,color='blue',width=0.3)+
        scale_x_log10(breaks =c(1,10,100,1000,10000))+
        scale_y_log10(breaks =c(1,10,30,40,50,100,500,1000))+
        theme(panel.grid.minor = element_blank())+
        labs(title="Beer count vs Review count",x="No of Beers",y="No of Reviews")
# we can easily see here sparce points between 100 to 10000 NoOfBeer and 
# NoOfReviews (1:40)
# No of reviews we can take N can be between 30 to 50
# seting N=40   
sum(beer_count_vs_N_review[which(beer_count_vs_N_review$review_count<40),2])
#No of beers [1] 37782
sum(beer_count_vs_N_review[which(beer_count_vs_N_review$review_count>=40),2])
#No of beers 2496

#lets see user review relation
user_N_review<-beer%>%
        group_by(review_profilename)%>%
        summarise(review_count=n())%>%
        arrange(desc(review_count))

user_count_vs_N_review<-user_N_review%>%
        group_by(review_count)%>%
        summarise(user_count=n())%>%
        arrange(desc(user_count))

ggplot(user_count_vs_N_review,aes(x=user_count,y=review_count))+
        geom_point(alpha=0.3,color='blue',position = "jitter")+
        labs(title="Beer count vs Review count",x="No of Users",y="No of Reviews")
#large no of users has given less reviews
#Lets see log scale view of the same data
ggplot(user_count_vs_N_review,aes(x=user_count,y=review_count))+
        geom_jitter(alpha=0.3,color='blue',width=0.3)+
        scale_x_log10(breaks =c(1,10,100,1000,5000,8000))+
        scale_y_log10(breaks =c(1,10,30,40,50,100,500,1000,1500))+
        theme(panel.grid.minor = element_blank())+
        labs(title="Beer count vs Review count",x="No of Users",y="No of Reviews")
#here we van see No of reviews can be between 30:50
#lets set  N=40  
sum(user_count_vs_N_review[which(user_count_vs_N_review$review_count<40),2])
#No of users who has reviewed less than 10 beers:[1] 19890
sum(user_count_vs_N_review[which(user_count_vs_N_review$review_count>=40),2])
#No of users who has reviewed greater  than 10 beers:[1] 2603

# in both beer vs no of review and user vs no of reviews 
# cut off No of reviews N = 40 captures most of the user inputs of beer

# we have to find ot all the users who has more than 40 reviews
# and beers which has more than 40 reviews
select_beer<-beer_N_review%>%filter(review_count>=40)
select_user<-user_N_review%>%filter(review_count>=40)
nrow(select_beer)#[1]  2496
nrow(select_user)#[1] 2603

beer_new<-beer%>%filter(beer_beerid%in%select_beer$beer_beerid & 
                                review_profilename%in%select_user$review_profilename)

table(beer_new$review_overall)
plot(table(beer_new$review_overall),xlab="Ratings",ylab="Count")
beer_new<-as.data.frame(beer_new)

#2.Convert this data frame to a “realratingMatrix” before you build your 
#collaborative filtering models

beer_r<- as(beer_new[,c(2,1,3)], "realRatingMatrix")
class(beer_r)

# Data Exploration
head(dimnames(beer_r))
head(rowCounts(beer_r))
head(colCounts(beer_r))
head(rowMeans(beer_r))

beer_df <- as(beer_r, "data.frame")
str(beer_df)

#2.Data Exploration
#        1.Determine how similar the first ten users are with each other 
#        and visualise it
similar_users <- similarity(beer_r[1:10,],
                            method = "cosine",
                            which = "users")
#Similarity matrix
as.matrix(similar_users)
#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#       2.Compute and visualise the similarity between the first 10 beers
similar_beer <- similarity(beer_r[,1:10],
                           method = "cosine",
                           which = "items")
#Similarity matrix
as.matrix(similar_beer)
#Visualise similarity matrix
image(as.matrix(similar_beer), main = "Beer similarity")

#       3.What are the unique values of ratings?
table(beer_df$rating)
# 1   1.5     2   2.5     3   3.5     4   4.5     5 
# 970  1166  3917  6198 18642 38049 77710 44749 11245 
length(unique(beer_df$rating)) #[1] 9

#       4.Visualise the rating values and notice:
beer_df %>% 
        group_by(user) %>% 
        summarize(mean_user_rating = mean(rating)) %>% 
        ggplot(aes(mean_user_rating)) +
        geom_histogram(fill = "cadetblue3", color = "grey20")
# average user rating is little right skewed
beer_df %>% 
        group_by(item) %>% 
        summarize(number_of_ratings_per_beer = n()) %>% 
        ggplot(aes(number_of_ratings_per_beer)) + 
        geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0,200))

beer_df %>% 
        ggplot(aes(x = rating, fill = factor(rating))) +
        geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE)


#               1.The average beer ratings
        beer_df %>% 
        group_by(item) %>% 
        summarize(mean_ratings_per_beer = mean(rating))%>%summary()

summary(colMeans(beer_r))
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max. 
# 1.272   3.644   3.853   3.795   4.032   4.628 
#               2.The average user ratings
        beer_df %>% 
        group_by(user) %>% 
        summarize(mean_ratings_per_user = mean(rating))%>%summary()
summary(rowMeans(beer_r))
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max. 
# 2.859   3.747   3.887   3.865   4.009   4.442 
#               3.The average number of ratings given to the beers
        beer_df%>%
        group_by(item)%>%
        summarise(Count=n())%>%summary()
#       Mean   : 92.26  
#               4.The average number of ratings given by the users
        beer_df%>%
        group_by(user)%>%
        summarise(Count=n())%>%summary()
#       Mean   : 88.47 
#       
#3.Recommendation Models

#       1.Divide your data into training and testing datasets
#       Experiment with 'split' and 'cross-validation' evaluation schemes
#       Build IBCF and UBCF models
#       
#       Here train test ratio is 80:20
#       given : -1  items given for evalution - All but one 
#       goodRating = 4 threshold at whichratings are considered good
scheme1 <- evaluationScheme(beer_r, method = "split", train = .8,
                            k = 1, given = -1, goodRating = 4)
#       K=5 folds for cross validation
scheme2 <- evaluationScheme(beer_r, method = "cross-validation", train = .9,
                            k = 5, given = -1, goodRating = 4)

#       2.Compare the performance of the two models and suggest the one 
#       that should be deployed

        algorithms <- list(
        "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                       method="Cosine",
                                                       nn=50)),
        "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
        ))
        )


        results_split <- evaluate(scheme1, algorithms, n=c(1, 3, 5, 10, 15, 20))
        results_cv <- evaluate(scheme2, algorithms, n=c(1, 3, 5, 10, 15, 20))


#       3.Plot the ROC curves for UBCF and IBCF and compare them
        plot(results_split, annotate = 1:4, legend="topleft")
        title(main = "ROC - Split method")
#       Area under curve is greater for method"UBCF" than "IBCF" in
#       split method. n=20
        plot(results_cv, annotate = 1:4, legend="topleft")
        title(main = "ROC - Cross Validation method")
#       Area under curve is greater for method"UBCF" than "IBCF" in
#       Cross Validation (K=5) method.
#       User based collaborative filtering gives more area under ROC thats
#       why more desirable than item based collaborative filtering model.
#       
#       Again here cross validation shows better result than split method
#       So relatively good model here is : Cross Validation "UBCF"

#       4.Give the names of the top 5 beers that you would recommend to the 
#       users "cokes", "genog" & "giblet"

    
        rec <- Recommender(beer_r, method = "UBCF") 
        rec
        
        recom_cokes <- predict(rec,  beer_r[c('cokes','genog','giblet')], n=5)
        as(recom_cokes, "list")
        # $cokes
        # [1] "34"    "41815" "1904"  "22227" "34420"
        # 
        # $genog
        # [1] "7971" "2093" "276"  "571"  "2512"
        # 
        # $giblet
        # [1] "10325" "1545"  "7971"  "29619" "4083" 