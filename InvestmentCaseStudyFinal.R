#delete all environment data
rm(list = ls())

#load Library
library("tidyr")
library("dplyr")
library("stringr")

 
#import data
#<------------------------------------Getting Data ---------------------------->
#For MAC
# companies <- read.csv('companies.csv',header=TRUE,stringsAsFactors = FALSE,na.strings = "")

# For windows :--
companies <- read.delim(file = 'companies.txt',header=TRUE,
                      sep = "\t",stringsAsFactors = FALSE,na.strings = "")

rounds2 <- read.csv("rounds2.csv",header=TRUE,
                    stringsAsFactors = FALSE,na.strings = "")

mapping <- read.csv("mapping.csv",header=TRUE,na.strings = "")
 
#<---------------------------- Data Cleaning/Preparation ----------------------> 

#convert company link into lower case in rounds2 and companies
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink)
companies$permalink <- str_to_lower(companies$permalink)


#*******************************************************************************
#CHECKPOINT 1
#<---- table1.1------------>

# Q1. Unique companies in rounds2 table
        distinct_rounds_company<-distinct(rounds2,company_permalink)
        nrow(distinct_rounds_company)
        #[1] 66368
        # there is 66368 distinct companies in rounds2

# Q2. Unique companies in company data frame

        distinct_company<-distinct(companies,permalink)
        nrow(distinct(companies,companies$permalink))
        #[1] 66368

# Q3. In the companies data frame, which column can be used as the unique key 
# for each company? 
# Write the name of the column.
        #Answer = permalink

# Q4. Are there any companies in the rounds2 file which are not present in 
# companies ? Answer Y/N.
        rounds2_not_in_companies<-anti_join(distinct_company,
                                          distinct_rounds_company,
                                          by=c("permalink"="company_permalink"))
        nrow(rounds2_not_in_companies)
        #[1] 0
        
# Answer = No

# Q5. Merge the two data frames so that all variables (columns) in the companies
# frame are added to the rounds2 data frame.Name the merged frame master_frame.
# How many observations are present in master_frame ?

#inner join
master_frame <-merge(rounds2,companies,
                     by.x = "company_permalink",
                     by.y = "permalink")
dim(master_frame)
# [1] 114949     15

#*******************************************************************************
# CHECKPOINT 2
#<------  Find Funding Type -------->

# Data preparation before starting Analysis
# converting data in one uniform case (upper,lower or title)
master_frame$country_code <- as.factor(str_to_upper(master_frame$country_code))
master_frame$funding_round_type <- as.factor(str_to_lower(master_frame$funding_round_type))

# Table 2.1
# Average Values of Investments for Each of these Funding Types
# filter 4 funding type data 
# group by then on funding type
# calculate average on raised_amount_usd
# then arrage the result in decreasing order
summerized_funding_type<-filter(master_frame,funding_round_type%in% 
                                c("venture","angel","seed","private_equity"))%>%
                         group_by(funding_round_type)%>%
                         summarize_at(.vars = "raised_amount_usd" ,
                                     .fun = mean,na.rm = TRUE) %>%
                         arrange(desc(raised_amount_usd))

summerized_funding_type
# funding_round_type raised_amount_usd
# <fct>                          <dbl>
# 1 private_equity             73308593.
# 2 venture                    11748949.
# 3 angel                        958694.
# 4 seed                         719818.

filter(summerized_funding_type,raised_amount_usd>=5000000,
                         raised_amount_usd<=15000000)

# 1 venture                    11748949.
## summary: funding type = venture  where 5000000>= avg(raised fund) <=15000000
# is the suitable investment type for Spark fund 


#*******************************************************************************
# CHECKPOINT 3   Country Analysis
#<--------- data preparation--------->

# Narrow down our search in FT = 'Venture' get data slice of 'venture' FT
master_frame_venture <- filter(master_frame,funding_round_type == "venture")
# Remove NA from country_code
master_frame_venture <- master_frame_venture[!is.na(master_frame_venture$country_code),]

#<----------------------------------------------------------------------------->
# Table 3.1: Analysing the Top 3 English-Speaking Countries
# 1. Top English-speaking country	              
# 2. Second English-speaking country	 
# 3. Third English-speaking country
#<----------------------------------------------------------------------------->
#To find out English Speaking Country using R 
#library("countrycode")
# english_sc<-read.csv("Countries_with_english_as_official_language.csv",
#                      header=TRUE,stringsAsFactors = FALSE,na.strings = "")
# # English_speaking col is created 
# english_sc<-countrycode(english_sc$english_speaking_country,'country.name','iso3c')
# master_frame<- mutate(master_frame,
#                       english_speaking = master_frame$country_code%in%english_sc)
# top_9<-group_by(master_frame_venture,country_code,english_speaking)%>%
#         summarise_at(.vars = "raised_amount_usd" ,
#                      .fun = sum,na.rm = TRUE)%>%
#         arrange(desc(raised_amount_usd))

top_9<-group_by(master_frame_venture,country_code)%>%
        summarise_at(.vars = "raised_amount_usd" ,
                     .fun = sum,na.rm = TRUE)%>%
        arrange(desc(raised_amount_usd))
head(top_9,9)
# country_code english_speaking raised_amount_usd
# <fct>        <lgl>                        <dbl>
# 1 USA          TRUE                  422510842796
# 2 CHN          FALSE                  39835418773
# 3 GBR          TRUE                   20245627416
# 4 IND          TRUE                   14391858718
# 5 CAN          TRUE                    9583332317
# 6 FRA          FALSE                   7259536732
# 7 ISR          FALSE                   6907514579
# 8 DEU          FALSE                   6346959822
# 9 JPN          FALSE                   3363676611


#top 3 english speaking countries are USA,GRB and IND

#*******************************************************************************
# CHECKPOINT  4   Sector Analysis 1

#<------------------  Data cleaning of mapping file --------------------------->
#Remove NA from category_list to remove 'Blank' sector from mapping.
mapping <- mapping[!is.na(mapping$category_list),]
#we have category_lists where string "na" is replaced by string"0"
#string_na_index <- which(!is.na(str_extract(mapping$category_list,"0")))
#converting wrong data into correct one i.e. "0" with "na"
mapping[,1]<-str_replace_all(mapping[,1],"0","na")
mapping[,1]<-str_replace_all(mapping[,1],"2.na","2.0")


#<----------- convert mapping file from wide formate to long format----------->
mapping_long <- gather(mapping,main_sector,sector_value,
                       Automotive...Sports:Social..Finance..Analytics..Advertising)

#Remove redundunt rows and columns
mapping_long<-mapping_long[!(mapping_long$sector_value == 0),]
mapping_long<-mapping_long[,-3]
mapping_long$main_sector <- as.factor(str_to_title(mapping_long$main_sector))
mapping_long$category_list <- as.factor(str_to_title(mapping_long$category_list))


#<----------------Prepare Master_frame----------------------------------------->
#  Extract the primary sector of each category list from the category_list column
master_frame<-separate(master_frame,
                 category_list,
                 into = c('primary_sector'),
                 sep="\\|", 
                 extra="drop",
                 remove = F)
master_frame$primary_sector <- as.factor(str_to_title(master_frame$primary_sector))

#<-------- join master_frame and mapping_long to get main sector--------------->
master_frame <-merge(master_frame,
                      mapping_long,
                      by.x = 'primary_sector',
                      by.y = 'category_list')

#******************************************************************************
# CHECKPONT 5 Sector Analysis 2
# Find out most heavily invested main sectors in each of three countries
#<--------- Narrow down analysis, filter by FT='venture', 
#Engish speaking country= USA,GRB,IND and  5M >= funding amount <= 15M -------->

D1<-filter(master_frame,country_code=="USA",funding_round_type == "venture",
           raised_amount_usd >= 5000000, raised_amount_usd <=15000000)
D2<-filter(master_frame,country_code=="GBR",funding_round_type == "venture",
           raised_amount_usd >= 5000000, raised_amount_usd <=15000000)
D3<-filter(master_frame,country_code=="IND",funding_round_type == "venture",
           raised_amount_usd >= 5000000, raised_amount_usd <=15000000)


# function takes dataframe and sector group-calculate sum,no of records sectorwise
# merge the result with dataframe in investment_sector_total,
# investment_sector_count respectively and returns mearged dataframe
group_main_sector <- function(x){
        group_sector<- group_by(x,main_sector)
        x_investment_count <- summarise(group_sector,'investment_sector_count'=n())
        x_investment_total <-summarise(group_sector, 
                        'investment_sector_total'=sum(raised_amount_usd,na.rm = TRUE))
        x<-merge(x,x_investment_count,by = "main_sector")
        x<-merge(x,x_investment_total,by = "main_sector")
        return(x)
        }
D1<-group_main_sector(D1)
D2<-group_main_sector(D2)
D3<-group_main_sector(D3)
        
#<----------------------------------------------------------------------------->
# Table 5.1 : Sector-wise Investment Analysis
# 1. Total number of investments (count)

#Here is.na gives result in True (1) or (False) (0), sum  will count all Trues
sum(!is.na(D1$raised_amount_usd))
# [1] 12063
sum(!is.na(D2$raised_amount_usd))
# [1] 621
sum(!is.na(D3$raised_amount_usd))
# [1] 328

# 2. Total amount of investment (USD)

sum(D1$raised_amount_usd,na.rm = TRUE)
#[1] 107757097294
sum(D2$raised_amount_usd,na.rm = TRUE)
#[1] 5379078691
sum(D3$raised_amount_usd,na.rm = TRUE)
#[1] 2949543602

# 3. Top sector (based on count of investments)	 
# 4. Second-best sector (based on count of investments)
# 5. Third-best sector (based on count of investments)


top_sector<-function(x) return (group_by(x,main_sector) %>%
                                        summarise(n=n())%>%
                                        arrange(desc(n)))
D1_top3<-top_sector(D1)[1:3,]
D2_top3<-top_sector(D2)[1:3,]
D3_top3<-top_sector(D3)[1:3,]

D1_top3
# 1 Others                                   2950
# 2 Social..Finance..Analytics..Advertising  2714
# 3 Cleantech...Semiconductors               2350

D2_top3
# 1 Others                                    147
# 2 Social..Finance..Analytics..Advertising   133
# 3 Cleantech...Semiconductors                130

D3_top3
# Others                                       110
# 2 Social..Finance..Analytics..Advertising    60
# 3 News..Search.and.messaging                 52


#9.For the top sector count-wise (point 3), which company received 
# the highest investment?
# function inputs: (data frame1,data frame2,rank)
# filter dataframe1 by the first sector in dataframe2
# group the result by coumpany on total funding
# arrange in desc 

Highest_investment_company <- function(df1,top3,n=1){
        df1[which(df1$main_sector==top3[["main_sector"]][n]),]%>%       
         group_by(company_permalink,name)%>%
                summarise_at(.vars="raised_amount_usd",
                             .fun=sum,na.rm = TRUE)%>%
                arrange(desc(raised_amount_usd))%>%
        return()
}

Highest_investment_company(D1,D1_top3,1)[1,]
# 1 /organization/virtustream Virtustream          64300000
Highest_investment_company(D2,D2_top3,1)[1,]
# 1 /organization/electric-cloud Electric Cloud          37000000
Highest_investment_company(D3,D3_top3,1)[1,]
# 1 /organization/firstcry-com FirstCry.com          39000000

Highest_investment_company(D1,D1_top3,2)[1,]
#1 /organization/shotspotter SST Inc. (Formerly ShotSpotter)          67933006
Highest_investment_company(D2,D2_top3,2)[1,]
#1 /organization/celltick-technologies Celltick Technologies          37500000
Highest_investment_company(D3,D3_top3,2)[1,]
#1 /organization/manthan-systems Manthan Systems          50700000

#******************************************************************************

#In order to show English speaking countries in Tableau plots
# We have created a boolean column in master_frame named 'english_speaking'
# Please uncomment if 'english_speaking' flag is needed in master_frame
#******************************************************************************
# english_sc<-read.csv("Countries_with_english_as_official_language.csv",
#                      header=TRUE,stringsAsFactors = FALSE,na.strings = "")
# # English_speaking col is created 
# english_sc<-countrycode(english_sc$english_speaking_country,'country.name','iso3c')
# master_frame<- mutate(master_frame,
#                       english_speaking = master_frame$country_code%in%english_sc)

write.csv(master_frame,"master_frame.csv",row.names = FALSE)


