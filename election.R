#library useful for string filtering
#install.packages("stringr")
#install.packages("sqldf")
library(stringr)


#for sql
library(sqldf)

#the file you store the raw data in.  NOT THE SAME AS THIS PROJECT OR THE PUMS DATA WILL BE BACKED UP ON GITHUB
#data_directory <- "C:/Users/staff/Downloads/ncvoter/"
data_directory <- "C:/Users/Zantan/Downloads/ncvoter/"

#county codes are found in the data format page on the FTP
#Forsyth 34
#Guilford 41
#Gaston 36
#Cumberland 26
counties_of_interest <- c("34","41","36","26")
#AJ was told that we should be paying attention to voted_county_id rather than county_id, but I'm pulling them both so I can make some extra checks

#based on unique(vhis_c$election_desc)
elections <- c("11/03/2009 GENERAL","11/03/2009 MUNICIPAL GENERAL","11/03/2015 MUNICIPAL","11/04/2008 GENERAL","11/04/2014 GENERAL","11/05/2013 MUNICIPAL GENERAL","11/06/2012 GENERAL","11/08/2011 MUNICIPAL","11/02/2010 GENERAL","11/08/2016 GENERAL")




county_data <- function(county){
  
  #a simple function that combines and filters the data files for a given county
  #for (county in c("34","41","36","26")){
  
  
  #get the voter history file for the county
  vhis_c <- read.table(paste(data_directory,"ncvhis",county,".txt",sep=""), 
                       header = TRUE,stringsAsFactors=FALSE,
                       sep = "\t")
  
  #filter to only get general or certain larger municipal
  vhis_c_small <- vhis_c[vhis_c$election_desc %in% elections, ]
  
  #only counties of interest
  vhis_c_small <- vhis_c_small[(vhis_c_small$county_id %in% counties_of_interest)|(vhis_c_small$voted_county_id %in% counties_of_interest), ]
  
  #merge to the voter registration file
  #v_registration <- read.table(paste(data_directory,"ncvoter",county,".txt",sep=""), 
  #                     header = TRUE,stringsAsFactors=FALSE,
  #                     sep = "\t")
  
  #the variables to pull from 
  #regvars <- c("voter_reg_num","status_cd","voter_status_desc","reason_cd","voter_status_reason_desc","municipality_abbrv","municipality_desc", "birth_year","birth_age","race_code","gender_code","ethnic_code","state_cd","zip_code","mail_city")   
  #votes_with_reg_data <- merge(vhis_c_small,v_registration[ , regvars], by="voter_reg_num",all.x=TRUE)
  
  #that was a left join, so now let's check the number of rows to make sure that none were gained (can't loose any)
  votes <- nrow(vhis_c_small)
  #votes_after_merge <- nrow(votes_with_reg_data)
  
  #if (votes != votes_after_merge){
  #  stop(paste("Problem with merge in",county,votes,"votes turned into",votes_after_merge,"records after merge"))
  #       }
  print(paste("county",county," imported with",votes,"records"))
  #return(votes_with_reg_data)
  return(vhis_c_small)
}


#voter history
#start with county 00
election_data <- county_data("1")

#get voter history data for the other 99 counties
for (cty in 2:100){
  cty_str <- toString(cty)
  election_data <- rbind(election_data,county_data(cty_str))
}

#now we make a list of unique voter registration numbers that we need to use to extract registration data.
reg_ids <- unique(election_data$voter_reg_num)


#now create a registration file based on these registration numbers

#first county 1
tmp <- read.table(paste(data_directory,"ncvoter","1",".txt",sep=""), 
                  header = TRUE,stringsAsFactors=FALSE,
                  sep = "\t")
registration_data <- tmp[tmp$voter_reg_num %in% reg_ids, ]

#now the other 99
for (cty in 2:100){
  cty_str <- toString(cty)
  tmp <- read.table(paste(data_directory,"ncvoter",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp_small <- tmp[tmp$voter_reg_num %in% reg_ids, ]
  registration_data <- rbind(registration_data,tmp_small)
  print(paste(cty_str,toString(nrow(tmp)),toString(nrow(tmp_small))))
}


regvars <- c("voter_reg_num","status_cd","voter_status_desc","reason_cd","voter_status_reason_desc","municipality_abbrv","municipality_desc", "birth_year","birth_age","race_code","gender_code","ethnic_code","state_cd","zip_code","mail_city")   

#merge the registration and voter history files
#I'm breaking this up by election because merging both files together takes forever

#empty list.  There has to be a better way to do this...
all_elections <- vector("list", 10)

election=elections[1]
print(election)
tmp <-  election_data[election_data$election_desc == election]
votes_with_reg_data <- merge(tmp,registration_data[ , regvars], by="voter_reg_num",all.x=TRUE)


for (election_num in 2:10){
  election=elections[election_num]
  print(election)
  tmp <-  election_data[election_data$election_desc == election]
  tmp_with_reg_data <- merge(tmp,registration_data[ , regvars], by="voter_reg_num",all.x=TRUE)
  votes_with_reg_data <- rbind(votes_with_reg_data,tmp_with_reg_data)
}




#this takes a long time to do, so save the output file for later
#ideally I should have broken this down into multiple merges and ensured that the voter_reg_num was a number
saveRDS(votes_with_reg_data, file='voter_data.rds')



#votes_with_reg_data <- readRDS('voter_data.rds')






#make sure no duplicate ids increased the number of rows
votes <- nrow(election_data)
votes_after_merge <- nrow(votes_with_reg_data)

if (votes != votes_after_merge){
  print(paste("Problem with merge in",county,votes,"votes turned into",votes_after_merge,"records after merge"))
}




#let's look at the voter numbers in each election
election_results <- sqldf("select voted_county_desc,election_desc,count(*) 
                          from votes_with_reg_data 
                          where voted_county_id in ('34','41','36','26')
                          group by voted_county_desc,election_desc")
print(election_results)

#it's weird that the ftp files were organized by county_id when voted_county_id is so important.  Let's look at those
voted_county_vs_county <- sqldf("select voted_county_desc,county_desc, count(*) from votes_with_reg_data group by voted_county_desc,county_desc")

print(voted_county_vs_county)


#now we'll get frequencies for each variable of interest

for (demographic_var in c("birth_year","birth_age","race_code","gender_code","ethnic_code")){
  tmp <- sqldf(paste("select",demographic_var,",count(*) from votes_with_reg_data group by",demographic_var))
  print(tmp)
  
  
}


#there were 5089 records which didn't seem to have matches in the registration files which I will now look at
missing_registration <- voted_county_vs_county <- sqldf("select * from votes_with_reg_data where ethnic_code is null")


misreg <- sqldf("select election_desc, count(*) from missing_registration group by election_desc")

misregc <- sqldf("select election_desc,voted_county_desc, count(*) from missing_registration group by election_desc,voted_county_desc")



#9119434 voted in 2008 in the alamance file but voted in guilford.  this number is not in the registration file for alamance, is it anywhere? probably guilford?
#it is nowhere in the registration file, but voted in two elections!
#let's try 9121406, who did onestop instead of in person
#this person voted once

#wait, AJ said that they get reassigned registration numbers when they move between counties; does that mean that their old registration is DELETED?
#less than 1% of votes are invalidated this way, but according to the census 5-7% of Forsyth's population moved from another county/state each year...similar nationally...

reg=9121406
for (cty in 1:100){
  cty_str <- toString(cty)
  tmp <- read.table(paste(data_directory,"ncvoter",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$voter_reg_num == reg,]
  results <- toString(nrow(tmp))
  print(paste("voter",reg,"has",results,"records in the",cty_str,"registration file."))

  tmp <- read.table(paste(data_directory,"ncvhis",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$voter_reg_num == reg,]
  results <- toString(nrow(tmp))
  print(paste("voter",reg,"has",results,"records in the",cty_str,"voter history file."))
}

#what about the ncid? BY480869 (9119434) was in alamance/guilford.  let's get everyone with ncid BY480869 in those counties (1,41) and see what we get.
#test this for AA100704 which is in Guilford 34

ncid="BY480869"
for (cty in 1:100){
  cty_str <- toString(cty)
  tmp <- read.table(paste(data_directory,"ncvoter",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$ncid == ncid,]
  results <- toString(nrow(tmp))
  print(paste("voter",ncid,"has",results,"records in the",cty_str,"registration file."))
  
  
  tmp <- read.table(paste(data_directory,"ncvhis",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$ncid == ncid,]
  results <- toString(nrow(tmp))
  print(paste("voter",ncid,"has",results,"records in the",cty_str,"voter history file."))
}




