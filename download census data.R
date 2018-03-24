#Extract block and ZCTA level variables from ACS using tidy census


#libraries
#install.packages("tidycensus")
#install.packages("tidyverse")
#install.packages("reshape")
library(tidycensus)
library(tidyverse)
library(reshape)

#see the tables to download.
 
v16 <- load_variables(2016, "acs1", cache = TRUE)
v16$year <- 2016
#it looks like this function doesn't work before 2012 (maybe because they care most about 5yr data?), but I know for this table the variables don't change
for (y in 2015:2008){
  print(y)
  x<-y
  if(y<2012){x<-2012}
  v <- load_variables(x, "acs1", cache = TRUE)
  v$year <- x
  v16 <- rbind(v16,v)
}
#View(v16) 


#we only need B05003 for adult citizens


census_api_key("3d94584bfd87fb43977be01dbddfc797af127c5c") 
options(tigris_use_cache = FALSE) # optional - to cache the Census shapefile



B05003 <- as.data.frame(tryCatch(get_acs(geography = "county", 
                                                table="B05003", #get all vars in tables
                                                year = 2016, #this year
                                                state = "NC", 
                                                county = "Forsyth",
                                                geometry = F, # no mapping geometry
                                                survey = "acs1",
                                                cache_table=T), error=function(e) NA))
#race/eth files
for (race_eth in c("B","H","I")){
  #
  tmp <- as.data.frame(tryCatch(get_acs(geography = "county", 
                                        table=paste("B05003",race_eth,sep=""), #get all vars in tables
                                        year = 2016, #this year
                                        state = "NC", 
                                        county = "Forsyth",
                                        geometry = F, # no mapping geometry
                                        survey = "acs1",
                                        cache_table=T), error=function(e) NA))
  tmp$year="2016"
  B05003 <- rbind(B05003,tmp)
}


#counties over time
for (year in c("2016","2015","2014","2013","2012","2010","2008")){
  for (county in c("Cumberland","Guilford","Gaston")){
    
    tmp <- as.data.frame(tryCatch(get_acs(geography = "county", 
                                            table="B05003", #get all vars in tables
                                            year = year, #this year
                                            state = "NC", 
                                            county = county,
                                            geometry = F, # no mapping geometry
                                            survey = "acs1",
                                            cache_table=T), error=function(e) NA))
    tmp$year=year
    B05003 <- rbind(B05003,tmp)
  }
}

#age has to come from PUMS; the only age breakdowns by citizenship in AFF are over/under 18

#add variable names
B05003$name_for_merging <- paste(B05003$variable,"E",sep="")


B05003 <- merge(B05003,v16, by.x=c("name_for_merging","year"),by.y=c("name","year"),all.x=TRUE)


#remove "Estimate" from the variable names
B05003$label <- gsub("Estimate!!", "", B05003$label)

#change labels to reflect variable tables
index <- B05003$concept == "SEX BY AGE BY NATIVITY AND CITIZENSHIP STATUS (BLACK OR AFRICAN AMERICAN ALONE)"
B05003$label[index] <- paste(B05003$label[index],": African American")
index <- B05003$concept == "SEX BY AGE BY NATIVITY AND CITIZENSHIP STATUS (HISPANIC OR LATINO)"
B05003$label[index] <- paste(B05003$label[index],": Hispanic")
index <- B05003$concept == "SEX BY AGE BY NATIVITY AND CITIZENSHIP STATUS (WHITE ALONE, NOT HISPANIC OR LATINO)"
B05003$label[index] <- paste(B05003$label[index],": White, nonHispanic")


#rename the geography column
names(B05003)[names(B05003) == 'NAME'] <- 'Location'



#calculate SE
B05003$SE <- B05003$moe/1.645
#replace missing SEs with 0; this means that the error is statistically controlled and should be treated as 0
B05003$SE[is.na(B05003$SE)] <- 0


#get just the variables we care about
varlist=c("Total!!Male!!18 years and over!!Foreign born!!Not a U.S. citizen",
          "Total!!Female!!18 years and over!!Foreign born!!Not a U.S. citizen",
          "Total!!Male!!18 years and over",
          "Total!!Female!!18 years and over")

B05003 <- B05003[B05003$label %in% varlist,c("NAME","estimate","SE","label","concept")]



#transpose the data so there is one record per year and county


B05003md <- melt(B05003, id=(c("Location", "time")))



v16
  
#empty list for year
yrs<-list()

for (k in 1:length(years)){
  #for each year
  year.pull<-as.numeric(gsub("var","",years[k]))
  #empty list- limited memory
  sts<-list()  
  
  for (j in 1:length(use.states)){
    use.state<-use.states[j]
    #print(use.state)
    #empty list for tables for each state - limited memory

      
      
      
    }
    tbs<-tbs[!grepl("tryCatch",tbs)] #remove empty tables
    
    #if no table for the state, give it na, otherwise bind into one data frame
    if(length(tbs)<1){sts[[j]]<-NA}else{
      sts[[j]]<- as.data.frame(do.call("rbind", na.omit.list(tbs)))
    }
    
  }
  tbs<-list()  #empty tbs list for memory limitations
  names(sts)<-use.states #name
  #save all states for year of choice here - for memory limitation
  save(sts,file=paste0("B:/GIS/acs/acs5yr",year.pull,".RData"))
  #remove the sts list for space issues
  sts<-list()
  #throw into list for all states corresponding to each year
  #yrs[[k]]<-as.data.frame(do.call("rbind", na.omit.list(sts)))
  #yrs[[k]]$yr<-year.pull
  #save(yrs,file=paste0("B:/GIS/acs/acs5yr",year.pull,".RData"))
}


#######################################
# ZCTAS - MUST GET FOR ENTIRE COUNTRY #
#######################################
#english speaking at household B16002 not available in 2016
#https://www.google.com/search?q=tidy+census+get+zcta&rlz=1C1NHXL_enUS694US694&oq=tidy+census+get+zcta&aqs=chrome..69i57.3343j0j4&sourceid=chrome&ie=UTF-8

#empty lists
zc<-list()
yrs<-list()

#extract zctas for all years loop
for (k in 1:length(years)){
  year.pull<-as.numeric(gsub("var","",years[k]))
  for (i in 1:length(tables)){
    census_api_key("407b30a597f4bd8ebd7b358ffc0ed9c5ba209c8d") # use `install = TRUE` to install the key
    options(tigris_use_cache = TRUE) # optional - to cache the Census shapefile
    zc[[i]]<- get_acs(geography = "zip code tabulation area", 
                      table=tables[i],
                      survey="acs5",
                      year = year.pull,
                      geometry = F)
    #see progress
    print(tables[i])
  }
  yrs[[k]]<-as.data.frame(do.call("rbind", na.omit.list(zc)))
  yrs[[k]]$yr<-year.pull
}
#yrsfinal<-as.data.frame(do.call("rbind", na.omit.list(yrs)))
#save(yrs,file=paste0("B:/GIS/acs/acs5yrzcta.RData"))




#checked variables and identified these table numbers
#matrix of tables needed for variable calculation
tables.of.interest<-as.data.frame.matrix(rbind(
  c("2015","total.pop","B01003"),
  c("2015","gender","B01001"),
  c("2015","age","B01002"),
  c("2015","race","B02001"),
  c("2015","black","B02009"),
  c("2015","hispanic","B03003"),
  c("2015","transportation","B08134"),
  c("2015","householdtype","B11001"),
  c("2015","education","B15003"),
  c("2015","english","B16002"), #confirmed
  c("2015","language","B16004"), #confirmed
  c("2015","income","B19013"), #confirmed
  c("2015","poverty","B17017"),   #????????? maybe?
  c("2015","povertyratio","C17002"),
  c("2015","veteran","B21001"),
  c("2015","employment","B23025"),
  c("2015","occupancy","B25002"),
  c("2015","tenure","B25003")))
names(tables.of.interest)<-c("acs.year","var","table.num")

