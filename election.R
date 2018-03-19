#library useful for string filtering
#install.packages(dplyr)
library(dplyr)


#the file you store the raw data in.  NOT THE SAME AS THIS PROJECT OR THE PUMS DATA WILL BE BACKED UP ON GITHUB
data_directory <- "C:/Users/staff/Downloads/ncvoter/"

#county codes are found in the data format page on the FTP
#Forsyth 34
#Guilford 41
#Gaston 36
#Cumberland 26

for county in ("34","41","36","26"){
  #get the voter history file for the county
  vhis_c <- read.table(paste(data_directory,"ncvhis",county,".txt",sep=""), 
                  header = TRUE,
                 sep = "\t")
  
  #filter to only get general elections
  vhis_c<- subset(vhis_c, PUMA == 1801 | PUMA == 1802 | PUMA == 1803)

  #merge to the voter registration file




}

dplyr::filter(mtcars, !grepl('Toyota|Mazda', type))

filter(vhis_c, !grepl("General",vhis_c$election_desc))