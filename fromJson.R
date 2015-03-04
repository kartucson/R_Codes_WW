# TODO: from json to csv 
# also generates/allocates files for each variables
# Author: karthik
###############################################################################

getwd()
# setwd("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\")
setwd(choose.dir())

library(RJSONIO)

print("Please select a json file")

JSON_file_choosen <- file.choose()

back_file_choosen <- fromJSON(JSON_file_choosen)

init_data_frame <- as.data.frame(back_file_choosen)

init_data <- init_data_frame

init_data[,1] <- init_data[,1] + 1420947910
init_data[,1] <- as.POSIXct(init_data[,1], origin = "1970-01-01")

# print("Select folder to place the converted folder")

write.csv(init_data,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data_aggregation\\test_2h.csv")

individual_var <- split(init_data,init_data$sensor_type)

for(j in 1:length(individual_var))
{
ind_var <- as.data.frame(individual_var[j])
variable_type <- levels(factor(as.data.frame(individual_var[j])[,3]))
colnames(ind_var) <- c("time_stamp","sensor_mac","sensor_type","sensor_value","node_location")
write.csv(ind_var,paste("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data_aggregation\\to_mySQL\\test_2h_",variable_type,".csv",sep=""),row.names=FALSE)

}
