# TODO: Add comment
# 
# Author: karthik
###############################################################################


library(RJSONIO)

print("Please select a flat file")

flat_file_choosen<-read.csv(file.choose(),header=T,colClasses=c("sensor_mac"="character"))

JSON_file <- toJSON(flat_file_choosen)

write(JSON_file,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data_aggregation\\test_2hours_8variables.json")


write(JSON_file,"F:\\Software_Envy\\XAMPP\\htdocs\\D3V\\psy_model.json")