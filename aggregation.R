# FInd the mean, median, std deviation, min, max, first value, last value for the aggregation of 'k' rows, where k is variable 

Data_in <- read.csv(file.choose(),header=T)

data_in <- Data_in

data_in[,1] <- Data_in[,1] + 1420947910

data_in[,1] <- as.POSIXct(data_in[,1], origin = "1970-01-01")

f<-data_in

variable_df <- split(f,f$sensor_type) # Splits as per sensor types (8 variables here and hence list of 8 dataframes)
str(variable_df)

### For now, assume that the first variable has a granularity of 1 sec

#Divide by that granularity, the aggregate value
# nrow(as.data.frame(variable_df[1]))
# nrow(as.data.frame(variable_df[2]))
# round(nrow(as.data.frame(variable_df[1]))/nrow(as.data.frame(variable_df[2])),0)
# round(nrow(as.data.frame(variable_df[1]))/nrow(as.data.frame(variable_df[3])),0)

aggregate_func <- function(agg_value)
{
	print(paste("Value entered is ",agg_value))
	
	for(j in 1:length(variable_df))
	{
		split_variable <-list()
		variable_type <- levels(factor(as.data.frame(variable_df[j])[,3]))
          freq_sensor <- round(nrow(as.data.frame(variable_df[1]))/nrow(as.data.frame(variable_df[j])),0)
	split_variable <- split(as.data.frame(variable_df[j]), ceiling(seq_along(as.data.frame(variable_df[j])[,1])/(agg_value/freq_sensor)))
	
	file<-list()
	
	for(i in 1:length(split_variable))
	{
		temp <- as.data.frame(split_variable[i])
		b<-temp[,4]
		file<-rbind(file,data.frame(temp[1,1],median(b),mean(b),sd(b),min(b),max(b),b[1],b[length(b)])
		)
	}
	colnames(file)<-c("Time","Median","Mean","Std Dev","Min","Max","First","Last")
	file_dir <- paste(getwd(),'/Aggregated for variable_',variable_type,'_with resolution_',agg_value,' seconds.csv',sep="")
	write.csv(file,file_dir)	
	print(paste("File stored as ", file_dir))
	}
	
}

#print("Please enter the aggregation count in seconds:")

take_in <- 3600

aggregate_func(take_in)




