rankall <- function(outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
outcome_data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)

state_names<-sort(unique(outcome_data$State))
if (outcome=="heart attack"){
	row_num<-11
}
else if (outcome=="heart failure"){
	row_num<-17	
}
else if (outcome=="pneumonia"){
	row_num<-23
}
else{
	stop("invalid outcome")
}
y<-outcome_data[order(outcome_data[,7],outcome_data[,row_num],outcome_data[,2]),]
s<-split(y,y$State)
df<-data.frame(hospital=character(length(state_names)),state=character(length(state_names)), stringsAsFactors=F)
rownames(df)<-state_names
for (i in 1:length(state_names)) {
	s[[i]][,row_num]<-suppressWarnings(as.numeric(s[[i]][,row_num],na.omit))
	s[[i]]<-s[[i]][order(s[[i]][,row_num],s[[i]][,2]),]
	if (num=="best"){
		h<-s[[i]][1,2]
	}
	else if (num =="worst"){
		final <- s[[i]][!(is.na(s[[i]][,row_num])),]
		h<-final[nrow(final),2]
	}
	else if (num>nrow(s[[i]])){
		h<-"NA"
	}

	else{
		h<-s[[i]][num,2]
	}
	df$hospital[i]<-h
	df$state[i]<-state_names[i]

		

}
head(df,20)
}




