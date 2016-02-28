rankhospital <- function(state, outcome, num = "worst") {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
outcome_data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
state_names<-unique(outcome_data$State)
if (!is.element(state,state_names))
{	stop("invalid state")
}
outcome_per_state<-outcome_data[ which(outcome_data$State==state),]
if (outcome=="heart attack"){
	outcome_per_state[,11]<-suppressWarnings(as.numeric(outcome_per_state[,11],na.omit))
	newdata <- outcome_per_state[order(outcome_per_state[,11], outcome_per_state[,2]),]
	row_num<-11
}
else if (outcome=="heart failure"){
	outcome_per_state[,17]<-suppressWarnings(as.numeric(outcome_per_state[,17],na.omit))
	newdata <- outcome_per_state[order(outcome_per_state[,17], outcome_per_state[,2]),]
	row_num<-17
	
}

else if (outcome=="pneumonia"){
	outcome_per_state[,23]<-suppressWarnings(as.numeric(outcome_per_state[,23],na.omit))
	newdata <- outcome_per_state[order(outcome_per_state[,23],outcome_per_state[,2]),]
	row_num<-23
}
else{
	stop("invalid outcome")
}
	final <- newdata[!(is.na(newdata[,row_num])),]

	h<-head(newdata[,2],10)
##final[nrow(final),2]
h
}