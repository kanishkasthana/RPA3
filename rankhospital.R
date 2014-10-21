#Author: Kanishk Asthana October 2014
rankhospital <- function(state,outcome,rank="best"){
    case<-NULL
    data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
    sorted_cases<-data["State"]==state
    #Checking if State is Valid
    if(sum(sorted_cases)==0){stop("invalid state")}
    #Checking if outcome is valid
    if(outcome=="heart attack")
    {
        case<-11
    }
    else if(outcome=="heart failure")
    {
        case<-17
    }
    else if(outcome=="pneumonia")
    {
        case<-23
    }
    else
    {
        stop("invalid outcome")
    }
    sorted_data<-data[sorted_cases,c(2,case)]
    sorted_data[,2]<-as.numeric(sorted_data[,2])
    sorted_data<-sorted_data[complete.cases(sorted_data),]
    order_of_rows=order(sorted_data[,2],sorted_data[,1])
    rankings=1:nrow(sorted_data)
    new_frame=cbind(sorted_data[order_of_rows,],rankings)
    
    pos=NULL
    if(rank=="best"){
        pos=1
    }
    else if(rank=="worst"){
        pos=nrow(sorted_data)
    }
    else if(rank>nrow(sorted_data)){
        return(NA)
    }
    else{
        pos=rank
    }
   
    return(new_frame[new_frame$rankings==pos,1])

}