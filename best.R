#Author: Kanishk Asthana October 2014
best <- function(state,outcome){
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
    minimum_cases<-sorted_data[,2]==min(sorted_data[,2])
    best_hospitals<-sorted_data[minimum_cases,1]
    sort(best_hospitals)
    return(best_hospitals[1]) 
}