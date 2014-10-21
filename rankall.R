#Author: Kanishk Asthana October 2014
rankall <- function(outcome,rank="best"){
    case<-NULL
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
    data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
 
    get_best_for_state<-function(data,state,rank,case)
    {
        sorted_cases<-data["State"]==state
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
  allstates=unique(data["State"])
  allstates=unlist(allstates)

  allrankings<-sapply(allstates,function(state){get_best_for_state(data,state,rank,case)})
  state=allstates
  hospital=allrankings
  data.frame(hospital,state)
}