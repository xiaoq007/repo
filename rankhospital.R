rankhospital <- function(state, outcome, num = "best"){
        dd<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Read outcome data and store in dd
        
        l1<-grep(state,dd[,7],ignore.case=TRUE)
        fp<-l1[1]
        state<-dd[,7][fp]
        l2<-grep(outcome,c("Heart Attack","Heart Failure","Pneumonia"),ignore.case=TRUE)
        #check if state and outcome are valid input, ignore case
        
        if(length(l1)==0) 
                stop("invalid state")
        #If an invalid state value is passed to rankhospital, the function 
        #throw an error via the stop function with the exact message “invalid state”
        
        if(length(l2)==0) 
                stop("invalid outcome")
        #If an invalid output value is passed to rankhospital, the function 
        #throw an error via the stop function with the exact message “invalid output"
        
        if(l2==1)(n=11)
        if(l2==2)(n=17)
        if(l2==3)(n=23)
        
        sp<-split(dd,dd[,7])
        cc<-sp[[state]]
#split data by state
        
        cc[,n]<-suppressWarnings(as.numeric(cc[,n]))
                r1<-rank(cc[,n])
                r2<-rank(cc[,2])
                rank<-rank(r1+r2/nrow(cc))
#calculate ranking for given outcome, hospital name=NA will get higher number in ranking 
#1 is "best"
                
          ss<-data.frame(hospital=cc[,2],rate=cc[,n],rank)
#construct a data frame for given output, consist of hospintal, motality rate and rank
          
          nr<-nrow(ss)
          sn<-sum(is.na(ss$rate))
          ma<-nr-sn
          
          if(num=="best") {as.character(ss$hospital[ss$rank==1])}
          else if(num=="worst") {as.character(ss$hospital[ss$rank==ma])}
          else if(as.numeric(num)>ma){NA}          
           else {as.character(ss$hospital[ss$rank==num])}
          
#return hospital name based on num
          
}
        
