best <- function(state, outcome) {
        dd<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Read outcome data and store in dd
        
        l1<-grep(state,dd[,7],ignore.case=TRUE)
        fp<-l1[1]
        state<-dd[,7][fp]
        l2<-grep(outcome,c("Heart Attack","Heart Failure","Pneumonia"),ignore.case=TRUE)
#check if state and outcome are valid input, ignore case
        
        if(length(l1)==0) 
           stop("invalid state")
#If an invalid state value is passed to best, the function 
#throw an error via the stop function with the exact message “invalid state”

        if(length(l2)==0) 
           stop("invalid outcome")
#If an invalid output value is passed to best, the function 
#throw an error via the stop function with the exact message “invalid output"

         if(l2==1)(n=11)
         if(l2==2)(n=17)
         if(l2==3)(n=23)
        dd[,n]<-suppressWarnings(as.numeric(dd[,n]))
        xx<-tapply(dd[,n],dd[,7],function(x) min(x,na.rm = TRUE))
#calculate best outcome in each state
        
        hn<-dd[,2][dd[,n]==xx[state]&!is.na(dd[,n])&dd[,7]==state]
        hn1<-sort(hn)
        hn1[1]
#output hosptical name, if there is tie output first name sorted by alphabetical order
}

