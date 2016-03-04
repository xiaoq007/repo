rankall <- function(outcome, num = "best"){
        dd<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Read outcome data and store in dd
       
        l1<-grep(outcome,c("Heart Attack","Heart Failure","Pneumonia"),ignore.case=TRUE)
        #check if outcome is valid input, ignore case
        if(length(l1)==0) stop("invalid outcome")
        #If an invalid output value is passed to rankall, the function 
        #throw an error via the stop function with the exact message “invalid output"
        
        if(l1==1)(n=11)
        if(l1==2)(n=17)
        if(l1==3)(n=23)
        
        sp<-split(dd,dd[,7])
        #split data by state
        
        hp<-character()
        #character vector hp for hospital names
        
        st<-names(sp)
        c<-length(st)
        #fetch state names and put into character vector "st" length of this vector used
        #in for loop below
        
        for (i in 1:c){
                state<-st[i]
                cc<-sp[[state]]
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
                
                if(num=="best") {hp[i]<-as.character(ss$hospital[ss$rank==1])}
                else if(num=="worst") {hp[i]<-as.character(ss$hospital[ss$rank==ma])}
                else if(hp[i]<-as.numeric(num)>ma){hp[i]<-NA}          
                else {hp[i]<-as.character(ss$hospital[ss$rank==num])}
                
        }
#for loop to populate the hospital column for the data frame to be constructed below
        data.frame(hospital=hp,state=st,row.names=st)
        
}