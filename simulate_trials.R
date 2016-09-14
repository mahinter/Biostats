simulate_trials <- function(my_n=3,my_p=0.5,my_sims=1000) {
my_q=1-my_p
probs=c(my_p,my_q)
pmf<-array(0,dim=my_n+1) # define a pmf that stores 4 different values
for(i in 1:my_sims) {
  trial<-sample(x=1:0,size=my_n,p=probs,replace=TRUE) # randomly select 3 binary numbers
  number_of_successes<-sum(trial)        # add up to see how many 1's ("boys")
  index<-number_of_successes+1          # adjust the index by 1 so that 0 successes is
  																		  # 1st index,etc.
                                       
  pmf[index]=pmf[index]+1 # increment our count by 1
  barplot(pmf,names=0:my_n) # plot our pmf.  name the bars from 0 up to number of trials
}

return(pmf/sum(pmf))      # print our final frequencies
}
