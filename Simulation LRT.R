#Note: this code is organized for the data presented in the associated paper, but can be reformulated for any other data/hypothesis structure for which likelihood can be calculated.
# General order of operations:
  # 1) Set up data
  # 2) Combine models
  # 3) Find optimum combination of models to maximize likelihood
  # 4) Identify weights
  # 5) Calculate new predictions
 
###################  Step 1  ####################
obs.x<- #vector of ‘successes’ in n trials
obs.y<- #vector of ‘failures in n trials
n<- #number of trials
A.x.exp<- #expected number of ‘successes’ under hypothesis ‘A’
B.x.exp<- #expected number of ‘successes’ under hypothesis ‘B’
x.exp<-cbind(A.x.exp, B.x.exp)
p.exp<- #similar matrix to x.exp, but with expected proportion of ‘successes’ under each hypothesis
p.obs<- #observed proportion of ‘successes’
 
###################  Step 2  ####################
propAB<-function(a,b){
         tot<-sum(a,b)
         a<-a/tot; b<-b/tot
         return(c(a,b))
}
comb.like.fit<-function(AB){
         abVec<-propAB(AB[1],AB[2])
         p.combine<-(abVec[1]*P.exp[,1] + abVec[2]* P.exp[,2])
         comb.like<-log((p.combine/p.obs)^obs.x*((1-p.combine)/(1-p.obs))^(obs.y))
         mask<-apply(as.matrix(comb.like),2,is.infinite) #finds all 'Inf'
         comb.like2<-comb.like
         comb.like2[mask]<-0 #replaces 'Inf' with 0 (because integer/0 can not be calculated; this should    result in a more conservative likelihood for all model-dataset combinations)
         mask<-apply(as.matrix(comb.like2),2,is.nan) #finds all 'NaN'
         comb.like3<-comb.like2
         comb.like3[mask]<-0 #replaces 'NaN' with 0 (these were all instances in which 0/0 = NaN)
         result<-sum(comb.like3)
         return(result)
}
 
###################  Step 3  ####################
AB<-c(1,1)
comb.like.fit(AB)
optim(AB,comb.like.fit,method="L-BFGS-B",control=list(fnscale=-1),lower=0)
solution<-optim(AB,comb.like.fit,method="L-BFGS-B",control=list(fnscale=-1),lower=0)$par
solution
 
###################  Step 4  ####################
A.weight<-solution[1]/sum(solution)
B.weight<-solution[2]/sum(solution)
 
###################  Step 5  ####################
combined.p.exp<-A.weight*p.exp[,1]+B.weight*p.exp[,2]
