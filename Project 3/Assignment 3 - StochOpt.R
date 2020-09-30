library(nnet)
library(pracma)


#Thompson Sampling:

Thompson <- function(bandit, r, t){
  beliefs = c(rbeta(1,bandit$successes[1], bandit$failures[1]),rbeta(1,bandit$successes[2], bandit$failures[2]))
  arm = which.max(beliefs) #chooses this because it selects ties at random
  
  #You have now selected an arm to play, based on sampled beliefs of the arms
  #Now use uniform sample together with underlying probabilities:
  if (runif(1) <= bandit$underlying[arm]){
    bandit$successes[arm] <<- bandit$successes[arm] + 1
    r <<- r + 0.95^t
  }
  else {
    bandit$failures[arm] <<- bandit$failures[arm] + 1
  }
  bandit$success_prob[arm] = bandit$successes[arm]/(bandit$successes[arm] + bandit$failures[arm])
  return(c("bandit" = bandit, "r" = r))
}

#Bayesian with Gittin's indices:

Gittins <- function(bandit, r, t){  
  if(t == 1){
    arm = sample(c(1,2),1) #for the first iteration, choose random arm, since both have same start prob
  }
  else {
   arm = which.max(bandit$success_prob)
  }
  if (runif(1) <= bandit$underlying[arm]){
    bandit$successes[arm] <- bandit$successes[arm] + 1
    r <- r + 0.95^t
  }
  else {
    bandit$failures[arm] <- bandit$failures[arm] + 1
  }
  bandit$success_prob[arm] = bandit$successes[arm]/(bandit$successes[arm] + bandit$failures[arm])
  return(c("bandit" = bandit,  "r" = r))
  
}

#Main simulation function for Thompson Sampling and Gittins index:

main_iter <- function(func,M,T){
  R = c()
  for(n in 1:M){
    #sample underlying success probabilities and create bandit
    underlying  = runif(2)
    bandit <- data.frame("successes" = c(1,1), "failures" = c(1,1), "success_prob" = c(0.5,0.5), "underlying" = underlying)
    r = 0
    for(t in 1:T){
      pullArm = func(bandit, r, t)
      bandit <- data.frame("successes" = pullArm$bandit.successes, "failures" = pullArm$bandit.failures, "success_prob" = pullArm$bandit.success_prob, "underlying" = underlying)
      r = pullArm$r
    }
    R = c(R,r)
  }
  return(sum(R)/M)
}


#Helping function to decide whether success or failure:

pull_arm <- function(arm, underlying){
  if(runif(1) <= underlying[arm]){
    return(1)
  }
  else{
    return(0)
  }
}

#Function for both Q-learning methods:

Qlearning <- function(epsilon,init_val,T, underlying, alpha, gamma=0.1){
  r = 0
  for(t in 1:T){
    if(t == 1) Q <- c(init_val,init_val)
    if(runif(1) < epsilon){
      arm = sample(c(1,2),1)
    } else {
      arm = which.max(Q) #which.max chooses random when ties
    }
    success = pull_arm(arm,underlying)
    if(success){
      r = r + alpha^t
    }
    Q[arm] = (1/t)*(success + alpha*max(Q)) + (1 - 1/t)*Q[arm]
    #Q[arm] = gamma*(success + alpha*max(Q)) + (1 - gamma)*Q[arm]
  }
  return(r)
}

#Main simulation of Q-learning methods:

Q_sim <- function(M,epsilon,init_val,T, alpha=0.95){
  R = c()
  for(i in 1:M){
    probs = runif(2)
    r = Qlearning(epsilon,init_val,T, probs, alpha)
    R = c(R,r)
  }
  return(sum(R)/M)
}

#Functions for the full information case:

#solving the optimality equation:

Find_Pi <- function(V,Pi,N,alpha){
  for(s1 in 0:(N-1)){
    for(f1 in 0:(N-1-s1)){
      for(s2 in 0:(N-1-s1-f1)){
        for(f2 in 0:(N-1-s1-f1-s2)){
          #calculating the value of pulling arm 1:
          arm1 = (1+s1)/(2+s1+f1) + alpha*((1+s1)/(2+s1+f1)*V[s1+2,f1+1,s2+1,f2+1] + (1 + f1)/(2+s1+f1)*V[s1+1,f1+2,s2+1,f2+1])
          
          #calculating the value of pulling arm 2:
          arm2 = (1+s2)/(2+s2+f2) + alpha*((1+s2)/(2+s2+f2)*V[s1+1,f1+1,s2+2,f2+1] + (1+f2)/(2+s2+f2)*V[s1+1,f1+1,s2+1,f2+2])
          
          #V updated as the maximum value:
          V[s1+1,f1+1,s2+1,f2+1] = max(c(arm1,arm2))
          
          #Pi updated as the arm giving the maximum value:
          Pi[s1+1,f1+1,s2+1,f2+1] = which.is.max(c(arm1,arm2))
        }
      }
    }
  }
  return(Pi)
}

#Doing runs and collect r

full_info_r <- function(Pi,underlying,N,alpha){
  s1=0;f1=0;s2=0;f2=0
  r = 0
    for(t in 1:N){
      arm = Pi[s1+1,f1+1,s2+1,f2+1]
      success = pull_arm(arm,underlying)
      if(arm == 1){
        if(success){
          s1 = s1 + 1
          r = r + alpha**t
        } else {
          f1 = f1+1
        }
      }else{
        if(success){
          s2 = s2 + 1
          r = r + alpha**t
        } else{
          f2 = f2 + 1
        }
      }
    }
  return (r)
}

#Main simulation of full information method;

Full_sim <- function(M,N,alpha=0.95){
  
  V_start <- array(rep(0,(N+1)**4),dim=c(N+1,N+1,N+1,N+1))
  Pi_start <- array(rep(0,(N+1)**4),dim=c(N+1,N+1,N+1,N+1))
  
  Pi = Find_Pi(V_start,Pi_start,N,alpha)
  R = c()
  for(m in 1:M){
    underlying = runif(2)
    r = full_info_r(Pi,underlying,N,alpha)
    R = c(R,r)
  }
  return(sum(R)/M)
}




#running main simulation for Thompson samping: 

R_Thompson = main_iter(Thompson,200,120)

#running main simulation for and Bayesian using Gittins index:
R_Gittins = main_iter(Gittins,200,120)

#running main simulation for greedy, optimistic Q-values:
R_OptimisticQ = Q_sim(200,0,10,120)

#running the main simulation for epsilon-greedy Q-values:
R_epsilon = Q_sim(200,0.01,0,120)

#running the main simulation for the full-information case: 
R_full = Full_sim(200,120);

#The objectives:
R_Thompson; R_Gittins; R_OptimisticQ; R_epsilon; R_full


