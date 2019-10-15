setwd("C:/Users/samml/Desktop/Baseball")


library(tidyverse); library(lambda.tools)

        ### Question 1
              ### load data
              A <- c(1, 3, 5, 5, 7); B <- c(0, 2, 6, 6, 11)

              # R does not have a native function to calculate mode, so I'll make one
              mode.v1 <- function(k) {
                unq <- unique(k)
                unq[which.max(tabulate(match(k, unq)))]
              }
      
              median(A)  > mean(A) ### TRUE, answer is (A)
              mode.v1(A)  > mode.v1(B)
              var(A)  > var(B)
              median(B)  > mode.v1(B)
      
        ### Queston 2
              ### Simulate Dice Throwing; for-loop is not necessary, just part of the tool-kit
                  sim <- 10000; X <- integer(sim); die <- c(1,2,3,4,5,6)
                  for(i in 1:10000){
                    X[i] <- sum(sample(x = die, size =2, replace=T))
                  }
                  
                  X <- sample(x = die, size = sim, replace = T) + sample(x = die, size = sim, replace = T) ## 10,000 rolls of two fair die.
                  
                  barplot(table(X), density = 100, main = "Sum of Two Die, 10000 Rolls", border="#404040") ## plotting the simulation
      
                  length(which(X == 2)) / sim ## p = .02
                  length(which(X >  9)) / sim ## p = .17
                  length(which(X <  7)) / sim ## p = .42
                  length(which(X == 4)) / sim ## p = .08
                  length(which(X >  6)) / sim ## p = .58 (Answer)
            
        ### Question 3
                  x <- rnorm(100000, mean = 1, sd = 1)        ## SD (Standard Deviation) is the square root of the variance (hence, it's still 1)      
                  y <- rnorm(100000, mean = 3, sd = sqrt(5))  ## same logic
                  var(x); var(y)                              ## sanity check: they should approximate 1 and 5

                  var((y - (2*x)))                            ## Answer Approximates 9
        

        ### Question 4
        ### You are asking about conditional probabilities, fundamental to bayes applications (and Bayes Rules in particular)
                  
                  ## Bayes Rule is as follows
                  ## P(A|B) = (P(B|A) * P(A)) / P(B)
                  
                  p.sunny                  <- .9    ## Suppose that 90% of days are sunny in Los Angeles. 
                  p.rainy                  <- .1    ## p.sunny' (1  - p.sunny)
                  p.bike.GIVEN.sunny       <- .75   ## When it's sunny, the probability that I bike to work is 75%. 
                  p.bike.GIVEN.rainy       <- .25   ## When it's not sunny, the probability that I bike to work is 25%. 
                  p.drive.GIVEN.sunny      <- .25
                  
                  ## following Bayes Rule, we can calculate the inverse conditions
                  p.sunny.GIVEN.bike       <- .964
                  p.rainy.GIVEN.bike       <- .036
                   
                  ## Continuing, we can calculate the unconditioned probabilty of biking
                  p.bike                   <- .7002
                  p.drive                  <- 1 - p.bike
                  p.sunny.GIVEN.Driving    <- (p.drive.GIVEN.sunny * p.sunny) / p.drive 
                  ## Answer = .75
                  
        ### Question 5
                  listF <- c(4,3,2,1)               ## From the right
                  fold(listF, function(a,b) a/b, 1) ## from the lambda.tools library 
                  ## Answer = .375
        
        
        