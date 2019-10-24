### Question B
      # Simulate the sample population (8 people)
      names  <- c("Sam", "Bucky", "James", "Jesse", "Roger", "Bohunk", "BarnCat", "Lucy")
  
      # Simulate trials
      weeks    <- numeric(250000)
      pairsMax <- NULL
      for(i in 1:250000){   
        
          # Simulate pairing selection per week
          pairings <- character()
          
                # As long as there are no repeats, continue drawing
                while(length(pairings) == length(unique(pairings))){
                  
                  # randomize the population
                  k <- sample(names) 
                  
                    # draw pairs
                    for(r in 1:4){
                      
                      # pair everyone in an even position with one in an odd
                      # sort so A|B is treated the same as B|A
                      j <- sort(c(k[c(T,F)][r], k[c(F,T)][r]))
                      pair <- paste(j[1],  j[2], sep=".")
                      pairings <- append(pairings, pair)
                      
                      # create a running list of all pairs created for validation
                      if(i <= 100){pairsMax <- append(pairsMax, pair)}
                    }
                }
      
      # calculate number of weeks until a repeat drawing    
      weeks[i] <- length(pairings) %/% 4
      }   

      # median weeks
      round(mean(weeks) , 3)   # 2.767

      # max possible pairs should equal binomial(8,2)
      length(unique(pairsMax)) # passed
      
      # It isn't possible to go beyond 8 weeks
      max(weeks) <= 8          # passed
    

      
### Question C      
      
      # Load libraries
      library(randomForest) 
      
      # Tune the model (determine the approriate number of parameters to include in each decision tree)
      tune <- tuneRF(genre[,c("Tempo", "Pitch", "Singing", "Instrument", "Length", ...)], factor(genre$Genre),
                     ntreeTry=1000, data=genre, plot=T)

      # Build the RF Model
      genRF <- randomForest(factor(Genre) ~ Tempo + Pitch + Singing + Instrument + Length + ... ,
                            data=genre, ntree=10000, replace=TRUE, keep.forest=TRUE, importance=TRUE, mtry=2)

      # Predict the Genre 
      genre$pred <- genRF$predicted  ## get predictions

      # RF models can provide a useful metric, Out-of-bag (OOB) error, to determine which predictors are doing the most work for us.
      varImpPlot(genRF, pch=19, type=1, main="Variable Importance", col="#E41A1C")
      
      
      
      