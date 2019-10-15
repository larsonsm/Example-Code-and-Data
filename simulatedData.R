setwd("C:/Users/samml/Desktop/Baseball")
library(tidyverse)
library(lambda.tools)
library(glmmTMB); library(lme4)
library(bbmle)
library(DHARMa)
library(MuMIn)
library(circlize)
library(RCurl)

        ### load in simulated data from the github repository
        ### The data simulated are "proximity scans," an instantaneous sample in which all identities of individuals are presently near a focal individual (here: 10m)
        ### It's purpose is to get a sense of who an individual's closest companions are.
            x <- getURL("https://raw.githubusercontent.com/larsonsm/Example-Code-and-Data/master/simData.csv")
            S1 <- read.csv(text = x)
            S1          <- S1[sample (1:nrow (S1), round (.5*nrow (S1))),] ## I'm going to randomly select half for expediency
              
            head(S1, 20)
            
        ### correct a common error in scans (double-counting)
            S1[,5:9] <- lapply(S1[,5:9], as.character)            ## convert from factor to character class
            S2       <- S1[,which(grepl("adult", colnames(S1)))]  ## only send through the loop what's needed
            
            for(i in 1:nrow(S1)){
              partnerIDs <-    as.character(S2[i, which(!S2[i,] == "")])
              unqPartIDs <-    sort(unique(partnerIDs))
              length     <-    length(unqPartIDs)
              S2[i,]     <-    ""
              if(length > 0){S2[i,1:length(unqPartIDs)]    <-    unqPartIDs}
            }; S1[,6:9] <- S2
            
         ### append date information via lubridate package
            S1$date  <- lubridate::mdy    (as.character(S1$date))
            S1$month <- lubridate::month  (S1$date)
            S1$week  <- lubridate::week   (S1$date)
            S1$Q     <- lubridate::quarter(S1$date)
            
         ### Bin proximity scans by individual | Year | Quarter
         ### Track an "isAlone" summary statistic
            S1$bin       <- paste(S1$focalID, S1$Q, sep=".")
            S1$count     <- 0     ; for(i in 1:nrow(S1)){S1$count[i] <- length(which(!S1[i,6:9] == ""))}
            S1$isAlone   <- 0     ; S1$isAlone[S1$count == 0] <- 1
            
         ### Summarizing Data by Bins via dplyr (tidyverse) and piping operators
            S2 <-
              S1 %>%
              dplyr::group_by(bin) %>% 
              dplyr::summarise(count = length(count), nAlone = sum(isAlone), group = mode.v1(group), isFemale = mode.v1(isFemale), pAlone = nAlone / count) %>%
              tidyr::separate(bin, sep="\\.", into=c("ID", "Q")) %>% 
              dplyr::arrange(ID, Q) %>%
              as.data.frame()
            
              S2$Q  <- as.numeric(S2$Q)  
            
          ### Basic Generalized Linear Mixed Modeling (Logistic Regression) and Model Comparison
              
              ### Assume a random intercept for each individual (repeated measures), and a random slope+intercept for week for every combination of group and year
              logR          <- glmmTMB(isAlone ~ isFemale + week + (1 | focalID) + (1 + week | year:group), data=S1, ziformula= ~ 0,  family=binomial)
              
              ### Does assuming the relationship with week of the year is quadratic improve the model?
              logR.poly     <- glmmTMB(isAlone ~ isFemale + poly(week , 2) + (1 | focalID) + (1 + week | year:group), data=S1, ziformula= ~ 0,  family=binomial)
              AICtab(logR, logR.poly) ## it does
              
              ### Does modelling zero-inflation improve the model?
              logR.poly.zi  <- update(logR.poly, ziformula = ~ 1)
              AICtab(logR.poly, logR.poly.zi) ## it doesn't
              
              ### Examine the Model Residuals (Note: the data are made-up, so of course the model is awful)
              simres <- DHARMa::simulateResiduals(logR.poly, n = 1000)
              testResiduals(simres) 
              MuMIn::r.squaredGLMM(logR.poly) ## calculate conditional and marginal R2 for random effects models
              
          
          ### Creating an advanced figure (Radial Calender) summarizing sampling effort for each group by week of the year
              
              ### Create a function that creates a track object for plotting
              calcTrack <- function(dataframe, col1, col2){
        
              D1 <- dataframe
              
              F2          <- colorRampPalette(c(col1, col2), bias = length(unique(D1$count)), space = "rgb", interpolate = "linear") 
              colCodes    <- F2(length(unique(D1$count)))
              colors      <- sapply(D1$count, function(x) colCodes[which(sort(unique(D1$count)) == x)])
              D1$colors   <- colors ## create a color scheme               
              
              circDF           <- data.frame(weekNum = seq(1,52))
              circDF$scanNum   <- 0
              circDF$color     <- "white"
              
              circDF$color  [D1$week] <- D1$colors
              circDF$scanNum[D1$week] <- D1$count
              
              return(circDF)
              
              }

              ### Building the Plot
              ### The following figure leaves a lot to be desired, I would finish this figure in a VGE program like inkscape
              ### Each ring represents a different group, increasing color intensity indicates greater sampling effort
              pdf("radCal.pdf", height = 11, width=9, onefile=F)
              trckH <- .085 *2
              spcH  <- .01 *1.75
              circos.clear()
              factors <- 1:52  
              
              ctF <- S1 %>% filter(S1$group == 1) %>% as.data.frame(); F2 <- colorRampPalette(c("#FFFFFF", "#E41A1C"), bias = 52, space = "rgb", interpolate = "linear"); S2 <- calcTrack(ctF, F2(52)[20], "#E41A1C")
              
              circos.par(gap.degree = 0, cell.padding = c(0, 0, 0, 0), start.degree = 360/20/2, track.margin = c(0, 0), clock.wise = T)
              circos.initialize(factors = factors, xlim = c(0, 1))
              circos.track(ylim = c(0, 1), factors = factors, bg.col = S2$color, track.height = trckH)
              
              circos.track(ylim = c(0, 1), factors = factors, bg.col = "#FFFFFF", track.height = spcH)
              
              ctF <- S1 %>% filter(group == 2) %>% as.data.frame(); F2 <- colorRampPalette(c("#FFFFFF", "#377EB8"), bias = 52, space = "rgb", interpolate = "linear"); S2 <- calcTrack(ctF, F2(52)[20], "#377EB8")
              circos.track(ylim = c(0, 1), factors = factors, bg.col = S2$color, track.height = trckH)
              
              circos.track(ylim = c(0, 1), factors = factors, bg.col = "#FFFFFF", track.height = spcH)
              
              ctF <- S1 %>% filter(group == 3) %>% as.data.frame(); F2 <- colorRampPalette(c("#FFFFFF", "#4DAF4A"), bias = 52, space = "rgb", interpolate = "linear"); S2 <- calcTrack(ctF, F2(52)[20], "#4DAF4A")
              circos.track(ylim = c(0, 1), factors = factors, bg.col = S2$color, track.height = trckH)
              
              circos.track(ylim = c(0, 1), factors = factors, bg.col = "#FFFFFF", track.height = spcH)
              
              ctF <- S1 %>% filter(group == 4) %>% as.data.frame(); F2 <- colorRampPalette(c("#FFFFFF", "#984EA3"), bias = 52, space = "rgb", interpolate = "linear"); S2 <- calcTrack(ctF, F2(52)[20], "#984EA3")
              circos.track(ylim = c(0, 1), factors = factors, bg.col = S2$color, track.height = trckH)
              
              dev.off(); circos.clear()
              browseURL("radCal.pdf")
              
