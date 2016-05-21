#DS 2016

#Working Directory
setwd("") #MUST SET WORKING DIRECTORY

#Data
links <- read.csv("NewData/Links.csv")
links$PublishDate <- as.POSIXct(links$PublishDate)
analysis <- read.csv("NewData/Analysis.csv")

ranktable <- read.csv("NewData/RankTable.csv")
ranktable.economy <- read.csv("NewData/RankTable_Economy.csv")
ranktable.microsoft <- read.csv("NewData/RankTable_Microsoft.csv")
ranktable.obama <- read.csv("NewData/RankTable_Obama.csv")
ranktable.palestine <- read.csv("NewData/RankTable_Palestine.csv")

links$X <- NULL
analysis$X <- NULL

analysis.economy <- analysis[analysis$IDLink %in% links[links$Topic=="economy",]$IDLink,]
analysis.microsoft <- analysis[analysis$IDLink %in% links[links$Topic=="microsoft",]$IDLink,]
analysis.obama <- analysis[analysis$IDLink %in% links[links$Topic=="obama",]$IDLink,]
analysis.palestine <- analysis[analysis$IDLink %in% links[links$Topic=="palestine",]$IDLink,]

#EXPERIMENTS#

#LIBRARIES
library(performanceEstimation)
library(uba)
library(bbmle)

#FUNCTIONS
eval.stats <- function(trues,preds,ph,ls) {
  
  prec <- util(preds,trues,ph,ls,util.control(umetric="P",event.thr=0.9))
  rec  <- util(preds,trues,ph,ls,util.control(umetric="R",event.thr=0.9))
  F05  <- util(preds,trues,ph,ls,util.control(umetric="Fm",beta=0.5,event.thr=0.9))
  F1   <- util(preds,trues,ph,ls,util.control(umetric="Fm",beta=1,event.thr=0.9))
  F2   <- util(preds,trues,ph,ls,util.control(umetric="Fm",beta=2,event.thr=0.9))
  
  mad=mean(abs(trues-preds))
  mse=mean((trues-preds)^2)
  mape= mean((abs(trues-preds)/trues))*100
  rmse= sqrt(mean((trues-preds)^2))
  mae_phi= mean(phi(trues,phi.parms=ph)*(abs(trues-preds)))
  mape_phi= mean(phi(trues,phi.parms=ph)*(abs(trues-preds)/trues))*100
  mse_phi= mean(phi(trues,phi.parms=ph)*(trues-preds)^2)
  rmse_phi= sqrt(mean(phi(trues,phi.parms=ph)*(trues-preds)^2))
  prec=prec
  rec=rec
  F05=F05
  F1=F1
  F2=F2
  
  c(
    mse=mse, mse_phi=mse_phi, prec=prec,rec=rec,F1=F1
  )
  
}


#/##########################
# /INITIALIZE TWITTER DATA #
#/##########################

#ADD DUMMY TARGET VARIABLE
columnames <- colnames(ranktable.economy)
columnames <- columnames[2:length(columnames)]
ranktable.economy["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.economy),replace=T),levels=columnames)

columnames <- colnames(ranktable.microsoft)
columnames <- columnames[2:length(columnames)]
ranktable.microsoft["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.microsoft),replace=T),levels=columnames)

columnames <- colnames(ranktable.obama)
columnames <- columnames[2:length(columnames)]
ranktable.obama["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.obama),replace=T),levels=columnames)

columnames <- colnames(ranktable.palestine)
columnames <- columnames[2:length(columnames)]
ranktable.palestine["TgtDummy"] <- factor(sample(columnames,nrow(ranktable.palestine),replace=T),levels=columnames)


#############
# WORKFLOWS #
#############

##KMEANS(IQR)
kmeans.iqr <- function(form,train,test,analysis,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  iqr <- numeric(0)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Get ids from train set
  test.ids <- unique(as.numeric(as.matrix(test[2:101])))
  test.ids <- as.numeric(test.ids[test.ids>0])
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  model.results <- data.frame(TimeSlice=numeric(0),
                              mse=numeric(0),mse_phi=numeric(0),
                              prec=numeric(0),rec=numeric(0),
                              F1=numeric(0))
  
  this.test <- data.frame(IDLink=numeric(0),TimeSlice=numeric(0),Prediction=numeric(0))
  
  for(row in 1:length(test.ids)) {
    
    id.explore <- test.ids[row]
    
    row.explore <- analysis[analysis$IDLink==id.explore,]
    
    if(nrow(row.explore)>0) {
      
      for(ts.exp in 1:3) {
        
        print(paste0(row,"-",length(test.ids),": ",ts.exp))
        
        if(row.explore[,ts.exp+1]==-1) {
          print("...")
          
        } else {
          
          upperbound <- as.numeric(row.explore[ts.exp+1] + iqr[ts.exp])
          lowerbound <- as.numeric(row.explore[ts.exp+1] - iqr[ts.exp])
          lowerbound[lowerbound<0] <- 0
          ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
          
          ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
          ids.intersect <- intersect(ids.over,ids.under)
          ids.intersect <- ids.intersect[ids.intersect != id.explore]
          
          if(length(ids.intersect)==0) {
            iqr_i <- 1
            while(length(ids.intersect)==0) {
              
              upperbound <- as.numeric(row.explore[ts.exp+1] + (iqr[ts.exp]*iqr_i))
              lowerbound <- as.numeric(row.explore[ts.exp+1] - (iqr[ts.exp]*iqr_i))
              lowerbound[lowerbound<0] <- 0
              
              ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
              
              ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
              ids.intersect <- intersect(ids.over,ids.under)
              ids.intersect <- ids.intersect[ids.intersect != id.explore]
              
              iqr_i <- iqr_i + 1
              
            }
          }
          
          ids.4analysis <- this.analysis[this.analysis$IDLink %in% ids.intersect & this.analysis[,ts.exp+1]>=0,]$IDLink
          
          if(length(ids.4analysis)>0) {
            
            results <- data.frame(IDLink=numeric(0),TweetsNow=numeric(0),DiffWeight=double(0),Tweets=numeric(0),KMeansPred=double(0))
            
            for(i in 1:length(ids.4analysis)) {
              
              row.analysis <- this.analysis[this.analysis$IDLink==ids.4analysis[i],]
              
              diff <- as.numeric(abs(row.analysis[ts.exp+1] - row.explore[ts.exp+1]))
              
              final <- as.numeric(row.analysis[ts.exp+1])
              
              id <- ids.4analysis[i]
              tweets.now <- as.numeric(row.analysis[ts.exp+1])
              weight <- as.numeric(1-(diff/final))
              if(is.na(weight)) {weight<-1}
              tweets.final <- as.numeric(row.analysis$TS144)
              
              if(weight>0 & weight<=1) {
                
                kmeanspred <- as.numeric(weight * tweets.final)
                
                row.aux <- data.frame(IDLink=id,TweetsNow=tweets.now,DiffWeight=weight,Tweets=tweets.final,KMeansPred=kmeanspred)
                if(!identical(names(row.aux), names(results))) { names(row.aux) <- names(results) }
                results <- rbind(results,row.aux)
                
              }
              
            }
            weighted.pred <- sum(results$KMeansPred)/sum(results$DiffWeight)
            if(is.na(weighted.pred)) {weighted.pred<-0}
            now.tpt <- row.explore[ts.exp+1]
            if(weighted.pred<now.tpt) {
              weighted.pred <- now.tpt
            }
            
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=weighted.pred)
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          } else {
            
            #if there is no evidence to support the prediction the prediction will be equal to the present amount
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1])
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          }
          
        }
        
      }
      
    }
    
  }
    
  this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
  this.test$Prediction <- unlist(this.test$Prediction)
  
  this.test.util <- this.test
  this.test.util <- this.test.util[this.test.util$NrTweets>=0 & !is.na(this.test.util$NrTweets),]
  this.test.util <- this.test.util[this.test.util$Prediction>=0,]
  
  for(a in 1:3) {
    
    utility.stats <- eval.stats(this.test.util[this.test.util$TimeSlice==a,]$NrTweets,this.test.util[this.test.util$TimeSlice==a,]$Prediction,PHIs,LSs)
    
    model.results_row <- data.frame(TimeSlice=a,
                                    mse=as.numeric(utility.stats[1]),
                                    mse_phi=as.numeric(utility.stats[2]),
                                    prec=as.numeric(utility.stats[3]),
                                    rec=as.numeric(utility.stats[4]),
                                    F1=as.numeric(utility.stats[5]))
    
    if(!identical(names(model.results_row),names(model.results))) {names(models.results_row) <- names(model.results)}
    model.results <- rbind(model.results,model.results_row)
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,modelresults=model.results)
  res
  
}

##KMEANS(IQR) + PRIOR
kmeans.iqr.prior <- function(form,train,test,analysis,p,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  iqr <- numeric(0)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Get ids from train set
  test.ids <- unique(as.numeric(as.matrix(test[2:101])))
  test.ids <- as.numeric(test.ids[test.ids>0])
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  model.results <- data.frame(TimeSlice=numeric(0),
                              mse=numeric(0),mse_phi=numeric(0),
                              prec=numeric(0),rec=numeric(0),
                              F1=numeric(0))
  
  this.test <- data.frame(IDLink=numeric(0),TimeSlice=numeric(0),Prediction=numeric(0))
  
  for(row in 1:length(test.ids)) {
    
    id.explore <- test.ids[row]
    
    row.explore <- analysis[analysis$IDLink==id.explore,]
    
    if(nrow(row.explore)>0) {
      
      for(ts.exp in 1:3) {
        
        print(paste0(row,"-",length(test.ids),": ",ts.exp))
        
        if(row.explore[,ts.exp+1]==-1) {
          print("...")
          
        } else {
          
          upperbound <- as.numeric(row.explore[ts.exp+1] + iqr[ts.exp])
          lowerbound <- as.numeric(row.explore[ts.exp+1] - iqr[ts.exp])
          lowerbound[lowerbound<0] <- 0
          
          ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
          
          ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
          ids.intersect <- intersect(ids.over,ids.under)
          ids.intersect <- ids.intersect[ids.intersect != id.explore]
          
          if(length(ids.intersect)==0) {
            iqr_i <- 1
            while(length(ids.intersect)==0) {
              
              upperbound <- as.numeric(row.explore[ts.exp+1] + (iqr[ts.exp]*iqr_i))
              lowerbound <- as.numeric(row.explore[ts.exp+1] - (iqr[ts.exp]*iqr_i))
              lowerbound[lowerbound<0] <- 0
              
              ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
              
              ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
              ids.intersect <- intersect(ids.over,ids.under)
              ids.intersect <- ids.intersect[ids.intersect != id.explore]
              
              iqr_i <- iqr_i + 1
              
            }
          }
          
          #PRIOR
          
          #ALTERNATIVE
          if(ts.exp==1) {
            #SKIP
          } else {
            
            it.prior <- 0
            if((ts.exp-p)<=0) {
              it.prior <- ts.exp-1
            } else if((ts.exp-p)>0) {
              it.prior <- p
            }
            
            for(var in 1:it.prior) {
              upperbound <- as.numeric(row.explore[ts.exp+1-var] + iqr[ts.exp-var])
              lowerbound <- as.numeric(row.explore[ts.exp+1-var] - iqr[ts.exp-var])
              lowerbound[lowerbound<0] <- 0
              
              ids.over <- this.analysis[this.analysis[,ts.exp+1-var]<=upperbound & this.analysis[,ts.exp+1-var]>=0,]$IDLink
              ids.under <- this.analysis[this.analysis[,ts.exp+1-var]>=lowerbound & this.analysis[,ts.exp+1-var]>=0,]$IDLink
              ids.aux <- intersect(ids.over,ids.under)
              
              if(length(ids.aux)==0) {
                iqr_i <- 1
                while(length(ids.aux)==0) {
                  
                  upperbound <- as.numeric(row.explore[ts.exp+1] + (iqr[ts.exp]*iqr_i))
                  lowerbound <- as.numeric(row.explore[ts.exp+1] - (iqr[ts.exp]*iqr_i))
                  lowerbound[lowerbound<0] <- 0
                  
                  ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
                  
                  ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
                  ids.aux <- intersect(ids.over,ids.under)
                  
                  iqr_i <- iqr_i + 1
                  
                }
              }
              
              if(length(ids.aux)>0) {
                ids.intersect <- intersect(ids.aux,ids.intersect)
              }
            }
            
          }
          
          #/PRIOR 
          
          ids.4analysis <- this.analysis[this.analysis$IDLink %in% ids.intersect & this.analysis[,ts.exp+1]>=0,]$IDLink
          
          if(length(ids.4analysis)>0) {
            
            results <- data.frame(IDLink=numeric(0),TweetsNow=numeric(0),DiffWeight=double(0),Tweets=numeric(0),KMeansPred=double(0))
            
            for(i in 1:length(ids.4analysis)) {
              
              row.analysis <- this.analysis[this.analysis$IDLink==ids.4analysis[i],]
              
              diff <- as.numeric(abs(row.analysis[ts.exp+1] - row.explore[ts.exp+1]))
              
              final <- as.numeric(row.analysis[ts.exp+1])
              
              id <- ids.4analysis[i]
              tweets.now <- as.numeric(row.analysis[ts.exp+1])
              weight <- as.numeric(1-(diff/final))
              if(is.na(weight)) {weight<-1}
              tweets.final <- as.numeric(row.analysis$TS144)
              
              if(weight>0 & weight<=1) {
                
                kmeanspred <- as.numeric(weight * tweets.final)
                
                row.aux <- data.frame(IDLink=id,TweetsNow=tweets.now,DiffWeight=weight,Tweets=tweets.final,KMeansPred=kmeanspred)
                if(!identical(names(row.aux), names(results))) { names(row.aux) <- names(results) }
                results <- rbind(results,row.aux)
                
              }
              
            }
            weighted.pred <- sum(results$KMeansPred)/sum(results$DiffWeight)
            if(is.na(weighted.pred)) {weighted.pred<-0}
            now.tpt <- row.explore[ts.exp+1]
            if(weighted.pred<now.tpt) {
              weighted.pred <- now.tpt
            }
            
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=weighted.pred)
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          } else {
            
            #if there is no evidence to support the prediction the prediction will be equal to the present amount
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1])
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          }
          
        }
        
      }
      
    }
    
  }
  
  this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
  this.test$Prediction <- unlist(this.test$Prediction)
  
  this.test.util <- this.test
  this.test.util <- this.test.util[this.test.util$NrTweets>=0 & !is.na(this.test.util$NrTweets),]
  this.test.util <- this.test.util[this.test.util$Prediction>=0,]
  
  for(a in 1:3) {
    
    utility.stats <- eval.stats(this.test.util[this.test.util$TimeSlice==a,]$NrTweets,this.test.util[this.test.util$TimeSlice==a,]$Prediction,PHIs,LSs)
    
    model.results_row <- data.frame(TimeSlice=a,
                                    mse=as.numeric(utility.stats[1]),
                                    mse_phi=as.numeric(utility.stats[2]),
                                    prec=as.numeric(utility.stats[3]),
                                    rec=as.numeric(utility.stats[4]),
                                    F1=as.numeric(utility.stats[5]))
    
    if(!identical(names(model.results_row),names(model.results))) {names(models.results_row) <- names(model.results)}
    model.results <- rbind(model.results,model.results_row)
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,modelresults=model.results)
  res
  
}

##KMEANS(IQR) + PREDICTION WITH SLOPE
kmeans.iqr.slope <- function(form,train,test,analysis,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  iqr <- numeric(0)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Get ids from train set
  test.ids <- unique(as.numeric(as.matrix(test[2:101])))
  test.ids <- as.numeric(test.ids[test.ids>0])
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  model.results <- data.frame(TimeSlice=numeric(0),
                              mse=numeric(0),mse_phi=numeric(0),
                              prec=numeric(0),rec=numeric(0),
                              F1=numeric(0))
  
  this.test <- data.frame(IDLink=numeric(0),TimeSlice=numeric(0),Prediction=numeric(0))
  
  for(row in 1:length(test.ids)) {
    
    id.explore <- test.ids[row]
    
    row.explore <- analysis[analysis$IDLink==id.explore,]
    
    if(nrow(row.explore)>0) {
      
      for(ts.exp in 1:3) {
        
        print(paste0(row,"-",length(test.ids),": ",ts.exp))
        
        if(row.explore[,ts.exp+1]==-1) {
          print("...")
          
        } else {
          
          upperbound <- as.numeric(row.explore[ts.exp+1] + iqr[ts.exp])
          lowerbound <- as.numeric(row.explore[ts.exp+1] - iqr[ts.exp])
          lowerbound[lowerbound<0] <- 0
          
          ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
          
          ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
          ids.intersect <- intersect(ids.over,ids.under)
          ids.intersect <- ids.intersect[ids.intersect != id.explore]
          
          if(length(ids.intersect)==0) {
            iqr_i <- 1
            while(length(ids.intersect)==0) {
              
              upperbound <- as.numeric(row.explore[ts.exp+1] + (iqr[ts.exp]*iqr_i))
              lowerbound <- as.numeric(row.explore[ts.exp+1] - (iqr[ts.exp]*iqr_i))
              lowerbound[lowerbound<0] <- 0
              
              ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
              
              ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
              ids.intersect <- intersect(ids.over,ids.under)
              ids.intersect <- ids.intersect[ids.intersect != id.explore]
              
              iqr_i <- iqr_i + 1
              
            }
          }
          
          ids.4analysis <- this.analysis[this.analysis$IDLink %in% ids.intersect & this.analysis[,ts.exp+1]>=0,]$IDLink
          
          if(length(ids.4analysis)>0) {
            
            rows.analysis <- analysis[analysis$IDLink %in% ids.intersect & analysis[,ts.exp+1]>=0,]
            
            slope <- numeric(0)
            slope.results <- (rows.analysis$TS144-rows.analysis[,ts.exp+1])/(144-ts.exp)
            #slope <- IQR(slope.results)
            slope <- median(slope.results)
            
            now <- as.numeric(row.explore[,ts.exp+1])
            
            scalar.pred <- now + (144-ts.exp)*slope
            
            
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=scalar.pred)
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          } else {
            
            #if there is no evidence to support the prediction the prediction will be equal to the present amount
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1])
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          }
          
        }
        
      }
      
    }
    
  }
  
  this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
  this.test$Prediction <- unlist(this.test$Prediction)
  
  this.test.util <- this.test
  this.test.util <- this.test.util[this.test.util$NrTweets>=0 & !is.na(this.test.util$NrTweets),]
  this.test.util <- this.test.util[this.test.util$Prediction>=0,]
  
  for(a in 1:3) {
    
    utility.stats <- eval.stats(this.test.util[this.test.util$TimeSlice==a,]$NrTweets,this.test.util[this.test.util$TimeSlice==a,]$Prediction,PHIs,LSs)
    
    model.results_row <- data.frame(TimeSlice=a,
                                    mse=as.numeric(utility.stats[1]),
                                    mse_phi=as.numeric(utility.stats[2]),
                                    prec=as.numeric(utility.stats[3]),
                                    rec=as.numeric(utility.stats[4]),
                                    F1=as.numeric(utility.stats[5]))
    
    if(!identical(names(model.results_row),names(model.results))) {names(models.results_row) <- names(model.results)}
    model.results <- rbind(model.results,model.results_row)
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,modelresults=model.results)
  res
  
}

##KMEANS(IQR) + PRIOR + PREDICTION WITH SLOPE
kmeans.iqr.slope.prior <- function(form,train,test,analysis,p,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  iqr <- numeric(0)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Get ids from train set
  test.ids <- unique(as.numeric(as.matrix(test[2:101])))
  test.ids <- as.numeric(test.ids[test.ids>0])
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  model.results <- data.frame(TimeSlice=numeric(0),
                              mse=numeric(0),mse_phi=numeric(0),
                              prec=numeric(0),rec=numeric(0),
                              F1=numeric(0))
  
  this.test <- data.frame(IDLink=numeric(0),TimeSlice=numeric(0),Prediction=numeric(0))
  
  for(row in 1:length(test.ids)) {
    
    id.explore <- test.ids[row]
    
    row.explore <- analysis[analysis$IDLink==id.explore,]
    
    if(nrow(row.explore)>0) {
      
      for(ts.exp in 1:3) {
        
        print(paste0(row,"-",length(test.ids),": ",ts.exp))
        
        if(row.explore[,ts.exp+1]==-1) {
          print("...")
          
        } else {
          
          upperbound <- as.numeric(row.explore[ts.exp+1] + iqr[ts.exp])
          lowerbound <- as.numeric(row.explore[ts.exp+1] - iqr[ts.exp])
          lowerbound[lowerbound<0] <- 0
          
          ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
          
          ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
          ids.intersect <- intersect(ids.over,ids.under)
          ids.intersect <- ids.intersect[ids.intersect != id.explore]
          
          if(length(ids.intersect)==0) {
            iqr_i <- 1
            while(length(ids.intersect)==0) {
              
              upperbound <- as.numeric(row.explore[ts.exp+1] + (iqr[ts.exp]*iqr_i))
              lowerbound <- as.numeric(row.explore[ts.exp+1] - (iqr[ts.exp]*iqr_i))
              lowerbound[lowerbound<0] <- 0
              
              ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
              
              ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
              ids.intersect <- intersect(ids.over,ids.under)
              ids.intersect <- ids.intersect[ids.intersect != id.explore]
              
              iqr_i <- iqr_i + 1
              
            }
          }
          
          #PRIOR
          
          #ALTERNATIVE
          if(ts.exp==1) {
            #SKIP
          } else {
            
            it.prior <- 0
            if((ts.exp-p)<=0) {
              it.prior <- ts.exp-1
            } else if((ts.exp-p)>0) {
              it.prior <- p
            }
            
            for(var in 1:it.prior) {
              upperbound <- as.numeric(row.explore[ts.exp+1-var] + iqr[ts.exp-var])
              lowerbound <- as.numeric(row.explore[ts.exp+1-var] - iqr[ts.exp-var])
              lowerbound[lowerbound<0] <- 0
              
              ids.over <- this.analysis[this.analysis[,ts.exp+1-var]<=upperbound & this.analysis[,ts.exp+1-var]>=0,]$IDLink
              ids.under <- this.analysis[this.analysis[,ts.exp+1-var]>=lowerbound & this.analysis[,ts.exp+1-var]>=0,]$IDLink
              ids.aux <- intersect(ids.over,ids.under)
              
              if(length(ids.aux)==0) {
                iqr_i <- 1
                while(length(ids.aux)==0) {
                  
                  upperbound <- as.numeric(row.explore[ts.exp+1] + (iqr[ts.exp]*iqr_i))
                  lowerbound <- as.numeric(row.explore[ts.exp+1] - (iqr[ts.exp]*iqr_i))
                  lowerbound[lowerbound<0] <- 0
                  
                  ids.over <- this.analysis[this.analysis[,ts.exp+1]<=upperbound,]$IDLink
                  
                  ids.under <- this.analysis[this.analysis[,ts.exp+1]>=lowerbound,]$IDLink
                  ids.aux <- intersect(ids.over,ids.under)
                  
                  iqr_i <- iqr_i + 1
                  
                }
              }
              
              if(length(ids.aux)>0) {
                ids.intersect <- intersect(ids.aux,ids.intersect)
              }
            }
            
          }
          
          #/PRIOR 
          
          
          ids.4analysis <- this.analysis[this.analysis$IDLink %in% ids.intersect & this.analysis[,ts.exp+1]>=0,]$IDLink
          
          if(length(ids.4analysis)>0) {
            
            rows.analysis <- analysis[analysis$IDLink %in% ids.intersect & analysis[,ts.exp+1]>=0,]
            
            slope <- numeric(0)
            slope.results <- (rows.analysis$TS144-rows.analysis[,ts.exp+1])/(144-ts.exp)
            #slope <- IQR(slope.results)
            slope <- median(slope.results)
            
            now <- as.numeric(row.explore[,ts.exp+1])
            
            scalar.pred <- now + (144-ts.exp)*slope
            
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=scalar.pred)
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          } else {
            
            #if there is no evidence to support the prediction the prediction will be equal to the present amount
            this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1])
            if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
            this.test <- rbind(this.test,this.test.row)
            
          }
          
        }
        
      }
      
    }
    
  }
  
  this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
  this.test$Prediction <- unlist(this.test$Prediction)
  
  this.test.util <- this.test
  this.test.util <- this.test.util[this.test.util$NrTweets>=0 & !is.na(this.test.util$NrTweets),]
  this.test.util <- this.test.util[this.test.util$Prediction>=0,]
  
  for(a in 1:3) {
    
    utility.stats <- eval.stats(this.test.util[this.test.util$TimeSlice==a,]$NrTweets,this.test.util[this.test.util$TimeSlice==a,]$Prediction,PHIs,LSs)
    
    model.results_row <- data.frame(TimeSlice=a,
                                    mse=as.numeric(utility.stats[1]),
                                    mse_phi=as.numeric(utility.stats[2]),
                                    prec=as.numeric(utility.stats[3]),
                                    rec=as.numeric(utility.stats[4]),
                                    F1=as.numeric(utility.stats[5]))
    
    if(!identical(names(model.results_row),names(model.results))) {names(models.results_row) <- names(model.results)}
    model.results <- rbind(model.results,model.results_row)
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,modelresults=model.results)
  res
}

##CONSTANT SCALING
const.scale <- function(form,train,test,analysis,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  iqr <- numeric(0)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Get ids from train set
  test.ids <- unique(as.numeric(as.matrix(test[2:101])))
  test.ids <- as.numeric(test.ids[test.ids>0])
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  model.results <- data.frame(TimeSlice=numeric(0),
                              mse=numeric(0),mse_phi=numeric(0),
                              prec=numeric(0),rec=numeric(0),
                              F1=numeric(0))
  
  this.test <- data.frame(IDLink=numeric(0),TimeSlice=numeric(0),Prediction=numeric(0))
  
  for(row in 1:length(test.ids)) {
    
    id.explore <- test.ids[row]
    
    row.explore <- analysis[analysis$IDLink==id.explore,]
    
    if(nrow(row.explore)>0) {
      
      for(ts.exp in 1:3) {
        
        print(paste0(row,"-",length(test.ids),": ",ts.exp))
        
        if(row.explore[,ts.exp+1]==-1) {
          print("...")
          
        } else {
          
          x <- this.analysis[,paste0("TS",ts.exp)]
          y <- this.analysis$TS144
          
          y <- y[x>=0]
          x <- x[x>=0]
          
          x_test <- row.explore[,paste0("TS",ts.exp)]
          
          alpha <- sum(x/y,na.rm=TRUE)/sum((x/y)^2,na.rm=TRUE)
          
          y_pred <- alpha*x_test
          
          this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=y_pred)
          if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
          this.test <- rbind(this.test,this.test.row)
          
        }
        
      }
      
    }
    
  }
  
  this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
  this.test$Prediction <- unlist(this.test$Prediction)
  
  this.test.util <- this.test
  this.test.util <- this.test.util[this.test.util$NrTweets>=0 & !is.na(this.test.util$NrTweets),]
  this.test.util <- this.test.util[this.test.util$Prediction>=0,]
  
  for(a in 1:3) {
    
    utility.stats <- eval.stats(this.test.util[this.test.util$TimeSlice==a,]$NrTweets,this.test.util[this.test.util$TimeSlice==a,]$Prediction,PHIs,LSs)
    
    model.results_row <- data.frame(TimeSlice=a,
                                    mse=as.numeric(utility.stats[1]),
                                    mse_phi=as.numeric(utility.stats[2]),
                                    prec=as.numeric(utility.stats[3]),
                                    rec=as.numeric(utility.stats[4]),
                                    F1=as.numeric(utility.stats[5]))
    
    if(!identical(names(model.results_row),names(model.results))) {names(models.results_row) <- names(model.results)}
    model.results <- rbind(model.results,model.results_row)
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,modelresults=model.results)
  res
  
}

##LINEAR LOG
linear.log <- function(form,train,test,analysis,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  iqr <- numeric(0)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Get ids from train set
  test.ids <- unique(as.numeric(as.matrix(test[2:101])))
  test.ids <- as.numeric(test.ids[test.ids>0])
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  model.results <- data.frame(TimeSlice=numeric(0),
                              mse=numeric(0),mse_phi=numeric(0),
                              prec=numeric(0),rec=numeric(0),
                              F1=numeric(0))
  
  this.test <- data.frame(IDLink=numeric(0),TimeSlice=numeric(0),Prediction=numeric(0))
  
  for(row in 1:length(test.ids)) {
    
    id.explore <- test.ids[row]
    
    row.explore <- analysis[analysis$IDLink==id.explore,]
    
    if(nrow(row.explore)>0) {
      
      for(ts.exp in 1:3) {
        
        print(paste0(row,"-",length(test.ids),": ",ts.exp))
        
        if(row.explore[,ts.exp+1]==-1) {
          print("...")
          
        } else {
          
          x <- this.analysis[,paste0("TS",ts.exp)]
          y <- this.analysis$TS144
          
          y <- y[x>0]
          x <- x[x>0]
          
          x <- log(x)
          y <- log(y)
          
          x_test <- row.explore[,paste0("TS",ts.exp)]
          if(x_test!=0) { x_test <- log(x_test) }
          
          
          LL <- function(beta0, beta1, mu, sigma) {
            R = y - x * beta1 - beta0
            #
            R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
            #
            -sum(R)
          }
          
          fit <- mle2(LL, start = list(beta0 = 2, beta1 = 1, mu = 0, sigma=1))
          beta0 <- fit@details$par[1]
          sigma2 <- (fit@details$par[4]^2)
          
          y_pred <- exp(as.numeric(x_test+beta0+sigma2/2))
          
          if(is.na(y_pred)) { y_pred <- 0 }
          
          this.test.row <- data.frame(IDLink=test.ids[row],TimeSlice=ts.exp,Prediction=y_pred)
          if(!identical(names(this.test.row), names(this.test))) { names(this.test.row) <- names(this.test) }
          this.test <- rbind(this.test,this.test.row)
          
        }
        
      }
      
    }
    
  }
  
  this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
  this.test$Prediction <- unlist(this.test$Prediction)
  
  this.test.util <- this.test
  this.test.util <- this.test.util[this.test.util$NrTweets>=0 & !is.na(this.test.util$NrTweets),]
  this.test.util <- this.test.util[this.test.util$Prediction>=0,]
  
  for(a in 1:3) {
    
    utility.stats <- eval.stats(this.test.util[this.test.util$TimeSlice==a,]$NrTweets,this.test.util[this.test.util$TimeSlice==a,]$Prediction,PHIs,LSs)
    
    model.results_row <- data.frame(TimeSlice=a,
                                    mse=as.numeric(utility.stats[1]),
                                    mse_phi=as.numeric(utility.stats[2]),
                                    prec=as.numeric(utility.stats[3]),
                                    rec=as.numeric(utility.stats[4]),
                                    F1=as.numeric(utility.stats[5]))
    
    if(!identical(names(model.results_row),names(model.results))) {names(models.results_row) <- names(model.results)}
    model.results <- rbind(model.results,model.results_row)
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,modelresults=model.results)
  res
  
}

################################################

#PERFORMANCE ESTIMATION EXPERIMENTS
#ECONOMY
exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr.slope", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("const.scale", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("linear.log", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

#MICROSOFT
exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr.slope", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("const.scale", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("linear.log", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

#OBAMA
exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr.slope", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("const.scale", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("linear.log", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

#PALESTINE
exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr.slope", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=1),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=2),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("kmeans.iqr.slope.prior", analysis=analysis, p=3),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("const.scale", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("linear.log", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)
