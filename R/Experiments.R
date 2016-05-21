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
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        id.explore <- this.test[row,]$IDLink
        
        row.explore <- analysis[analysis$IDLink==id.explore,]
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
        print(paste0(length(ids.4analysis),"-",row))
        
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
              results <- rbind(results,row.aux)
              
            }
            
          }
          weighted.pred <- sum(results$KMeansPred)/sum(results$DiffWeight)
          if(is.na(weighted.pred)) {weighted.pred<-0}
          now.tpt <- row.explore[ts.exp+1]
          if(weighted.pred<now.tpt) {
            weighted.pred <- now.tpt
          }
          
          this.test[row,]$Prediction <- weighted.pred
          
        } else {
          
          #if there is no evidence to support the prediction the prediction will be equal to the present amount
          this.test[row,]$Prediction <- analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1]
          
          
        }
        
        this.test
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
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
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        
        id.explore <- this.test[row,]$IDLink
        
        row.explore <- analysis[analysis$IDLink==id.explore,]
        
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
        print(paste0(length(ids.4analysis),"-",row))
        
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
              results <- rbind(results,row.aux)
              
            }
            
          }
          weighted.pred <- sum(results$KMeansPred)/sum(results$DiffWeight)
          if(is.na(weighted.pred)) {weighted.pred<-0}
          now.tpt <- row.explore[ts.exp+1]
          if(weighted.pred<now.tpt) {
            weighted.pred <- now.tpt
          }
          this.test[row,]$Prediction <- weighted.pred
          
        } else {
          
          #if there is no evidence to support the prediction the prediction will be equal to the present amount
          this.test[row,]$Prediction <- analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1]
          
          
        }
        
        this.test
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
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
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        
        id.explore <- this.test[row,]$IDLink
        
        row.explore <- analysis[analysis$IDLink==id.explore,]
        
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
        print(paste0(length(ids.4analysis),"-",row))
        
        if(length(ids.4analysis)>0) {
          
          rows.analysis <- analysis[analysis$IDLink %in% ids.intersect & analysis[,ts.exp+1]>=0,]
          
          slope <- numeric(0)
          slope.results <- (rows.analysis$TS144-rows.analysis[,ts.exp+1])/(144-ts.exp)
          #slope <- IQR(slope.results)
          slope <- median(slope.results)
          
          now <- as.numeric(row.explore[,ts.exp+1])
          
          this.test[row,]$Prediction <- now + (144-ts.exp)*slope
          
          
        } else {
          
          #if there is no evidence to support the prediction the prediction will be equal to the present amount
          this.test[row,]$Prediction <- analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1]
          
          
        }
        
        this.test
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
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
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  for(i in 2:145) {
    values <- this.analysis[this.analysis[,i]>=0,][,i]
    iqr <- c(iqr,IQR(values[!(values %in% boxplot.stats(values)$out)]))
  }
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        
        id.explore <- this.test[row,]$IDLink
        
        row.explore <- analysis[analysis$IDLink==id.explore,]
        
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
        print(paste0(length(ids.4analysis),"-",row))
        
        if(length(ids.4analysis)>0) {
          
          rows.analysis <- analysis[analysis$IDLink %in% ids.intersect & analysis[,ts.exp+1]>=0,]
          
          slope <- numeric(0)
          slope.results <- (rows.analysis$TS144-rows.analysis[,ts.exp+1])/(144-ts.exp)
          slope <- median(slope.results)
          
          now <- as.numeric(row.explore[,ts.exp+1])
          
          this.test[row,]$Prediction <- now + (144-ts.exp)*slope
          
          
        } else {
          
          #if there is no evidence to support the prediction the prediction will be equal to the present amount
          this.test[row,]$Prediction <- analysis[analysis$IDLink==this.test[row,]$IDLink,][,ts.exp+1]
          
          
        }
        
        this.test
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
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
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        
        id.explore <- this.test[row,]$IDLink
        
        row.explore <- analysis[analysis$IDLink==id.explore,]
        
        x <- this.analysis[,paste0("TS",ts.exp)]
        y <- this.analysis$TS144
        
        y <- y[x>=0]
        x <- x[x>=0]
        
        x_test <- row.explore[,paste0("TS",ts.exp)]
        
        alpha <- sum(x/y,na.rm=TRUE)/sum((x/y)^2,na.rm=TRUE)
        
        y_pred <- alpha*x_test
        print(paste0(y_pred,"-",row.explore$TS144))
        
        this.test[row,]$Prediction <- y_pred
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
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
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        
        id.explore <- this.test[row,]$IDLink
        
        row.explore <- analysis[analysis$IDLink==id.explore,]
        
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
        
        fit <- mle2(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma=1))
        beta0 <- fit@details$par[1]
        sigma2 <- (fit@details$par[4]^2)
        
        y_pred <- exp(as.numeric(x_test+beta0+sigma2/2))
        print(paste0(y_pred,"-",row.explore$TS144))
        
        if(is.na(y_pred)) { y_pred <- 0 }
        
        this.test[row,]$Prediction <- y_pred
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
  res
  
}

##TIME
baseline.time <- function(form,train,test,analysis,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PublishDate"] <- links[match(this.test$IDLink,links$IDLink),]$PublishDate
    this.test["PredictedRank"] <- nrow(this.test) - rank(this.test$PublishDate,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
  res
  
}

##LIVE
baseline.live <- function(form,train,test,analysis,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        
        id.explore <- this.test[row,]$IDLink
        
        row.explore <- analysis[analysis$IDLink==id.explore,]
        
        y_pred <- row.explore[,paste0("TS",ts.exp)]
        
        if(is.na(y_pred)) { y_pred <- 0 }
        
        this.test[row,]$Prediction <- y_pred
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
  res
  
}

##SOURCE
baseline.source <- function(form,train,test,analysis,...) {
  
  #Get PHIs and LSs
  utility.data.ids <- unique(as.numeric(as.matrix(train[,2:101])))
  utility.data <- analysis[match(utility.data.ids,analysis$IDLink),]
  utility.ntweets <- utility.data[!is.na(utility.data$TS144) & utility.data$TS144>=0,]$TS144
  PHIs <- phi.control(utility.ntweets, method="extremes", coef=3)
  LSs <- loss.control(utility.ntweets)
  
  #Get ids from train set
  train.ids <- unique(as.numeric(as.matrix(train[2:101])))
  
  #Reduce to those which have a time difference bigger than 2 days with the lowest timestamp in the testset
  train.ids <- train.ids[difftime(as.POSIXct(test[1,]$Timestamp),links[links$IDLink %in% train.ids,]$PublishDate,unit="hours")>48]
  
  #Analysis matrix
  this.analysis <- analysis[analysis$IDLink %in% train.ids,]
  
  global.results <- data.frame(Case=numeric(0),
                               PRED_AP=numeric(0),PRED_RP=numeric(0),
                               PRED_RR=numeric(0),PRED_NDCG=numeric(0))
  
  rank.results <- data.frame(TimeSlice=numeric(0), PredictedRank=numeric(0))
  
  ranks <- data.frame(TimeSlice=numeric(0), TwitterRank=numeric(0), PredictedRank=numeric(0))
  
  for(test.i in 1:nrow(test)) {
    
    test.one <- test[test.i,]
    print(paste0(test.i,"-",nrow(test)))
    
    test.ids <- as.numeric(as.matrix(test.one[2:101]))
    
    this.test <- data.frame(IDLink=test.ids)
    
    this.test["GoogleRank"] <- seq(1,nrow(this.test),by=1)
    
    this.test <- this.test[this.test$IDLink>=0,]
    
    this.test["TimeDiff"] <- NA
    for(time.i in 1:nrow(this.test)) {
      this.test[time.i,]$TimeDiff <- difftime(test.one$Timestamp,links[links$IDLink == this.test[time.i,]$IDLink,]$PublishDate,unit="mins")
    }
    
    #Add column with reference to the timeslice to which the row is associated (1=15min, 2=30min, and so on)
    this.test["TimeSlice"] <- -1
    
    #Add column with prediction
    this.test["Prediction"] <- -1
    
    #Intervals of analysis
    timeslice <- 20
    
    #Add reference to timeslice in each row -> explanation for timeslice.i - (144*20 = 2880 minutes = 48 hours = 2 days)
    timeslice.i <- 144
    
    for(i in 1:timeslice.i) {
      this.test.aux <- this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]
      if(nrow(this.test.aux)>0) {
        this.test[this.test$TimeDiff<=(timeslice*i) & this.test$TimeSlice<0,]$TimeSlice <- i
      }
    }
    
    for(row in 1:nrow(this.test)) {
      
      ts.exp <- this.test[row,]$TimeSlice
      
      if(ts.exp<=0 | ts.exp==144) {
        #PREDICTION IS EQUAL TO THE T144
        real.tpt <- analysis[analysis$IDLink==this.test[row,]$IDLink,]$TS144
        if(length(real.tpt)>0) {
          this.test[row,]$Prediction <- real.tpt
        } else {
          this.test[row,]$Prediction <- 0
        }
        
      } else {
        
        id.explore <- this.test[row,]$IDLink
        
        src <- as.character(links[links$IDLink==id.explore,]$Source)
        src.ids <- links[links$IDLink %in% train.ids & links$Source==src,]$IDLink
        
        if(length(src.ids)==0) {
          this.test[row,]$Prediction <- 0
        } else {
          vls <- this.analysis[this.analysis$IDLink %in% src.ids,]$TS144
          vls <- vls[vls!=-1]
          this.test[row,]$Prediction <- mean(vls)
        }
        
      }
      
    }
    
    this.test["NrTweets"] <- analysis[match(this.test$IDLink,analysis$IDLink),]$TS144
    this.test$Prediction <- unlist(this.test$Prediction)
    this.test["TwitterRank"] <- rank(-this.test$NrTweets,ties.method="random")
    this.test["PredictedRank"] <- rank(-this.test$Prediction,ties.method="random")
    
    this.test <- this.test[with(this.test,order(this.test$TwitterRank)),]
    
    prediction_ap <- AP(this.test$PredictedRank,10)
    prediction_rp <- RPREC(this.test$PredictedRank,10)
    prediction_rr <- RR(this.test$PredictedRank,10)
    prediction_ndcg <- NDCGatK(this.test$PredictedRank,10)
    
    global.results.row <- data.frame(Case=test.i,
                                     PRED_AP=prediction_ap,PRED_RP=prediction_rp,
                                     PRED_RR=prediction_rr,PRED_NDCG=prediction_ndcg)
    
    global.results <- rbind(global.results,global.results.row)
    
    rt_pred <- this.test[this.test$PredictedRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt_pred[rt_pred$TwitterRank<=10,])>0) rt_pred[rt_pred$TwitterRank<=10,]$PredictedRank <- 1
    if(nrow(rt_pred[rt_pred$TwitterRank>10,])>0) rt_pred[rt_pred$TwitterRank>10,]$PredictedRank <- 0
    rt_pred$TwitterRank <- NULL
    rank.results <- rbind(rank.results,rt_pred)
    
    rt <- this.test[this.test$TwitterRank<=10 & this.test$TimeSlice>0,c("TimeSlice","TwitterRank","PredictedRank")]
    if(nrow(rt)>0) {
      if(nrow(rt[rt$PredictedRank<=10,])>0) rt[rt$PredictedRank<=10,]$PredictedRank <- 1
      if(nrow(rt[rt$PredictedRank>10,])>0) rt[rt$PredictedRank>10,]$PredictedRank <- 0
      ranks <- rbind(ranks,rt)
    }
    
  }
  
  #INFORMATION RETURN
  trues_aux <- responseValues(form,test)
  preds_aux <- sample(levels(trues_aux),nrow(test),replace=TRUE)
  res <- list(trues=trues_aux,preds=preds_aux,globalresults=global.results,ranktable=rank.results,ranks=ranks)
  res
  
}
#/############
# /WORKFLOWS #
#/############


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

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("baseline.time", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("baseline.live", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.economy),
                             Workflow("baseline.source", analysis=analysis),
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

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("baseline.time", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("baseline.live", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.microsoft),
                             Workflow("baseline.source", analysis=analysis),
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

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("baseline.time", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("baseline.live", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.obama),
                             Workflow("baseline.source", analysis=analysis),
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

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("baseline.time", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("baseline.live", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

exp <- performanceEstimation(PredTask(TgtDummy ~ .,ranktable.palestine),
                             Workflow("baseline.source", analysis=analysis),
                             EstimationTask("err",method=MonteCarlo(nReps=10,szTrain=504,szTest=72))
)

#/###########################
# /MONTE CARLOS EXPERIMENTS #
#/###########################
