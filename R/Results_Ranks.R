economy.weight <- read.csv("Ranks/economy_weight.csv")
economy.weight.p1 <- read.csv("Ranks/economy_weight_p1.csv")
economy.weight.p2 <- read.csv("Ranks/economy_weight_p2.csv")
economy.weight.p3 <- read.csv("Ranks/economy_weight_p3.csv")
economy.scalar <- read.csv("Ranks/economy_scalar.csv")
economy.scalar.p1 <- read.csv("Ranks/economy_scalar_p1.csv")
economy.scalar.p2 <- read.csv("Ranks/economy_scalar_p2.csv")
economy.scalar.p3 <- read.csv("Ranks/economy_scalar_p3.csv")
economy.constscale <- read.csv("Ranks/economy_constscale.csv")
economy.linearlog <- read.csv("Ranks/economy_linearlog.csv")
economy.time <- read.csv("Ranks/economy_time.csv")
economy.live <- read.csv("Ranks/economy_live.csv")
economy.source <- read.csv("Ranks/economy_source.csv")

microsoft.weight <- read.csv("Ranks/microsoft_weight.csv")
microsoft.weight.p1 <- read.csv("Ranks/microsoft_weight_p1.csv")
microsoft.weight.p2 <- read.csv("Ranks/microsoft_weight_p2.csv")
microsoft.weight.p3 <- read.csv("Ranks/microsoft_weight_p3.csv")
microsoft.scalar <- read.csv("Ranks/microsoft_scalar.csv")
microsoft.scalar.p1 <- read.csv("Ranks/microsoft_scalar_p1.csv")
microsoft.scalar.p2 <- read.csv("Ranks/microsoft_scalar_p2.csv")
microsoft.scalar.p3 <- read.csv("Ranks/microsoft_scalar_p3.csv")
microsoft.constscale <- read.csv("Ranks/microsoft_constscale.csv")
microsoft.linearlog <- read.csv("Ranks/microsoft_linearlog.csv")
microsoft.time <- read.csv("Ranks/microsoft_time.csv")
microsoft.live <- read.csv("Ranks/microsoft_live.csv")
microsoft.source <- read.csv("Ranks/microsoft_source.csv")

obama.weight <- read.csv("Ranks/obama_weight.csv")
obama.weight.p1 <- read.csv("Ranks/obama_weight_p1.csv")
obama.weight.p2 <- read.csv("Ranks/obama_weight_p2.csv")
obama.weight.p3 <- read.csv("Ranks/obama_weight_p3.csv")
obama.scalar <- read.csv("Ranks/obama_scalar.csv")
obama.scalar.p1 <- read.csv("Ranks/obama_scalar_p1.csv")
obama.scalar.p2 <- read.csv("Ranks/obama_scalar_p2.csv")
obama.scalar.p3 <- read.csv("Ranks/obama_scalar_p3.csv")
obama.constscale <- read.csv("Ranks/obama_constscale.csv")
obama.linearlog <- read.csv("Ranks/obama_linearlog.csv")
obama.time <- read.csv("Ranks/obama_time.csv")
obama.live <- read.csv("Ranks/obama_live.csv")
obama.source <- read.csv("Ranks/obama_source.csv")

palestine.weight <- read.csv("Ranks/palestine_weight.csv")
palestine.weight.p1 <- read.csv("Ranks/palestine_weight_p1.csv")
palestine.weight.p2 <- read.csv("Ranks/palestine_weight_p2.csv")
palestine.weight.p3 <- read.csv("Ranks/palestine_weight_p3.csv")
palestine.scalar <- read.csv("Ranks/palestine_scalar.csv")
palestine.scalar.p1 <- read.csv("Ranks/palestine_scalar_p1.csv")
palestine.scalar.p2 <- read.csv("Ranks/palestine_scalar_p2.csv")
palestine.scalar.p3 <- read.csv("Ranks/palestine_scalar_p3.csv")
palestine.constscale <- read.csv("Ranks/palestine_constscale.csv")
palestine.linearlog <- read.csv("Ranks/palestine_linearlog.csv")
palestine.time <- read.csv("Ranks/palestine_time.csv")
palestine.live <- read.csv("Ranks/palestine_live.csv")
palestine.source <- read.csv("Ranks/palestine_source.csv")

economy <- data.frame(Model=character(0),RP=numeric(0),NDCG=numeric(0))
row <- data.frame(Model="Weight",RP=mean(economy.weight$PRED_RP,na.rm=TRUE),NDCG=mean(economy.weight$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Weight.p1",RP=mean(economy.weight.p1$PRED_RP,na.rm=TRUE),NDCG=mean(economy.weight.p1$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Weight.p2",RP=mean(economy.weight.p2$PRED_RP,na.rm=TRUE),NDCG=mean(economy.weight.p2$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Weight.p3",RP=mean(economy.weight.p3$PRED_RP,na.rm=TRUE),NDCG=mean(economy.weight.p3$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Scalar",RP=mean(economy.scalar$PRED_RP,na.rm=TRUE),NDCG=mean(economy.scalar$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Scalar.p1",RP=mean(economy.scalar.p1$PRED_RP,na.rm=TRUE),NDCG=mean(economy.scalar.p1$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Scalar.p2",RP=mean(economy.scalar.p2$PRED_RP,na.rm=TRUE),NDCG=mean(economy.scalar.p2$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Scalar.p3",RP=mean(economy.scalar.p3$PRED_RP,na.rm=TRUE),NDCG=mean(economy.scalar.p3$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="ConstScale",RP=mean(economy.constscale$PRED_RP,na.rm=TRUE),NDCG=mean(economy.constscale$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="LinearLog",RP=mean(economy.linearlog$PRED_RP,na.rm=TRUE),NDCG=mean(economy.linearlog$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Time",RP=mean(economy.time$PRED_RP,na.rm=TRUE),NDCG=mean(economy.time$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Live",RP=mean(economy.live$PRED_RP,na.rm=TRUE),NDCG=mean(economy.live$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)
row <- data.frame(Model="Source",RP=mean(economy.source$PRED_RP,na.rm=TRUE),NDCG=mean(economy.source$PRED_NDCG,na.rm=TRUE))
economy <- rbind(economy,row)

microsoft <- data.frame(Model=character(0),RP=numeric(0),NDCG=numeric(0))
row <- data.frame(Model="Weight",RP=mean(microsoft.weight$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.weight$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Weight.p1",RP=mean(microsoft.weight.p1$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.weight.p1$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Weight.p2",RP=mean(microsoft.weight.p2$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.weight.p2$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Weight.p3",RP=mean(microsoft.weight.p3$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.weight.p3$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Scalar",RP=mean(microsoft.scalar$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.scalar$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Scalar.p1",RP=mean(microsoft.scalar.p1$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.scalar.p1$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Scalar.p2",RP=mean(microsoft.scalar.p2$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.scalar.p2$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Scalar.p3",RP=mean(microsoft.scalar.p3$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.scalar.p3$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="ConstScale",RP=mean(microsoft.constscale$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.constscale$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="LinearLog",RP=mean(microsoft.linearlog$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.linearlog$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Time",RP=mean(microsoft.time$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.time$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Live",RP=mean(microsoft.live$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.live$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)
row <- data.frame(Model="Source",RP=mean(microsoft.source$PRED_RP,na.rm=TRUE),NDCG=mean(microsoft.source$PRED_NDCG,na.rm=TRUE))
microsoft <- rbind(microsoft,row)


obama <- data.frame(Model=character(0),RP=numeric(0),NDCG=numeric(0))
row <- data.frame(Model="Weight",RP=mean(obama.weight$PRED_RP,na.rm=TRUE),NDCG=mean(obama.weight$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Weight.p1",RP=mean(obama.weight.p1$PRED_RP,na.rm=TRUE),NDCG=mean(obama.weight.p1$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Weight.p2",RP=mean(obama.weight.p2$PRED_RP,na.rm=TRUE),NDCG=mean(obama.weight.p2$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Weight.p3",RP=mean(obama.weight.p3$PRED_RP,na.rm=TRUE),NDCG=mean(obama.weight.p3$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Scalar",RP=mean(obama.scalar$PRED_RP,na.rm=TRUE),NDCG=mean(obama.scalar$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Scalar.p1",RP=mean(obama.scalar.p1$PRED_RP,na.rm=TRUE),NDCG=mean(obama.scalar.p1$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Scalar.p2",RP=mean(obama.scalar.p2$PRED_RP,na.rm=TRUE),NDCG=mean(obama.scalar.p2$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Scalar.p3",RP=mean(obama.scalar.p3$PRED_RP,na.rm=TRUE),NDCG=mean(obama.scalar.p3$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="ConstScale",RP=mean(obama.constscale$PRED_RP,na.rm=TRUE),NDCG=mean(obama.constscale$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="LinearLog",RP=mean(obama.linearlog$PRED_RP,na.rm=TRUE),NDCG=mean(obama.linearlog$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Time",RP=mean(obama.time$PRED_RP,na.rm=TRUE),NDCG=mean(obama.time$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Live",RP=mean(obama.live$PRED_RP,na.rm=TRUE),NDCG=mean(obama.live$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)
row <- data.frame(Model="Source",RP=mean(obama.source$PRED_RP,na.rm=TRUE),NDCG=mean(obama.source$PRED_NDCG,na.rm=TRUE))
obama <- rbind(obama,row)


palestine <- data.frame(Model=character(0),RP=numeric(0),NDCG=numeric(0))
row <- data.frame(Model="Weight",RP=mean(palestine.weight$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.weight$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Weight.p1",RP=mean(palestine.weight.p1$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.weight.p1$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Weight.p2",RP=mean(palestine.weight.p2$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.weight.p2$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Weight.p3",RP=mean(palestine.weight.p3$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.weight.p3$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Scalar",RP=mean(palestine.scalar$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.scalar$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Scalar.p1",RP=mean(palestine.scalar.p1$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.scalar.p1$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Scalar.p2",RP=mean(palestine.scalar.p2$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.scalar.p2$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Scalar.p3",RP=mean(palestine.scalar.p3$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.scalar.p3$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="ConstScale",RP=mean(palestine.constscale$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.constscale$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="LinearLog",RP=mean(palestine.linearlog$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.linearlog$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Time",RP=mean(palestine.time$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.time$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Live",RP=mean(palestine.live$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.live$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)
row <- data.frame(Model="Source",RP=mean(palestine.source$PRED_RP,na.rm=TRUE),NDCG=mean(palestine.source$PRED_NDCG,na.rm=TRUE))
palestine <- rbind(palestine,row)

results <- cbind(economy,microsoft[,c(2,3)],obama[,c(2,3)],palestine[,c(2,3)])

sig.tbl <- data.frame(Win=0,WinSig=0,Tie=0,Lose=0,LoseSig=0)

for(i in 1:10) {
  begin <- (i-1)*72 + 1
  end <- i*72
  rep <- palestine.weight[begin:end,]
  rep_base <- palestine.scalar[begin:end,]
  
  m <- mean(rep$PRED_NDCG,na.rm=TRUE)-mean(rep_base$PRED_NDCG,na.rm=TRUE)
  tst <- wilcox.test(rep$PRED_NDCG,rep_base$PRED_NDCG,paired=T,alternative="greater")$p.value
  
  print(paste0(m,":::",tst))
  
  if(m>0) {
    if(tst<0.05) {
      sig.tbl$WinSig <- sig.tbl$WinSig + 1
    } else {
      sig.tbl$Win <- sig.tbl$Win + 1
    }
  } else if(m<0) {
    if(tst<0.05) {
      sig.tbl$LoseSig <- sig.tbl$LoseSig + 1
    } else {
      sig.tbl$Lose <- sig.tbl$Lose + 1
    }
  } else {
    sig.tbl$Tie <- sig.tbl$Tie + 1
  }
  
}

sig.tbl