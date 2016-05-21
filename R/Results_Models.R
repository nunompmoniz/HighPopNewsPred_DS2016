economy.weight <- read.csv("Models/economy_weight.csv")
economy.weight.p1 <- read.csv("Models/economy_weight_p1.csv")
economy.weight.p2 <- read.csv("Models/economy_weight_p2.csv")
economy.weight.p3 <- read.csv("Models/economy_weight_p3.csv")
economy.scalar <- read.csv("Models/economy_scalar.csv")
economy.scalar.p1 <- read.csv("Models/economy_scalar_p1.csv")
economy.scalar.p2 <- read.csv("Models/economy_scalar_p2.csv")
economy.scalar.p3 <- read.csv("Models/economy_scalar_p3.csv")
economy.constscale <- read.csv("Models/economy_constscale.csv")
economy.linearlog <- read.csv("Models/economy_linearlog.csv")

microsoft.weight <- read.csv("Models/microsoft_weight.csv")
microsoft.weight.p1 <- read.csv("Models/microsoft_weight_p1.csv")
microsoft.weight.p2 <- read.csv("Models/microsoft_weight_p2.csv")
microsoft.weight.p3 <- read.csv("Models/microsoft_weight_p3.csv")
microsoft.scalar <- read.csv("Models/microsoft_scalar.csv")
microsoft.scalar.p1 <- read.csv("Models/microsoft_scalar_p1.csv")
microsoft.scalar.p2 <- read.csv("Models/microsoft_scalar_p2.csv")
microsoft.scalar.p3 <- read.csv("Models/microsoft_scalar_p3.csv")
microsoft.constscale <- read.csv("Models/microsoft_constscale.csv")
microsoft.linearlog <- read.csv("Models/microsoft_linearlog.csv")

obama.weight <- read.csv("Models/obama_weight.csv")
obama.weight.p1 <- read.csv("Models/obama_weight_p1.csv")
obama.weight.p2 <- read.csv("Models/obama_weight_p2.csv")
obama.weight.p3 <- read.csv("Models/obama_weight_p3.csv")
obama.scalar <- read.csv("Models/obama_scalar.csv")
obama.scalar.p1 <- read.csv("Models/obama_scalar_p1.csv")
obama.scalar.p2 <- read.csv("Models/obama_scalar_p2.csv")
obama.scalar.p3 <- read.csv("Models/obama_scalar_p3.csv")
obama.constscale <- read.csv("Models/obama_constscale.csv")
obama.linearlog <- read.csv("Models/obama_linearlog.csv")

palestine.weight <- read.csv("Models/palestine_weight.csv")
palestine.weight.p1 <- read.csv("Models/palestine_weight_p1.csv")
palestine.weight.p2 <- read.csv("Models/palestine_weight_p2.csv")
palestine.weight.p3 <- read.csv("Models/palestine_weight_p3.csv")
palestine.scalar <- read.csv("Models/palestine_scalar.csv")
palestine.scalar.p1 <- read.csv("Models/palestine_scalar_p1.csv")
palestine.scalar.p2 <- read.csv("Models/palestine_scalar_p2.csv")
palestine.scalar.p3 <- read.csv("Models/palestine_scalar_p3.csv")
palestine.constscale <- read.csv("Models/palestine_constscale.csv")
palestine.linearlog <- read.csv("Models/palestine_linearlog.csv")

weight <- rbind(economy.weight[economy.weight$TimeSlice==1,],microsoft.weight[microsoft.weight$TimeSlice==1,],obama.weight[obama.weight$TimeSlice==1,],palestine.weight[palestine.weight$TimeSlice==1,])
weight.p1 <- rbind(economy.weight.p1[economy.weight.p1$TimeSlice==1,],microsoft.weight.p1[microsoft.weight.p1$TimeSlice==1,],obama.weight.p1[obama.weight.p1$TimeSlice==1,],palestine.weight.p1[palestine.weight.p1$TimeSlice==1,])
weight.p2 <- rbind(economy.weight.p2[economy.weight.p2$TimeSlice==1,],microsoft.weight.p2[microsoft.weight.p2$TimeSlice==1,],obama.weight.p2[obama.weight.p2$TimeSlice==1,],palestine.weight.p2[palestine.weight.p2$TimeSlice==1,])
weight.p3 <- rbind(economy.weight.p3[economy.weight.p3$TimeSlice==1,],microsoft.weight.p3[microsoft.weight.p3$TimeSlice==1,],obama.weight.p3[obama.weight.p3$TimeSlice==1,],palestine.weight.p3[palestine.weight.p3$TimeSlice==1,])
scalar <- rbind(economy.scalar[economy.scalar$TimeSlice==1,],microsoft.scalar[microsoft.scalar$TimeSlice==1,],obama.scalar[obama.scalar$TimeSlice==1,],palestine.scalar[palestine.scalar$TimeSlice==1,])
scalar.p1 <- rbind(economy.scalar.p1[economy.scalar.p1$TimeSlice==1,],microsoft.scalar.p1[microsoft.scalar.p1$TimeSlice==1,],obama.scalar.p1[obama.scalar.p1$TimeSlice==1,],palestine.scalar.p1[palestine.scalar.p1$TimeSlice==1,])
scalar.p2 <- rbind(economy.scalar.p2[economy.scalar.p2$TimeSlice==1,],microsoft.scalar.p2[microsoft.scalar.p2$TimeSlice==1,],obama.scalar.p2[obama.scalar.p2$TimeSlice==1,],palestine.scalar.p2[palestine.scalar.p2$TimeSlice==1,])
scalar.p3 <- rbind(economy.scalar.p3[economy.scalar.p3$TimeSlice==1,],microsoft.scalar.p3[microsoft.scalar.p3$TimeSlice==1,],obama.scalar.p3[obama.scalar.p3$TimeSlice==1,],palestine.scalar.p3[palestine.scalar.p3$TimeSlice==1,])
constscale <- rbind(economy.constscale[economy.constscale$TimeSlice==1,],microsoft.constscale[microsoft.constscale$TimeSlice==1,],obama.constscale[obama.constscale$TimeSlice==1,],palestine.constscale[palestine.constscale$TimeSlice==1,])
linearlog <- rbind(economy.linearlog[economy.linearlog$TimeSlice==1,],microsoft.linearlog[microsoft.linearlog$TimeSlice==1,],obama.linearlog[obama.linearlog$TimeSlice==1,],palestine.linearlog[palestine.linearlog$TimeSlice==1,])

wilcox.test(weight$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p1$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p2$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p3$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p1$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p2$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p3$F1,constscale$F1,paired=T,alternative="greater")$p.value

wilcox.test(weight$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p1$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p2$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p3$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p1$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p2$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p3$F1,linearlog$F1,paired=T,alternative="greater")$p.value

weight <- rbind(economy.weight[economy.weight$TimeSlice==2,],microsoft.weight[microsoft.weight$TimeSlice==2,],obama.weight[obama.weight$TimeSlice==2,],palestine.weight[palestine.weight$TimeSlice==2,])
weight.p1 <- rbind(economy.weight.p1[economy.weight.p1$TimeSlice==2,],microsoft.weight.p1[microsoft.weight.p1$TimeSlice==2,],obama.weight.p1[obama.weight.p1$TimeSlice==2,],palestine.weight.p1[palestine.weight.p1$TimeSlice==2,])
weight.p2 <- rbind(economy.weight.p2[economy.weight.p2$TimeSlice==2,],microsoft.weight.p2[microsoft.weight.p2$TimeSlice==2,],obama.weight.p2[obama.weight.p2$TimeSlice==2,],palestine.weight.p2[palestine.weight.p2$TimeSlice==2,])
weight.p3 <- rbind(economy.weight.p3[economy.weight.p3$TimeSlice==2,],microsoft.weight.p3[microsoft.weight.p3$TimeSlice==2,],obama.weight.p3[obama.weight.p3$TimeSlice==2,],palestine.weight.p3[palestine.weight.p3$TimeSlice==2,])
scalar <- rbind(economy.scalar[economy.scalar$TimeSlice==2,],microsoft.scalar[microsoft.scalar$TimeSlice==2,],obama.scalar[obama.scalar$TimeSlice==2,],palestine.scalar[palestine.scalar$TimeSlice==2,])
scalar.p1 <- rbind(economy.scalar.p1[economy.scalar.p1$TimeSlice==2,],microsoft.scalar.p1[microsoft.scalar.p1$TimeSlice==2,],obama.scalar.p1[obama.scalar.p1$TimeSlice==2,],palestine.scalar.p1[palestine.scalar.p1$TimeSlice==2,])
scalar.p2 <- rbind(economy.scalar.p2[economy.scalar.p2$TimeSlice==2,],microsoft.scalar.p2[microsoft.scalar.p2$TimeSlice==2,],obama.scalar.p2[obama.scalar.p2$TimeSlice==2,],palestine.scalar.p2[palestine.scalar.p2$TimeSlice==2,])
scalar.p3 <- rbind(economy.scalar.p3[economy.scalar.p3$TimeSlice==2,],microsoft.scalar.p3[microsoft.scalar.p3$TimeSlice==2,],obama.scalar.p3[obama.scalar.p3$TimeSlice==2,],palestine.scalar.p3[palestine.scalar.p3$TimeSlice==2,])
constscale <- rbind(economy.constscale[economy.constscale$TimeSlice==2,],microsoft.constscale[microsoft.constscale$TimeSlice==2,],obama.constscale[obama.constscale$TimeSlice==2,],palestine.constscale[palestine.constscale$TimeSlice==2,])
linearlog <- rbind(economy.linearlog[economy.linearlog$TimeSlice==2,],microsoft.linearlog[microsoft.linearlog$TimeSlice==2,],obama.linearlog[obama.linearlog$TimeSlice==2,],palestine.linearlog[palestine.linearlog$TimeSlice==2,])

wilcox.test(weight$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p1$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p2$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p3$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p1$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p2$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p3$F1,constscale$F1,paired=T,alternative="greater")$p.value

wilcox.test(weight$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p1$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p2$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p3$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p1$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p2$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p3$F1,linearlog$F1,paired=T,alternative="greater")$p.value

weight <- rbind(economy.weight[economy.weight$TimeSlice==3,],microsoft.weight[microsoft.weight$TimeSlice==3,],obama.weight[obama.weight$TimeSlice==3,],palestine.weight[palestine.weight$TimeSlice==3,])
weight.p1 <- rbind(economy.weight.p1[economy.weight.p1$TimeSlice==3,],microsoft.weight.p1[microsoft.weight.p1$TimeSlice==3,],obama.weight.p1[obama.weight.p1$TimeSlice==3,],palestine.weight.p1[palestine.weight.p1$TimeSlice==3,])
weight.p2 <- rbind(economy.weight.p2[economy.weight.p2$TimeSlice==3,],microsoft.weight.p2[microsoft.weight.p2$TimeSlice==3,],obama.weight.p2[obama.weight.p2$TimeSlice==3,],palestine.weight.p2[palestine.weight.p2$TimeSlice==3,])
weight.p3 <- rbind(economy.weight.p3[economy.weight.p3$TimeSlice==3,],microsoft.weight.p3[microsoft.weight.p3$TimeSlice==3,],obama.weight.p3[obama.weight.p3$TimeSlice==3,],palestine.weight.p3[palestine.weight.p3$TimeSlice==3,])
scalar <- rbind(economy.scalar[economy.scalar$TimeSlice==3,],microsoft.scalar[microsoft.scalar$TimeSlice==3,],obama.scalar[obama.scalar$TimeSlice==3,],palestine.scalar[palestine.scalar$TimeSlice==3,])
scalar.p1 <- rbind(economy.scalar.p1[economy.scalar.p1$TimeSlice==3,],microsoft.scalar.p1[microsoft.scalar.p1$TimeSlice==3,],obama.scalar.p1[obama.scalar.p1$TimeSlice==3,],palestine.scalar.p1[palestine.scalar.p1$TimeSlice==3,])
scalar.p2 <- rbind(economy.scalar.p2[economy.scalar.p2$TimeSlice==3,],microsoft.scalar.p2[microsoft.scalar.p2$TimeSlice==3,],obama.scalar.p2[obama.scalar.p2$TimeSlice==3,],palestine.scalar.p2[palestine.scalar.p2$TimeSlice==3,])
scalar.p3 <- rbind(economy.scalar.p3[economy.scalar.p3$TimeSlice==3,],microsoft.scalar.p3[microsoft.scalar.p3$TimeSlice==3,],obama.scalar.p3[obama.scalar.p3$TimeSlice==3,],palestine.scalar.p3[palestine.scalar.p3$TimeSlice==3,])
constscale <- rbind(economy.constscale[economy.constscale$TimeSlice==3,],microsoft.constscale[microsoft.constscale$TimeSlice==3,],obama.constscale[obama.constscale$TimeSlice==3,],palestine.constscale[palestine.constscale$TimeSlice==3,])
linearlog <- rbind(economy.linearlog[economy.linearlog$TimeSlice==3,],microsoft.linearlog[microsoft.linearlog$TimeSlice==3,],obama.linearlog[obama.linearlog$TimeSlice==3,],palestine.linearlog[palestine.linearlog$TimeSlice==3,])

wilcox.test(weight$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p1$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p2$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p3$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p1$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p2$F1,constscale$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p3$F1,constscale$F1,paired=T,alternative="greater")$p.value

wilcox.test(weight$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p1$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p2$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(weight.p3$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p1$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p2$F1,linearlog$F1,paired=T,alternative="greater")$p.value
wilcox.test(scalar.p3$F1,linearlog$F1,paired=T,alternative="greater")$p.value

########################

economy <- data.frame(Model=character(0),F1_1=numeric(0),F1_2=numeric(0),F1_3=numeric(0))
economy <- rbind(economy,data.frame(Model="Weighted",F1_1=mean(economy.weight[economy.weight$TimeSlice==1,]$F1),F1_2=mean(economy.weight[economy.weight$TimeSlice==2,]$F1),F1_3=mean(economy.weight[economy.weight$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="Weighted.p1",F1_1=mean(economy.weight.p1[economy.weight.p1$TimeSlice==1,]$F1),F1_2=mean(economy.weight.p1[economy.weight.p1$TimeSlice==2,]$F1),F1_3=mean(economy.weight.p1[economy.weight.p1$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="Weighted.p2",F1_1=mean(economy.weight.p2[economy.weight.p2$TimeSlice==1,]$F1),F1_2=mean(economy.weight.p2[economy.weight.p2$TimeSlice==2,]$F1),F1_3=mean(economy.weight.p2[economy.weight.p2$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="Weighted.p3",F1_1=mean(economy.weight.p3[economy.weight.p3$TimeSlice==1,]$F1),F1_2=mean(economy.weight.p3[economy.weight.p3$TimeSlice==2,]$F1),F1_3=mean(economy.weight.p3[economy.weight.p3$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="Scalar",F1_1=mean(economy.scalar[economy.scalar$TimeSlice==1,]$F1),F1_2=mean(economy.scalar[economy.scalar$TimeSlice==2,]$F1),F1_3=mean(economy.scalar[economy.scalar$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="Scalar.p1",F1_1=mean(economy.scalar.p1[economy.scalar.p1$TimeSlice==1,]$F1),F1_2=mean(economy.scalar.p1[economy.scalar.p1$TimeSlice==2,]$F1),F1_3=mean(economy.scalar.p1[economy.scalar.p1$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="Scalar.p2",F1_1=mean(economy.scalar.p2[economy.scalar.p2$TimeSlice==1,]$F1),F1_2=mean(economy.scalar.p2[economy.scalar.p2$TimeSlice==2,]$F1),F1_3=mean(economy.scalar.p2[economy.scalar.p2$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="Scalar.p3",F1_1=mean(economy.scalar.p3[economy.scalar.p3$TimeSlice==1,]$F1),F1_2=mean(economy.scalar.p3[economy.scalar.p3$TimeSlice==2,]$F1),F1_3=mean(economy.scalar.p3[economy.scalar.p3$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="ConstScale",F1_1=mean(economy.constscale[economy.constscale$TimeSlice==1,]$F1),F1_2=mean(economy.constscale[economy.constscale$TimeSlice==2,]$F1),F1_3=mean(economy.constscale[economy.constscale$TimeSlice==3,]$F1)))
economy <- rbind(economy,data.frame(Model="LinearLog",F1_1=mean(economy.linearlog[economy.linearlog$TimeSlice==1,]$F1),F1_2=mean(economy.linearlog[economy.linearlog$TimeSlice==2,]$F1),F1_3=mean(economy.linearlog[economy.linearlog$TimeSlice==3,]$F1)))

microsoft <- data.frame(Model=character(0),F1_1=numeric(0),F1_2=numeric(0),F1_3=numeric(0))
microsoft <- rbind(microsoft,data.frame(Model="Weighted",F1_1=mean(microsoft.weight[microsoft.weight$TimeSlice==1,]$F1),F1_2=mean(microsoft.weight[microsoft.weight$TimeSlice==2,]$F1),F1_3=mean(microsoft.weight[microsoft.weight$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="Weighted.p1",F1_1=mean(microsoft.weight.p1[microsoft.weight.p1$TimeSlice==1,]$F1),F1_2=mean(microsoft.weight.p1[microsoft.weight.p1$TimeSlice==2,]$F1),F1_3=mean(microsoft.weight.p1[microsoft.weight.p1$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="Weighted.p2",F1_1=mean(microsoft.weight.p2[microsoft.weight.p2$TimeSlice==1,]$F1),F1_2=mean(microsoft.weight.p2[microsoft.weight.p2$TimeSlice==2,]$F1),F1_3=mean(microsoft.weight.p2[microsoft.weight.p2$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="Weighted.p3",F1_1=mean(microsoft.weight.p3[microsoft.weight.p3$TimeSlice==1,]$F1),F1_2=mean(microsoft.weight.p3[microsoft.weight.p3$TimeSlice==2,]$F1),F1_3=mean(microsoft.weight.p3[microsoft.weight.p3$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="Scalar",F1_1=mean(microsoft.scalar[microsoft.scalar$TimeSlice==1,]$F1),F1_2=mean(microsoft.scalar[microsoft.scalar$TimeSlice==2,]$F1),F1_3=mean(microsoft.scalar[microsoft.scalar$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="Scalar.p1",F1_1=mean(microsoft.scalar.p1[microsoft.scalar.p1$TimeSlice==1,]$F1),F1_2=mean(microsoft.scalar.p1[microsoft.scalar.p1$TimeSlice==2,]$F1),F1_3=mean(microsoft.scalar.p1[microsoft.scalar.p1$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="Scalar.p2",F1_1=mean(microsoft.scalar.p2[microsoft.scalar.p2$TimeSlice==1,]$F1),F1_2=mean(microsoft.scalar.p2[microsoft.scalar.p2$TimeSlice==2,]$F1),F1_3=mean(microsoft.scalar.p2[microsoft.scalar.p2$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="Scalar.p3",F1_1=mean(microsoft.scalar.p3[microsoft.scalar.p3$TimeSlice==1,]$F1),F1_2=mean(microsoft.scalar.p3[microsoft.scalar.p3$TimeSlice==2,]$F1),F1_3=mean(microsoft.scalar.p3[microsoft.scalar.p3$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="ConstScale",F1_1=mean(microsoft.constscale[microsoft.constscale$TimeSlice==1,]$F1),F1_2=mean(microsoft.constscale[microsoft.constscale$TimeSlice==2,]$F1),F1_3=mean(microsoft.constscale[microsoft.constscale$TimeSlice==3,]$F1)))
microsoft <- rbind(microsoft,data.frame(Model="LinearLog",F1_1=mean(microsoft.linearlog[microsoft.linearlog$TimeSlice==1,]$F1),F1_2=mean(microsoft.linearlog[microsoft.linearlog$TimeSlice==2,]$F1),F1_3=mean(microsoft.linearlog[microsoft.linearlog$TimeSlice==3,]$F1)))

obama <- data.frame(Model=character(0),F1_1=numeric(0),F1_2=numeric(0),F1_3=numeric(0))
obama <- rbind(obama,data.frame(Model="Weighted",F1_1=mean(obama.weight[obama.weight$TimeSlice==1,]$F1),F1_2=mean(obama.weight[obama.weight$TimeSlice==2,]$F1),F1_3=mean(obama.weight[obama.weight$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="Weighted.p1",F1_1=mean(obama.weight.p1[obama.weight.p1$TimeSlice==1,]$F1),F1_2=mean(obama.weight.p1[obama.weight.p1$TimeSlice==2,]$F1),F1_3=mean(obama.weight.p1[obama.weight.p1$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="Weighted.p2",F1_1=mean(obama.weight.p2[obama.weight.p2$TimeSlice==1,]$F1),F1_2=mean(obama.weight.p2[obama.weight.p2$TimeSlice==2,]$F1),F1_3=mean(obama.weight.p2[obama.weight.p2$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="Weighted.p3",F1_1=mean(obama.weight.p3[obama.weight.p3$TimeSlice==1,]$F1),F1_2=mean(obama.weight.p3[obama.weight.p3$TimeSlice==2,]$F1),F1_3=mean(obama.weight.p3[obama.weight.p3$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="Scalar",F1_1=mean(obama.scalar[obama.scalar$TimeSlice==1,]$F1),F1_2=mean(obama.scalar[obama.scalar$TimeSlice==2,]$F1),F1_3=mean(obama.scalar[obama.scalar$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="Scalar.p1",F1_1=mean(obama.scalar.p1[obama.scalar.p1$TimeSlice==1,]$F1),F1_2=mean(obama.scalar.p1[obama.scalar.p1$TimeSlice==2,]$F1),F1_3=mean(obama.scalar.p1[obama.scalar.p1$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="Scalar.p2",F1_1=mean(obama.scalar.p2[obama.scalar.p2$TimeSlice==1,]$F1),F1_2=mean(obama.scalar.p2[obama.scalar.p2$TimeSlice==2,]$F1),F1_3=mean(obama.scalar.p2[obama.scalar.p2$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="Scalar.p3",F1_1=mean(obama.scalar.p3[obama.scalar.p3$TimeSlice==1,]$F1),F1_2=mean(obama.scalar.p3[obama.scalar.p3$TimeSlice==2,]$F1),F1_3=mean(obama.scalar.p3[obama.scalar.p3$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="ConstScale",F1_1=mean(obama.constscale[obama.constscale$TimeSlice==1,]$F1),F1_2=mean(obama.constscale[obama.constscale$TimeSlice==2,]$F1),F1_3=mean(obama.constscale[obama.constscale$TimeSlice==3,]$F1)))
obama <- rbind(obama,data.frame(Model="LinearLog",F1_1=mean(obama.linearlog[obama.linearlog$TimeSlice==1,]$F1),F1_2=mean(obama.linearlog[obama.linearlog$TimeSlice==2,]$F1),F1_3=mean(obama.linearlog[obama.linearlog$TimeSlice==3,]$F1)))

palestine <- data.frame(Model=character(0),F1_1=numeric(0),F1_2=numeric(0),F1_3=numeric(0))
palestine <- rbind(palestine,data.frame(Model="Weighted",F1_1=mean(palestine.weight[palestine.weight$TimeSlice==1,]$F1),F1_2=mean(palestine.weight[palestine.weight$TimeSlice==2,]$F1),F1_3=mean(palestine.weight[palestine.weight$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="Weighted.p1",F1_1=mean(palestine.weight.p1[palestine.weight.p1$TimeSlice==1,]$F1),F1_2=mean(palestine.weight.p1[palestine.weight.p1$TimeSlice==2,]$F1),F1_3=mean(palestine.weight.p1[palestine.weight.p1$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="Weighted.p2",F1_1=mean(palestine.weight.p2[palestine.weight.p2$TimeSlice==1,]$F1),F1_2=mean(palestine.weight.p2[palestine.weight.p2$TimeSlice==2,]$F1),F1_3=mean(palestine.weight.p2[palestine.weight.p2$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="Weighted.p3",F1_1=mean(palestine.weight.p3[palestine.weight.p3$TimeSlice==1,]$F1),F1_2=mean(palestine.weight.p3[palestine.weight.p3$TimeSlice==2,]$F1),F1_3=mean(palestine.weight.p3[palestine.weight.p3$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="Scalar",F1_1=mean(palestine.scalar[palestine.scalar$TimeSlice==1,]$F1),F1_2=mean(palestine.scalar[palestine.scalar$TimeSlice==2,]$F1),F1_3=mean(palestine.scalar[palestine.scalar$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="Scalar.p1",F1_1=mean(palestine.scalar.p1[palestine.scalar.p1$TimeSlice==1,]$F1),F1_2=mean(palestine.scalar.p1[palestine.scalar.p1$TimeSlice==2,]$F1),F1_3=mean(palestine.scalar.p1[palestine.scalar.p1$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="Scalar.p2",F1_1=mean(palestine.scalar.p2[palestine.scalar.p2$TimeSlice==1,]$F1),F1_2=mean(palestine.scalar.p2[palestine.scalar.p2$TimeSlice==2,]$F1),F1_3=mean(palestine.scalar.p2[palestine.scalar.p2$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="Scalar.p3",F1_1=mean(palestine.scalar.p3[palestine.scalar.p3$TimeSlice==1,]$F1),F1_2=mean(palestine.scalar.p3[palestine.scalar.p3$TimeSlice==2,]$F1),F1_3=mean(palestine.scalar.p3[palestine.scalar.p3$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="ConstScale",F1_1=mean(palestine.constscale[palestine.constscale$TimeSlice==1,]$F1),F1_2=mean(palestine.constscale[palestine.constscale$TimeSlice==2,]$F1),F1_3=mean(palestine.constscale[palestine.constscale$TimeSlice==3,]$F1)))
palestine <- rbind(palestine,data.frame(Model="LinearLog",F1_1=mean(palestine.linearlog[palestine.linearlog$TimeSlice==1,]$F1),F1_2=mean(palestine.linearlog[palestine.linearlog$TimeSlice==2,]$F1),F1_3=mean(palestine.linearlog[palestine.linearlog$TimeSlice==3,]$F1)))

results <- cbind(economy,microsoft,obama,palestine)
results <- results[,-c(5,9,13)]
colnames(results) <- c("Model","F1_1","F1_2","F1_3","F1_1","F1_2","F1_3","F1_1","F1_2","F1_3","F1_1","F1_2","F1_3")


