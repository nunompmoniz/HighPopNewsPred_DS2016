library(ggplot2)
library(lsr)

# Function that returns Mean Absolute Error
mae <- function(error) {
  mean(abs(error))
}

mse <- function(error) {
  mean((error)^2, na.rm = TRUE)
}

trues <-    c(2.71,3.35,3.46,3.73,4.08,4.16,4.31,5.55,5.78,6.40)
m1 <-       c(2.67,3.29,3.43,3.77,3.97,4.28,4.54,4.91,4.33,4.72)
m2 <-       c(1.03,4.59,2.74,4.18,3.20,4.93,3.42,5.59,5.74,6.37)

error_m1 <- m1 - trues
error_m2 <- m2 - trues

trues_name <- c("true","true","true","true","true","true","true","true","true","true")
m1_name <- c("M1","M1","M1","M1","M1","M1","M1","M1","M1","M1")
m2_name <- c("M2","M2","M2","M2","M2","M2","M2","M2","M2","M2")

table <- data.frame(trues=c(trues,trues),value=c(m2,m1),model=c(m2_name,m1_name))
table$model <- factor(table$model)
misleadingscenario <- ggplot(table,aes(x=trues,y=value,shape=model,color=model)) + geom_point(size=4) + ylim(0,8) + xlim(0,8) + geom_abline(h=0) + xlab("y") + ylab(expression(hat(y))) + geom_vline(xintercept=5,linetype="longdash") + geom_hline(yintercept=5,linetype="longdash")

mae(error_m1)
mae(error_m2)
aad(error_m1)
aad(error_m2)
mse(error_m1)
mse(error_m2)

pdf("misleadingscenario.pdf",width=8,height=4)
print(misleadingscenario)
dev.off()