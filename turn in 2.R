
## this code is to set an amortization table and calculate each period's interest, principle repaid, and the out-standing balance


L <- 200000
j<- 0.03
n <- 120
monthly_rate <- (1+j)^(1/12)-1
payment_size <- 200000/((1-(1/(1+monthly_rate))^120)/monthly_rate)
row<-NULL

name = list(row, "outstanding balance")
name2 = list(row,"interestpaid")
name3 = list(row, "principal repaid")
OB<- matrix(0, 1+n, 1, dimnames= name)
I <- matrix(0, 1+n, 1, dimnames= name2)
PR<- matrix(0, 1+n, 1, dimnames= name3)
OB[1]<- L
I[1]<-0
PR[1]<-0

for (time in 1:n){
  I[time + 1] <- OB[time] * (monthly_rate)
  PR[time + 1] <- payment_size -(monthly_rate*OB[time])
  OB[time+1] <- OB[time] - PR[time+1]
}
x<- cbind(OB, I, PR)

print(x)

