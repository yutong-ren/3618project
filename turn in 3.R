## this project is to use the formula of bond price to write a bond amortization table

C<- 10000
r <- 0.06
i <- 0.04
n <- 10
I <- r/2 ##semi-anual coupon rate
j <- i/2 ##semi-anual interest rate
period <- n*2 ##total period of 10 years
coupon_size <- 10000*I
price <- coupon_size*((1-(1+j)^(-period))/j)+C*((1/(1+j))^period)

interest <- 0
premium <- 0


for(time in 1:period) {
  interest [time+1] <- price[time]*j
  premium [time+1] <- coupon_size-interest [time+1]
  price[time+1] <- price[time]-premium[time+1]
}

matrix <- matrix(c(interest, premium,price), ncol=3)
colnames(matrix) = c("interest", "premium", "book value")
rownames(matrix) = c("duration", 1:period)

print(matrix)





