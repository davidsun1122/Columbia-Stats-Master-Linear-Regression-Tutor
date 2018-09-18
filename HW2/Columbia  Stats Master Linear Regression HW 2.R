data <- read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt", header = FALSE)
names(data)[1]<-paste("y")
names(data)[2]<-paste("x")
attach(data)

reg <- lm(y ~ x)
summary(reg)
reg

'
b0 = 2.11405, b1 = 0.03883 
y = 0.03883 * x + 2.11405
'
b1 <- reg$coefficients[2][["x"]]

t <- abs(qt(0.995,120))

s <- abs(summary(reg)$coefficients[4])

low<- b1-t*s
high<-b1+t*s

print(low)
print(high)
' with 99% CI:

0.005394756 <= b1 <= 0.072254975 

It does not include 0.

If it include 0, then H0:b1=0 Ha: b1!= 0  will be accepted; thus, simple linear relationship is valid and need to check higher order relationship.
Also, regression equation y = b0 + b1*x could have either positive, negative, or none inclinations.  

'
'b)

alpha = 0.01, two tail

H0: b1 = 0 

Ha: b1 not = 0
'

t_star <- b1/s 

print(t_star)
'
Decision:  reject H0 if |t_star| > t{1-alpha/2,118}

3.039777 > 2.61742

Thus we reject H0: b1 = 0.

'

'
c)
'

p_value <- 2*pt(- abs(t_star),df=118)
print(p_value)
'
p value = 0.0029166 < 0.01. Therefore we reject H0: b1 = 0, which means there is a valid higher order relationship
'