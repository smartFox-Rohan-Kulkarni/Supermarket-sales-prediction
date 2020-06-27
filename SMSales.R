#supermarket sales

#install.packages("xlsx")
library("xlsx")
library("ggplot2")
library(expss)
library(lubridate)

SMSales <- read.csv(file="C:/Users/Dell/Documents/supermarket-sales/supermarket_sales.csv", header=TRUE)
which(duplicated(SMSales$Invoice.ID)) # to check if any row data is duplicate 
index=c(1:nrow(SMSales))
SMSales=data.frame(index,SMSales)

#which(is.na(SMSales))
#SMSales$gross.income[which(is.na(SMSales)]<-SMSales$Quantity[which(is.na(SMSales))]
#SMSales$Quantity[is.na(SMSales$gross.income)]

#SMSales$gross.income
giFunction=function(){
  # Total Slaes stats per branch
  branch_A<-SMSales[SMSales$Branch=="A",1:ncol(SMSales)]
  nrow(branch_A)
  sA=sum(branch_A$gross.income)
  
  branch_B<-SMSales[SMSales$Branch=="B",1:ncol(SMSales)]
  nrow(branch_B)
  sB=sum(branch_B$gross.income)
  
  branch_C<-SMSales[SMSales$Branch=="C",1:ncol(SMSales)]
  nrow(branch_C)
  sC=sum(branch_C$gross.income)
  totalABC=sum(sA,sB,sC)
  SumABC=data.frame(sA,sB,sC,totalABC)
  SumABC=apply_labels(SumABC,sA="total gross income of A",sB="total gross income of B",sC="total gross income of C",totalABC="Total GI")
  cro(SumABC,total_label ="")
}

productStats=function(){
  #Stats in productLine
  Product_line<-levels(SMSales$Product.line)
  qty=1
  ttl=1
  cogst=1
  grossIncome=1
  for(x in 1:6){
    qty[x]<-sum(SMSales$Quantity[SMSales$Product.line==Product_line[x]])
    ttl[x]<-sum(SMSales$Total[SMSales$Product.line==Product_line[x]])
    cogst[x]<-sum(SMSales$cogs[SMSales$Product.line==Product_line[x]])
    grossIncome[x]<-sum(SMSales$gross.income[SMSales$Product.line==Product_line[x]])
  }
  Product_stats<-data.frame(Product_line,qty,ttl,cogst,grossIncome)
  print(Product_stats)
  return(Product_stats)
}
giFunction()
Product_stats<-productStats()

#PorductLine vs qty
ggplot(data=Product_stats, aes(x=Product_line, y=qty)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=qty), vjust=1.6, color="white", size=3.5)
#Productline vs cogs
ggplot(data=Product_stats, aes(x=Product_line, y=cogst)) +
  geom_bar(stat="identity", fill="red")+
  geom_text(aes(label=cogst), vjust=1.6, color="white", size=3.5)
#productline vs gi
ggplot(data=Product_stats, aes(x=Product_line, y=grossIncome)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=grossIncome), vjust=1.6, color="white", size=3.5)

#Stats on methods of Payment
ggplot(SMSales,aes(x=factor(Payment),fill=factor(Payment)))+
  geom_bar()+ 
  geom_text(aes(label=..count..),stat="count")



#Total Product Sales and gender interests stats
ggplot(SMSales,aes(x=Product.line))+
  geom_bar(color="Red",fill="yellow")+
  geom_text(aes(label=..count..),stat="count")                                                           

ggplot(SMSales,aes(x=Product.line,fill=Gender))+
  geom_bar(position="dodge")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#month wise segretaion of cogs 
D<-SMSales$Date
#D
income=1
for(x in 1:length(D))
  income[x]=sum(SMSales$cogs[SMSales$Date==D[x]])
a=D
c=gsub("/", '-',a)
x=as.Date(as.character(c), "%m-%d-%Y")
#x
#r=as.Date(gsub("0019", '2019',x,fixed = T))
x1<-month(as.POSIXlt(x,format="%m/%d/%Y"))
D2<-data.frame(x,x1,income)
#D2
SMSales<-data.frame(SMSales,x)
jan=1
feb=2
mar=3
ch=as.integer(readline(prompt="Enter month :as 1:jan 2:feb etc "))
switch(ch,ggplot(data=D2[D2$x1 %in% jan,], aes(x=D2$x[D2$x1 %in% jan], y=D2$income[D2$x1 %in% jan], group = 1)) +geom_line()+geom_point(),
       ggplot(data=D2[D2$x1 %in% feb,], aes(x=D2$x[D2$x1 %in% feb], y=D2$income[D2$x1 %in% feb], group = 1)) +geom_line()+geom_point(),
       ggplot(data=D2[D2$x1 %in% mar,], aes(x=D2$x[D2$x1 %in% mar], y=D2$income[D2$x1 %in% mar], group = 1)) +geom_line()+geom_point(),)




#Does rating get affected with the tpr of parameter?

rCash<-sum(SMSales$Rating[SMSales$Payment=="Cash"])
rCC<-sum(SMSales$Rating[SMSales$Payment=="Credit card"])
rEW<-sum(SMSales$Rating[SMSales$Payment=="Ewallet"])
sum=c(rCash,rCC,rEW)
type_of_payment=c("Cash","Credit card","Ëwallet")
d<-data.frame(type_of_payment=type_of_payment , sum=sum)
ggplot(d,aes(x=type_of_payment, y=sum, fill=factor(type_of_payment)))+ 
  geom_bar(stat="identity") +
  geom_text(aes(label=sum), vjust=1.6, color="white", size=3.5)

pds=data.frame(a=c(1),b=c(2),c=c(3))
pie(pds,labels=names)
#from the graph we can analyze that people who made payment with Cash and EW they had a btr exp than people who made payment with CC


#Sales vs Time


day1=as.integer(readline(prompt="Enter day"))
month1=as.integer(readline(prompt="Enter month"))
year1=as.integer(readline(prompt="Enter year"))

as.Date(paste(year1,month1,day1,sep="-"),format = "%Y-%m-%d")->datet
#datet
datet
#SMSales$x
ggplot(SMSales[SMSales$x==datet,],aes(x=SMSales$Time[SMSales$x==datet],y=SMSales$gross.income[SMSales$x==datet],group=1))+
  geom_line()+geom_point()
