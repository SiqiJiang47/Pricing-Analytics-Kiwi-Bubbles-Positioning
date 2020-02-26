rm(list=ls())
#LOAD packages
library("dummies")
library("AER")
library("plotly")
library('RColorBrewer')
library("data.table")
library("mlogit")
library("gmnl")
####Part One --- Logit Model without segmentation#####
#Estimate a multinomial logit model using gmnl and mlogit
#Read data
#getwd()
data=fread("2020 Spring A/MKT440 Pricing Analytics/project 2/kiwi_bubbles_P2.csv",stringsAsFactors = F)
#Load demographic data
demo=fread("2020 Spring A/MKT440 Pricing Analytics/project 2/demo_P2.csv",stringsAsFactors = F)
#Number of individuals
#Number of individuals
n = 1000
#Drop observations with stockout

data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

#Calculate elasticities using average price
avgPriceKB = mean(data$price.KB) #1.381332
avgPriceKR = mean(data$price.KR) #1.378087
avgPriceMB = mean(data$price.MB) #1.345585
avgPriceAll=c(avgPriceKB,avgPriceKR,avgPriceMB)

##################################### Functions we will use later#####################################
#Estimate multinomial logit model
#Convert data using 'mlogit.data'
mlogitdata = mlogit.data(data, id = 'id', varying = 4:7,choice = 'choice', shape = 'wide')
#Run MLE
mle = gmnl(choice ~ price, data = mlogitdata)
summary(mle) 
#beta0KB = 4.25316, beta0KR = 4.36240, beta0MB = 4.20440, beta1 = -3.73793 
#beta0 of three products are similar, meaning they are selected equally frequently
beta1=-3.73793
beta0KB = 4.25316
beta0KR = 4.36240
beta0MB = 4.20440
#Functions:
#Demand:
#two proudcts:
para=c(beta0KB,beta0KR,beta0MB,beta1)

demand2=function(price1,price2,para){  #2 products, para contain all 4 numbers
  prob=exp(para[1]+para[4]*price1)/(1+exp(para[1]+para[4]*price1)+exp(para[2]+para[4]*price2))
  return(prob)
}

#three products:

demand1=function(priceKB,priceKR,priceMB,para){
  prob=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(prob)
}

#agg_choice for two products
agg_choice2=function(priceKB,priceKR, own, rival) {  #2 products
  m=c(own+1,rival+1,5)
  agg_choice=seg.share[1]*demand2(priceKB,priceKR,as.numeric(coef.est[1,m]))+
    seg.share[2]*demand2(priceKB,priceKR,as.numeric(coef.est[2,m]))+
    seg.share[3]*demand2(priceKB,priceKR,as.numeric(coef.est[3,m]))+
    seg.share[4]*demand2(priceKB,priceKR,as.numeric(coef.est[4,m]))+
    seg.share[5]*demand2(priceKB,priceKR,as.numeric(coef.est[5,m]))+ 
    seg.share[6]*demand2(priceKB,priceKR,as.numeric(coef.est[6,m]))+
    seg.share[7]*demand2(priceKB,priceKR,as.numeric(coef.est[7,m]))+
    seg.share[8]*demand2(priceKB,priceKR,as.numeric(coef.est[8,m]))+
    seg.share[9]*demand2(priceKB,priceKR,as.numeric(coef.est[9,m]))
  return(agg_choice)
}

#agg_choice for three products


agg_choice=function(priceKB,priceKR,priceMB) {
  
  agg_choice=seg.share[1]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))+ 
    seg.share[6]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))+
    seg.share[7]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))+
    seg.share[8]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[8,2:5]))+
    seg.share[9]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[9,2:5]))
  
  return(agg_choice)
}

#sum demand of KB+KR (based on 3 products environment)
demand_KB_KR=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR))
}
demand_KB_KR_MB=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probMB=exp(para[3]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR,probMB))
}
#sum profit of KB+KR (based on 3 products environment)
profit_KB_KR=function(priceKB,priceKR, priceMB,para){
  profitKB=1000*(demand_KB_KR(priceKB,priceKR, priceMB,para)[,1]*(priceKB-0.5))
  profitKR=1000*(demand_KB_KR(priceKB,priceKR, priceMB,para)[,2]*(priceKR-0.5))
  return(cbind(profitKB,profitKR))
}


#################Part 3##############
###Elasticity

#beta1=-3.73793
#beta0KB = 4.25316
#beta0KR = 4.36240
#beta0MB = 4.20440
avgPriceKB = mean(data$price.KB) #1.381332
avgPriceKR = mean(data$price.KR) #1.378087
avgPriceMB = mean(data$price.MB) #1.345585

#KB+KR+MB
demandKB = demand1(avgPriceKB,avgPriceKR,avgPriceMB,para) 
demandKR = demand1(avgPriceKR,avgPriceKB,avgPriceMB,para[c(2,1,3,4)])  
demandMB = demand1(avgPriceMB,avgPriceKB,avgPriceKR,para[c(3,1,2,4)]) 
#Own-price elasticity of KB
ownElasKB = -(beta1)*avgPriceKB*(1-demandKB) #4.222673
#Own-price elasticity of KR
ownElasKR = -(beta1)*avgPriceKR*(1-demandKR) #4.143605
#Own-price elasticity of MB 
ownElasMB = -(beta1)*avgPriceMB*(1-demandMB) #4.072252

#Cross-elasticities
crossElasKR = -(beta1)*avgPriceKR*demandKR #1.012787 -> canibalization
crossElasMB = -(beta1)*avgPriceMB*demandMB #0.9574505
crossElasKB = -(beta1)*avgPriceKB*demandKB #0.9188917
#CREATE DATA FRAME
elas_KB_KR_MB <- data.frame("Product"=c("KB","KR","MB"), "Own Elasticity"=c(ownElasKB,ownElasKR,ownElasMB), 
                            "Cross Price Elasticity"=c(crossElasKB,crossElasKR,crossElasMB))

elas_KB_KR_MB


############2. Optimal Price for KB AND KR

#Multinomial logit: illustration

#Unit cost
uc=0.5

#KB:(intercept) KR:(intercept) MB:(intercept)  price 
#4.253157       4.362403       4.204396      -3.737931 

#Set parameter
#The first element of "para" is beta0KB, the second element is beta0KR and the third beta1, FORTH IS beta0MB.

priceMB=1.43

demand_KB_KR=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR))
}

#Write profit as a function of prices we set and model parameters
profit_KB_KR=function(priceKB,priceKR, priceMB,para){
  profitKB=1000*(demand_KB_KR(priceKB,priceKR, priceMB,para)[,1]*(priceKB-0.5))
  profitKR=1000*(demand_KB_KR(priceKB,priceKR, priceMB,para)[,2]*(priceKR-0.5))
  return(cbind(profitKB,profitKR))
}

#Choose space of prices to search for the optimal price over
#Because we search over two dimensions, create complete combination of the two prices
#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)
#Compute profit at each realization of this price space.
#I write for-loop. While there are ways to get this done in a vectorized way,
#this for-loop code probably helps some in project 2.
#Calculate profit

#At each iteration of the loop, I take one realization of [P^KB,P^KR] pair and evaluate
#profit at that realization.
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
  profitmat[i]=sum(profit_KB_KR(pricespace[i,1],pricespace[i,2],1.43,para))  
}

#Draw figure
xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace[,1],y=pricespace[,2],z=as.numeric(profitmat),
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
  layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
  config(mathjax = 'cdn')

p
max(profitmat) #393.408; optimal price for KB is 1.16, optimal price for KR is 1.16
pricespace[profitmat==max(profitmat)] #optimal price for KB is 1.16, optimal price for KR is 1.16

#######Part 4######################

set.seed(0)
###### try and find the number of segments
#Clustering
#number of individuals in demo
N=283
#Try optimal Number of Clusters
#Clustering&kmeans
#cluster=8
segList = list()
#create a copy of original data
data2 = data
#write a for-loop to test what is the reasonable number of segments
for(i in 1:18){
  demo_cluster = kmeans(x=demo[, 2:18], centers = i, nstart = 1000)
  data2=data
  
  # now combine cluster identity into the raw data
  cluster_id = data.frame(id = demo$id)
  cluster_id$cluster = demo_cluster$cluster
  data2 = merge(data2, cluster_id, by = "id", all.x = T)
  
  # for those who don't fit in any cluster, group them into one additional cluster
  data2$cluster[is.na(data2$cluster)] = i+1
  
  #segment share
  seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N
  
  #input seg.share into segTable
  segList[[i]] = seg.share
  
  #recover original data
  data2 = data2[,1:8]
}
segList[[8]]
segList[[9]]
#we can see from the segList that when we set the number of segments to 9, the segment share becomes very small (0.014) 
#which means that segment contains less than 4 people
#Therefore, we choose 8 segments
##############use 8 segments
demo_cluster = kmeans(x=demo[, 2:18], centers = 8, nstart = 1000)
summary(demo_cluster)

# now combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)

# for those who don't fit in any cluster, group them into one additional cluster
data$cluster[is.na(data$cluster)] = 9

#segment share
seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N


#Estimate multinomial logit model separately for each segment
#store the coefficients
coef.est = data.frame(segment = 1:9, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 

for (seg in 1:9) {
  # During each loop, pick subset of data of consumers from each segment.
  data.sub = subset(data, cluster == seg)
  
  #Using that data, the rest remains the same.
  mlogitdata = mlogit.data(data.sub,id="id",varying=4:7,choice="choice",shape="wide")
  
  #Run MLE.
  mle = gmnl(choice ~  price, data = mlogitdata)
  mle
  #Store the outcome in the coef.est matrix.
  coef.est[seg, 2:5] = mle$coefficients
}
coef.est
popularityCompare = data.frame(segments = 1:9, differKR = coef.est$intercept.KR-coef.est$intercept.KB, 
                               differMB = coef.est$intercept.MB-coef.est$intercept.KB)
popularityCompare
differCompare = data.frame(differKR = mean(popularityCompare$differKR), differMB = mean(popularityCompare$differMB))
differCompare
################################################################
######Elasticity
############### ############### PART (4)################### ############### 
############### ############### PART (4)################### ############### 
#AVG PRICE (3 products, kb kr mb) for all 3 segments

demand1=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probMB=exp(para[3]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR,probMB))
}
agg_choice=function(priceKB,priceKR,priceMB) {  #3 products segment
  
  agg_choice=seg.share[1]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))+ 
    seg.share[6]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))+
    seg.share[7]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[7,2:5]))+
    seg.share[8]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[8,2:5]))+
    seg.share[9]*demand1(priceKB,priceKR,priceMB,as.numeric(coef.est[9,2:5]))
  return(agg_choice)
}

avgPriceVec=list()
for (i in 1:3){
  avgPriceKB = mean(data$price.KB[data$cluster==i]) 
  avgPriceKR = mean(data$price.KR[data$cluster==i]) 
  avgPriceMB = mean(data$price.MB[data$cluster==i]) 
  avgPriceVec[[i]]=c(avgPriceKB,avgPriceKR,avgPriceMB)
}
###aggregate_choice and demand#########

para2=c(beta0KR,beta0MB,beta1)
demand2=function(price1,price2,para2){  #2 products, para contains 3 #s, func SAME NAME
  prob1=exp(para2[1]+para2[3]*price1)/(1+exp(para2[1]+para2[3]*price1)+exp(para2[2]+para2[3]*price2))
  prob2=exp(para2[2]+para2[3]*price1)/(1+exp(para2[1]+para2[3]*price1)+exp(para2[2]+para2[3]*price2))
  return(cbind(prob1,prob2))
}

agg_choice2=function(priceKB,priceKR, own, rival) {  #2 products segment
  m=c(own+1,rival+1,5)
  agg_choice=seg.share[1]*demand2(priceKB,priceKR,as.numeric(coef.est[1,m]))+
    seg.share[2]*demand2(priceKB,priceKR,as.numeric(coef.est[2,m]))+
    seg.share[3]*demand2(priceKB,priceKR,as.numeric(coef.est[3,m]))+
    seg.share[4]*demand2(priceKB,priceKR,as.numeric(coef.est[4,m]))+
    seg.share[5]*demand2(priceKB,priceKR,as.numeric(coef.est[5,m]))+ 
    seg.share[6]*demand2(priceKB,priceKR,as.numeric(coef.est[6,m]))+
    seg.share[7]*demand2(priceKB,priceKR,as.numeric(coef.est[7,m]))+
    seg.share[8]*demand2(priceKB,priceKR,as.numeric(coef.est[8,m]))+
    seg.share[9]*demand2(priceKB,priceKR,as.numeric(coef.est[9,m]))
  return(agg_choice)
}

#own elasticity 
elasticity_own=function(avgPriceAll,own){
  coef2=c(1,2,3)[c(1,2,3)!=own][1]
  coef3=c(1,2,3)[c(1,2,3)!=own][2]
  agg_choice=agg_choice(avgPriceAll[own],avgPriceAll[coef2],avgPriceAll[coef3])
  agg_choicenew=agg_choice(avgPriceAll[own]*1.01,avgPriceAll[coef2],avgPriceAll[coef3])
  (agg_choicenew-agg_choice)/agg_choice
  
} 
KB4=(elasticity_own(avgPriceAll,1)[1])*-100 #KB  -0.04392695 
KR4=(elasticity_own(avgPriceAll,2)[1])*-100 #KR   -0.04356302 
MB4=(elasticity_own(avgPriceAll,3)[1])*-100 #MB  -0.04112575

Own_elas_KB_KR_MB <- data.frame("Product"=c("KB","KR","MB"), "Own Elasticity"=c(KB4,KR4,MB4))
Own_elas_KB_KR_MB 
#cross elasticity 
elasticity_cross=function(avgPriceAll,own){
  e=vector()
  coef2=c(1,2,3)[c(1,2,3)!=own][1]
  coef3=c(1,2,3)[c(1,2,3)!=own][2]
  agg_choice=agg_choice(avgPriceAll[own],avgPriceAll[coef2],avgPriceAll[coef3])[1]
  agg_choicenew1=agg_choice(avgPriceAll[own],avgPriceAll[coef2]*1.01,avgPriceAll[coef3])[1]
  agg_choicenew2=agg_choice(avgPriceAll[own],avgPriceAll[coef2],avgPriceAll[coef3]*1.01)[1]
  
  e=append(e, (agg_choicenew1-agg_choice)/agg_choice)
  e=append(e, (agg_choicenew2-agg_choice)/agg_choice)
  e
  
} 
Cros_KB_KR_MB=(elasticity_cross(avgPriceAll, 1))*100  
Cros_KB_KR_MB
Cros_KR_KB_MB=(elasticity_cross(avgPriceAll, 2))*100
Cros_KR_KB_MB
Cros_MB_KB_KR=(elasticity_cross(avgPriceAll, 3))*100 
Cros_MB_KB_KR


###### (4) last question###########
#Profit
#given MB, look at KR

pricespace=seq(1,3,0.01)
uc=0.5
profit=1000*agg_choice2(pricespace,1.43,2,3)[,1]*(pricespace-uc)
priceKRBest=pricespace[profit==max(profit)] 
priceKRBest#best price for KR=1.07
max(profit)  #max profit for KR 294.4419

#profit for MB
profit_MB=1000*agg_choice2(1.43,priceKRBest,3,2)[,1]*(1.43-uc)  #109.1702  
profit_MB

#GIVEN MB, look at KR, KB
para=c(beta0KB,beta0KR,beta0MB,beta1)
profit_KB_KR_seg=function(priceKB,priceKR, priceMB,para){
  profitKB=1000*(agg_choice(priceKB,priceKR, priceMB)[,1]*(priceKB-0.5))
  profitKR=1000*(agg_choice(priceKB,priceKR, priceMB)[,2]*(priceKR-0.5))
  return(cbind(profitKB,profitKR))
}

aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)

profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
  profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],1.43,para))  
}

max(profitmat) #387.6119; 
priceKB_KRBest=pricespace[profitmat==max(profitmat)] #optimal price for KB is 1.15, optimal price for KR is 1.19
priceKB_KRBest
#MB new profit

profit_MB2=1000*agg_choice(1.43,priceKB_KRBest,priceKB_KRBest)[2]*(1.43-uc)  #244.3255 new MB profit
profit_MB2 #89.63174


#part 5
# round 1
#MANGO:
pricespace=seq(0,2,0.01)
uc=0.5
profit=1000*agg_choice(pricespace,1.15,1.19)[,1]*(pricespace-uc)

max(profit)  #170.9814
pricespace[profit==max(profit)] #0.96

#KB KR
#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
  profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],0.96,para))  
}
max(profitmat) #269.6908
pricespace[profitmat==max(profitmat)] #1.01 1.09

####Round 2
#MANGO:
pricespace=seq(0,2,0.01)
uc=0.5
profit=1000*agg_choice(pricespace,1.01,1.09)[,1]*(pricespace-uc)

max(profit)  #140.0129
pricespace[profit==max(profit)] #0.92

#KB KR
#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
  profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],0.92,para))  
}
max(profitmat) #256.2545
pricespace[profitmat==max(profitmat)] #1.00 1.08
####Round 3
#MANGO:
pricespace=seq(0,2,0.01)
uc=0.5
profit=1000*agg_choice(pricespace,1.00,1.08)[,1]*(pricespace-uc)

max(profit)  #137.4193
pricespace[profit==max(profit)] #0.92

#KB KR
#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.01)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
  profitmat[i]=sum(profit_KB_KR_seg(pricespace[i,1],pricespace[i,2],0.92,para))  
}
max(profitmat) #256.2545
pricespace[profitmat==max(profitmat)] #1.00 1.08
