# for MSEM, diff iccs are considered
options(java.parameters = "-Xmx1024m")
library(MplusAutomation)
library(data.table)
library(ggplot2)
library(reshape2)
library(reshape)
library(gridExtra)
library(lattice)
library(dplyr)
#==================organize result and write xlsx===========
wdID<-c(1)
s <-4
studys<-c("study1","study2","study3",'supp')
studyType <-studyTypes[s]
wd0<-"D:/current/"       
resultwd<-paste(wd0,"AllResults",sep="")
conditions<-6
con<-conditions+3

iccs<-c(0.05,0.1,0.2)#
iccLs<-c("005","01","02")#
if(s>=3){
  iccs<-c(0.1,0.2)
  iccLs<-c("01","02")
}
report<-c("RB","SE_SD","MSE","95Coverage","Power")
NewEstiLevel<-c("ml","wlsmv","nonif","l_3sd_w","l_1sd_w","cor_w","r_1sd_w","r_3sd_w",
                "l_3sd_m","l_1sd_m","cor_m","r_1sd_m","r_3sd_m","l_3sd_s",
                "l_1sd_s","cor_s","r_1sd_s","r_3sd_s")
                
NR<-100
#set wd & param
filt_p<-c("b11","b21","b31","b01","b10","b20","b30")
if (wdID==1){
  wd<-paste(wd0,"every/",sep="")#
  param<-c("re_yw","b11","b21","b31","b01","b00",
           "b10","b20","b30","re_yb","g10^2","g20^2","g30^2")
  #C for concerned, L for linearly, V for var, N for not important
  #lable<-c("V","C","C","C","C","N",
  #         "L","L","L","V","V","V","V")
  if(s==3){  
    param<-c("b11","b21","b31","b01","b10",
             "b20","b30","b00","re_yb","g10^2","g20^2","g30^2")
    lable<-c("C","C","C","C","L","L","L","L","V","V","V","V")
  }
  if(s==4){ 
    param<-c("b10","b20","b30","b01","b00","re_yb")
    lable<-c("L","L","L","C","N","V")
  }
  Lev2N <- c(20,30,40,50)
  Lev1N <- c(30,60,150)
}
if (wdID==2){
  wd<-paste(wd0,"MSEM/new/",sep="")
  param<-c("l_yw1","l_yw2","l_yw3","l_xw1","l_xw2","l_xw3",
           "var_xij","res_X1","res_X2","res_X3","res_Y1","res_Y2","res_Y3","res_yij",
           "l_yb1","l_yb2","l_yb3","l_xb1","l_xb2","l_xb3",
           "b01","b21","b11","intc_X1","intc_X2","intc_X3","intc_Y1",
           "intc_Y2","intc_Y3","b10","var_xj",
           "res_X1b","res_X2b","res_X3b","res_Y1b","res_Y2b","res_Y3b","res_yj","res_b1j")
  #FLW(B) for factor loadings within(between),T for thred
  lable<-c("FLW","FLW","FLW","FLW","FLW","FLW","V","V","V","V","V","V","V","V",
           "FLB","FLB","FLB","FLB","FLB","FLB","l","C","C","I","I","I","I","I","I","T",
           "V","V","V","V","V","V","V","V","V")
  Lev2N <- c(40,50)
  Lev1N <- c(30,60,150)
}

subname<-function(s,n){
  return(s[1:n])
}
wd1<-paste(wd,studyType,sep="")
setwd(wd1)
# Single Reps results
if(s!=2){
  modelResults<-readModels(wd1,recursive=TRUE,what="parameters")
  setwd(resultwd)
  save(modelResults,file=paste0(studyType,".Rdata"))
}
if(s==2){
  # modelResultsp<-readModels(paste0(wd1,'/ef/pos'),recursive=TRUE,what="parameters")
  # modelResultsn<-readModels(paste0(wd1,'/ef/neg'),recursive=TRUE,what="parameters")
  setwd(resultwd)
  # save(modelResultsp,file=paste0(studyType,"p.Rdata"))
  # save(modelResultsn,file=paste0(studyType,"n.Rdata"))
  load(paste0(studyType,"p.Rdata"))
  load(paste0(studyType,"n.Rdata"))
  modelResults <- rbind(modelResultsp,modelResultsn)
  names(modelResults) <- c(names(modelResultsp),names(modelResultsn))
}

parametersResults <- sapply(modelResults,"[","parameters")
#find non-converged rep
converge<-which(sapply(parametersResults,length)==2)
parametersResults <- parametersResults[converge]
unstandardResults <- sapply(parametersResults,"[","unstandardized")
unstandardResults <- as.data.table(unstandardResults)

est<-subset(unstandardResults, select = setdiff(grep("est", names(unstandardResults)),
                                                grep("est_se", names(unstandardResults))))
est2<-stack(est)
sd<-subset(unstandardResults, select = c(grep("sd", names(unstandardResults)),
                                         grep("d.se", names(unstandardResults))))
sd2<-stack(sd)
CIResults <- sapply(parametersResults,"[","ci.unstandardized")
CIResults<- as.data.table(CIResults)
# remember to check the CI results!!!
if(s==3){
  CIResults<-CIResults[-1,]
}
if(s==4){
  CIResults<-CIResults[-4,]
}
# remember to check the CI results!!!
CIResults[,1]
CID<-subset(CIResults, select = grep("low2.5", names(CIResults)))
CID2<-stack(CID)
CIU<-subset(CIResults, select = grep("up2.5", names(CIResults)))
CIU2<-stack(CIU)

est2$ind<-as.character(est2$ind)
outname<-strsplit(est2$ind,"out")
est2$ind<-t(data.frame(outname)[1,])
sd2$ind<-as.character(sd2$ind)
outname<-strsplit(sd2$ind,"out")
sd2$ind<-t(data.frame(outname)[1,])
CID2$ind<-as.character(CID2$ind)
outname<-strsplit(CID2$ind,"out")
CID2$ind<-t(data.frame(outname)[1,])
CIU2$ind<-as.character(CIU2$ind)
outname<-strsplit(CIU2$ind,"out")
CIU2$ind<-t(data.frame(outname)[1,])
colnames(est2)[1]<-"est"
colnames(sd2)[1]<-"sd"
colnames(CID2)[1]<-"CID"
colnames(CIU2)[1]<-"CIU"
est2<-cbind(param,est2)
sd2<-cbind(param,sd2)
CIU2<-cbind(param,CIU2)
CID2<-cbind(param,CID2)

allr<-merge(merge(merge(est2,sd2),CID2),CIU2)
allr$param<-factor(allr$param,levels=param)

# =================split filenames and get conditions=============
filename<-allr$ind
allr<-select(allr,-ind)
filename<-lapply(filename,strsplit,"[.]")
a<-data.frame(filename)

#change Ef, Skew, IccC, L1, L2 if needed
Ef<-t(a[1+add,])
Skew<-t(a[2+add,])
IccC<-t(a[3+add,])
Icc<-rep(0,length(IccC))
Icc[which(IccC=="005")]<-rep(0.05,length(IccC[which(IccC=="005")]))
Icc[which(IccC=="01")]<-rep(0.1,length(IccC[which(IccC=="01")]))
Icc[which(IccC=="02")]<-rep(0.2,length(IccC[which(IccC=="02")]))

L2C<-t(a[4+add,])
L2<-rep(0,length(L2C))
L2[which(L2C=="20")]<-rep(20,length(L2[which(L2C=="20")]))
L2[which(L2C=="30")]<-rep(30,length(L2[which(L2C=="30")]))
L2[which(L2C=="40")]<-rep(40,length(L2[which(L2C=="40")]))
L2[which(L2C=="50")]<-rep(50,length(L2[which(L2C=="50")]))

L1C<-t(a[5+add,])
L1<-rep(0,length(L1C))
L1[which(L1C=="30")]<-rep(30,length(L1[which(L1C=="30")]))
L1[which(L1C=="60")]<-rep(60,length(L1[which(L1C=="60")]))
L1[which(L1C=="150")]<-rep(150,length(L1[which(L1C=="150")]))

Esti<-t(a[6+add,])
repi<-t(a[7+add,])

temp<-Esti
temp<-gsub("cor_","c_0sd_",temp)
temp<-gsub("ml","999_999_999",temp)
temp<-gsub("wlsmv","999_999_999",temp)
temp<-gsub("nonif","888_888_888",temp)
tempdf<-strsplit(temp,"_")
priorinf<-as.data.frame(t(as.data.frame(tempdf)))
direction<-priorinf[,1]
bias<-priorinf[,2]
var<-priorinf[,3]

#=====get est, se, ci results===================
conr<-data.frame(Ef,Skew,Icc,L2,L1,Esti,direction,bias,var,repi)
colnames(conr)<-c("Ef","Skew","Icc","L2","L1","Esti","direction",
                  "bias","var","repi")
allr<-data.frame(conr,allr)
colnames(allr)<-c("Ef","Skew","Icc","L2","L1","Esti","direction",
                  "bias","var","repi","Para","est","sd","CID","CIU")
rownames(allr)<-NULL
ncols<-ncol(allr)

# pop value (different for different conditions)
re_yw<-NA
b01 = gamma_01 = c(1.5)#,0
b00 = gamma_00 = 0  
b10 = gamma_10 = 2.3  
b20 = gamma_20 = -0.25 
b30 = gamma_30 = -0.75
b11 = gamma_11 = 0.12  
b21 = gamma_21 = -0.05  
b31 = gamma_31 = -0.75 
re_yb<-NA
s00=0.5
pop<-data.frame("pop"=c(re_yw,b11,b21,b31,b01,b00,b10,b20,b30,re_yb,s00,s00,s00))
pop$Para<-c("re_yw","b11","b21","b31","b01","b00","b10","b20","b30","re_yb",
            "g10^2","g20^2","g30^2")
allr<-merge(allr,pop)
probw = 0.5 
var_w=probw*(1-probw)
x1_mean=0 
x1_sd=1 
var_x1=x1_sd^2
x2_pro1=0.5 
var_x2=x2_pro1*(1-x2_pro1)
x3_pro1=0.75
var_x3=x3_pro1*(1-x3_pro1)

allr[which(allr$Para=="b01"&allr$Ef=="nonef"),"pop"]<-0
if(s<3){
  icc<-allr[which(allr$Para=="re_yw"),"Icc"]
  var_tot<-13.801   
  var_yb<-var_tot*icc
  var_yw<-var_tot-var_yb
  yj_resid<-var_yb-gamma_01^2*var_w
  yij_resid<-var_yw-(gamma_10^2*var_x1+gamma_11^2*(var_w+var_x1)+(s00+var_x1)+
                       gamma_20^2*var_x2+gamma_21^2*(var_w+var_x2)+(s00+var_x2)+
                       gamma_30^2*var_x3+gamma_31^2*(var_w+var_x3)+(s00+var_x3))
  allr[which(allr$Para=="re_yb"),"pop"]<-yj_resid
  allr[which(allr$Para=="re_yw"),"pop"]<-yij_resid
}
if(s>=3){
  yij_resid=1
  if(s==3){
    icc<-allr[which(allr$Para=="re_yb"),"Icc"]
    var_yw<-yij_resid+(gamma_10^2*var_x1+gamma_11^2*(var_w+var_x1)+(s00+var_x1)+
                         gamma_20^2*var_x2+gamma_21^2*(var_w+var_x2)+(s00+var_x2)+
                         gamma_30^2*var_x3+gamma_31^2*(var_w+var_x3)+(s00+var_x3))
    var_yb=var_yw*icc/(1-icc)
    yj_resid<-var_yb-gamma_01^2*var_w
    allr[which(allr$Para=="re_yb"),"pop"]<-yj_resid
    allr[which(allr$Para=="re_yw"),"pop"]<-yij_resid
  }
  if(s==4){
    ##var_yw<-yij_resid+gamma_10^2*var_x1+gamma_20^2*var_x2+gamma_30^2*var_x3
    yj_resid<-1
    allr[which(allr$Para=="re_yb"),"pop"]<-yj_resid
    allr[which(allr$Para=="re_yw"),"pop"]<-yij_resid
  }
}

#converged rate
conver<-allr[allr$Para=="b00",]
conver<-aggregate(as.double(conver$repi),list(conver$Ef,conver$Skew,conver$Icc,
                                              conver$L2,conver$L1,conver$Esti),
                  length)
colnames(conver)<-c("Ef","Skew","Icc","L2","L1","Esti","succ_num")
conver$convrate<-conver$succ_num/NR

setwd(resultwd)
write.csv(conver, file = paste("Converge Rate for",wdID,studyType,".csv"),row.names = F)

#==========="RB","SE_SD","MSE","95Coverage","Power"===========
paramr<-allr
estsd<-aggregate(paramr$est,list(paramr$Para,paramr$Ef,
                                 paramr$Skew,paramr$Icc,paramr$L2,paramr$L1,paramr$Esti,
                                 paramr$direction,paramr$bias,paramr$var,paramr$pop),sd)
colnames(estsd)<-c("Para","Ef","Skew","Icc","L2","L1","Esti",
                   "direction","bias","var","pop","estsd")
presult<-merge(paramr,estsd)
#presult<-merge(presult,conver)
presult$RB<-(presult$est-presult$pop)/presult$pop*100
presult$SE_SD<-presult$sd/presult$estsd
presult$MSE<-(presult$est-presult$pop)^2
presult$Coverage<-(presult$CID<presult$pop)&(presult$CIU>presult$pop)  
presult$Power<-1-((presult$CID<=0)&(presult$CIU>=0))
colnames(presult)[grep("Coverage", colnames(presult))]<-"95Coverage"

write.csv(presult, file = paste("Complete Resullts for",wdID,studyType,".csv"),row.names = F)


aresult=presult[,c("RB","SE_SD","MSE","95Coverage","Power")]
aresult<-stack(aresult)
Report<-aresult[,2]
nreport<-nlevels(Report)
#conditions matrix before the values
cons<-presult[,c(1:(1+con),3+con)]
cons<-cons[rep(1:nrow(cons),nreport),]
Value<-aresult[,1]
aresult<-cbind(cons,Report,Value)
faresult<-aresult[aresult$Para %in% filt_p,]
faresult$Esti<-factor(faresult$Esti,levels=NewEstiLevel)

# mean parameters (for anova analysis)
afaresult<-faresult
afaresult$Value<-abs(afaresult$Value)
pmean<-aggregate(afaresult[,"Value"],
                 list(afaresult$Ef,afaresult$Skew,afaresult$Icc,afaresult$L2,
                      afaresult$L1,afaresult$Esti,afaresult$direction,afaresult$bias,
                      afaresult$var,afaresult$repi,afaresult$Report),mean)
colnames(pmean)<-c("Ef","Skew","Icc","L2","L1","Esti","direction","bias","var",
                   "repi","Report0","Value")
tempca<-cast(pmean,Report+Skew+Icc+L2+L1+repi~Esti,value="Value")
tempca1[is.na(tempca1)]<-""
write.csv(tempca,file = paste("NewAnova Result for",wdID,studyType,".csv"),row.names = F)

tempca1<-read.csv(file = "NewAnova Result for 1 study1_aftericc .csv")
tempca2<-read.csv(file = "NewAnova Result for 1 study2_aftericc .csv")
merge2<-rbind(tempca1,tempca1)
write.csv(merge2,file = paste("NewAnova Result for study2+nor.csv"),row.names = F)

# mean repitations (for plotting)
repimean<-aggregate(faresult[,"Value"], 
                    list(faresult$Para,faresult$Ef,faresult$Skew,faresult$Icc,faresult$L2,
                         faresult$L1,faresult$Esti,faresult$direction,faresult$bias,
                         faresult$var,faresult$Report),mean)
colnames(repimean)<-c("Para","Ef","Skew","Icc","L2","L1","Esti","direction","bias","var",
                      "Report","Value")
repimean$Value<-round(repimean$Value,3)


LongResult<-arrange(repimean,Report,Para,Ef,Skew,Icc,L2,L1,Esti,direction,bias,var)

write.csv(LongResult, file = paste("Summarized Result for",wdID,studyType,".csv"),row.names = F)

ef<-levels(factor(LongResult$Ef))
skew<-levels(factor(LongResult$Skew))
iccs<-levels(factor(LongResult$Icc))
for(e in ef){
  temp0<-LongResult[which(LongResult$Ef==e),]
  for(ic in iccs){
    temp1<-temp0[which(temp0$Icc==ic),]
    for(s in skew){
      temp2<-temp1[which(temp1$Skew==s),]
      tempca<-cast(temp2,Report+Para+L2+L1~Esti,value="Value")
      for(re in report){
        write.xlsx(tempca[which(tempca$Report==re),-1], 
                   file = paste("Result for",wdID,studyType,e,ic,s,".xlsx"),row.names = F,
                   append=T,sheetName=paste("subR",re))
      }
    }
  }
}
# =====================other plot & analysis===============================
wdID<-c(1)#for 2,chage the Icc
wd0<-"F:/current/"       #D:
for (s in c(3)){ #s<-3
  mixed<-T 
  slim<-F 
  studyTypes<-c("study1_aftericc", "study2_aftericc","study3","study4")
  resultwd<-paste(wd0,"AllResults",sep="")
  picwd<-paste(resultwd,"pic/",sep="/")
  Icc<-c(0.05,0.1,0.2)
  if (wdID==1){
    Lev2N <- c(20,30,40,50)
    Lev1N <- c(30,60,150)
  }
  if (wdID==2){
    param<-c("l_yw1","l_yw2","l_yw3","l_xw1","l_xw2","l_xw3",
             "var_xij","res_X1","res_X2","res_X3","res_Y1","res_Y2","res_Y3","res_yij",
             "l_yb1","l_yb2","l_yb3","l_xb1","l_xb2","l_xb3",
             "b01","b21","b11","mW1J","intc_X1","intc_X2","intc_X3","intc_Y1",
             "intc_Y2","intc_Y3","b10","vW1j","var_xj",
             "res_X1b","res_X2b","res_X3b","res_Y1b","res_Y2b","res_Y3b","res_yj","res_b1j")
    pp<-1:length(param) #
    Lev2N <- c(40,50)
    Lev1N <- c(30,60)
  }
  NewEstiLable<-c("M\nL\nR","W\nL\nS","B\nD","L\n3\nW","L\n1\nW","C\nW","R\n1\nW","R\n3\nW",
                  "L\n3\nM","L\n1\nM","C\nM","R\n1\nM","R\n3\nM","L\n3\nS",
                  "L\n1\nS","C\nS","R\n1\nS","R\n3\nS")
  NewEstiLevel<-c("ml","wlsmv","nonif","l_3sd_w","l_1sd_w","cor_w","r_1sd_w","r_3sd_w",
                  "l_3sd_m","l_1sd_m","cor_m","r_1sd_m","r_3sd_m","l_3sd_s",
                  "l_1sd_s","cor_s","r_1sd_s","r_3sd_s")
  
  
  if(wdID==1&s!=3){
    param<-c("re_yw","b11","b21","b31","b01","b00",
             "b10","b20","b30","re_yb","g10^2","g20^2","g30^2")
    pp<-1:length(param) # set pp for params painted
  }
  if(wdID==1&s==3){ 
    param<-c("b11","b21","b31","b01","b10",
             "b20","b30","b00","re_yb","g10^2","g20^2","g30^2")
    pp<-1:length(param)
  }
  if(wdID==1&s==4){  
    param<-c("b10","b20","b30","b01","b00","re_yb")
    pp<-1:length(param)
  }
  studyType<-studyTypes[s]
  setwd(resultwd)
  LongResult<-read.csv(file = paste("Summarized Result for",wdID,studyType,".csv"))
  if(s==2&mixed){
    LongResult1<-read.csv(file = paste("Summarized Result for",wdID,studyTypes[1],".csv"))
    LongResult<-rbind(LongResult1,LongResult)
    if(slim){
      LongResult<-rbind(LongResult1,LongResult[which(LongResult$Skew=='neg'),])
    }
  }
  # ggplot
  for (r in 1:length(report)){#r<-4
    for (p in pp){#p<-5
      if(nrow(LongResult[which((LongResult$Para==param[p])
                               &(LongResult$Report==report[r])),])!=0){
        data<-LongResult[which((LongResult$Para==param[p])
                               &(LongResult$Report==report[r])),]
        data$Esti<-factor(data$Esti,levels=NewEstiLevel,labels=NewEstiLable)
        data$L2<-factor(data$L2,levels=Lev2N,labels=paste("NL2 =",Lev2N))
        data$L1<-factor(data$L1,levels=Lev1N,labels=paste("NL1 =",Lev1N))
        data$Icc<-factor(data$Icc,levels=Icc,labels=paste("ICC =",Icc))
        #data$Skew<-factor(data$Skew)
        title<-paste(report[r],"for",param[p])
        
        picwd2<-paste(picwd,wdID," ",studyType,"/",report[r],sep = "")
        if(s!=2){
          dir.create(paste(picwd,wdID," ",studyType," new",sep = ""))
          picwd2<-paste(picwd,wdID," ",studyType," new/",report[r],sep = "")
        }
        if(s==2){
          if(mixed){
            if(!slim){
              dir.create(paste(picwd,wdID," ",studyType," mixed",sep = ""))
              picwd2<-paste(picwd,wdID," ",studyType," mixed/",report[r],sep = "")
            }
            
            if(slim){
              dir.create(paste(picwd,wdID," ",studyType," mixed&slim",sep = ""))
              picwd2<-paste(picwd,wdID," ",studyType," mixed&slim/",report[r],sep = "")
            }
          }
          if(!mixed){
            if(!slim){
              dir.create(paste(picwd,wdID," ",studyType," nomixed",sep = ""))
              picwd2<-paste(picwd,wdID," ",studyType," nomixed/",report[r],sep = "")
            }
            
            if(slim){
              dir.create(paste(picwd,wdID," ",studyType," nomixed&slim",sep = ""))
              picwd2<-paste(picwd,wdID," ",studyType," nomixed&slim/",report[r],sep = "")
            }
          }
        }
        dir.create(picwd2)
        setwd(picwd2)
        
        tiff(file = paste0(title," of ",studyType,"_",wdID,".tiff"),
             width=4500,height=2250,res = 300)
        
        if (wdID==1&s!=2){
          pic<-ggplot(data, 
                      aes(x=Esti, y=Value, colour=Icc, shape=Icc, group=Icc))+ 
            xlab("Estimator") + ylab(report[r])+
            scale_colour_manual(values=c("#f17c67","#008573","#404040"))+
            scale_shape_manual(values=c(15,16,17))+
            facet_grid(L2 ~ L1)+
            theme(legend.position="bottom")+
            geom_line(colour="#191970")+
            geom_point(size=2.5)+
            ggtitle(title)+
            theme(plot.title = element_text(hjust = 0.5))+
            guides(colour=guide_legend(title=NULL),shape=guide_legend(title=NULL))
          if(report[r]=="RB"){
            pic<-pic+geom_hline(yintercept=0,linetype=3,colour="darkgreen")+
              geom_hline(yintercept=-10,linetype=4,colour="darkred")+
              geom_hline(yintercept=10,linetype=4,colour="darkred")
          }
          if (report[r]=="95Coverage"){
            pic<-pic+geom_hline(yintercept=0.9,linetype=3,colour="darkred")+
              geom_hline(yintercept=0.95,linetype=4,colour="darkgreen")
          }
          if (report[r]=="SE_SD"){#
            pic<-pic+geom_hline(yintercept=1,linetype=3,colour="darkgreen")+
              geom_hline(yintercept=0.9,linetype=4,colour="darkred")+
              geom_hline(yintercept=1.1,linetype=4,colour="darkred")
          }
          print(pic)
        }
        
        if (wdID==1&s==2){
          data$Skew<-factor(data$Skew)##
          data$groupv<-paste(data$Icc,data$Skew)
          pic<-ggplot(data, 
                      aes(x=Esti, y=Value, colour=Icc, shape=Skew, linetype=Skew, group=groupv))+
            xlab("Estimator") + ylab(report[r])+
            labs(linetype="skew",shape="skew")+
            scale_colour_manual(values=c("#f17c67","#008573","#404040"))+
            scale_shape_manual(values=c(15,16,17))+
            scale_linetype_manual(values=c(1,2,6))+
            facet_grid(L2 ~ L1)+
            theme(legend.position="bottom")+
            geom_line()+
            geom_point(size=2.5)+
            ggtitle(title)+
            theme(plot.title = element_text(hjust = 0.5))+
            guides(colour=guide_legend(title=NULL))
          if(report[r]=="RB"){
            pic<-pic+geom_hline(yintercept=0,linetype=3,colour="darkgreen")+
              geom_hline(yintercept=-10,linetype=4,colour="darkred")+
              geom_hline(yintercept=10,linetype=4,colour="darkred")
          }
          if (report[r]=="95Coverage"){
            pic<-pic+geom_hline(yintercept=0.90,linetype=3,colour="darkred")+
              geom_hline(yintercept=0.95,linetype=4,colour="darkgreen")
          }
          if (report[r]=="SE_SD"){
            pic<-pic+geom_hline(yintercept=1,linetype=3,colour="darkgreen")+
              geom_hline(yintercept=0.9,linetype=4,colour="darkred")+
              geom_hline(yintercept=1.1,linetype=4,colour="darkred")
          }
          print(pic)
        }
        dev.off()
      }
    }
  }
}
