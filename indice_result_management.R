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
library(tidyr)
library(xlsx)
new_sub<-function(A,B){   #subt_by_element
  return(lapply(1:length(A), function(i) A[[i]]-B[[i]]))
}
new_mul<-function(A,B){   #multi_by_element
  return(lapply(1:length(A), function(i) A[[i]]*B[[i]]))
}
new_div<-function(A,B){   #division_by_element
  return(lapply(1:length(A), function(i) A[[i]]/B[[i]]))
}

study<-4
num=6

NR<-100
studys<-c("study1b","study2","study3",'supp')

wd0<-"F:/current/硕士毕设/模拟研究"   #D:
wd<- paste0(wd0,"/",paste0(studys[study]))  #,'_',num

resultwd<-paste(wd0,"AllResults",sep="/")

#妯℃嫙鏉′欢
conditions<-c('skew','ICC','L2N','L1N','esti','mis','analyze_l','rep')
con<-length(conditions)
ind <- c('DIC','PPP','pD','parameters','ChiSqDiff_L','ChiSqDiff_U',
         'CFI', 'TLI', 'AIC', 'BIC', 'aBIC', 'RMSEA', 'SRMR.W', 'SRMR.B', 'AICC',
         'ChiSqM_Value', 'ChiSqM_DF', 'ChiSqM_P', 'ChiSqM_SC',
         'P_RMSEAb', 'P_RMSEAw')
rel_inds<-c('DIC','AIC', 'BIC', 'aBIC', 'AICC')

if (studys[study]=='supp'){
  ind <- c('DIC','PPP','pD','parameters','ChiSqDiff_L','ChiSqDiff_U')
  rel_inds<-c('DIC')
}

Lev2N <- c(30,50,100,200) #
Lev1N <- c(10,30,50,100)#  
iccs<-c(0.1,0.3,0.5)#
iccLs<-c("01","03","05")#
if(study==3){
  iccs<-c(0.1,0.3)#,
  iccLs<-c("01","03")#,,
}
skew<-'nor'


# ==========Read Single Reps results==============
setwd(wd)
# modelResults<-readModels(wd,recursive=TRUE,what="all")
setwd(resultwd)
# save(modelResults,file=paste0(studys[study],".Rdata"))

load(file=paste0(studys[study],".Rdata"))

AllFitResults <- sapply(modelResults,"[","summaries")
#delete non-converged rep
converge<-which(sapply(AllFitResults,length)>11)
FitResults<-AllFitResults[converge]

# split filenames and get conditions
filename <- sapply(FitResults,"[","Filename")
filename <-lapply(filename,strsplit,"[.]")

all_results <- data.frame(filename)[1,]
all_results <- sapply(all_results,strsplit,"[-]")
all_results <-data.frame(t(data.frame(all_results, row.names = conditions)))
all_results$L2N<-as.numeric(all_results$L2N)
all_results$L1N<-as.numeric(all_results$L1N)

bayes <- which(all_results$esti=='bayes')
ml <- which(all_results$esti=='ml')
bet <- which(all_results$analyze_l=='b')
wit <- which(all_results$analyze_l=='w')

# extract indices 
all_results[,ind] <- NA
all_results$DIC[bayes] <- matrix(sapply(FitResults[bayes],"[","DIC"))
all_results$pD[bayes] <- matrix(sapply(FitResults[bayes],"[","pD"))
all_results$PPP[bayes] <- matrix(sapply(FitResults[bayes],"[","PostPred_PValue"))
all_results$parameters[bayes] <- matrix(sapply(FitResults[bayes],"[","Parameters"))
all_results$ChiSqDiff_L[bayes] <- matrix(sapply(FitResults[bayes],"[",
                                                "ObsRepChiSqDiff_95CI_LB"))
all_results$ChiSqDiff_U[bayes] <- matrix(sapply(FitResults[bayes],"[",
                                                "ObsRepChiSqDiff_95CI_UB"))

# indices in ml estimators
all_results$CFI[ml] <- matrix(sapply(FitResults[ml],"[","CFI"))
all_results$TLI[ml] <- matrix(sapply(FitResults[ml],"[","TLI"))
all_results$AIC[ml] <- matrix(sapply(FitResults[ml],"[","AIC"))
all_results$BIC[ml] <- matrix(sapply(FitResults[ml],"[","BIC"))
all_results$aBIC[ml] <- matrix(sapply(FitResults[ml],"[","aBIC"))
all_results$RMSEA[ml] <- matrix(sapply(FitResults[ml],"[","RMSEA_Estimate"))
all_results$SRMR.W[ml] <- matrix(sapply(FitResults[ml],"[","SRMR.Within"))
all_results$SRMR.B[ml] <- matrix(sapply(FitResults[ml],"[","SRMR.Between"))
all_results$AICC[ml] <- matrix(sapply(FitResults[ml],"[","AICC"))
all_results$ChiSqM_Value[ml] <- matrix(sapply(FitResults[ml],"[","ChiSqM_Value"))
all_results$ChiSqM_DF[ml] <- matrix(sapply(FitResults[ml],"[","ChiSqM_DF"))
all_results$ChiSqM_P[ml] <- matrix(sapply(FitResults[ml],"[","ChiSqM_PValue"))
all_results$ChiSqM_sig<-lapply(1:length(all_results$ChiSqM_P), function(i) all_results$ChiSqM_P[[i]]<0.05)

# the retreival of ChiSqM_SC is kind of complex
for (m in ml){
  x <- try(all_results$ChiSqM_SC[m] <- 
             FitResults[[m]]$"ChiSqM_ScalingCorrection", silent=T)
  if ('try-error' %in% class(x)){
    all_results$ChiSqM_SC[m] <- 1 #if the correction is undefined
    next
  } 
}
attach(all_results)
# sqrt((ChiSqM_Value[ml][1]*ChiSqM_SC[ml][1] - ChiSqM_DF[ml][1]*ChiSqM_SC[ml][1])/
# (ChiSqM_DF[ml][1] * ChiSqM_SC[ml][1] * L2N[ml][1]))
b<-new_div(new_sub(new_mul(ChiSqM_Value,ChiSqM_SC),
                   new_mul(ChiSqM_DF,ChiSqM_SC)),
           new_mul(new_mul(ChiSqM_DF,ChiSqM_SC),L2N))
b[which(b<0)]<-0
all_results$P_RMSEAb <- unlist(lapply(1:length(b), function(i) sqrt(b[[i]])))

w<-new_div(new_sub(new_mul(ChiSqM_Value,ChiSqM_SC),
                   new_mul(ChiSqM_DF,ChiSqM_SC)),
           new_mul(new_mul(ChiSqM_DF,ChiSqM_SC),L1N*L2N-L2N))
w[which(w<0)]<-0
all_results$P_RMSEAw <- unlist(lapply(1:length(w), function(i) sqrt(w[[i]])))
detach(all_results)

write.xlsx(all_results,file=paste("fit indices for",studys[study],".xlsx"),
           sheetName = "all_results", row.names = F)
all_results0 <- read.xlsx(file=paste("fit indices for",studys[study],".xlsx"),
                          sheetName = "all_results")
all_results <- all_results0

for(c in rel_inds){
  if(c=='DIC'){
    tempdata <- all_results0[bayes,c(conditions,c)]
  }
  if(c!='DIC'){
    tempdata <- all_results0[ml,c(conditions,c)]
  }
  temp <- cast(tempdata, skew+ICC+L2N+L1N+esti+analyze_l+rep~mis, value = c)
  temp[,paste0(c,"_r")] <- temp$m-temp$c
  temp<-temp[,-which(colnames(temp)%in%c('c','m'))]
  all_results<-merge(temp, all_results, all=T)
}

write.xlsx(all_results,file=paste("fit indices for",studys[study],".xlsx"),
           sheetName = "all_results", row.names = F)

#converged rate
conver<-aggregate(as.double(all_results$rep),list(all_results$skew,all_results$ICC,
                                                  all_results$L2N,all_results$L1N,
                                                  all_results$esti,all_results$mis,
                                                  all_results$analyze_l),
                  length)
colnames(conver)<-c(conditions[-length(conditions)],"succ_num")
conver$convrate<-conver$succ_num/NR

setwd(resultwd)
write.xlsx(conver,file=paste("fit indices for",studys[study],".xlsx"),
           sheetName = "convrate", row.names = F, append = T)


# =====================other plot & analysis===============================
picwd<-paste(resultwd,"pic",sep="/")
setwd(resultwd)

all_results <- read.xlsx(file=paste("fit indices for",studys[study],".xlsx"),
                         sheetName = "all_results")
bayes <- which(all_results$esti=='bayes')
ml <- which(all_results$esti=='ml')
bet <- which(all_results$analyze_l=='b')
wit <- which(all_results$analyze_l=='w')

setwd(picwd)
all_results <- all_results[which(all_results$L1N!='5'),]
useless_ind<-c('pD','parameters','ChiSqDiff_L','ChiSqDiff_U', 
               'ChiSqM_Value', 'ChiSqM_DF', 'ChiSqM_SC')
report <- c(setdiff(ind,useless_ind),paste0(rel_inds,"_r"))

# boxplot
for (r in report){#r<-"DIC"  _r
  if(r %in% c('DIC','DIC_r','PPP')){
    data<-all_results[bayes,c(conditions,r)]
  }else{
    data<-all_results[ml,c(conditions,r)]
  }
  if(r %in% paste0(rel_inds,"_r")){
    data<-data[-which(data$mis=='c'),]
  }
  # data<-na.omit(data)
  data$mis<-factor(data$mis,levels=unique(data$mis))
  data$analyze_l<-factor(data$analyze_l,levels=unique(data$analyze_l))
  data$esti<-factor(data$esti,levels=c('bayes','ml'),labels=c('Bayes','ML'))
  data$L2N<-factor(data$L2N,levels=Lev2N,labels=paste("NL2 =",Lev2N))
  data$L1N<-factor(data$L1N,levels=Lev1N,labels=paste("NL1 =",Lev1N))
  data$ICC<-factor(data$ICC,levels=iccLs,labels=paste("ICC =",iccs))
  #data$Skew<-factor(data$Skew)
  title<-paste(r,"Boxplot for",unique(data$esti),"Estimator")
  
  tiff(file = paste0(title," of ",studys[study],".tiff"),
       width=4500,height=2250,res = 300)
  
  data$mis_level<-paste(data$mis,data$analyze_l,sep='_')
  data$mis_level<-factor(data$mis_level)
  
  pic<-ggplot(data, 
              aes(x=mis_level, y=get(r), fill=ICC))+ 
    scale_x_discrete(name="Model Mis-specification") + ylab(r)+
    scale_fill_manual(values=c("#f17c67","#404040","#008573"))+
    facet_grid(L2N ~ L1N)+
    theme(legend.position="bottom")+
    geom_boxplot(alpha=0.7) + theme_bw() +
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_fill_brewer(palette = 'Accent')+
    guides(fill=guide_legend(title=NULL))
  
  if(r=="PPP"){
    pic<-pic+geom_hline(yintercept=0.05,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=0.1,linetype=3,colour="darkgreen")
  }
  if(r=="DIC_r"){
    pic<-pic+geom_hline(yintercept=-7,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=7,linetype=3,colour="darkgreen")
  }
  if(r=="CFI"|r=="TLI"){   #Hu and Bentler (1999)
    pic<-pic+geom_hline(yintercept=0.9,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=0.95,linetype=3,colour="darkgreen")
  }
  if(r=="RMSEA"|r=="P_RMSEAb"|r=="P_RMSEAw"){   #Hu and Bentler (1999)
    pic<-pic+geom_hline(yintercept=0.06,linetype=3,colour="darkgreen")
    pic<-pic+geom_hline(yintercept=0.08,linetype=3,colour="darkgreen")
  }
  if(r=="SRMR.W"|r=="SRMR.B"){    #Maccallum et al., 1996
    pic<-pic+geom_hline(yintercept=0.06,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=0.95,linetype=3,colour="darkgreen")
  }
  print(pic)
  dev.off()
}

# boxplot for indices_r at misspecified between-level 
mis_plot <- all_results[all_results$mis=='m',]
between_plot <- mis_plot[mis_plot$analyze_l=='b',]
for (r in c('P_RMSEAb', paste0(rel_inds,"_r"))){#  r<-"P_RMSEAb"
  if(r %in% c('DIC','DIC_r','PPP')){
    data<-between_plot[which(between_plot$esti=='bayes'),c(conditions,r)]
  }else{
    data<-between_plot[which(between_plot$esti=='ml'),c(conditions,r)]
  }
  # data<-na.omit(data)
  data$mis<-factor(data$mis,levels=unique(data$mis))
  data$analyze_l<-factor(data$analyze_l,levels=unique(data$analyze_l))
  data$esti<-factor(data$esti,levels=c('bayes','ml'),labels=c('Bayes','ML'))
  data$L2N<-factor(data$L2N,levels=Lev2N,labels=paste("NL2 =",Lev2N))
  data$L1N<-factor(data$L1N,levels=Lev1N,labels=paste("NL1 =",Lev1N))
  data$ICC<-factor(data$ICC,levels=iccLs,labels=paste("ICC =",iccs))
  #data$Skew<-factor(data$Skew)
  title<-paste(r,"Boxplot for",unique(data$esti),"Estimator at Between-level")
  
  tiff(file = paste0(title," of ",studys[study],".tiff"),
       width=4500,height=2250,res = 300)
  
  data$mis_level<-paste(data$mis,data$analyze_l)
  pic<-ggplot(data, 
              aes(x=mis_level, y=get(r), fill=ICC))+ 
    scale_x_discrete(name="Model Mis-specification") + ylab(r)+
    scale_fill_manual(values=c("#f17c67","#404040","#008573"))+
    facet_grid(L2N ~ L1N)+
    theme(legend.position="bottom")+
    geom_boxplot(alpha=0.7)+theme_bw()+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_fill_brewer(palette = 'Accent')+
    guides(fill=guide_legend(title=NULL))
  if(r=="PPP"){
    pic<-pic+geom_hline(yintercept=0.05,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=0.1,linetype=3,colour="darkgreen")
  }
  if(r=="DIC_r"){
    pic<-pic+geom_hline(yintercept=-7,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=7,linetype=3,colour="darkgreen")
  }
  if(r=="CFI"|r=="TLI"){   #Hu and Bentler (1999)
    pic<-pic+geom_hline(yintercept=0.9,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=0.95,linetype=3,colour="darkgreen")
  }
  if(r=="RMSEA"|r=='P_RMSEAb'){   #Hu and Bentler (1999)
    pic<-pic+geom_hline(yintercept=0.06,linetype=3,colour="darkgreen")
    pic<-pic+geom_hline(yintercept=0.08,linetype=3,colour="darkgreen")
  }
  if(r=="SRMR.W"|r=="SRMR.B"){    #Maccallum et al., 1996
    pic<-pic+geom_hline(yintercept=0.06,linetype=3,colour="darkgreen")+
      geom_hline(yintercept=0.95,linetype=3,colour="darkgreen")
  }
  print(pic)
  dev.off()
}

# ROC analysis
library("pROC")
between_r<-all_results[-wit,]
within_r<-all_results[-bet,]
any_r<-all_results

#### Choose which level to analysis
test_level<-c('within','any','between')
for(t_l in test_level){ #t_l='within'
  plot_ind<-c("PPP", "ChiSqM_P", "CFI","TLI","RMSEA") 
  if(t_l=='between'){
    plot_ind<-c(plot_ind,'SRMR.B', 'P_RMSEAb')
    test_r<-between_r}
  if(t_l=='within'){
    plot_ind<-c(plot_ind,'SRMR.W', 'P_RMSEAw')
    test_r<-within_r}
  if(t_l=='any'){
    plot_ind<-c(plot_ind)   #,'SRMR.B','SRMR.W'
    test_r<-any_r}
  
  l<-rep(list(),length(plot_ind))
  
  for(i in 1:length(plot_ind)){
    l[[plot_ind[i]]]<-roc(response=test_r$mis, predictor=test_r[,plot_ind[i]],
                          levels=c('c','m'),smooth=T)#
  }
  
  #ROC analysis curves ploting for identifying correctly specified models
  library(ggplot2)
  title<-paste0(t_l,"-level mis-specification")
  tiff(file = paste0(title," of ",studys[study],".tiff"),
       width=2500,height=2250,res = 300)
  g <- ggroc(l,aes=c("linetype", "color")) +   # legacy.axes = TRUE, alpha = 0.5, size = 2)
    theme_minimal() + 
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))+
    geom_segment(aes(x=1,xend=0,y=0,yend=1), colour="black",
                 linetype=length(plot_ind)+1)+
    guides(color=guide_legend(title=NULL),linetype=guide_legend(title=NULL))
  print(g)
  dev.off()
}

# list without smoothing (for threshold analysis) 
for(t_l in test_level){ #t_l='within'
  plot_ind<-c("PPP", "ChiSqM_P", "CFI","TLI","RMSEA") 
  if(t_l=='between'){
    plot_ind<-c(plot_ind,'SRMR.B', 'P_RMSEAb')
    test_r<-between_r}
  if(t_l=='within'){
    plot_ind<-c(plot_ind,'SRMR.W', 'P_RMSEAw')
    test_r<-within_r}
  if(t_l=='any'){
    plot_ind<-c(plot_ind)   #,'SRMR.B','SRMR.W'
    test_r<-any_r}
  l_nosth<-rep(list(),length(plot_ind))
  attach(test_r)
  for(i in 1:length(plot_ind)){
    l_nosth[[plot_ind[i]]]<-roc(response=test_r$mis,predictor=get(plot_ind[i]),
                                levels=c('c','m'))
  }
  detach(test_r)
  coords_summary<-unique(all_results[,c('skew','ICC','L2N','L1N')])
  ret<-c("threshold","specificity","sensitivity")
  
  for(p in 1:length(plot_ind)){
    ret_s<-paste(plot_ind[p],substring(ret,1,4),sep="_")
    for (ic in 1:length(iccLs)){#ic=2
      for (i in 1:length(Lev2N)){#i=j=3
        for (j in 1:length(Lev1N)){
          for (s in skew){#s="nor"
            subdata<-test_r[which(test_r$skew==s & test_r$ICC==iccLs[ic] & 
                                    test_r$L2N==Lev2N[i] & test_r$L1N==Lev1N[j]),
                            c('skew','ICC','L2N','L1N','mis', plot_ind[p])]
            attach(subdata)
            fit<- try(subdata[,ret_s] <- 
                        roc(response=subdata$mis,predictor=get(plot_ind[p]),levels=c('c','m')) %>% 
                        coords(x="best", best.method="closest.topleft", #"youden"
                               ret = ret))
            detach(subdata)
            if('try-error' %in% class(fit)){
              next
            }
            coords_summary[which(coords_summary$skew==s & coords_summary$ICC==iccLs[ic] & 
                                   coords_summary$L2N==Lev2N[i] & coords_summary$L1N==Lev1N[j]),
                           ret_s] <- unique(subdata[,ret_s]) 
          }
        }
      }
    }
  }
  setwd(resultwd)
  write.xlsx(coords_summary,file=paste("fit indices for",studys[study],".xlsx"),
             sheetName = paste0(t_l,"-level thre"), append=T, row.names = F)
}


# parameter estimation===============================
parametersResults <- sapply(modelResults,"[","parameters")
#find non-converged rep
converge<-which(sapply(parametersResults,length)==2)
parametersResults<-parametersResults[converge]
unstandardResults <- sapply(parametersResults,"[","unstandardized")
unstandardResults<- as.data.table(unstandardResults)
est<-subset(unstandardResults, select = grep("est$", names(unstandardResults)))
