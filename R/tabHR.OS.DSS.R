#' Title
#'
#' @param data
#' @param del
#' @param x
#' @param x2
#' @param x_gpe
#' @param x_name
#' @param x_stratOS
#' @param x_stratDSS
#'
#' @return
#' @export
#'
#' @examples
tabHR.OS.DSS <- function(data, del, x, x2, x_gpe, x_name,x_stratOS, x_stratDSS){
  N.intOS<-ifelse(is.na(x_stratOS[1]),0,ifelse(x_stratOS[1]==0,1,length(x_stratOS)+1))
  N.intDSS<-ifelse(is.na(x_stratDSS[1]),0,ifelse(x_stratDSS[1]==0,1,length(x_stratDSS)+1))
  if(is.factor(data[,x_gpe])){
    tab <- table(data[,x_gpe],useNA='ifany')
    tab.na <- tab[!is.na(names(tab))]
    SPACE=''
    ONE=1
  }else{SPACE=ONE=NULL}
  # 1 OS
  if(N.intOS>0){ #ie on a bien cette var en OS
    x_gpeMod<-x_gpe
    if(N.intOS==1){ #ie pas d'intrecation avec le temps : version classique
      Data.OS<-data
      colnames(Data.OS)[which(colnames(Data.OS)==del)]<-"tstop"
      colnames(Data.OS)[which(colnames(Data.OS)==x)]<-"event"
      FormulaOS<-as.formula("Surv(Data.OS$tstop,Data.OS$event)~Data.OS[,x_gpe]")
      if(is.factor(data[,x_gpe])){NAMES<-c(names(tab.na))}else{NAMES<-x_name}
    }else{  #ie intrecation avec le temps
      data2<-data[complete.cases(data[,x_gpe]),]
      if(is.factor(data[,x_gpe])){
        dtemp<-dummy(data2[,x_gpe], sep = "_")
        data2<-cbind(data2,dtemp )
        x_gpeMod<-colnames(dtemp)[-1]
      }
      Data.OS<-survSplit(Surv(data2[,del], data2[,x]) ~.,data=as.data.frame(data2[,x_gpeMod]), cut=x_stratOS,episode="tgroup", id = "id")
      colnames(Data.OS)[1:length(x_gpeMod)]<-x_gpeMod
      FormulaOS<-as.formula("Surv(Data.OS$tstart,Data.OS$tstop,Data.OS$event)~.:strata(Data.OS$tgroup)")
      times<-sapply(1:N.intOS, function(i){
        a<-x_stratOS[i-1]
        b<-x_stratOS[i]
        if(length(a)==0){a<-0}
        if(is.na(b)){b<-30}
        return(paste0(a,"<t<",b))})
      if(is.factor(data[,x_gpe])){NAMES<-paste(c(names(tab.na)[1],rep(names(tab.na)[-1],each=N.intOS)),c("",rep(times,length(names(tab.na))-1)))}else{
        NAMES<-paste(rep(x_name),times)
      }
    }
    # On recupere  summary du modele
    s <- summary(coxph(FormulaOS, data=as.data.frame(Data.OS[,x_gpeMod])))
    pval <- c(SPACE,ifelse(s$coefficients[,5]<0.001,"<0.001",round(s$coefficients[,5],3)))
    ic <- c(SPACE,paste('[',round(s$conf.int[,3],2),' ; ', round(s$conf.int[,4],2),']',sep=''))
    hr <- c(ONE,round(s$conf.int[,1],2))
  }else{
    pval<-ic<-hr<-NAMES<-NULL
  }
  # 2 DSS
  if(N.intDSS>0){ #ie on a bien cette var en DSS
    x_gpeMod<-x_gpe
    if(N.intDSS==1){ #ie pas d'intrecation avec le temps : version classique
      Data.DSS<-data
      colnames(Data.DSS)[which(colnames(Data.DSS)==del)]<-"tstop"
      colnames(Data.DSS)[which(colnames(Data.DSS)==x2)]<-"event"
      FormulaDSS<-as.formula("Surv(Data.DSS$tstop,Data.DSS$event)~Data.DSS[,x_gpe]")
      if(is.factor(data[,x_gpe])){NAMES2<-c(names(tab.na))}else{NAMES2<-x_name}
    }else{  #ie intrecation avec le temps
      data2<-data[complete.cases(data[,x_gpe]),]
      if(is.factor(data[,x_gpe])){
        dtemp<-dummy(data2[,x_gpe], sep = "_")
        data2<-cbind(data2,dtemp )
        x_gpeMod<-colnames(dtemp)[-1]
      }
      Data.DSS<-survSplit(Surv(data2[,del],data2[,x2]) ~., data=as.data.frame(data2[,x_gpeMod]),cut=x_stratDSS,episode="tgroup",id = "id")
      colnames(Data.DSS)[1:length(x_gpeMod)]<-x_gpeMod
      # FormulaDSS<-as.formula("Surv(Data.DSS$tstart,Data.DSS$tstop,Data.DSS$event)~Data.DSS[,x_gpe]:strata(Data.DSS$tgroup)")
      FormulaDSS<-as.formula("Surv(Data.DSS$tstart,Data.DSS$tstop,Data.DSS$event)~.:strata(Data.DSS$tgroup)")
      times<-sapply(1:N.intDSS, function(i){
        a<-x_stratDSS[i-1]
        b<-x_stratDSS[i]
        if(length(a)==0){a<-0}
        if(is.na(b)){b<-30}
        return(paste0(a,"<t<",b))})
      if(is.factor(data[,x_gpe])){NAMES2<-paste(c(names(tab.na)[1],rep(names(tab.na)[-1],each=N.intDSS)),c("",rep(times,length(tab.na)-1)))}else{
        NAMES2<-paste(rep(x_name),times)
      }
    }
    s2 <- summary(coxph(FormulaDSS, data=as.data.frame(Data.DSS[,x_gpeMod])))
    pval2 <- c(SPACE,ifelse(s2$coefficients[,5]<0.001,"<0.001",round(s2$coefficients[,5],3)))
    ic2 <- c(SPACE,paste('[',round(s2$conf.int[,3],2),' ; ', round(s2$conf.int[,4],2),']',sep=''))
    hr2 <- c(ONE,round(s2$conf.int[,1],2))
  }else{
    pval2<-ic2<-hr2<-NAMES2<-NULL
  }
  # If number of int differs add lines
  if(N.intOS>N.intDSS){
    nb<-length(pval)-length(pval2)
    pval2<-c(pval2,rep("",nb))
    ic2<-c(ic2,rep("",nb))
    hr2<-c(hr2,rep("",nb))
    NAMES2<-c(NAMES2,rep("",nb))
  }
  if(N.intOS<N.intDSS){
    nb<-length(pval2)-length(pval)
    pval<-c(pval,rep("",nb))
    ic<-c(ic,rep("",nb))
    hr<-c(hr,rep("",nb))
    NAMES<-c(NAMES,rep("",nb))
  }
  # BIND OS & DSS
  if(!is.factor(data[,x_gpe]) & max(N.intOS,N.intDSS,na.rm=T)==1){
    res1 <-matrix(data=c(x_name,sum(!is.na(data[,x_gpe])),hr,ic,pval,x_name,hr2,ic2,pval2),nrow=1) #to check
  }else{
    if(!is.factor(data[,x_gpe])){Ns<-rep(sum(!is.na(data[,x_gpe])),length(pval))}else{Ns<-c(t(t(tab.na))[1],rep(t(t(tab.na))[-1],each=max(N.intOS,N.intDSS,na.rm=T)))}
    res <- cbind(NAMES,Ns,hr,ic,pval,  NAMES2,hr2,ic2,pval2)
    res1 <- rbind(c(x_name,'','','','',"","","",""),res)
    # res <- cbind(names(tab.na),t(t(tab.na)),hr,ic,pval, hr2,ic2,pval2)
  }
  return(res1)
}
