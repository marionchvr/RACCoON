#' Title
#'
#' @param x_OS
#' @param x_DSS
#' @param data
#' @param a
#' @param a2
#' @param x_namei
#' @param data.OS
#' @param data.DSS
#' @param Split.OS
#' @param Split.DSS
#'
#' @return
#' @export
#'
#' @examples
tabHR_mult.OS.DSS <- function(x_OS,x_DSS,data,a,a2,x_namei, data.OS,data.DSS,Split.OS,Split.DSS){
  if(!is.na(x_OS)){# ie var est dans le modele OS
    tt<-c(0,Split.OS,30)
    times<-sapply(2:(length(tt)), function(ii){
      a<-tt[ii-1]
      b<-tt[ii]
      return(paste0(a,"<t<",b))})
    if(is.factor(data[,x_OS])){ #ie var is a factor
      tab <- table(as.factor(data.OS[data.OS$tgroup==1,x_OS]),useNA='ifany')
      tab.na <- tab[!is.na(names(tab))]
      NOM<-paste(x_OS,names(tab),sep='')
      SPACE=''
      ONE=1
      NAMES<-c(names(tab.na)[1],paste( rep(names(tab.na)[-1], each=length(times)), rep(times,length(names(tab.na))-1) ))
      N1<-c(t(t(tab.na)), rep("",length(NAMES)-length(t(t(tab.na)))))
    }else{
      NOM<-x_OS
      SPACE=ONE=NULL
      NAMES<-paste( rep(x_namei, each=length(times)), times)
      N1<-c(nlevels(as.factor(data.OS$id)), rep("",length(NAMES)-1))
    }
    s1<-a$coefficients[grep(x_OS,rownames(a$coefficients)), ]
    s2<-a$conf.int[grep(x_OS,rownames(a$conf.int)), ]
    if(!is.null(dim(s1))){
      pval <- c(SPACE,ifelse(s1[,5]<0.001,"<0.001",round(s1[,5],3)))
      ic <- c(SPACE,paste('[',round(s2[,3],2),' ; ', round(s2[,4],2),']',sep=''))
      hr <- c(ONE,round(s2[,1],2))
    }else{
      pval <- c(SPACE,ifelse(s1[5]<0.001,"<0.001",round(s1[5],3)))
      ic <- c(SPACE,paste('[',round(s2[3],2),' ; ', round(s2[4],2),']',sep=''))
      hr <- c(ONE,round(s2[1],2))
    }
    if(length(NOM)==length(pval)){#ie pas de strata
      NAMES<-NOM
      N1<-N1[1:length(NOM)]
    }
  }
  if(!is.na(x_DSS)){
    ttt<-c(0,Split.DSS,30)
    times2<-sapply(2:(length(ttt)), function(ii){
      a<-ttt[ii-1]
      b<-ttt[ii]
      return(paste0(a,"<t<",b))})
    if(is.factor(data[,x_DSS])){ #ie var is a factor
      tab2 <- table(as.factor(data.DSS[data.DDS$tgroup==1,x_DSS]),useNA='ifany')
      tab.na2 <- tab2[!is.na(names(tab2))]
      NOM<-paste(x_DSS,names(tab2),sep='')
      SPACE=''
      ONE=1
      NAMES2<-c(names(tab.na2)[1],paste( rep(names(tab.na2)[-1], each=length(times2)), rep(times2,length(names(tab.na2))-1) ))
      N2<-c(t(t(tab.na2)), rep("",length(NAMES2)-length(t(t(tab.na2)))))
    }else{
      NOM<-x_DSS
      SPACE=ONE=NULL
      NAMES2<-paste( rep(x_namei, each=length(times2)), times2)
      N2<-c(nlevels(as.factor(data.DSS$id)), rep("",length(NAMES2)-1))
    }

    s12<-a2$coefficients[grep(x_DSS,rownames(a2$coefficients)), ]
    s22<-a2$conf.int[grep(x_DSS,rownames(a2$conf.int)), ]
    if(!is.null(dim(s12))){
      pval2 <- c(SPACE,ifelse(s12[,5]<0.001,"<0.001",round(s12[,5],3)))
      ic2 <- c(SPACE,paste('[',round(s22[,3],2),' ; ', round(s22[,4],2),']',sep=''))
      hr2 <- c(ONE,round(s22[,1],2))
    }else{
      pval2 <- c(SPACE,ifelse(s12[5]<0.001,"<0.001",round(s12[5],3)))
      ic2 <- c(SPACE,paste('[',round(s22[3],2),' ; ', round(s22[4],2),']',sep=''))
      hr2 <- c(ONE,round(s22[1],2))
    }
    if(length(NOM)==length(pval2)){#ie pas de strata
      NAMES2<-NOM
      N2<-N2[1:length(NOM)]
    }
  }else{
    pval2<-ic2<-hr2<-tab.na2<-NAMES2<-N2<-rep("",length(pval))
    names(tab.na2)<-rep(" ",length(tab.na2))
  }
  if(is.na(x_OS)){pval<-ic<-hr<-tab.na<-NAMES<-N1<-rep("",length(pval2))}
  # If number of int differs add lines
  nb<-length(pval)-length(pval2)
  if(nb>0){
    pval2<-c(pval2,rep("",nb))
    ic2<-c(ic2,rep("",nb))
    hr2<-c(hr2,rep("",nb))
    # tab.na2<-c(tab.na2,rep("",nb))
    NAMES2<-c(NAMES2,rep("",nb))
    N2<-c(N2,rep("",nb))
  }
  if(nb<0){
    pval<-c(pval,rep("",-nb))
    ic<-c(ic,rep("",-nb))
    hr<-c(hr,rep("",-nb))
    # tab.na<-c(tab.na,rep("",-nb))
    NAMES<-c(NAMES,rep("",-nb))
    N1<-c(N1,rep("",-nb))
  }
  # BIND OS & DSS
  if(length(pval)==1){ #cont pas de strat
    res1 <-matrix(data=c(x_namei,nlevels(as.factor(data.OS$id)),hr,ic,pval,x_namei,nlevels(as.factor(data.DSS$id)),hr2,ic2,pval2),nrow=1)
  }else{
    if(length(NOM)==length(pval)){ #fact pas de strat
      res <- cbind(names(tab.na),t(t(tab.na)),hr,ic,pval,  names(tab.na2),t(t(tab.na2)),hr2,ic2,pval2)
      res1 <- rbind(c(x_namei,'','','','',"","","","",""),res)
    }else{ # strat
      res <- cbind(NAMES,N1,hr,ic,pval,  NAMES2,N2,hr2,ic2,pval2)
      res1 <- rbind(c(x_namei,'','','','',"","","","",""),res)
    }
  }
  return(res1)
}
