#' Title
#'
#' @param data
#' @param x_all
#' @param x2
#' @param xall_name
#' @param x2_name
#' @param nb_dec
#' @param pcol
#' @param plig
#' @param ptot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabcont.qual_<- function(data, x_all, x2, xall_name, x2_name, nb_dec, pcol, plig, ptot, ...){
  fun<-function(x1){
    x1_name<-xall_name[x_all==x1]
    obs <- tableau_(x1, x2, data, nb_dec)
    pligne <- tableau_(x1, x2, data, nb_dec, px1 = T)
    pcolonne <- tableau_(x1, x2, data, nb_dec, px2 = T)
    pcase <- tableau_(x1, x2, data, nb_dec, pc = T)
    if(is.null(obs)){
      return(NULL)
    } else {
      nomcol <- dimnames(obs)[[2]]
      nomligne <- dimnames(obs)[[1]]
      # joindre les pligne et pcolonne (vrai avec ou sans NA)
      if(pcol==TRUE){
        if(plig==TRUE){ #pcol==TRUE & plig==TRUE
          plc <- matrix(paste(pligne, "%/ ", pcolonne, "%", sep=""),ncol=dim(pligne)[2],nrow=dim(pcolonne)[1]) # pas sur nrow=dim(pcolonne)[2]
        } else { #pcol==TRUE & plig=FALSE
          plc <- matrix(paste(pcolonne, "%", sep=""),ncol=dim(pligne)[2],nrow=dim(pcolonne)[1])
        }
      } else {
        if(plig==TRUE){ #pcol==FALSE & plig==TRUE
          plc <- matrix(paste(pligne, "%", sep=""),ncol=dim(pligne)[2],nrow=dim(pcolonne)[1])
        } else { #pcol==FALSE & plig==FALSE
          plc <- matrix("",ncol=dim(pligne)[2],nrow=dim(pcolonne)[1])
        }
      }
      if(ptot==TRUE){
        pcase <- matrix(paste(pcase, "%", sep=""),ncol=dim(pcase)[2],nrow=dim(pcase)[1])
        # modifier pcase : mettre " " au lieu des totaux
        pcase[,dim(pcase)[2]] <- c(rep("",dim(pcase)[1]))
        pcase[dim(pcase)[1],] <- c(rep("",dim(pcase)[2]))
      } else {
        pcase <- matrix("",ncol=dim(pcase)[2],nrow=dim(pcase)[1])
      }
      # modifier pligne, pcol et pcase si NA
      # si NA en ligne et en colonne (parcours des deux if)
      if (sum(is.na(rownames(obs)))!=0){ # si un NA en ligne
        plc <- rbind(plc[1:(dim(plc)[1]-1),],plc[dim(plc)[1],],"")
        nomligne[is.na(nomligne)] <- "NA"
        pcase <- rbind(pcase[1:(dim(pcase)[1]-1),],pcase[dim(pcase)[1],],"")
      }
      if(sum(is.na(colnames(obs)))!=0){ # si un NA en colonne
        plc <- cbind(plc[,1:(dim(plc)[2]-1)],plc[,dim(plc)[2]],"")
        pcase <- cbind(pcase[,1:(dim(pcase)[2]-1)],pcase[,dim(pcase)[2]],"")
        nomcol[is.na(nomcol)] <- "NA"
      }
      # rbind les 3 tableau_x
      tabf <- do.call(rbind,lapply(seq(1:dim(obs)[1]), function(x) return(rbind(obs[x,],plc[x,],pcase[x,]))))
      # Mise en forme du tableau_
      tabf <- cbind(x1_name,c(rep(nomligne, each=3)),tabf)
      # supprimer la derni?re ligne
      tabf <- tabf[1:dim(tabf)[1]-1,]
      # supprimer les lignes vides si NA en ligne ou si pcol=FALSE et plig=FALSE ou si ptot=FALSE
      colo <- seq(3,ncol(tabf))
      request <- paste('tabf[,',colo,']==""',collapse=" & ", sep="")
      tabf <- tabf[!(eval(parse(text=request))),]
      nlev<-nlevels(as.factor(data[,x1]))
      pval <- testKhi2_p(data=data,x1,x2)
      pval_c <- ifelse(!is.na(pval),ifelse(pval<0.001,"<0.001",round(pval,3)),'Not enough')
      pval_c <- rep(pval_c,nlev)
      pval<-ifelse(is.na(pval),1,pval)
      star <- symnum(pval,cutpoints=c(0,0.001,0.01,.05,0.1,1),symbols=c("***","**","*","."," "))
      star <- rep(star,nlev)
      tabf2<-cbind(tabf,p=pval_c,star)
      return(tabf2)
    }
  }
  tab <- do.call(rbind, lapply(x_all,fun))
  obs <- tableau_(x_all[1], x2, data, nb_dec)
  nomcol <- dimnames(obs)[[2]]
  tab <- rbind(x2_name,c('','',nomcol,'p',''),tab)
  return(tab)
}
