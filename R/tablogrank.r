#' Title
#'
#' @param x
#' @param lescombs
#' @param data
#' @param delai
#' @param etat
#' @param etat2
#' @param nb_dec
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tablogrank <- function(x, lescombs, data, delai, etat, etat2, nb_dec,...){
  tabfinal <- NULL
  pvalues <- NULL
  pvalues2 <- NULL
  # OS
  tmp <- coxph(Surv(data[, delai], data[, etat]) ~ data[, x])
  data2<-data[complete.cases(data[,etat2]),]
  # DSS
  tmp2<- coxph(Surv(data2[, delai], data2[, etat2]) ~ data2[, x])
  p1<-summary(tmp)$sctest[3]
  p2<-summary(tmp2)$sctest[3]
  pvalues <- ifelse(p1<10^-nb_dec,paste0("<",eval(10^-nb_dec)),round(p1, nb_dec)) # donne p-value du Test du logrank global
  pvalues2<- ifelse(p2<10^-nb_dec,paste0("<",eval(10^-nb_dec)),round(p2, nb_dec)) # donne p-value du Test du logrank global
  tabfinal<- cbind(lescombs[1],lescombs[2], pvalues,pvalues2)
  colnames(tabfinal)<- c("Group 1","Group 2", "P value OS", "P value DSS")
  rownames(tabfinal)<-NULL
  return(tabfinal)
}
