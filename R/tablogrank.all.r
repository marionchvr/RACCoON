#' Title
#'
#' @param data
#' @param x
#' @param COMB
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
tablogrank.all <- function(data, x,  COMB, delai, etat, etat2, nb_dec, ...){
  t2 <- NULL
  for(i in 1:ncol(COMB)){
    data.temp<-data[is.element(data[,x],COMB[,i]),]
    data.temp[,x]<-factor(data.temp[,x], levels=COMB[,i])
    t1 <- tablogrank(x=x,lescombs=COMB[,i], data=data.temp, delai = delai, etat=etat,etat2=etat2, nb_dec=nb_dec)
    t2 <- rbind(t2, t1)
  }
  return(t2)
}
