#' Title
#'
#' @param x
#' @param data
#' @param nb_dec
#' @param na.p
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabdesc.qual <- function(x, data, nb_dec, na.p, ...){
  # x : variable
  # data : base de donn?es
  # nb_dec : nombre de d?cimale
  # na.p : NA compt?s dans les pourcentages
  xna <- length(data[,x][is.na(data[,x])])
  if(na.p==FALSE){
    xtab <- as.matrix(table(data[,x][drop=TRUE]))
  } else {
    xtab <- as.matrix(table(data[,x][drop=TRUE],useNA="ifany"))
  }
  xp <- as.matrix(round(xtab/sum(xtab)*100,nb_dec))
  if(xna!=dim(data)[1]){
    if(xna==0){
      n <- length(xtab)
      noms <- row.names(xtab)
      tab <- as.data.frame(cbind(noms,xtab[1:n], xp[1:n]))
      tot <- sum(xtab)+xna
    }else{
      if(na.p==FALSE){
        n <- length(xtab)+1
        noms <- c(row.names(xtab),"NA")
        tab <- as.data.frame(cbind(noms,c(xtab[1:n-1],xna), c(xp[1:n-1],"")))
        tot <- sum(xtab)+xna
      } else {
        n <- length(xtab)
        row.names(xtab) <- c(row.names(xtab)[1:n-1],"NA")
        tab <- as.data.frame(cbind(row.names(xtab),xtab,xp))
        tot <- sum(xtab)
      }
    }
    names(tab) <- c(" ",paste("n (",tot,")",sep=""),"%")
    return(tab)
  }else{
    return(NULL)}
}
