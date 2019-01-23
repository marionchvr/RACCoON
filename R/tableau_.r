#' Title
#'
#' @param x1
#' @param x2
#' @param data
#' @param nb_dec
#' @param px1
#' @param px2
#' @param pc
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tableau_<- function(x1, x2, data, nb_dec, px1=F , px2=F , pc=F , ...) {
  if(sum(is.na(data[,x1]))==dim(data)[1] | sum(is.na(data[,x2]))==dim(data)[1]){
    return(NULL)
  } else {
    tab <- table(data[,x1][drop=TRUE], data[,x2][drop=TRUE], useNA="ifany")
    tabx <- table(data[,x1][drop=TRUE], useNA="ifany")
    taby <- table(data[,x2][drop=TRUE], useNA="ifany")
    nomx <- names(tabx)
    nomy <- names(taby)
    nx <- length(nomx)
    ny <- length(nomy)
    dimnames(tab) <- list(nomx, nomy)
    if (px1 | px2 | pc) {
      tab <- table(data[,x1][drop=TRUE], data[,x2][drop=TRUE], useNA="no")
      tabx <- table(data[,x1][drop=TRUE], useNA="no")
      taby <- table(data[,x2][drop=TRUE], useNA="no")
      nomx <- names(tabx)
      nomy <- names(taby)
      nx <- length(nomx)
      ny <- length(nomy)
      dimnames(tab) <- list(nomx, nomy)
      if (px1) {
        tab <- round(tab/apply(tab,1,sum)*100,nb_dec)
      }
      if (px2) {
        y <- t(tab)
        tab <- round(t(t(tab)/apply(t(tab),1,sum))*100,nb_dec)
      }
      if (pc) {
        tab <- round(tab/tab[nx, ny]*100,nb_dec)
      }
    }
    return(tab)
  }
}
