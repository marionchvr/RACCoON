#' Title
#'
#' @param data
#' @param del
#' @param x
#' @param x2
#' @param x_gpe_all
#' @param x_name_all
#' @param Stratas.OS
#' @param Stratas.DSS
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabHRall.OS.DSS <- function(data, del, x, x2, x_gpe_all, x_name_all,Stratas.OS,Stratas.DSS,...){
  t <- NULL
  vect_names <- NULL
  i <- 1
  for (i in 1:length(x_gpe_all)){
    s<-tabHR.OS.DSS(data=data,del=del,x=x,x2=x2,x_gpe=x_gpe_all[i],x_name=x_name_all[i],x_stratOS=Stratas.OS[[i]],x_stratDSS=Stratas.DSS[[i]])
    t <- rbind(t,s)
  }
  tab <- rbind(c("","n","HR OS","IC95%(HR OS)","p-value OS","","HR DSS","IC95%(HR DSS)","p-value DSS"),t)
  return(tab)
}
