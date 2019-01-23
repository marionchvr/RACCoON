#' Title
#'
#' @param x_var_OS
#' @param x_var_DSS
#' @param data
#' @param a
#' @param a2
#' @param x_name
#' @param data.OS
#' @param data.DSS
#' @param Split.OS
#' @param Split.DSS
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabHRall_mult.OS.DSS <- function(x_var_OS,x_var_DSS,data,a,a2,x_name, data.OS,data.DSS,Split.OS,Split.DSS,...){
  t <- NULL
  vect_names <- NULL
  i <- 1
  for (i in 1:length(x_name)){
    s<-tabHR_mult.OS.DSS(x_OS=x_var_OS[i],x_DSS=x_var_DSS[i],data=data,a=a,a2=a2,x_namei=x_name[i], data.OS=data.OS,data.DSS=data.DSS,Split.OS=Split.OS,Split.DSS=Split.DSS)
    t <- rbind(t,s)
  }
  tab <- rbind(c("","n OS","HR OS","IC95%(HR OS)","p-value OS","","n DSS","HR DSS","IC95%(HR DSS)","p-value DSS"),t)
  return(tab)
}
