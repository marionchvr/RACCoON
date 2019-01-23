#' Title
#'
#' @param data
#' @param x_all
#' @param x_name_all
#' @param nb_dec
#' @param na.p
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
taball.qual <- function(data, x_all, x_name_all, nb_dec, na.p, ...){
  t <- NULL
  vect_names <- NULL
  for (i in 1:length(x_all)){
    s <- tabdesc.qual(x_all[i], data=data, nb_dec=nb_dec, na.p)
    t <- rbind(t,s)
    if(!is.null(s)){vect_names <- c(vect_names,rep(x_name_all[i],nrow(s)))}
  }
  taball <- cbind(" "=vect_names,t)
  return(taball)
}
