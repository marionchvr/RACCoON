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
#' @param titre
#' @param widths
#' @param test
#'
#' @return
#' @export
#'
#' @examples
tabcont.qual.fun_ <- function(data, x_all, x2, xall_name, x2_name, nb_dec, pcol=TRUE, plig=TRUE, ptot=TRUE, titre, widths=NULL, test=FALSE){
  tab2 <- tabcont.qual_(data, x_all, x2, xall_name, x2_name, nb_dec, pcol, plig, ptot)
  if(!is.null(tab2)){
    doc2 <<- addParagraph(doc2, value= titre ,stylename = "rTableLegend")
    tab = FlexTable(data = tab2, add.rownames = F, header.columns = F, header.par.props = parProperties(text.align = "center"), body.par.props = parProperties(text.align = "center"))
    tab[,] = textProperties(font.family="Arial", font.size=10)
    if(is.null(widths)){
      if(sum(is.na(colnames(tab2)))==1){
        widths <- c(1.1,0.7,rep(1.2,dim(tab2)[2]-4),0.5,1)
      } else {
        widths <- c(1.1,0.7,rep(1.2,dim(tab2)[2]-2))
      }
    } else {
      widths <- widths
    }
    nlev_x2<-nlevels(as.factor(data[,x2]))
    tab = setFlexTableWidths(tab, widths = widths)
    tab = spanFlexTableRows(tab, j=1, runs=as.character(tab2[,1]))
    tab = spanFlexTableRows(tab, j=2, runs=as.character(tab2[,2]))
    tab = spanFlexTableRows(tab, j=(nlev_x2+3), runs=as.character(tab2[,1]))
    tab = spanFlexTableRows(tab, j=(nlev_x2+4), runs=as.character(tab2[,1]))
    tab = spanFlexTableColumns(tab, i=1, runs=as.character(tab2[1,]))
    # bordures
    tab = setFlexTableBorders(tab, inner.vertical = borderNone(), inner.horizontal = borderNone(),outer.vertical = borderNone(),
                              outer.horizontal = borderSolid())
    tab[seq(1,nrow(tab2))[!duplicated(tab2[,1])],1:ncol(tab2),side="top"] = borderSolid()
    doc2 <<- addFlexTable(doc2, tab)
  }
}
