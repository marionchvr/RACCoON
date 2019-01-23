#' Title
#'
#' @param data
#' @param x_all
#' @param x_name_all
#' @param nb_dec
#' @param na.p
#' @param titre
#' @param widths
#'
#' @return
#' @export
#'
#' @examples
taball.qual.fun <- function(data, x_all, x_name_all, nb_dec, na.p=FALSE, titre, widths=NULL){
  tab1 = taball.qual(data, x_all=x_all, x_name_all=x_name_all, nb_dec=nb_dec, na.p=na.p)
  if(!is.null(tab1)){
    doc2 <<- addParagraph(doc2, value= titre ,stylename = "rTableLegend")
    tab = FlexTable(data = tab1, add.rownames = F, header.columns = T)
    if(is.null(widths)){
      tab = setFlexTableWidths(tab, widths = c(2, 3, 0.6, 0.6))
    } else {
      tab = setFlexTableWidths(tab, widths = widths)
    }
    tab = spanFlexTableRows(tab, j=1, runs=as.character(tab1[,1]))
    tab[,] = textProperties(font.family="Arial", font.size=10)
    tab[,] = parProperties(text.align = "left")
    tab[,] = cellProperties(vertical.align = "top")
    # bordures
    tab = setFlexTableBorders(tab, inner.vertical = borderNone(), inner.horizontal = borderNone(),outer.vertical = borderNone(),
                              outer.horizontal = borderSolid())
    tab[!duplicated(tab1[,1]),2:dim(tab1)[2],side="top"] = borderSolid()
    doc2 <<- addFlexTable(doc2, tab)
  }
}
