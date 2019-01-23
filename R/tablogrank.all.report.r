#' Title
#'
#' @param data
#' @param x
#' @param delai
#' @param etat
#' @param etat2
#' @param nb_dec
#' @param titre
#'
#' @return
#' @export
#'
#' @examples
tablogrank.all.report <- function(data, x,  delai, etat,etat2, nb_dec, titre){
  COMB<-combn(levels(as.factor(data[,x])),2)
  tab1 <- tablogrank.all(data=data,  x=x, COMB=COMB, delai=delai, etat=etat, etat2=etat2, nb_dec=nb_dec)
  doc2 <<- addParagraph(doc2, value= titre ,stylename = "rTableLegend")
  tab = FlexTable(data = tab1, add.rownames = F, header.columns = T)
  tab = setFlexTableWidths(tab, widths = c(2,2, 1, 1))
  tab[,] = textProperties(font.family="Arial", font.size=10)
  tab[,] = parProperties(text.align = "left")
  tab[,] = cellProperties(vertical.align = "middle")
  tab = setFlexTableBorders(tab, inner.vertical = borderNone(), inner.horizontal = borderNone(),
                            outer.vertical = borderNone(), outer.horizontal = borderSolid())
  doc2 <<- addFlexTable(doc2, tab)
}
