#' Title
#'
#' @param data
#' @param del
#' @param x
#' @param x2
#' @param x_gpe_all
#' @param Stratas.OS
#' @param Stratas.DSS
#' @param x_name_all
#' @param titre
#' @param widths
#'
#' @return
#' @export
#'
#' @examples
tabHRall.fun.OS.DSS<-function(data, del, x, x2, x_gpe_all,Stratas.OS,Stratas.DSS, x_name_all, titre, widths=NULL){
  # del=> delais
  #x statutus OS
  # x2 = status DSS att. ordre important
  #x_gpe_all <-variable qualitative
  tab1 <- tabHRall.OS.DSS(data=data, del=del, x=x, x2=x2, x_gpe_all=x_gpe_all, x_name_all=x_name_all,Stratas.OS=Stratas.OS,Stratas.DSS=Stratas.DSS)
  if(!is.null(tab1)){
    doc2 <<- addParagraph(doc2, value= titre ,stylename = "rTableLegend")
    tab = FlexTable(data = tab1, add.rownames = F, header.columns = F)
    if(is.null(widths)){
      tab = setFlexTableWidths(tab, widths = c(1.5, 0.4, 0.4, 1, 0.8, 1.5,0.4, 1, 0.8))
    } else {
      tab = setFlexTableWidths(tab, widths = widths)
    }
    tab[,] = textProperties(font.family="Arial", font.size=10)
    tab[,] = parProperties(text.align = "left")
    tab[,] = cellProperties(vertical.align = "top")
    # bordures
    tab = setFlexTableBorders(tab, inner.vertical = borderNone(), inner.horizontal = borderNone(),outer.vertical = borderNone(),
                              outer.horizontal = borderSolid())
    tab[tab1[,2]=="",,side="top"] = borderSolid()
    tab[which(tab1[,1] %in% x_name_all),] = textProperties(font.family="Arial", font.size=10, font.weight = "bold")
    doc2 <<- addFlexTable(doc2, tab)
  }
}
