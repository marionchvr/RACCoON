#' Title
#'
#' @param data
#' @param Split.OS
#' @param Split.DSS
#' @param formula.OS
#' @param formula.DSS
#' @param x_var_OS
#' @param x_var_DSS
#' @param x_name
#' @param titre
#' @param widths
#'
#' @return
#' @export
#'
#' @examples
tabHRall.fun_mult.OS.DSS <- function(data,Split.OS,Split.DSS,formula.OS,formula.DSS,x_var_OS,x_var_DSS,x_name,titre, widths=NULL){
  Fact<-unique(c(grep(".f",x_var_OS),grep(".f",x_var_DSS)))
  data2<-data
  if(length(Fact)>0){ sapply(Fact, function(kk){
    var.name<-unique(c(x_var_OS[kk],x_var_DSS[kk])[!is.na(c(x_var_OS[kk],x_var_DSS[kk]))])
    # creer des var indicatires
    dtemp<-dummy(data[,var.name], sep = "_")
    colnames(dtemp)<-paste0(var.name, levels(data[,var.name]))
    data2<<-cbind(data2,dtemp )
    x_gpeMod<-colnames(dtemp)[-1]
    x_gpeMod<-paste(paste0("`",x_gpeMod,"`"),collapse = "+")
    if(is.element(var.name,x_var_OS)){formula.OS<<-gsub(var.name,paste0("(",x_gpeMod,")"),formula.OS)}
    if(is.element(var.name,x_var_DSS)){formula.DSS<<-gsub(var.name,paste0("(",x_gpeMod,")"),formula.DSS)}
  })
  }
  data.OS<<-survSplit(Surv(t, status)~.,data=data2,cut=Split.OS,episode="tgroup",id="id")
  data.DSS<<-survSplit(Surv(t, statusDSS)~.,data=data2,cut=Split.DSS,episode="tgroup",id="id")
  a<-summary(coxph(as.formula(formula.OS),data=data.OS[complete.cases(data.OS),]))
  a2<-summary(coxph(as.formula(formula.DSS),data=data.DSS[complete.cases(data.DSS),]))
  tab1<-tabHRall_mult.OS.DSS(x_var_OS=x_var_OS,x_var_DSS=x_var_DSS,data=data,a=a,a2=a2,x_name=x_name,data.OS=data.OS,data.DSS=data.DSS,Split.OS=Split.OS,Split.DSS=Split.DSS)
  if(!is.null(tab1)){
    doc2 <<- addParagraph(doc2, value= paste(titre, "n.ev=",a$nevent, ";", a2$nevent) ,stylename = "rTableLegend")
    tab = FlexTable(data = tab1, add.rownames = F, header.columns = F)
    if(is.null(widths)){
      tab = setFlexTableWidths(tab, widths = c(1, 0.6, 0.6, 1, 0.6,1,0.6,0.6, 1,0.6))
    } else {  tab = setFlexTableWidths(tab, widths = widths)  }
    tab[,] = textProperties(font.family="Arial", font.size=10)
    tab[,] = parProperties(text.align = "left")
    tab[,] = cellProperties(vertical.align = "top")
    # bordures
    tab = setFlexTableBorders(tab, inner.vertical = borderNone(), inner.horizontal = borderNone(),outer.vertical = borderNone(),outer.horizontal = borderSolid())
    tab[tab1[,2]=="",,side="top"] = borderSolid()
    tab[which(tab1[,1] %in% x_name),] = textProperties(font.family="Arial", font.size=10, font.weight = "bold")
    doc2 <<- addFlexTable(doc2, tab)
  }
}
