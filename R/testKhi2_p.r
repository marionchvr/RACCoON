#' Title
#'
#' @param data
#' @param x1
#' @param x2
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
testKhi2_p <- function(data, x1, x2, ...){
  data<-data[complete.cases(data[,c(x1,x2)]),]
  # Calcul des effectifs th?oriques
  obs <- addmargins(table(data[,x1],data[,x2],useNA="no"))
  nx1 <- dim(obs)[1]
  nx2 <- dim(obs)[2]
  tot <- obs[nx1,nx2]
  att <- outer(obs[, nx2], obs[nx1, ], "*")/obs[nx1,nx2]
  ddl <- prod(dim(obs) - 2)
  # Choix du test
  # Crit?re de Cochran (1954) :
  #   - toutes les classes (i,j) doivent avoir une valeur th?orique non nulle
  #   - 80 % des classes doivent avoir une valeur th?orique sup?rieure ou ?gale ? 5
  # si nombre de classes petit alors toutes les classes doivent contenir un effectif th?orique sup?rieur ou ?gal ? 5.
  if(any(att<1)) {
    CA <- F
  } else {
    if (all(att >= 5)) {
      CA <- T
      methode <- "Khi2"
    } else {
      if (all(att >= 3)) {
        CA <- T
        methode <- "Yates"
      } else {
        if (any(att < 3)) {
          CA <- T
          methode <- "Fisher"
        } else {
          CA <- F
        }
      }
    }
  }
  # R?alisation du test
  if(CA==T){
    ind <- match(methode, c("Khi2", "Yates", "Fisher"))
    testq <- switch(ind, chisq.test(data[,x1],data[,x2], correct=F), suppressWarnings(chisq.test(data[,x1],data[,x2], correct=T)),
                    fisher.test(data[,x1],data[,x2],workspace=10000000))
    p <- switch(ind, testq$p.value, testq$p.value, testq$p.value)
  } else {
    testq <- NA
    p <- NA
  }
  # Mise en forme de la p-value
  return(p)
}
