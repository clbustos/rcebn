#' For a specific categoric variable, presents frequency and percentage
#' @export
n.y.p <- function(x, ...) {
  t <- table(x, useNA = "ifany")
  p.t <- round(prop.table(t) * 100, 1)
  pander::pandoc.table(data.frame(n = t, p = unname(as.numeric(p.t))), ...)
}


#' Describe a set of variables 
#' use ggplot
#' @export
descripcion.general <- function(x, vars.desc = colnames(x)) {
  for (i in vars.desc) {
    cat("\n")
    pander::pandoc.header(gsub(".", " ", i, fixed = T), 2)
    cl <- class(x[[i]])
    un <- unique(x[[i]])
    if (cl == "character" | cl == "factor") {
      if (length(un) > 10) {
        cat("\n\n ** Respuesta abierta. Se debe clasificar ** ")
      } else {
        t1 <- table(x[[i]])
        n.y.p(x[[i]])
        gg1 <- ggplot2::ggplot(data.frame(t1), aes(x = Var1, y = Freq)) +
          ggplot2::geom_bar(
            stat = "identity",
            color = "#282B62",
            fill = "#616CB1"
          ) +
          ggplot2::scale_x_discrete(limits = rev(levels(data.frame(t1)$Var1))) +
          ggplot2::theme_bw() + ggplot2::coord_flip() +
          ggplot2::xlab(" ") + ggplot2::ylab("n")
        print(gg1)
      }
    } else if (cl == "integer" || cl == "numeric") {
      pander::pandoc.table(psych::describe(x[[i]]))
      if (length(unique(x[[i]])) < 10) {
        t1 <- table(factor(x[[i]]))
        g2 <- ggplot2::ggplot(data.frame(x = x[[i]]), aes(x = x)) +
          ggplot2::geom_histogram(
            binwidth = 1,
            color = "#282B62",
            fill = "#616CB1"
          ) +
          ggplot2::theme_bw() +
          ggplot2::xlab(" ") + ggplot2::ylab("n")
      } else {
        g2 <- ggplot2::ggplot(data.frame(x = x[[i]]), aes(x = x)) +
          ggplot2::geom_histogram(color = "#282B62", fill = "#616CB1") +
          ggplot2::theme_bw() +
          ggplot2::xlab(" ") +
          ggplot2::ylab("n")
        
        
      }
      print(g2)
    } else {
      cat("Tipo:", cl)
    }
  }
}
