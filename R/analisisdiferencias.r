#' Compara las diferencias entre dos bases de datos
#' @param x1 Base de datos uno
#' @param x2 Base de datos dos
#' @param vars.rel nombre de columnas a reportar si hay diferencias
#' @param vars.report vector o matriz de ids a mantener
#' @export

analisisDiferencias <-
  function(x1,
           x2,
           vars.rel = colnames(x1),
           vars.report = x1[, 1],
           verbose = T) {
    if (nrow(x1) != nrow(x2)) {
      stop("Different number of rows between database 1 and database 2")
    }
    n <- nrow(x1)
    reporte <- list()
    if (is.vector(vars.report)) {
      reporte$ID <- vars.report
      report_id<-"ID"
    } else {
      for (i in 1:ncol(vars.report)) {
        reporte[[paste0("ID.", i)]] <- vars.report[, i]
      }
      report_id<-paste0("ID.", 1:ncol(vars.report))
    }
    
    # delete id variables in vars.rel
    vars.rel<-setdiff(vars.rel, report_id)
    reporte$problemas <- character(n)
    casos.problemas <- numeric()
    var.problema <- c()
    for (i in names(x1)) {
      reportar <- i %in% vars.rel
      if (!reportar) {
        next
      }
      reporte[[i]] <- character(n)
      if (is.null(x2[[i]])) {
        if (verbose)
          cat("No existe variable ", i, " en la segunda base de datos\n")
        var.problema <- c(var.problema, i)
        next
      }

      # comparar datos faltantes
      na1 <- is.na(x1[, i])
      na2 <- is.na(x2[, i])
      if (reportar & verbose) {
        cat("Variable:", i, "\n")
      }
      val.perdidos <- na1 != na2
      if (sum(val.perdidos) > 0) {
        #    print(val.perdidos)
        d1 <- character(n)
        d2 <- character(n)
        # Perdidos en 1
        d1[val.perdidos & na1] <- "NE1"
        d1[val.perdidos & na2] <- "NE2"
        d2[val.perdidos & !na1] <- x1[val.perdidos & !na1, i]
        d2[val.perdidos & !na2] <- x2[val.perdidos & !na2, i]
        reporte[[i]] <- paste(d1, "-", d2)
        reporte[[i]][-which(val.perdidos)] <- ""

        if (reportar) {
          casos.problemas <- c(casos.problemas, which(val.perdidos))
          if (verbose) {
            cat("Problemas en datos perdidos:\n")
            print(which(val.perdidos))
          }
        }
      }

      conj <- cbind(as.character(x1[[i]]), as.character(x2[[i]]))
      falt.conj <- which(conj[, 1] != conj[, 2])
      if (length(falt.conj) > 0) {
        difs <- paste("D(", x1[[i]], "/", x2[[i]], ")")
        reporte[[i]][falt.conj] <- difs[falt.conj]
        if (reportar) {
          casos.problemas <- c(casos.problemas, falt.conj)
          if (verbose) {
            cat("Problemas en datos concretos\n")
            print(falt.conj)
          }
        }
      }
    }
    reporte$problemas[casos.problemas] <- "S"
    list(reporte = data.frame(reporte), var.problema = var.problema)
  }
