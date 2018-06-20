#' Export a data.frame to SPSS, using the metadata
#' of another .sav
#'
#' @param original object returned by \code{read.spss}
#' @param df The data.frame to export
#' @param basename Base name for datafile and codefile
#' @param varnames
#'
#' @export
#'
exportSpss<-function(original,df,basename,varnames=NULL) {
  datafile=paste0(basename,".dat")
  codefile=paste0(basename,".sps")
    adQuote <-  function (x) paste("\"", x, "\"", sep = "")
    #write.table(df, file = datafile, row = FALSE, col = FALSE,
    #                  sep = ",", quote = T, na = "-99", eol = ",\n")
    write.table(df,file=datafile,row.names=F,col.names=T,quote=F,sep="\t",na="-99")
    df.names<-names(df)

    varlabels <- names(df)
    if (is.null(varnames)) {
        #varnames <- abbreviate(names(df), 8L)
        varnames<-names(df)
  #      if (any(sapply(varnames, nchar) > 8L))
  #          stop("I cannot abbreviate the variable names to eight or fewer letters")
  #      if (any(varnames != varlabels))
  #          warning("some variable names were abbreviated")
    }
    varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
    dl.varnames <- varnames
    if (any(chv <- sapply(df, is.character))) {
        lengths <- sapply(df[chv], function(v) max(nchar(v)))
        if (any(lengths > 255L))
            stop("Cannot handle character variables longer than 255")
        lengths <- paste("(A", lengths, ")", sep = "")
        star <- ifelse(c(FALSE, diff(which(chv) > 1)), " *",
                       " ")
        dl.varnames[chv] <- paste(star, dl.varnames[chv], lengths)
    }
    cat("GET TRANSLATE FILE=",adQuote(datafile),"\n/TYPE=TAB\n/FIELDNAMES.\n",
        file = codefile)

  #  cat("/", dl.varnames, " .\n\n", file = codefile, append = TRUE)

    for(i in 1:length(df.names)) {
      v.or<-varlabels[i]
      res<-attr(original,"variable.labels")[v.or]
      if(!is.na(res)) {
        varlabels[i]<-res
      }
    }

    # Labels de  variables

     cat("VARIABLE LABELS\n", file = codefile, append = TRUE)

     cat(paste(varnames, adQuote(varlabels), "\n"), ".\n", file = codefile,
              append = TRUE)
     cat(paste("MISSING VALUES ",varnames," (-99).\n",sep=""),file = codefile,
              append = TRUE)
    # Valores en variables
    labels.t<-attr(original,"label.table")
     for(i in 1:length(df.names)) {
      labs<-labels.t[[df.names[i]]]
      if(!is.null(labs)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        cat(dl.varnames[i],"\n",file = codefile, append = TRUE)
        cat(paste(labs,adQuote(names(labs)),"\n",sep=" "), file = codefile, append = TRUE)
        cat(".\n", file = codefile, append = TRUE)
        }
     }



    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
  }
