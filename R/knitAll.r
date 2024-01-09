#' Knit all documents on a directory
#'
#' @param dir directory
#' @param type type of document
#' @export

knit.all<-function(dir=".", type=c("word_document", "html_document","pdf_document")) {
	rmd_files<-list.files(path=dir, pattern = "\\.Rmd$")
	for (file in rmd_files) {
		print(file)
		rmarkdown::render(file, output_format = type[1])
	}

}
