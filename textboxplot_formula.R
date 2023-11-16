library(tidyverse)
library(txtplot)


#txtboxplot(mtcars$cyl, mtcars$mpg)
#txtboxplot(mtcars$cyl)
#
#txtboxplot.formula <- function(form, data){
#  myterms <- terms(formula)
#
#
#  vec_list <- sapply()
#}
#
#terms(x ~ y)

txtboxplot.formula <- function (formula, data = NULL, ..., subset, na.action = NULL,
    drop = FALSE, sep = ".", lex.order = FALSE){
    #library(txtplot)
    if (missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- m$drop <- m$sep <- m$lex.order <- NULL
    m$na.action <- na.action
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")

    b <- split(mf[[response]], mf[-response], drop = drop, 
                      sep = sep, lex.order = lex.order)
    txtboxplot_...string <- paste0("b[['", names(b), "']]") 
    txtboxplot_...string <- paste(txtboxplot_...string, collapse = ",") 
    txtboxplot_call <- paste0("txtboxplot(", txtboxplot_...string, ")")
    eval(parse(text = txtboxplot_call))
    #return(txtboxplot_call)
    #txtboxplot(vec_list)
}

