oneVScrList <- function (base, data){

    lists <- list()

    colNames <- colnames(data)
    sortData <- function(colNum){
        for(i in 1:length(lists)){
            if(i == colNum){
                writeList(lists[[i]], sort(data[,i]), clear = TRUE)
            }else{
                writeList(lists[[i]], data[match(sort(data[,colNum]),
                              data[,colNum]), i], clear = TRUE)
            }
        }
    }
    for (i in 1:length(colNames)) {
        tempFrame <- tkframe(base)
        tkpack(tkbutton(tempFrame, text = colNames[i],
                  width = nchar(colNames[i])), expand = FALSE, fill = "x")
        lists[[colNames[i]]] <- tklistbox(tempFrame, exportselection = FALSE,
                                     width = max(sapply(data[,i], nchar)))
        writeList(lists[[colNames[i]]], data[,i])
        tkpack(lists[[colNames[i]]], expand = TRUE, fill = "both")
        fun <- function() {}
        body <- list(as.name("{"),
                     substitute(sortData(j), list(j = i)))
        body(fun) <- as.call(body)
        tempBut <- tkbutton(tempFrame, text = "Sort", width = 5,
                            command = fun)
        tkpack(tempBut, expand = FALSE, fill = "x")
        tkpack(tempFrame, side = "left", expand = FALSE, fill = "y")
    }
    bindYView <- function(...){
        for(i in lists){
            tkyview(i,...)
        }
    }

    vScr <- tkscrollbar(base, orient = "vertical", command = bindYView)
    tkpack(vScr, side = "right", fill = "y")
    for (i in lists) {
        tkconfigure(i, yscrollcommand = function(...) tkset(vScr,
            ...))
    }
    return(lists)
}
