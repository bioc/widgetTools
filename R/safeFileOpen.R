# A wraper of the file function that checks to see if a given file
# name does exist.

safeFileOpen <- function(fileName) {

    if (file.exists(fileName)){
        info <- file.info(fileName)
        if(info$isdir){
            return(paste(fileName, "is a directory"))
        }
        if(file.access(fileName, mode = 4) != 0){
            return(paste("You don't have read permission to file", fileName))
        }
        if(regexpr("\\.rda$|\\.Rda$", fileName) > 0){
            if(readLines(fileName, n = 1) == "RDX2"){
                return(paste(fileName, "is a binary file"))
            }else{
                return(file(fileName))
            }
        }
        if(regexpr("\\.gz", fileName) > 0){
            return(gzfile(fileName))
        }else if(regexpr("\\.zip", fileName) > 0){
            return(unz(fileName))
        }else if(regexpr("\\.bz2", fileName) > 0 ){
            return(bzfile(fileName))
        }else{
            return(file(fileName))
        }
    }else
        return(paste(fileName,"doest not exist"))
}


