# A wraper of the file function that checks to see if a given file
# name does exist.

safeFileOpen <- function(fileName) {

    if (file.exists(fileName))
        file(fileName)
    else
        paste(fileName,"doest not exist")
}

