#
# fetchandstore.r
# 

# utility functions
store <- function(..., path=.store){
    # stores object(s) in storage directory path (SvB May 2008)
    names <- as.character(substitute(list(...)))[-1]
    for (i in 1:length(names)){
        name <- names[i]
        file <- paste(path, name, sep="")
        file.access
        save(list=name,file=file)
    }
}                  

fetch <- function(..., path=.store){
    # fetches the object from the storage directory in path (SvB May 2008)
    names <- as.character(substitute(list(...)))[-1]
    for (i in 1:length(names)){
        name <- names[i]
        file <- paste(path, name, sep="")
        if (file.access(file,0)== -1) cat(paste("Warning: No file",file,"\n")) else load(file,.GlobalEnv)
    }
}


