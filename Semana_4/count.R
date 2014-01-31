count <- function(cause = NULL) {
    homicides <- readLines("homicides.txt")
    cause_arr <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
    if(!cause %in% cause_arr){
        stop(cause)
    } else {
        if(cause == "asphyxiation"){
            count <- length(grep("[Aa]sphyxiation", homicides))
        } else if(cause == "blunt force"){
            count <- length(grep("[Cc]ause: [Bb]lunt [Ff]orce", homicides))
        } else if(cause == "shooting"){
            count <- length(grep("[Cc]ause: [Ss]hooting", homicides))
        } else if(cause == "stabbing"){
            count <- length(grep("[Cc]ause: [Ss]tabbing", homicides))
        } else if(cause == "other"){
            count <- length(grep("[Cc]ause: [Oo]ther", homicides))
        } else { 
            count <- length(grep("[Cc]ause: [Uu]nknown", homicides))
        }
        count
    }
}
