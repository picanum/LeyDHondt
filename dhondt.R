dhondt <- function(x, nombres = NULL, escanos, umbral){
    partidos_que_entran <- which(x/sum(x) >= umbral)
    x <- x[partidos_que_entran]
    if(!is.null(nombres)) nombres <- nombres[partidos_que_entran]
    temp1 <- sort(sapply(1:escanos, function(i) x/i), decreasing=T)[1:escanos]
    mat <- matrix(sapply(1:escanos, function(i) x/i), ncol = length(x), byrow=T)
    if(is.null(nombres)){
        colnames(mat) <- names(x)
    }
    else{
        colnames(mat) <- nombres
    }
    Escaños <- NULL
    for(i in 1:escanos){
        Escaños <- c(Escaños, colnames(mat)[which(mat[i,] >= min(temp1))])
    }
    ret <- sort(table(Escaños),decreasing=T)
    return(ret)
}
