# LeyDHondt
Pequeña función para obtener el reparto de escaños según la Ley D'Hondt a partir de un vector de votos:

```R
dhondt <- function(x, nombres = NULL, escanos, umbral){
    x <- x[which(x/sum(x) >= umbral)]
    if(!is.null(nombres)) nombres <- nombres[which(x/sum(x) >= umbral)]
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
    ret <- sort(table(Escaños), decreasing=T)
    return(ret)
}
```

Hacemos la prueba para los resultados de las Elecciones Europeas de 2019 en España:

```R
x <- c(7359617,4510193,2726642,2252378,1388681,1257484,1025411,
633265,296091,294657,65921,51674,32291,30938,29259,25355,23979,
22280,19081,16672,15776,14369,12906,12760,12492,11798,11742,9812,9511,7901,6719,5572)

names(x) <- c("PSOE", "PP", "Cs", "PODEMOS-IU", "VOX", "AHORA REPÚBLICAS", "JUNTS", "CEUS", "CPE", 
"PACMA", "CV-EC", "RECORTES CERO-LV-GVE", "VOLT", "I.Fem", "PCPE-PCPC-PCPA", "PACT", "AxSÍ", "PUM+J", 
"PCTE", "pirates.cat/ep", "CXE", "FAC", "IZQP", "ALTER", "CONTIGO", "ADÑ", "CEX-CREX-PREX", 
"MCR", "IGRE", "PH", "MIEL", "SAIn")

dhondt(x, escanos = 54, umbral = 0) #Fijamos el umbral en 0 porque no hay 
                                    #% mínimo para ser adjudicatario de escaños en estas elecciones
```
```
Escaños
PSOE               PP               Cs       PODEMOS-IU AHORA REPÚBLICAS              VOX            JUNTS             CEUS 
  20               12                7                6                3                3                2                1 
```
