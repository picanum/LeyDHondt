library(tidyverse)

dat <- read.csv("por_sep.csv", sep = ";", header = T)

dat2 <- read.csv("juntos.csv", sep = ";", header = T)

dhondt <- function(x,   #x = vector donde cada elemento es el número de votos a una candidatura
    nombres = NULL,     #nombres = vector donde el i-ésimo elemento es el nombre de la candidatura que ha recibido
                        #          el nº de votos indicados en el i-ésimo elemento de x.
                        #          Si no se especifica, se cogen los nombres del vector x.
    escanos,            #escanos = número de escaños a repartir
    umbral              #umbral = número entre 0 y 1 que indica el porcentaje mínimo que una candidatura ha de tener
                        #         para que pueda considerarse adjudicataria de escaños. P. ej. si es 5% -> umbral = 0.05
    ){
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

#### PARA SACAR EL Nº DE ESCAÑOS EN TODA ANDALUCÍA SI HUBIESEN IDO JUNTOS ####

dat2 %>% pivot_longer(-Partido, names_to = "provincia", values_to = "votos") %>%
    mutate(escanos = case_when(
        provincia == "Almería" ~ 12,
        provincia == "Cádiz" ~ 15,
        provincia == "Córdoba" ~ 12,
        provincia == "Granada" ~ 13,
        provincia == "Huelva" ~ 11,
        provincia == "Jaén" ~ 11,
        provincia == "Málaga" ~ 17,
        provincia == "Sevilla" ~ 18
    )) %>%
    group_by(provincia) %>% 
    summarise(escanos_final = dhondt(x = votos[complete.cases(votos)], nombres = Partido[complete.cases(votos)], escanos = escanos[1], umbral = 0.03)) %>% 
    pull(escanos_final) %>% as.data.frame() %>% group_by(Var1) %>% summarise(esc_total = sum(Freq))

#### PARA SACAR LOS ESCAÑOS POR SEPARADO ####

diferencias_prov <- function(prov_elegida){
    
    porsep <- dat %>% pivot_longer(-Partido, names_to = "provincia", values_to = "votos") %>%
        mutate(escanos = case_when(
            provincia == "Almería" ~ 12,
            provincia == "Cádiz" ~ 15,
            provincia == "Córdoba" ~ 12,
            provincia == "Granada" ~ 13,
            provincia == "Huelva" ~ 11,
            provincia == "Jaén" ~ 11,
            provincia == "Málaga" ~ 17,
            provincia == "Sevilla" ~ 18
        )) %>%
        filter(provincia == prov_elegida) %>%
        group_by(provincia) %>%
        summarise(escanos_final = dhondt(x = votos[complete.cases(votos)], nombres = Partido[complete.cases(votos)], escanos = escanos[1], umbral = 0.03)) %>%
        pull(escanos_final) %>% as.data.frame() %>% group_by(Escaños) %>% summarise(esc_total = sum(Freq))
    
    juntos <- dat2 %>% pivot_longer(-Partido, names_to = "provincia", values_to = "votos") %>%
        mutate(escanos = case_when(
            provincia == "Almería" ~ 12,
            provincia == "Cádiz" ~ 15,
            provincia == "Córdoba" ~ 12,
            provincia == "Granada" ~ 13,
            provincia == "Huelva" ~ 11,
            provincia == "Jaén" ~ 11,
            provincia == "Málaga" ~ 17,
            provincia == "Sevilla" ~ 18
        )) %>%
        filter(provincia == prov_elegida) %>%
        group_by(provincia) %>%
        summarise(escanos_final = dhondt(x = votos[complete.cases(votos)], nombres = Partido[complete.cases(votos)], escanos = escanos[1], umbral = 0.03)) %>%
        pull(escanos_final) %>% as.data.frame() %>% group_by(Escaños) %>% summarise(esc_total = sum(Freq))
    
    ret <- porsep %>% full_join(juntos, by = "Escaños")
    return(ret)
}

diferencias_prov("Almería")
diferencias_prov("Cádiz")
diferencias_prov("Córdoba")
diferencias_prov("Granada")
diferencias_prov("Huelva")
diferencias_prov("Jaén")
diferencias_prov("Málaga")
diferencias_prov("Sevilla")
