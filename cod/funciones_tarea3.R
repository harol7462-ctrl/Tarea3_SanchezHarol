# alerta.movil
# Recibe: df (transacciones con id_tx y fecha), ventana (días usados para determinar si la cantidad de 
# transacciones es normal o no) 
# y k.alerta (desviaciones necesarias para marcar alerta)
# Devuelve: data frame con fecha, n.tx, media, desv y alerta, una fila por día.

alerta.movil <-function(df,ventana,k.alerta){
  #Cuenta las transacciones por dia 
  conteo <- aggregate(id_tx ~ fecha, df, length)
  
  names(conteo) <- c("fecha", "n.tx")
  #Se ordena las fechas de forma ascendente
  
  conteo <- conteo[order(conteo$fecha), ]
  
  conteo$media =NA 
  conteo$desv= NA 
  conteo$alerta =NA
  
  #Los primeros v dias no se cuentan
  for(t in ((ventana + 1): nrow(conteo))) {
    previos <- conteo$n.tx[(t-ventana):(t-1)]
    conteo$media[t]  <- mean(previos)
    conteo$desv[t]   <- sd(previos)
    #Es True o False, depende de si la expresion se cumple o no
    conteo$alerta[t] <- conteo$n.tx[t] > conteo$media[t] + k.alerta*conteo$desv[t]
  }
  
  conteo 
}
