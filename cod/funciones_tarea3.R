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
# Entrada: se recibe un vector con montos 
# Salida: devuelve su desviacion absoluta media con respecto a la ley de benford

library(dplyr)

benford.mad <- function(x) {
  
  # Se escogen los montos reales (finitos y mayores a 0)
  x <- x[is.finite(x) & x > 0]

  # Revisa que hayan quedado montos, sino NA
  if (length(x) == 0) {
    return(NA)
  }
  
  # Obtiene el primer digito de cada monto
  primer.digito <- floor(x / 10^floor(log10(x)))
  
  # Se cuenta la cantidad de veces que aparecen los digitos del 1 al 9 como 
  # primeros digitos de un monto.
  conteos <- tabulate(primer.digito, nbins = 9)
  
  # Calcula las proporciones observadas.
  proporcion.obs <- conteos / length(x)
  
  # Calcula las proporciones de Benford.
  proporcion.benford <- log10(1 + 1 / (1:9))
  
  # Se promedian la resta en valor absoluto
  mean(abs(proporcion.obs - proporcion.benford))
} 
# Entrada: recibe el dataframe, el canal y la tabla de cantones 
# Salida: devuelve el codigo del cantón, su nombre, cantidad de montos validos,
# desviacion absoluta media y la clase. El resultado está ordenado de forma 
# decreciente respecto a la MAD. 

benford.canton <- function(df, canal, cantones) {
  
  # Se conservan los montos cuyo canal sea el recibido, no otro o NA.
  transacciones.canal <- df[
    !is.na(df$canal) & df$canal == canal,
  ]
  
  # Se seleccionan los montos reales (finitos y mayores a 0). 
  transacciones.canal <- transacciones.canal[
    is.finite(transacciones.canal$monto_crc) &
      transacciones.canal$monto_crc > 0,
  ]
  
  # Se agrupan las transacciones por codigo de cantón, se pone el numero de filas
  # , se calcula la desviacion absoluta mediana (MAD) con la función anterior
  # y se desagrupa. 
  resumen <- transacciones.canal |>
    group_by(cod_canton) |>
    summarise(
      n = n(),
      mad = benford.mad(monto_crc),
      .groups = "drop"
    )
  
  # Agrega el nombre de cada cantón.
  resumen <- resumen |>
    left_join(
      cantones |> select(cod_canton, canton),
      by = "cod_canton"
    )
  
  # Se clasifica los cantones según el rango en que se encuentra su mad
  resumen <- resumen |>
    mutate(
      clase = case_when(
        mad <= 0.006 ~ "conformidad cercana",
        mad <= 0.012 ~ "conformidad aceptable",
        mad <= 0.015 ~ "conformidad marginal",
        TRUE ~ "no conformidad"
      )
    )
  
  # Se seleccionan las columnas para ponerlas en ese orden y se ordena de manera 
  # descendente para dejar arriba la mayor desviación (mad).
  resumen |>
    select(cod_canton, canton, n, mad, clase) |>
    arrange(desc(mad))
} 


# Recibe las transacciones, la tasa efectiva anual y el canal.
# Devuelve una lista con la tabla con los montos de cada mes,tasa mensual, valor acumulado y intereses ganados.
valor.acumulado <- function(df, tasa.anual, canal) {
  
  # Selecciona las transacciones del canal solicitado y verifica que sean validas (diferentes de NA)
  transacciones.canal <- df[
    !is.na(df$canal) & df$canal == canal,
  ]
  
  # A partir de la fecha de cada transaccion, se extrae el mes. 
  transacciones.canal$mes <- as.integer(
    format(as.Date(transacciones.canal$fecha), "%m")
  )
  
  # Se agrupa por mes las transacciones, se muestra la suma de los montos para cada mes y se desagrupa.
  tabla <- transacciones.canal |>
    group_by(mes) |>
    summarise(
      monto = sum(monto_crc),
      .groups = "drop"
    )
  
  # Se construye una tabla con los doce meses y se le unen las transacciones por mes, quedando algunos sin transacciones.
  tabla <- data.frame(mes = 1:12) |>
    left_join(tabla, by = "mes")
  #Se convierten los NA a 0. 
  tabla$monto[is.na(tabla$monto)] <- 0
  
  # Calcula la tasa efectiva mensual equivalente.En base a la ecuacion (1+im)^12=(1+i)
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  
  # El factor de acumulacion se obtiene de multiplicar por (1+im) elevado al numero de periodos que faltan.
  tabla$factor <- (1 + tasa.mensual)^(12 - tabla$mes)
  
  # Formulas para el valor acumulado y el interés.
  # Suma del monto del monto de cada més multiplicado por el valor de acumulación.
  va <- sum(tabla$monto * tabla$factor)
  # El interés es el valor acumulado menos la suma de los montos. 
  interes <- va - sum(tabla$monto)
  
  # Se devuelve la lista con la tabla, la tasa mensual, el valor acumulado y el interés ganado.
  list(
    tabla = tabla,
    tasa.mensual = tasa.mensual,
    va = va,
    interes = interes
  )
}