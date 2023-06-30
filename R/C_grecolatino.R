#'Diseño en Cuadro Grecolatino
#'
#'Es una tecnica estadistica que permite controlar el efecto de dos o mas
#'en un estudio
#'
#'@param respuesta (vector)datos de la variable respuesta
#'@param tratamiento (vector) datos de mi variable explicativa
#'@param observaciones (vector) datos de mis variables totales
#'@param data (dataframe)tabla de datos en formato largo
#'@return crea una tabla con los calculos correspondientes para
#'el analisis de la varianza
#'@export
#'
#'@examples
#'\dontrun{
#'Lectura del archivo de datos
#'library(readxl)
#'DATOS_DeCG <- read_excel("C:/Users/Ivonne/Desktop/DATOS.DeCG.xlsx")
#'View(DATOS_DeCG)
#'df<-data.frame(DATOS_DeCG)
#'
#'#Ejecutamos la funcion
#'Tabla_ANOVA(respuesta = tabla_anova,tratamiento = DatOS_DeCG,observaciones = 25,data = data.frame())
#'}
Tabla_ANOVA<-function(respuesta,tratamiento,observaciones,data){

  # Calcular la suma total y el total de observaciones
  suma_total <- sum()
  total_observaciones <- length(n)

  # Calcular las sumas de cuadrados entre tratamientos y los grados de libertad correspondientes
  suma_cuadrados_entre <- sum(colSums()^2) / total_observaciones - suma_total^2 / (total_observaciones^2)
  grados_libertad_entre <- ncol() - 1

  # Calcular las sumas de cuadrados dentro de los tratamientos y los grados de libertad correspondientes
  suma_cuadrados_dentro <- sum(colSums(datos^2)) - sum(colSums(datos)^2) / total_observaciones
  grados_libertad_dentro <- total_observaciones - ncol(datos)

  # Calcular las sumas de cuadrados total y los grados de libertad totales
  suma_cuadrados_total <- sum(datos^2) - suma_total^2 / total_observaciones
  grados_libertad_total <- total_observaciones - 1

  # Calcular los cuadrados medios
  cm_entre <- suma_cuadrados_entre / grados_libertad_entre
  cm_dentro <- suma_cuadrados_dentro / grados_libertad_dentro

  # Calcular el estadístico F y el valor p
  estadistico_F <- cm_entre / cm_dentro
  valor_p <- 1 - pf(estadistico_F, grados_libertad_entre, grados_libertad_dentro)

  # Crear la tabla ANOVA
  tabla<- data.frame(
    Fuente = c("Tratamientos", "Error", "Total"),
    SumaCuadrados = c(suma_cuadrados_entre, suma_cuadrados_dentro, suma_cuadrados_total),
    GradosLibertad = c(grados_libertad_entre, grados_libertad_dentro, grados_libertad_total),
    CuadradosMedios = c(cm_entre, cm_dentro, NA),
    Estadistico_F = c(estadistico_F, NA, NA),
    Valor_p = c(valor_p, NA, NA)
  )

 return(respuesta)
}

