# Crear la tabla de contingencia
datos <- matrix(c(2, 11, 10,   # Buenos Aires
                  7, 3, 3,     # Rosario
                  31, 17, 11), # Virtual
                nrow = 3, byrow = TRUE)

# Asignar nombres a filas y columnas
rownames(datos) <- c("Buenos Aires", "Rosario", "Virtual")
colnames(datos) <- c("Gerencial", "Junior", "Senior")

# Mostrar la tabla
print("Tabla de contingencia:")
print(datos)

# Calcular totales marginales
print("\nTotales por fila:")
print(rowSums(datos))
print("\nTotales por columna:")
print(colSums(datos))
print(paste("\nTotal general:", sum(datos)))

# Realizar el test Chi-cuadrado
test_chi <- chisq.test(datos)

# Mostrar resultados
print("\n===== RESULTADOS DEL TEST CHI-CUADRADO =====")
print(test_chi)

# Mostrar frecuencias esperadas
print("\nFrecuencias esperadas bajo H0:")
print(round(test_chi$expected, 2))

# Mostrar residuos estandarizados
print("\nResiduos estandarizados:")
print(round(test_chi$residuals, 2))

# Interpretación
cat("\n===== INTERPRETACIÓN =====\n")
cat(paste("Estadístico Chi-cuadrado:", round(test_chi$statistic, 4), "\n"))
cat(paste("Grados de libertad:", test_chi$parameter, "\n"))
cat(paste("P-valor:", round(test_chi$p.value, 4), "\n"))

if(test_chi$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Con un nivel de significancia de 0.05, RECHAZAMOS H0.")
  cat("\nExiste evidencia estadística de asociación entre la sede y la categoría de alumno.\n")
} else {
  cat("\nCONCLUSIÓN: Con un nivel de significancia de 0.05, NO RECHAZAMOS H0.")
  cat("\nNo hay evidencia suficiente de asociación entre la sede y la categoría de alumno.\n")
}

# Verificar supuestos del test
cat("\n===== VERIFICACIÓN DE SUPUESTOS =====\n")
esperadas_menores_5 <- sum(test_chi$expected < 5)
cat(paste("Celdas con frecuencia esperada < 5:", esperadas_menores_5, "de", length(test_chi$expected), "\n"))

if(esperadas_menores_5 > 0.2 * length(test_chi$expected)) {
  cat("ADVERTENCIA: Más del 20% de las celdas tienen frecuencia esperada < 5.")
  cat("\nSe recomienda usar el test exacto de Fisher o combinar categorías.\n")
}


# Test Exacto de Fisher para tablas mayores a 2x2
test_fisher <- fisher.test(datos, simulate.p.value = TRUE, B = 10000)

print("\n===== TEST EXACTO DE FISHER =====")
print(test_fisher)

cat("\nInterpretación:")
cat(paste("\nP-valor:", round(test_fisher$p.value, 4)))

if(test_fisher$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Existe evidencia significativa de asociación entre sede y categoría.\n")
} else {
  cat("\nCONCLUSIÓN: No hay evidencia significativa de asociación entre sede y categoría.\n")
}


# Chi-cuadrado con simulación Monte Carlo
test_chi_sim <- chisq.test(datos, simulate.p.value = TRUE, B = 10000)

print("\n===== CHI-CUADRADO CON SIMULACIÓN MONTE CARLO =====")
print(test_chi_sim)

cat("\nInterpretación:")
cat(paste("\nP-valor (simulado):", round(test_chi_sim$p.value, 4)))

if(test_chi_sim$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Existe evidencia significativa de asociación entre sede y categoría.\n")
} else {
  cat("\nCONCLUSIÓN: No hay evidencia significativa de asociación entre sede y categoría.\n")
}




# --- DATOS ---
# Tiempos en formato "h:mm:ss" convertidos a segundos

# Grupo 1 semilla
semilla_1 <- c("0:29:21", "0:35:45", "0:36:33", "0:34:34", "0:38:51", 
               "0:31:59", "0:29:38", "0:33:22", "0:29:47", "0:24:07", 
               "0:28:44", "0:34:56", "0:21:16", "0:31:54", "0:38:59", 
               "0:44:18", "0:34:07", "0:28:37", "0:31:57", "0:34:12", 
               "0:37:24", "0:34:26", "0:48:36", "0:31:05", "0:42:06")

# Grupo 5 semillas
semilla_5 <- c("3:19:47", "5:51:44", "2:59:23", "5:41:11", "3:41:05", 
               "2:59:36", "3:54:04", "2:42:06", "3:10:13", "2:59:42", 
               "2:28:48", "2:45:15", "2:38:09", "5:52:40", "4:40:53", 
               "4:33:43", "2:21:47", "3:33:50", "1:40:54", "3:21:28", 
               "3:13:28", "2:33:03", "7:31:33", "4:11:19", "2:11:21")

# Función para convertir a segundos
tiempo_a_segundos <- function(tiempo) {
  partes <- as.numeric(strsplit(tiempo, ":")[[1]])
  return(partes[1] * 3600 + partes[2] * 60 + partes[3])
}

# Convertir a segundos
semilla_1_seg <- sapply(semilla_1, tiempo_a_segundos)
semilla_5_seg <- sapply(semilla_5, tiempo_a_segundos)

# --- TEST DE WILCOXON-MANN-WHITNEY ---

cat("\n=== TEST DE WILCOXON-MANN-WHITNEY ===\n")
cat("H0: Las medianas son iguales\n")
cat("Ha: Las medianas son diferentes\n\n")

wilcox_test <- wilcox.test(semilla_1_seg, semilla_5_seg, 
                           alternative = "two.sided",
                           conf.int = TRUE)

print(wilcox_test)

cat("\n--- ESTADÍSTICOS DESCRIPTIVOS ---\n")
cat("Mediana 1 semilla:", round(median(semilla_1_seg)/60, 2), "minutos\n")
cat("Mediana 5 semillas:", round(median(semilla_5_seg)/60, 2), "minutos\n")

cat("\n--- CONCLUSIÓN (α = 0.05) ---\n")
if(wilcox_test$p.value < 0.05) {
  cat("*** HAY DIFERENCIAS SIGNIFICATIVAS ***\n")
  cat("p-value =", format(wilcox_test$p.value, scientific = TRUE), "< 0.05\n")
} else {
  cat("*** NO hay diferencias significativas ***\n")
  cat("p-value =", round(wilcox_test$p.value, 4), "> 0.05\n")
}