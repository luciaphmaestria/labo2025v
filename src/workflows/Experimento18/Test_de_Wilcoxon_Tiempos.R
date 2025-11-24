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