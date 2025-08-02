# Script para eliminar el parámetro DEFF del archivo RData
# Ruta al archivo RData
rdata_path <- "Parámetros generales/parametros.RData"

# Cargar el archivo RData en un nuevo entorno
env <- new.env()
load(rdata_path, envir = env)

# Mostrar los objetos que contiene
cat("Objetos en el archivo RData antes de modificar:\n")
print(ls(env))

# Verificar si existe el objeto DEFF
if("DEFF" %in% ls(env)) {
  # Eliminar el objeto DEFF
  rm("DEFF", envir = env)
  cat("\nEl parámetro DEFF ha sido eliminado.\n")
} else {
  cat("\nEl parámetro DEFF no existe en el archivo.\n")
}

# Guardar el entorno modificado de vuelta al archivo RData
save(list = ls(env), file = rdata_path, envir = env)

cat("\nObjetos en el archivo RData después de modificar:\n")
env2 <- new.env()
load(rdata_path, envir = env2)
print(ls(env2))

cat("\nProceso completado.\n")
