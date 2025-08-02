corregir_nombres_loc <- function(tph) {
  corrected <- ifelse(
    grepl("(?i)Pz[A-Za-z0-9]+-", tph),
    # Caso Pz: Extraer el código después de Pz
    stringr::str_match(tph, "(?i)Pz([A-Za-z0-9]+)-")[, 2],
    ifelse(
      grepl("(?i)MNF-[A-Za-z0-9-]+-[A-Za-z0-9]+$", tph),
      # Caso MNF: Extraer todo el código después de MNF- y antes del último guion, eliminar guiones
      gsub("-", "", stringr::str_match(tph, "(?i)(MNF-[A-Za-z0-9-]+)-[A-Za-z0-9]+$")[, 2]),
      ifelse(
        grepl("(?i)MC[0-9]+-[0-9]+-", tph),
        # Caso MC: Extraer todo el código MC con guiones intermedios y eliminarlos
        gsub("-", "", stringr::str_match(tph, "(?i)(MC[0-9]+-[0-9]+)-")[, 2]),
        ifelse(
          grepl("(?i)(MN|MC)-[A-Za-z0-9-]+-", tph),
          # Caso MN y MC: Extraer todo el código y eliminar guiones
          gsub("-", "", stringr::str_match(tph, "(?i)(MN|MC)-([A-Za-z0-9-]+)-")[, 2]),
          # Otros casos: Mantener el comportamiento original
          stringr::str_match(tph, ",.*?,([A-Za-z0-9]+)-")[, 2]
        )
      )
    )
  )
  return(corrected)
}