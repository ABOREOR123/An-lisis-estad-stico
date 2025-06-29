summary_statistics <- function(x) {
  stats <- c(
    Media = mean(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    Desviación = sd(x, na.rm = TRUE),
    Mínimo = min(x, na.rm = TRUE),
    Máximo = max(x, na.rm = TRUE)
  )
  return(stats)
}