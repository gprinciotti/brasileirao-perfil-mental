clip <- function(x, min = -Inf, max = -Inf) {
  #' @title limitação de valores numéricos
  #' 
  #' @description realiza a limitação (clipping) de valores numéricos dentro de um intervalo:
  #' 1. valores abaixo do mínimo são elevados ao mínimo
  #' 2. valores acima do máximo são reduzidos ao máximo
  #' 3. valores dentro do intervalo permanecem inalterados
  #' 
  #' @param x vetor numérico a ser processado
  #' @param min limite inferior do intervalo (valor padrão: -Inf)
  #' @param max limite superior do intervalo (valor padrão: Inf)
  #' @return vetor numérico com valores limitados ao intervalo [min, max]
  
  # verificação de inputs
  if(!is.numeric(x)) stop('x deve ser numérico')
  if(!is.numeric(min) || !is.numeric(max)) stop('min/max deve ser numérico')
  if(min > max) stop('min deve ser menor ou igual a max')
  
  # aplicação do clipping
  pmax(min, pmin(x, max))
}
