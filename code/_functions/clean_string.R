clean_string <- function(string) {
  #' @title limpeza e padronização de strings
  #' 
  #' @description realiza uma série de transformações para limpar e padronizar strings:
  #' 1. remove acentos e caracteres especiais
  #' 2. elimina pontuação e números
  #' 3. remove espaços extras
  #' 
  #' @param string vetor de caracteres a ser processado
  #' @return vetor de caracteres padronizado
  
  if (!requireNamespace('stringr', quietly = TRUE)) {
    stop('Pacote "stringr" necessário. Instale com install.packages("stringr")')
  }
  if (!requireNamespace('stringi', quietly = TRUE)) {
    stop('Pacote "stringi" necessário. Instale com install.packages("stringi")')
  }
  
  string |>
    # remoção de acentos e caracteres especiais com uso do "stringi"
    stringi::stri_trans_general(id = 'Latin-ASCII') |> 
    # remoção de pontuação e números
    stringr::str_replace_all(pattern = '[[:punct:]]', replacement = '') |> 
    stringr::str_replace_all(pattern = '[[:digit:]]', replacement = '') |> 
    # formatação p/ primeira letra maiúscula
    stringr::str_to_title() |>
    # remoção de espaços múltiplos/desnecessários
    stringr::str_squish()
}
