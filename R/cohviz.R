# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   Reload Package:            'Ctrl + Shift + L'


#' token text into words
#' @export
tidy_text <- function(text_vector, n_gram = 1) {
  text_tokens <- text_vector %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  return(text_tokens)
}

#' Barchart of most common words
#' @export
most_common_words <- function(tokens, word_filter = 30) {
  most_common_words <- tokens %>% 
    count(word, sort = TRUE) %>%
    filter(n > word_filter) %>%
    mutate(word = reorder(word, n))
  
  return(ggplot(most_common_words, aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip())
}

#' Scatterplot of word frequencies and 
#' match between two texts
#' @export
compare_texts <- function(tidy_text_one, tidy_text_two) {
  frequency <- bind_rows(
    mutate(
      tidy_text_one, author = "author1"),
    mutate(
      tidy_text_two, author = "author2")
    ) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-n) %>%
    spread(author, proportion)
  
  
  scatter <- ggplot(frequency, aes(x = author1, y = author2)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = '#cccccc') +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)

  return(scatter)
}
