#' @title Plot keystrokes on a keyboard
#' @description This function takes a character string and fills the keys on a plotted keyboard depending on the appearance of each character.
#' @param string character, a character string to visualize on a keyboard.
#' @param layout character, define the keyboard layout. Default is 'qwertz'.
#' @details The provided character string will be split into single upper-case characters and each character then gets matched to a given keyboard layout dataset. Besides the respective character or letter the keyboard layout dataset also includes the positions of each key on the keyboard. According to the number of the appearance of each character the single keys on the keyboard get filled.
#' @import ggplot2
#' @import magrittr
#' @return ggplot2 object
#' @examples
#' plot_keystrokes(string = "Hello! Isn't R 3.2.5 just great?", layout = "qwerty")
plot_keystrokes <- function(string, layout = "qwerty") {
  if (!is.character(string)) {
    stop("Not a valid character string.")
  }
  if (layout == "qwerty") {
    data(qwerty, envir = environment())
    map <- qwerty
    qwerty <- NULL
    greys <- c(1,2,3,4,5,6,7,8,9,20,21,33,34,61)
  } else {
    stop("No other keyboard layouts supported at the moment.")
  }
  chars <- string %>%
    toupper() %>%
    strsplit(split = "") %>%
    unlist()
  map$count <- sapply(chars, FUN = function(x) grepl(x, map$major, fixed= T)) %>%
    rowSums()
  map$count <- map$count + sapply(chars, FUN = function(x) grepl(x, map$minor, fixed= T)) %>%
    rowSums()
  keyboard <- ggplot(map, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = count)) +
    geom_rect(colour = "black", size = 1.2) +
    coord_equal() +
    annotate("text", x = map$major.x, y = map$major.y, label = map$major, size = 6) +
    annotate("text", x = map$minor.x, y = map$minor.y, label = map$minor, size = 5) +
    scale_fill_continuous(low = "white", high = "red") +
    geom_rect(data = map[greys,],
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "grey",
              colour = "black",
              size = 1.2) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.background = element_rect(colour = "black")) +
    labs(fill = "Count")

  return(keyboard)
}
