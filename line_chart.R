ylab_names <- c('procent', '', 'ilosc mieszkan', 'PLN', 'PLN')  # nazwy osi y

lines_chart <- function(df, n, name, min, max) {
  if (length(df) == 1){ # sprawdza poprawność danych 
    return(0)
  }

  df <- melt(df, id.vars='Rok', variable.name='Miasto')  # stopienie kolumn z warotściami w jedną kolumnę

  ggplot(df, aes(Rok, value)) +  # stworzenie wykresu
    geom_line(aes(colour = Miasto), size=1.3) +  # linie na wykresie
    geom_point(aes(colour = Miasto), size = 2) +  # kropki na wykresie
    # theme_ipsum_ps() +
    ggtitle(paste('', name)) +  # nazwa wykresu (zależna od wyświetlanej zmiennej)
    ylab(ylab_names[n]) +  # nazwa osi y (zależna od wyświetlanej zmiennej)
    xlab('rok') +  # nazwa osi x
    theme(legend.position="bottom") +  # legenda znajduje się pod wykresem
    scale_color_brewer(palette="Dark2") +  # gama kolorystyczna
    scale_x_continuous(breaks=c(2004:2019))  # oś x zawiera liczby naturalne
  }
