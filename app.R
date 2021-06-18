# install.packages('ggplot2')  # służy do tworzenia wykrsów
# install.packages('readxl')  # służy do otwierania pliku xlsx
# install.packages('reshape2')  # została wykorzystana funkcja melt
# install.packages('hrbrthemes')

library(ggplot2)
library(readxl)
library(reshape2)
library(shinythemes)
# library(hrbrthemes)

source("line_chart.R")  # zimportowanie funkcji

list_of_frames <- c()  # wektor tabeli

for (i in c(1,2,3,4,5)) {
  list_of_frames[[i]] <- read_excel("dane.xlsx", sheet=i, col_names=TRUE)  # utworzenie listy zawierającej tebele
}
  
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Ekonomia polskich miast"),  # tytuł
  
  sidebarLayout(
    sidebarPanel(
      selectInput("data", 
                  label = "Wybierz dane, które chcesz nanieść na wykres:",
                  choices = c("Bezrobocie rejestrowane", 
                              # "Średnia cena mieszkania", 
                              "Nowe mieszkania",
                              "Wydatki na jednego mieszkańca", 
                              "Średnie miesięczne wynagrodzenie"),
                  selected = "Bezrobocie rejestrowane"),  # wybór danych
      sliderInput("years", 
                  label = "Wybierz zakres lat:",
                  min = 2004, max = 2019, value = c(2004, 2019),
                  sep = ''),  # wybór zakresu lat
      checkboxGroupInput('cities',
                         label = "Wybierz miasta:",
                         choices = c("Poznań" = 8, 
                                     "Warszawa" = 6, 
                                     "Kraków" = 5, 
                                     "Wrocław" = 3, 
                                     "Łódź" = 4,
                                     "Gdańsk" = 7),
                         selected = c(8,6))  # wybór miast
      ),
    
    mainPanel(plotOutput("chart"), p(em('Źródło: GUS'), align='right'))  # wyświetlenie wykresu u źródła
  )
  )


server <- function(input, output) {
  output$chart <- renderPlot({
    n <- switch(input$data, 
                "Bezrobocie rejestrowane" = 1, 
                # "Średnia cena mieszkania" = 2, 
                "Nowe mieszkania" = 3,
                "Wydatki na jednego mieszkańca" = 4, 
                "Średnie miesięczne wynagrodzenie" = 5)  # przekazanie wyboru danych w postaci liczby
    
    data <- list_of_frames[[n]]  # wybranie n-tej tabelki z listy
    
    data <- data[data$Rok <= input$years[2] & input$years[1] <= data$Rok,]  # wybranie zakresu lat
    
    if (n == 3){
      data <- data.frame(data[1], data[as.integer(input$cities)])
    }
    else
      data <- data.frame(data[1], data[2], data[as.integer(input$cities)])  # 'nowe mieszkania' nie posiadają wartości dla Polski

    lines_chart(data, n, input$data, c(input$years[1], input$years[2]))  # wywołanie funkcji rysującej wykres
    })
}

shinyApp(ui, server)  # uruchomienie aplikacji
