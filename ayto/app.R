

library(shiny)
library(bslib)
library(showtext)
library(thematic)
library(DT)
library(rebus)

my_theme2 <- bs_theme(
  #bg = "#002B36", fg = "#EEE8D5", 
  primary = "#6f42c1",
  secondary = "#ea39b8",
  bg = "#1a0933",
  fg = "#ffffff",
  version = 5,
  #bootswatch = "vapor",
  base_font = font_google("Comfortaa")
)

thematic_shiny(font = "auto")








#### UI ----

ui = fluidPage(
  theme = my_theme2,
  titlePanel(h1("Are You The One? - Interaktiv Lösung überprüfen", align = "center") ),
  p(),
  sidebarLayout(
    sidebarPanel(
      div(
      h6("In der Tabelle rechts siehst du, wie die Kandidat:innen in den jeweiligen Matching Nights zusammen saßen.
      Klicke die Zellen der Paare an, von denen du glaubst, dass sie ein Perfect Match bilden.
      Finde so heraus, ob du richtig liegst, oder wo ggf. Kollisionen existieren.
      Paare, von denen bereits bekannt ist, dass sie ein Perfect Match bilden, sind bereits vorselektiert.
      Paare, von denen hingegen bereits bekannt ist, dass sie ein No Match sind, können nicht selektiert werden.
      In der Tabelle unten findest du weitere Informationen, die du brauchst, um das AYTO Rätsel zu lösen."),
          h6("Hinweis: Du musst nicht alle zehn/elf Paare auswählen,
             sondern kannst auch nur einen Teil deiner Lösung ausprobieren
             und berechnen lassen, wie viele verschiedene Möglichkeiten es dafür gibt.
             Außerdem kannst du auch die Matching Night begrenzen."),
          selectInput("berechne_bis_nacht",
                      "Wähle aus, bis zu welcher Matching Night berechnet werden soll:",
                      choices = 1:ncol(night_lights),
                      selected = ncol(night_lights)),
          tableOutput("facts")
          
        
      )
    ),
    mainPanel(
      DT::DTOutput("ayto_tbl"),
      p(),
      conditionalPanel(
        condition = "input.ayto_tbl_cells_selected.length > 0",
        #condition = "output.couples_selected_print != 'Noch nichts ausgewählt.'",
        h3("Deine Auswahl:"),
        wellPanel(textOutput("couples_selected_print")),
        tableOutput("problems_with_selection")
        
      ),
      p(),
      fluidRow(
        column(4),
        column(4),
        column(4, actionButton("submitSelected", "Auswahl auswerten"))
        
      ),
      
      conditionalPanel(
        condition = "output.calculate_selection_sinnvoll != 'no'",
        h2("Mögliche Lösungen", align = "center"),
        DT::DTOutput("possible_combs")
      )
      
    )
    
  ),
  h2("Verlauf der Perfect-Match-Wahrscheinlichkeiten", align = "center"),
  fluidRow(
    column(2, 
           selectInput("selectGirl", "Kandidatin auswählen", choices = names(ayto_tbl)),
           selectInput("selectGraphType", "Graphen auswählen", choices = c("Faceted Line Graph", "Area Graph"))),
    column(10, plotly::plotlyOutput("girlGridPlot"))#,
    #column(5, plotOutput("girlGridArea"))
    
  )
  
  
  
  )


#### Server ----

server = function(input, output) {
  
  ### preparation ----
  
  ayto_tbl_reactive <- reactive({
    n <- 1 + as.numeric(input$berechne_bis_nacht)
    max_n <- ncol(night_lights)
    if(n - 1 == max_n){
      ayto_tbl
    } else {
      ayto_tbl %>%
        mutate(across(everything(), #ncol(ayto_tbl)
                      ~ str_remove(.x, or1(n:max_n) %R% ".*") %>%
                        str_remove(", " %R% END))) #%>%
      #mutate(across(1:ncol(ayto_tbl)), 
      #       ~ str_replace(., START %R% n %R% END, n))
    }
  })
  
  
  facts <- reactive({
    no_match <- no_matches %>% 
      complete(night = 1:9, fill = list(no_matches = "-")) %>%
      group_by(night) %>% 
      summarise(no_match = toString(no_matches)) 
    perfect_match <- perfect_matches %>% 
      complete(night = 1:9, fill = list(perfect_matches = "-")) %>%
      group_by(night) %>%
      summarise(perfect_match = toString(perfect_matches))
    night_lights %>% pivot_longer(everything(), 
                                  names_to = "night", 
                                  values_to = "lights", 
                                  names_transform = list(night = as.numeric)) %>%
      full_join(no_match, by = "night") %>%
      full_join(perfect_match, by = "night") %>%
      filter(night <= as.numeric(input$berechne_bis_nacht))
  })
  
  output$facts <- renderTable({
    df <- facts() %>%
      mutate(night = as.integer(night), lights = as.integer(lights))
    names(df) = c("Matching Night", "Lichter", "No Match", "Perfect Match")
    df
  })
  
  considered_no_matches <- reactive(
    facts() %>%
      filter(no_match != "-") %>%
      pull(no_match)
  )
  
  considered_perfect_matches <- reactive(
    facts() %>%
      filter(perfect_match != "-") %>%
      pull(perfect_match)
  )
  
  
  
  
  output$ayto_tbl = DT::renderDT(ayto_tbl_reactive(), 
                                selection = list(target = 'cell', 
                                                 selected = match_to_matrix(considered_perfect_matches()), 
                                                 selectable = selectable_cells(
                                                   match_to_matrix(considered_no_matches())
                                                 )),
                                class = 'cell-border stripe', 
                                options = list(searching = FALSE,
                                               info = FALSE,
                                               bPaginate = FALSE)
  )
  
  
                                
  
  output$celltext <- renderPrint(
    if(nrow(input$ayto_tbl_cells_selected) > 0 ){
      input$ayto_tbl_cells_selected
    } else {
      ""
    }
  )
  
  boys_selected <- reactive({
    if(nrow(input$ayto_tbl_cells_selected) > 0 ){
      rownames(ayto_tbl)[input$ayto_tbl_cells_selected[,1]]
    } else {
      ""
    }
  })
  
  girls_selected <- reactive({
    if(nrow(input$ayto_tbl_cells_selected) > 0 ){
      names(ayto_tbl)[input$ayto_tbl_cells_selected[,2]]
    } else {
      ""
    }
  })
  
  couples_selected <- reactive({
    if(nrow(input$ayto_tbl_cells_selected) > 0 ){
      # boys_selected <- rownames(ayto_tbl)[input$ayto_tbl_cells_selected[,1]]
      # girls_selected <- names(ayto_tbl)[input$ayto_tbl_cells_selected[,2]]
      paste(girls_selected(), boys_selected(), sep = "+") %>%
        sort() %>%
        paste(collapse = ", ")
    } else {
      ""
    }
  })
  
  output$couples_selected_print <- renderText({
    couples <- couples_selected()
    if(couples != ""){
      couples
    } else {
      "Noch nichts ausgewählt."
    }
  })
  
  
  
  ### calculate solution ----
  
  
  check_selection_ok <- reactive({
    if(couples_selected() != ""){
      df <- check_selection(as.numeric(input$berechne_bis_nacht), couples_selected(), no_matches)
    } else {
      df <- tibble(Problem = "Nichts ausgewählt.")
    }
  })
  
  output$calculate_selection_sinnvoll <- renderText({
    df <- check_selection_ok()
    sinnvoll <- "yes"
    if(nrow(df) > 0 ){
      sinnvoll <- "no"
    }
    sinnvoll
  })
  
  
  
  output$problems_with_selection <- renderTable({
    df <- check_selection_ok()
    if(ncol(df) != 1){
      df <- df %>%
        mutate(max = as.integer(max), zu_viele = as.integer(zu_viele)) %>%
        select(matching_night, max, collision_with, zu_viele)
      names(df) = c("Matching Night", "Lichter", "Problem mit", "Zu viele")
    }
    if(nrow(df) > 0){
      df
    } else {
      tibble()
    } 
  })
    
  
  calculate_solution <- eventReactive(input$submitSelected, {
    girls <- girls_selected()
    boys <- boys_selected()
    #if((length(girls) > 1 & length(boys) > 1) |
    #   (girls != "" & boys != "") 
    if(girls[1] != "") {
      df <- combs_dfs_list[[as.numeric(input$berechne_bis_nacht) - 1]]
      for (i in 1:length(girls)) {
        df <- df %>% filter(.data[[girls[[i]]]] == boys[[i]])
      }
      df <- df %>% select(names(ayto_tbl))
      DT::datatable(df, 
                    filter = "top", 
                    class = 'cell-border stripe', 
                    options = list(searching = TRUE, 
                                   pagingType = "numbers",
                                   language = list(info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen",
                                                   emptyTable = "Keine Daten vorhanden. Deine Lösung scheint leider nicht möglich zu sein.", 
                                                   infoEmpty = "Zeige 0 bis 0 von 0 Einträgen",
                                                   zeroRecords = "Keine passenden Einträge gefunden",
                                                   lengthMenu = "Zeige _MENU_ Einträge",
                                                   infoFiltered = "(von ursprünglich ungefilterten _MAX_ Einträgen)",
                                                   search = "Suchen:"
                                                   # paginate = list(
                                                   #   next = "Nächste",
                                                   #   previous = "Vorherige"
                                                   #   )
                                   )
                    ),
                    selection = "none"
      )
    }
  })
  
  
  
  
  output$possible_combs = DT::renderDT({
    calculate_solution()
  })
  
  # output$calculated_solution <- renderTable({
  #   df <- calculate_solution()
  #   if(ncol(df) != 1){
  #     df <- df %>%
  #       mutate(max = as.integer(max), zu_viele = as.integer(zu_viele)) %>%
  #       select(matching_night, max, collision_with, zu_viele)
  #     names(df) = c("Matching Night", "Lichter", "Problem mit", "Zu viele")
  #   }
  #   if(nrow(df) > 0){
  #     df
  #   } else {
  #     tibble()
  #   } 
  # })
  
  #### plots ----
  
  output$girlGridPlot <- plotly::renderPlotly({
    girl <- input$selectGirl
    if(input$selectGraphType == "Area Graph"){
      plot_match_proportions_area(girl, "girl", summarized_table)
    } else {
      plot_match_proportions_grid(girl, "girl", summarized_table)
    }
  })
  
  # output$girlGridArea <- renderPlot({
  #   plot_match_proportions_area(input$selectGirl, "girl", summarized_table)
  # })
  
}


shinyApp(ui = ui, server = server)