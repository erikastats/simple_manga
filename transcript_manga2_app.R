library(shiny)
library(shinyjs)
library(tidyverse)
library(reactable)
library(magick)
library(tesseract)
library(shinyWidgets)

url_image <- 'https://tenseikizokunoisekaiboukenroku.com/images/mwfjkInNneO0QH5SJRWX1611713201.jpg'
eng <- tesseract("eng")


# Function to clean oct transcription -------------------------------------

# test <-    'WANT
# YOUTO
# SOU•ONE
# FORE!!'
clean_oct <- function(text){
  text %>% 
    str_replace_all('\\n', ' ') %>% 
    str_replace_all('- ', '') 

}


# Function to crop the image based on selected coordinates

crop_image <- function(url_image, coords) {
  manga_page <- image_read(url_image)
  im_info <- image_info(manga_page)
  width <- as.integer(im_info$width)
  height <- as.integer(im_info$height)
  
  
  x1 <- coords$x1 * width
  y1 <- coords$y1 * height
  x2 <- coords$x2 * width
  y2 <- coords$y2 * height
  
  cropped_image <- image_crop(manga_page, 
                              geometry = sprintf("%dx%d+%d+%d", 
                                                 as.integer(x2 - x1), 
                                                 as.integer(y2 - y1), 
                                                 as.integer(x1), 
                                                 as.integer(y1)))
  return(cropped_image)
}

ui <- fluidPage(
  title = "Rectangular Image Selector Example",
  useShinyjs(),
  
  tags$style(HTML(".image-container { position: relative; overflow: hidden; } .selection-box { position: absolute; border: 2px dashed red; background-color: transparent; }")),
  
  fluidRow(
    column(6, div(
      class = "image-container",
      img(
        id = "image",
        src = url_image,
        width = "100%"
      ),
      div(id = "selection-box", class = "selection-box")
    )),
    column(6,
           fluidRow(
             h4("Selected Coordinates:"),
             textOutput("coords")
           ),
           fluidRow(
             column(6,
                    actionButton("enable_selection", "Enable Selection"),
                    imageOutput("cropped_image")
                    ),
             column(6, 
                    actionButton("save_selection", "Save Selection"),
                    actionButton('oct_image', "Perform transcription"),
                    textOutput('oct_result'),
                    prettySwitch(
                      inputId = "is_history",
                      label = "Is part of the history?"
                    ),
                    
                    radioGroupButtons(
                      inputId = "type_lines",
                      label = "Type of lines",
                      choices = c("Normal", 
                                  "Not history", "Flashback", "Thought", "Fight"),
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle", 
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o", 
                                    style = "color: steelblue"))
                    ),
                    # searchInput(
                    #   inputId = "insert_cha",
                    #   label = "Insert characters", 
                    #   placeholder = "New character",
                    #   btnSearch = icon("search"), 
                    #   btnReset = icon("remove"),
                    #   width = "100%"
                    # ),
                    textInputIcon(
                      inputId = "insert_cha",
                      label = "Insert characters",
                      icon = icon("circle-user")
                    ),
                    actionButton('add_char', 'Add', class = "btn-success"),
                    uiOutput('cha_list'), 
                    actionButton('add_line', 'Add line', class = "btn-success"),
                    tableOutput({'table_line'})
                    # verbatimTextOutput('result_selection')
                    )
           )
           
    )
  )
)

server <- function(input, output) {
  
  temp_dir <- tempdir()
  
  observeEvent(input$enable_selection, {
    jscode <- '
      var isSelecting = false;
      var startX, startY;
      var selectionBox = document.getElementById("selection-box");

      document.getElementById("image").onmousedown = function(e) {
        e.preventDefault(); // Prevent default drag behavior
        isSelecting = true;
        startX = e.clientX;
        startY = e.clientY;
        selectionBox.style.left = startX + "px";
        selectionBox.style.top = startY + "px";
        selectionBox.style.width = "0";
        selectionBox.style.height = "0";
        selectionBox.style.display = "block";
      }

      document.onmousemove = function(e) {
        if (isSelecting) {
          var width = e.clientX - startX;
          var height = e.clientY - startY;
          
          // Adjust for page scroll position
          var scrollX = window.scrollX || window.pageXOffset;
          var scrollY = window.scrollY || window.pageYOffset;
          
          var left = width < 0 ? startX + width + scrollX : startX + scrollX;
          var top = height < 0 ? startY + height + scrollY : startY + scrollY;

          selectionBox.style.left = left + "px";
          selectionBox.style.top = top + "px";
          selectionBox.style.width = Math.abs(width) + "px";
          selectionBox.style.height = Math.abs(height) + "px";
        }
      }

      document.onmouseup = function() {
        if (isSelecting) {
          isSelecting = false;
          var image = document.getElementById("image");
          var imageRect = image.getBoundingClientRect();
          var x1 = (startX - imageRect.left) / imageRect.width;
          var y1 = (startY - imageRect.top) / imageRect.height;
          var x2 = x1 + parseFloat(selectionBox.style.width) / imageRect.width;
          var y2 = y1 + parseFloat(selectionBox.style.height) / imageRect.height;
          Shiny.setInputValue("selected_coords", {x1: x1, y1: y1, x2: x2, y2: y2});
        }
      }
    '
    

    
    runjs(jscode)
  })
  
  output$coords <- renderPrint({
    input$selected_coords
  })
  
  data_coord <- reactiveValues(df = tibble(x1 = NA, y1 = NA, x2 = NA, y2 = NA))
  
  observeEvent(input$save_selection, {
    data_coord$df <- bind_rows(data_coord$df, data.frame(input$selected_coords))
    output$result_selection <- renderPrint({
      df <- input$selected_coords
      print(df)
      print(typeof(df))
    })
    
    
    
    # Display the cropped image
    
    cropped_image2 <- crop_image(url_image, input$selected_coords)
    cropped_image_path <- file.path(temp_dir, "cropped_image.png")
    magick::image_write(cropped_image2, path = cropped_image_path)
    
    output$cropped_image <- renderImage({
      list(src = cropped_image_path, width = "50%")
    }, deleteFile = FALSE)
  })
  
  # output$table_coord <- renderTable({
  #   data_coord$df
  # })
  
  ocr_result <- reactiveValues(ocr = NA)
  
  observeEvent(input$oct_image, {
    
    cropped_image_path <- file.path(temp_dir, "cropped_image.png")
    
    ocr_result$ocr <- ocr(cropped_image_path, engine = eng) %>% clean_oct()
    
    output$oct_result <-  renderText({
      ocr_result$ocr
    })
  })
  
  character_list <- reactiveValues(df = tibble(character = NA))
  
  
  observeEvent(input$add_char, {
    character_list$df <- bind_rows(character_list$df, tibble(character = input$insert_cha)) %>% 
      na.exclude()
    
    
  }) 
  
  output$cha_list <-  renderUI({
    prettyRadioButtons(
      inputId = "char_choise",
      label = "Choose character:", 
      choices = character_list$df$character,
      inline = TRUE, 
      status = "danger",
      fill = TRUE
    )
  })
  
  character_lines <- reactiveValues(
    df = tibble(Character = NA, Line = NA,	is_it_history = NA, 	line_type = NA)
)
  
  
  observeEvent(input$add_line, {
    character_lines$df <- bind_rows(character_lines$df,
                                    tibble(Character = as.character(input$char_choise) ,
                                           Line = ocr_result$ocr,
                                           is_it_history = input$is_history,
                                           line_type = input$type_lines)) %>%  
      na.exclude()
    
    
  }) 
  
  output$table_line <- renderTable({
    character_lines$df
  })
  
  
}

shinyApp(ui, server)
