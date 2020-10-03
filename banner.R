library(ggplot2)
library(patchwork)
library(stringr)

make_info_plot <- function(plot_data,
                           plot_question,
                           top_n_answers = NULL,
                           plot_title,
                           plot_type = c("bar", "pie"),
                           box_fill = "#B270A9",
                           text_color = "white",
                           wrap_length = 50) {
  if (is.numeric(plot_question)) {
    to_plot <- plot_data %>% 
      filter(question_number == plot_question)
  } else {
    to_plot <- plot_data %>% 
      filter(question == plot_question)
  }
  
  tallied <- to_plot %>% 
    group_by(answer) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    mutate(answer = stringr::str_wrap(answer,15))
  
  if (!is.null(top_n_answers)) {
    tallied <- tallied %>% 
      slice_head(n = top_n_answers)
  }
  
  col <-
    ggplot(tallied) +
    aes(x = answer,
        y = n,
        fill = answer) +
    geom_col(color = "gray") +
    # geom_text(aes(label = n),
    #           position = position_stack(vjust = .9)) +
    #scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    ggplot2::scale_fill_viridis_d(option = "A") +
    labs(title = stringr::str_wrap(plot_title, wrap_length)) +
    labs(x = "", y = "", fill = "") +
    theme_classic() +
    theme(panel.background = element_rect(fill = box_fill, color = box_fill)) +
    theme(plot.background = element_rect(fill = box_fill, color = box_fill)) +
    theme(legend.background = element_rect(fill = box_fill, color = box_fill)) +
    theme(plot.title = element_text(color = text_color)) +
    theme(axis.title = element_text(color = text_color)) +
    theme(axis.text = element_text(color = text_color)) +
    theme(axis.line = element_line(color = text_color)) +
    theme(axis.ticks = element_line(color = text_color)) +
    theme(legend.position = "top") +
    theme(legend.text = element_text(color = text_color))
  
  if (plot_type == "pie") {
    col <- col + coord_polar()
  }
  return(col)
}

make_info_box <-
  function(box_width = 5,
           box_height = 5,
           box_fill = "#B270A9",
           message,
           text_color = "white",
           text_size = 15,
           wrap_length = 30) {
    df <- data.frame(x = 1:10, y = 1:10)
    ggplot(data = df, aes(x = x, y = y)) +
      #scale_x_continuous(limits = c(0, box_width  + box_height * 0.1)) +
      #scale_y_continuous(limits = c(0, box_height + box_height * 0.1)) +
      # coord_fixed() +
      annotate(
        geom = "rect",
        xmin = 0,
        ymin = 0,
        xmax = box_width,
        ymax = box_height,
        fill = box_fill
      ) +
      annotate(
        geom = "text",
        x = box_width / 2,
        y = box_height / 2,
        label = str_wrap(message, wrap_length),
        size = text_size,
        colour = text_color
      ) +
      theme_void() +
      theme(panel.background = element_rect(fill = box_fill, color = box_fill)) +
      theme(plot.background = element_rect(fill = box_fill, color = box_fill)) 
  }

gg_banner <-
  function(banner_data,
           banner_question,
           banner_title,
           banner_message = "Само 11% се солидно запознаени со концептот на отворени податоци",
           plot_title = "Колку сте запознаени со концептот на отворени податоци",
           top_n_answers = NULL,
           banner_message_font_size  = 12,
           banner_message_font_color  = "white",
           wrap_length = 30,
           message_right = FALSE) {
    
    Info <- make_info_box(
      message = banner_message,
      box_width = 5,
      box_height = 4,
      text_color = banner_message_font_color,
      text_size = banner_message_font_size
    )
    
    Graph <- make_info_plot(
      plot_data = banner_data,
      plot_question = banner_question,
      plot_title = plot_title,
      plot_type = "bar",
      top_n_answers = top_n_answers,
      box_fill = "#B270A9",
      text_color = "white"
    )
    
    Logo <- ggplot2::ggplot() + geom_blank() +
      cowplot::draw_image("photos/banner-photo.png") +
      theme(panel.background = element_rect(fill = "#B270A9", color = "#B270A9")) +
      theme(plot.background = element_rect(fill = "#B270A9", color = "#B270A9")) +
      theme(legend.background = element_rect(fill = "#B270A9", color = "#B270A9"))
    
    layout <- "
    AACC
    AACC
    AACC
    BBCC
    "
    
    #these make the same arrangement... 
    # collage <- Info + Logo + Graph + plot_layout(design = layout)
    collage <- ((Info / Logo) + plot_layout(heights = c(0.7, 0.3)) | Graph)
    
    collage + plot_annotation(
      title = stringr::str_wrap(banner_title, 100),
      caption = "Отворени податоци | Слободен софтвер Македонија"
    ) +
      theme(plot.background = element_rect(fill="#B270A9"))
  }
