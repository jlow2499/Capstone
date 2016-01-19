five_gram <- reactive({
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Twitter"){
    twitter <- twitter_n5
  } else{
    twitter <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    blogs <- blogs_n5
  } else{
    blogs <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    news <- news_n5
  } else{
    news <- NULL
  }
  five_gram <- rbind(twitter,blogs,news) %>%
    arrange(desc(Freq))
  five_gram
})

four_gram <- reactive({
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Twitter"){
    twitter <- twitter_n4
  } else{
    twitter <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    blogs <- blogs_n4
  } else{
    blogs <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    news <- news_n4
  } else{
    news <- NULL
  }
  four_gram <- rbind(twitter,blogs,news) %>%
    arrange(desc(Freq))
  four_gram
})

three_gram <- reactive({
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Twitter"){
    twitter <- twitter_n3
  } else{
    twitter <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    blogs <- blogs_n3
  } else{
    blogs <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    news <- news_n3
  } else{
    news <- NULL
  }
  three_gram <- rbind(twitter,blogs,news) %>%
    arrange(desc(Freq))
  three_gram
})

two_gram <- reactive({
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Twitter"){
    twitter <- twitter_n2
  } else{
    twitter <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    blogs <- blogs_n2
  } else{
    blogs <- NULL
  }
  if(input$rmv[1]|input$rmv[2]|input$rmv[3]=="Blogs"){
    news <- news_n2
  } else{
    news <- NULL
  }
  two_gram <- rbind(twitter,blogs,news) %>%
    arrange(desc(Freq))
  two_gram
})
