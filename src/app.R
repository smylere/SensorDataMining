# 센서데이터 기반 척추관협착증 판단모델 구축
# shiny web app 구현 부분

library(shiny)
library(dplyr)
library(ggplot2)


source("data_scale.R")


ui = fluidPage(

  titlePanel("센서 데이터 기반 척추관 협착증 분석"),

  br(),

  sidebarLayout(

    sidebarPanel(

      width = 3,

      fileInput('file',
                'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv'),
                placeholder = "No file"),

      hr(),
      # textInput("date",
      #           "측정일",
      #           placeholder = "ex) 13/08/2017"),

      textInput('startTime',
                ' 측정시작시간',
                placeholder = "ex) 15:00:00"),

      textInput('endTime',
                ' 측정종료시간',
                placeholder = "ex) 16:00:00"),

      checkboxInput('fullTime',
                    '관측전체시간',
                    FALSE),

      hr(),
      numericInput('frequency_rate',
                '시계열 주기부여(초)',
                10),

      numericInput('select_column',
                '속성선택 (1 ~ 5)',
                5,
                min = 1,
                max = 5,
                step = 1),

      numericInput('paa_rate',
                   '패턴 스케일',
                   100),


      actionButton('calculate', '데이터 처리'),

      br()
      ),

    mainPanel(

      tabsetPanel(

        type = "tabs",

        tabPanel("Data Check",
                 br(),
                 tableOutput('dataCheck')),

        tabPanel("Decomposed Time Series Data",
                 br(),
                 plotOutput('decomposedOut')),

        tabPanel("Detected Walking Pattern",
                 br(),
                 # plotOutput('walkingPatternRaw'),
                 plotOutput('walkingPattern')),
                 # textOutput('saxPattern')),

        tabPanel("Re -Clustering Data",
                 br(),
                 plotOutput("reclustering")),

        tabPanel("Result",
                 br(),
                 # br("NORMAL STANDARD = 20%"),
                 textOutput('result'),
                 plotOutput('pieChart'),
                 # plotOutput('checkTime'),
                 plotOutput('checkTime2'),
                 plotOutput('sum_grp_data'),
                 plotOutput('cnt_grp_data'))
        )
      )
    )
  )

server = function(input, output) {

  filedata = reactive({

    infile = input$file
    if (is.null(infile)){return(NULL)}

    data = read.csv(infile$datapath,stringsAsFactors = FALSE)

    print(length(data[12,]))

    if (isTRUE(data[1,2])){return (data)}

    else {
        data = read.csv(infile$datapath,stringsAsFactors = FALSE,skip = 10)
        return (data)
    }

    })

    observeEvent(input$calculate,{
        input$date
        input$startTime
        input$endTime
        input$select_column
        input$frequency_rate
        input$paa_rate
    })

  subsetData = eventReactive(input$calculate,{

    start = which(filedata()[,2] == input$startTime)
    end = which(filedata()[,2] == input$endTime)

    filedata()[start:end,]

    })

  scale = eventReactive(input$calculate,{

    if(input$fullTime) {

      return(scale_decompose(filedata(),
                      as.integer(input$select_column),
                      as.integer(input$frequency_rate)))
    }

    else {

      return(scale_decompose(subsetData(),
                      as.integer(input$select_column),
                      as.integer(input$frequency_rate)))
    }
  })

  scale2 = eventReactive(input$calculate,{

      if(input$fullTime) {return(filedata())}
      else { return(subsetData()) }
  })


  output$dataCheck = renderTable({head(filedata(),10)})

  output$decomposedOut = renderPlot({
    plot(scale()$decomposed_data)
    })

  sax = eventReactive(input$calculate,{
    sax_process(scale())
  })

  # output$walkingPatternRaw = renderPlot({
  #   plot(sax()$rawdata,type = 'l',
  #        main = "센서 데이터 기반 최초 정지 시점 데이터 추출")
  #   })

  # output$walkingPattern = renderPlot({
  #   plot(sax()$paa,type = 'l',col='blue',
  #        main = "PAA 알고리즘 기반 패턴 추출 그래프")
  #   points(sax()$paa,pch=16,lwd=5,col='blue')
  #   })

  output$walkingPattern = renderPlot({
      plot(patternScale(scale2(),
                        as.integer(input$select_column),
                        input$paa_rate),

           type = 'l',
           col='blue',
           main = "PAA 알고리즘 기반 패턴 추출 그래프")

      points(patternScale(scale2(),
                          as.integer(input$select_column),
                          input$paa_rate),
             pch=16,lwd=5,col='blue')
  })



  # output$saxPattern = renderText({
  #   sax_output = as.character(sax()$sax)
  #   print (sax_output)
  #   })

  output$reclustering = renderPlot({
    plot(scale()$reclustering)
    })

  # output$result = renderText({
  #   scale()$result
  #   })

  output$pieChart = renderPlot({

        pie(summary(as.factor(scale()$reclustering_rawdata)),
        main = ("rate of wlk / nwk"))

        })

  output$checkTime2 = renderPlot({

      tmp = sax_process(scale())
      qplot(unique(as.vector(tmp)))
      # qplot(sax_process(scale())[,1],sax_process(scale())[,1])
  })


  # output$checkTime = renderPlot({
  #
  #   lengthData = length(scale()$decomposed_data$x)/60
  #
  #   barplot(lengthData,
  #           horiz = TRUE,
  #           width = 0.5,
  #           main = "보행 중단 시간 측정",
  #           xlab = "관측시간 (단위 : 분)",
  #           xlim = c(0,as.numeric(lengthData)),
  #           ylim = (c(0,1)),
  #           beside = TRUE)
  #
  #   lengthStop = length(sax()$rawdata)/60
  #
  #   abline(v= lengthStop,
  #          col = "red")
  #   # text(lengthStop,"test",col = "red")
  #   # text(lengthStop, "cutoff", col = "red",pos=2)
  #   })
  #
  # output$totalTime = renderText({
  #   lengthData = length(scale()$decomposed_data$x)/60
  #   sprintf("전체 관측 시간 : %i 분" , round(lengthData))
  # })
  #
  # output$stopTime = renderText({
  #   lengthStop = length(sax()$rawdata)/60
  #   sprintf("보행 중단 시점 : %i 분", round(lengthStop))
  # })

  # output$sum_grp_data  = renderPlot({
  #
  #     xlim = c(0,1.1*max(c(mean(sum_ctrl_grp_data),mean(sum_expr_grp_data))))
  #     barplot(c(mean(sum_ctrl_grp_data),mean(sum_expr_grp_data)),
  #             main=("환자군 / 대조군 관측시간 대비 평균 최초 정지시점(초)"),
  #             names=c(mean(sum_ctrl_grp_data),mean(sum_expr_grp_data)),
  #             col=c("orange","blue"),
  #             horiz = TRUE,
  #             xlim = xlim,
  #             legend=c("ctrl_grp","expr_grp"),
  #             width = 0.1)
  # })
  #
  # output$cnt_grp_data  = renderPlot({
  #
  #     xlim = c(0,1.5*max(c(mean(cnt_ctrl_grp_length),mean(cnt_expr_grp_length))))
  #     barplot(c(mean(cnt_ctrl_grp_length),mean(cnt_expr_grp_length)),
  #             main=("환자군 / 대조군 관측시간 대비 평균정지 횟수"),
  #             names=c(mean(cnt_ctrl_grp_length),mean(cnt_expr_grp_length)),
  #             col=c("orange","blue"),
  #             horiz = TRUE,
  #             xlim = xlim,
  #             legend=c("ctrl_grp","expr_grp"),
  #             width = 0.1)
  # })
  }

shinyApp(ui = ui, server = server)
