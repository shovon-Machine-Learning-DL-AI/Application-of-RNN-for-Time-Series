# Contributed by Joe Cheng, February 2013
# Requires googleVis version 0.4.0 and shiny 0.4.0 or higher
# server.R
library(googleVis)
library(shiny)

shinyServer(function(input, output) {
  datasetInputSeg1  <- reactive({Predicted_Control_Seg1(input$KPI,input$Slide1,input$Slide2,input$Slide3,input$Slide4,input$Slide5,input$Slide6,input$Slide7,input$Slide8,input$Slide9,input$Slide10,input$Slide11,input$Slide12,input$Slide13,input$Slide14)  
                            })
  datasetInputSeg2  <- reactive({Predicted_Control_Seg2(input$KPI,input$Slide15,input$Slide16,input$Slide17,input$Slide18,input$Slide19,input$Slide20,input$Slide21,input$Slide22,input$Slide23,input$Slide24,input$Slide25,input$Slide26)  
                            })
  datasetInputSeg3  <- reactive({Predicted_Control_Seg3(input$KPI,input$Slide27,input$Slide28,input$Slide29,input$Slide30,input$Slide31,input$Slide32,input$Slide33,input$Slide34,input$Slide35,input$Slide36,input$Slide37,input$Slide38,input$Slide39,input$Slide40)  
                            })
  datasetInputSeg4  <- reactive({Predicted_Control_Seg4(input$KPI,input$Slide41,input$Slide42,input$Slide43,input$Slide44,input$Slide45,input$Slide46,input$Slide47,input$Slide48,input$Slide49,input$Slide50,input$Slide51,input$Slide52,input$Slide53,input$Slide54)  
                            })
  datasetInputSeg5  <- reactive({Predicted_Control_Seg5(input$KPI,input$Slide55,input$Slide56,input$Slide57,input$Slide58,input$Slide59,input$Slide60,input$Slide61,input$Slide62,input$Slide63,input$Slide64,input$Slide65,input$Slide66,input$Slide67,input$Slide68)  
                            })
  
  output$view1      <- renderGvis({
    gvisMerge(
      gvisTable(unique(datasetInputSeg1()[,c("Mean_Absolute_Percent_Error","Mean_Absolute_Percent_Difference")]), options = list(width=950, height = 80)), 
      gvisComboChart(datasetInputSeg1(), "Time", c("Strline1", "Actual", "Original_Forecast", "Simulated_Forecast"),
                   options=list(seriesType="line",#title="Metric Forecast", 
                                backgroundColor="White",
                                titleTextStyle="{color:'#2F4F4F', fontName:'Courier', fontSize:20}", 
                                series="[{targetAxisIndex:0, type:'bars', visibleInLegend: 'false'}]", 
                                hAxes="[{direction:1, slantedText:true, slantedTextAngle:60}]", 
                                vAxes="[{title:'Actual/Predicted value of Metric'}]",
                                height=420, width=950, bar="{groupWidth:'5%'}",
                                chartArea= "{width: '60%', height: '70%'}",
                                colors="['DarkSlateGray', 'lightskyblue', 'green', 'firebrick']",#lineDashStyle="[[4,4],[4,4],[4,4],[2, 2, 40, 2, 40, 2]]",#lineWidth="[1, 10, 10]",
                                legend ="{position: 'right'}")),
    tableOptions = "cellspacing=\"20\" border=\"0\" bgcolor=\"#AABBCC\"", horizontal=FALSE
                                )})
  
  output$view2      <- renderGvis({
    gvisMerge(
      gvisTable(unique(datasetInputSeg2()[,c("Mean_Absolute_Percent_Error","Mean_Absolute_Percent_Difference")]), options = list(width=950, height = 80)), 
      gvisComboChart(datasetInputSeg2(), "Time", c("Strline1", "Actual", "Original_Forecast", "Simulated_Forecast"),
                   options=list(seriesType="line",#title="Metric Forecast", 
                                backgroundColor="White",
                                titleTextStyle="{color:'#2F4F4F', fontName:'Courier', fontSize:20}", 
                                series="[{targetAxisIndex:0, type:'bars', visibleInLegend: 'false'}]", 
                                hAxes="[{direction:1, slantedText:true, slantedTextAngle:60}]", 
                                vAxes="[{title:'Actual/Predicted value of Metric'}]",
                                height=420, width=950, bar="{groupWidth:'5%'}",
                                chartArea= "{width: '60%', height: '70%'}",
                                colors="['DarkSlateGray', 'lightskyblue', 'forestgreen', 'firebrick']",#lineDashStyle="[[2, 2, 20, 2, 20, 2]]",#lineWidth="[1, 10, 10]",
                                legend ="{position: 'right'}")),
      horizontal=FALSE
    )})
  

  output$view3      <- renderGvis({
    gvisMerge(
      gvisTable(unique(datasetInputSeg3()[,c("Mean_Absolute_Percent_Error","Mean_Absolute_Percent_Difference")]), options = list(width=950, height = 80)), 
      gvisComboChart(datasetInputSeg3(), "Time", c("Strline1", "Actual", "Original_Forecast", "Simulated_Forecast"),
                   options=list(seriesType="line",#title="Metric Forecast", 
                                backgroundColor="White",
                                titleTextStyle="{color:'#2F4F4F', fontName:'Courier', fontSize:20}", 
                                series="[{targetAxisIndex:0, type:'bars', visibleInLegend: 'false'}]", 
                                hAxes="[{direction:1, slantedText:true, slantedTextAngle:60}]", 
                                vAxes="[{title:'Actual/Predicted value of Metric'}]",
                                height=420, width=950, bar="{groupWidth:'5%'}",
                                chartArea= "{width: '60%', height: '70%'}",
                                colors="['DarkSlateGray', 'lightskyblue', 'forestgreen', 'firebrick']",#lineDashStyle="[[2, 2, 20, 2, 20, 2]]",#lineWidth="[1, 10, 10]",
                                legend ="{position: 'right'}")),
      horizontal=FALSE
    )})
  
  
  output$view4      <- renderGvis({
    gvisMerge(
      gvisTable(unique(datasetInputSeg4()[,c("Mean_Absolute_Percent_Error","Mean_Absolute_Percent_Difference")]), options = list(width=950, height = 80)), 
      gvisComboChart(datasetInputSeg4(), "Time", c("Strline1", "Actual", "Original_Forecast", "Simulated_Forecast"),
                   options=list(seriesType="line",#title="Metric Forecast", 
                                backgroundColor="White",
                                titleTextStyle="{color:'#2F4F4F', fontName:'Courier', fontSize:20}", 
                                series="[{targetAxisIndex:0, type:'bars', visibleInLegend: 'false'}]", 
                                hAxes="[{direction:1, slantedText:true, slantedTextAngle:60}]", 
                                vAxes="[{title:'Actual/Predicted value of Metric'}]",
                                height=420, width=950, bar="{groupWidth:'5%'}",
                                chartArea= "{width: '60%', height: '70%'}",
                                colors="['DarkSlateGray', 'lightskyblue', 'forestgreen', 'firebrick']",#lineDashStyle="[[2, 2, 20, 2, 20, 2]]",#lineWidth="[1, 10, 10]",
                                legend ="{position: 'right'}")),
      horizontal=FALSE
    )})
  

  output$view5      <- renderGvis({
    gvisMerge(
      gvisTable(unique(datasetInputSeg5()[,c("Mean_Absolute_Percent_Error","Mean_Absolute_Percent_Difference")]), options = list(width=950, height = 80)), 
      gvisComboChart(datasetInputSeg5(), "Time", c("Strline1", "Actual", "Original_Forecast", "Simulated_Forecast"),
                   options=list(seriesType="line",#title="Metric Forecast", 
                                backgroundColor="White",
                                titleTextStyle="{color:'#2F4F4F', fontName:'Courier', fontSize:20}", 
                                series="[{targetAxisIndex:0, type:'bars', visibleInLegend: 'false'}]", 
                                hAxes="[{direction:1, slantedText:true, slantedTextAngle:60}]", 
                                vAxes="[{title:'Actual/Predicted value of Metric'}]",
                                height=420, width=950, bar="{groupWidth:'5%'}",
                                chartArea= "{width: '60%', height: '70%'}",
                                colors="['DarkSlateGray', 'lightskyblue', 'forestgreen', 'firebrick']",#lineDashStyle="[[4,4],[4,4],[4,4],[2, 2, 40, 2, 40, 2]]",#lineWidth="[1, 10, 10]",
                                legend ="{position: 'right'}")),
      horizontal=FALSE
    )})
  
    
    output$downloadData <- downloadHandler(
      filename = function() { paste('data', '.csv', sep='') },
      content = function(file) {
        if (input$tabs == 'seg1') {
        write.csv(datasetInputSeg1(), file)
        } else if (input$tabs == 'seg2'){
        write.csv(datasetInputSeg2(), file)
        } else if (input$tabs == 'seg3'){
          write.csv(datasetInputSeg3(), file)
        } else if (input$tabs == 'seg4'){
          write.csv(datasetInputSeg4(), file)
        } else if (input$tabs == 'seg5'){
          write.csv(datasetInputSeg5(), file)
        }
      }
                                            )
  
    }
            )


