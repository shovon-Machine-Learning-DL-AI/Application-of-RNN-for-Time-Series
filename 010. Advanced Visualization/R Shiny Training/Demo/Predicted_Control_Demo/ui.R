# ui.R

shinyUI(fluidPage(theme = "sandstone.css",
  
                 
  pageWithSidebar(
  headerPanel(list(tags$head(tags$style('body {background-color:  Beige; }')),h3("Workplace Guidance Rep - Goal Planning Tool"),HTML('<height="400px" style="float:right"/>' ))),
  
  sidebarPanel(

    selectInput("KPI", label = h4("Select a Metric:"), 
                choices = unique(c('KPI 1 ','KPI 2 ','KPI 3 ','KPI 4 ','KPI 5 '))), h4("Predictors : "),

    conditionalPanel(
      condition <- ("input.tabs == 'seg1' && input.KPI == 'KPI 1 '"), 

      sliderInput("Slide1","Driver 1",
                  min=round(1,2),max=round(300,2), value = seg1$Driver_1[20]),  
      sliderInput("Slide2","Driver 2",
                  min=round(0.00,2),max=round(100.00,2), value = seg1$Driver_2[20]*100)

            ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg2' && input.KPI == 'KPI 1 '"), 

      sliderInput("Slide15","Driver 1",
                  min=round(100,2),max=round(1000,2), value = seg2$Driver_1[20]),  
      sliderInput("Slide16","Driver 2",
                  min=round(500,2),max=round(5000,2), value = seg2$Driver_3[20])  

      ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg3' && input.KPI == 'KPI 1 '"), 
      
      sliderInput("Slide27","Driver 1",
                  min=round(1000000,2),max=round(10000000,2), value = seg3$Driver_4[20]),  
      sliderInput("Slide28","Driver 2",
                  min=round(500,2),max=round(5000,2), value = seg3$Driver_5[20]),  
      sliderInput("Slide29","Driver 3",
                  min=round( 10000000,2),max=round(80000000,2), value =seg3$KPI_5[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg4' && input.KPI == 'KPI 1 '"), 
      
      sliderInput("Slide41","Driver 1",
                  min=round(15.00,2),max=round(50.00,2), value = seg4$Driver_6[20]*100),  
      sliderInput("Slide42","Driver 2",
                  min=round(500,2),max=round(5000,2), value = seg4$Driver_5[20]),  
      sliderInput("Slide43","Driver 3",
                  min=round(50,2),max=round(500,2), value =seg4$Driver_7[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg5' && input.KPI == 'KPI 1 '"), 
      
      sliderInput("Slide55","Driver 1",
                  min=round(200,2),max=round(2000,2), value = seg5$Driver_5[20]),  
      sliderInput("Slide56","Driver 2",
                  min=round(0.00,2),max=round(0.30,2), value = seg5$Driver_8[20]),  
      sliderInput("Slide57","Driver 3",
                  min=round( 20000000 ,2),max=round(200000000,2), value =seg5$KPI_4[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg1' && input.KPI == 'KPI 2 '"), 

      sliderInput("Slide3","Driver 1",
                  min=round(1000,2),max=round( 5000 ,2), value = seg1$Driver_9[20]),
      sliderInput("Slide4","Driver 2",
                  min=round(0.00,2),max=round(100.00,2), value = seg1$Driver_10[20]*100)

          ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg2' && input.KPI == 'KPI 2 '"), 

      sliderInput("Slide17","Driver 1",
                  min=round(500000,2),max=round(5000000,2), value = seg2$Driver_4[20]),
      sliderInput("Slide18","Driver 2",
                  min=round(50,2),max=round(500,2), value = seg2$Driver_7[20])

          ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg3' && input.KPI == 'KPI 2 '"), 
      
      sliderInput("Slide30","Driver 1",
                  min=round(100,2),max=round(1000,2), value = seg3$Driver_1[20]),
      sliderInput("Slide31","Driver 2",
                  min=round(20000,2),max=round(100000,2), value = seg3$Driver_11[20]),
      sliderInput("Slide32","Driver 3",
                  min=round(0.00,2),max=round(30.00,2), value = seg3$Driver_12[20]*100)
    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg4' && input.KPI == 'KPI 2 '"), 
      
      sliderInput("Slide44","Driver 1",
                  min=round(2000,2),max=round(20000,2), value = seg4$KPI_1[20]),
      sliderInput("Slide45","Driver 2",
                  min=round(50,2),max=round(500,2), value = seg4$Driver_13[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg5' && input.KPI == 'KPI 2 '"), 
      
      sliderInput("Slide58","Driver 1",
                  min=round( 15000000 ,2),max=round(150000000,2), value = seg5$KPI_5[20]),
      sliderInput("Slide59","Driver 2",
                  min=round( 10000000 ,2),max=round(80000000,2), value = seg5$Driver_4[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg1' && input.KPI == 'KPI 3 '"), 
      
      sliderInput("Slide5","Driver 1",
                  min=round(2000000,2),max=round(20000000,2), value = seg1$KPI_4[20]),  
      sliderInput("Slide6","Driver 2",
                  min=round(0.00,2),max=round(70.00,2), value = seg1$Driver_6[20]*100),  
      sliderInput("Slide7","Driver 3",
                  min=round(100000,2),max=round(1000000,2), value = seg1$Driver_4[20]),
      sliderInput("Slide8","Driver 4",
                  min=round( 100 ,2),max=round(2000,2), value = seg1$Driver_3[20])
      
    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg2' && input.KPI == 'KPI 3 '"), 
      
      sliderInput("Slide19","Driver 1",
                  min=round(100,2),max=round(1000,2), value = seg2$Driver_1[20]),  
      sliderInput("Slide20","Driver 2",
                  min=round( 500000,2),max=round(5000000,2), value = seg2$Driver_4[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg3' && input.KPI == 'KPI 3 '"), 
      
      sliderInput("Slide33","Driver 1",
                  min=round(0.00,2),max=round(100.00,2), value = seg3$Driver_10[20]*100),  
      sliderInput("Slide34","Driver 2",
                  min=round( 1000000 ,2),max=round(10000000,2), value = seg3$Driver_4[20]),  
      sliderInput("Slide35","Driver 3",
                  min=round( 10000000 ,2),max=round(80000000,2), value = seg3$KPI_5[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg4' && input.KPI == 'KPI 3 '"), 
      
      sliderInput("Slide46","Driver 1",
                  min=round(500,2),max=round(5000,2), value = seg4$Driver_5[20]),  
      sliderInput("Slide47","Driver 2",
                  min=round(0.00,2),max=round(0.40,2), value = seg4$Driver_8[20]),  
      sliderInput("Slide48","Driver 3",
                  min=round(50000000,2),max=round(200000000,2), value = seg4$KPI_4[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg5' && input.KPI == 'KPI 3 '"), 
      
      sliderInput("Slide60","Driver 1",
                  min=round(200,2),max=round(2000,2), value = seg5$Driver_5[20]),  
      sliderInput("Slide61","Driver 2",
                  min=round(0.00,2),max=round(0.30,2), value = seg5$Driver_8[20]),  
      sliderInput("Slide62","Driver 3",
                  min=round( 10000000 ,2),max=round(80000000,2), value = seg5$Driver_4[20]),
      sliderInput("Slide63","Driver 4",
                  min=round( 20000000 ,2),max=round(200000000,2), value = seg5$KPI_4[20])
    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg1' && input.KPI == 'KPI 4 '"), 
      
      sliderInput("Slide9","Driver 1",
                  min=round( 5000,2),max=round( 40000 ,2), value = seg1$Driver_11[20]),  
      sliderInput("Slide10","Driver 2",
                  min=round(0,2),max=round(200,2), value = seg1$Driver_13[20]),  
      sliderInput("Slide11","Driver 3",
                  min=round(40000,2),max=round(700000,2), value = seg1$Driver_4[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg2' && input.KPI == 'KPI 4 '"),
      
      sliderInput("Slide21","Driver 1", 
                  min=round(0.00,2),max=round(10.00,2), value = seg2$Driver_14[20]*100),  
      sliderInput("Slide22","Driver 2",
                  min=round(50,2),max=round(300,2), value = seg2$Driver_13[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg3' && input.KPI == 'KPI 4 '"),
      
      sliderInput("Slide36","Driver 1", 
                  min=round(100,2),max=round(1000,2), value = seg3$Driver_1[20]),  
      sliderInput("Slide37","Driver 2",
                  min=round( 10000000,2),max=round(100000000,2), value = seg3$KPI_2[20]),  
      sliderInput("Slide38","Driver 3",
                  min=round(0.00,2),max=round(0.60,2), value = seg3$Driver_15[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg4' && input.KPI == 'KPI 4 '"),
      
      sliderInput("Slide49","Driver 1", 
                  min=round(200,2),max=round(2000,2), value = seg4$KPI_3[20]),  
      sliderInput("Slide50","Driver 2",
                  min=round(500,2),max=round(4000,2), value = seg4$Driver_16[20]),  
      sliderInput("Slide51","Driver 3",
                  min=round(20.00,3),max=round(100.00,2), value = seg4$Driver_10[20]*100)

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg5' && input.KPI == 'KPI 4 '"),
      
      sliderInput("Slide64","Driver 1", 
                  min=round(300,2),max=round(1500,2), value = seg5$KPI_3[20]),  
      sliderInput("Slide65","Driver 2",
                  min=round(0.00,2),max=round(50.00,2), value = seg5$Driver_17[20]*100),  
      sliderInput("Slide66","Driver 3",
                  min=round(50000000,2),max=round(300000000,2), value = seg5$KPI_2[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg1' && input.KPI == 'KPI 5 '"),
      
      sliderInput("Slide12","Driver 1", 
                  min=round(100,2),max=round(2000,2), value = seg1$KPI_3[20]),  
      sliderInput("Slide13","Driver 2",
                  min=round(100,2),max=round(1000,2), value = seg1$Driver_18[20]),  
      sliderInput("Slide14","Driver 3",
                  min=round(2000,2),max=round(15000,2), value = seg1$Driver_19[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg2' && input.KPI == 'KPI 5 '"), 
      
      sliderInput("Slide23","Driver 1",
                  min=round(30.00,2),max=round(100.00,2), value = seg2$Driver_10[20]*100),  
      sliderInput("Slide24","Driver 2",
                  min=round( 10000000 ,2),max=round(80000000,2), value = seg2$KPI_2[20]),  
      sliderInput("Slide25","Driver 3",
                  min=round(50,2),max=round(500,2), value = seg2$Driver_7[20]),
      sliderInput("Slide26","Driver 4",
                  min=round(0.00,2),max=round(50.00,2), value = seg2$Driver_20[20]*100)
      
    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg3' && input.KPI == 'KPI 5 '"), 
      
      sliderInput("Slide39","Driver 1",
                  min=round(1000,2),max=round(10000,2), value = seg3$Driver_21[20]),  
      sliderInput("Slide40","Driver 2",
                  min=round(500,2),max=round(3000,2), value = seg3$Driver_18[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg4' && input.KPI == 'KPI 5 '"), 
      
      sliderInput("Slide52","Driver 1",
                  min=round(2000,2),max=round(20000,2), value = seg4$KPI_1[20]),  
      sliderInput("Slide53","Driver 2",
                  min=round( 10000000,2),max=round(80000000,2), value = seg4$Driver_4[20]),  
      sliderInput("Slide54","Driver 3",
                  min=round(0.00,2),max=round(0.50,2), value = seg4$Driver_15[20])

    ),
    conditionalPanel(
      condition <- ("input.tabs == 'seg5' && input.KPI == 'KPI 5 '"), 
      
      sliderInput("Slide67","Driver 1",
                  min=round(0.00,2),max=round(30.00,2), value = seg5$Driver_12[20]*100),  
      sliderInput("Slide68","Driver 2",
                  min=round(0.00,2),max=round(0.30,2), value = seg5$Driver_8[20])

    ),
    
    column(3, offset = 3,
    downloadButton("downloadData", "Export Data", class = "butt"), 
    tags$head(tags$style(".butt{background-color:DarkSlateGrey;} .butt{color: white;}"))), width = 3),
 
  mainPanel(
    img(src='myimage.png', height='40',width='250', align="right"),
    tabsetPanel(id = "tabs",
      tabPanel("G1 Segment 1",  value = 'seg1', htmlOutput("view1")), 
      tabPanel("G1 Segment 2",  value = 'seg2', htmlOutput("view2")),
      tabPanel("G1 Segment 3",  value = 'seg3', htmlOutput("view3")),
      tabPanel("G2 Segment 4",  value = 'seg4', htmlOutput("view4")),
      tabPanel("G2 Segment 5",  value = 'seg5', htmlOutput("view5"))
                )
            )
              ),
  
  # WHERE YOUR FOOTER GOES
  #  hr(),
  print(h3("- Mean Absolute Percent Error : the error between the Actual and Forecasted values during the training period (prior to the black line marker)"
           ,tags$br()
           ,"- Mean Absolute Percent Difference : the percentage difference between the simulated and original forecasts (post the black line marker)"
           ,tags$br()
           ,"- Metric calculation has a 180 days post period measurement window to be realised"
           ,tags$br()
           ,"- Actual values take 6 months to reach their stable values, hence, users should avoid comparing Actual vs Forecasted values for the most recent 6 months during the training period (prior to the black line marker)"
           ,style = "font-size : 9pt"))
  
)
)

