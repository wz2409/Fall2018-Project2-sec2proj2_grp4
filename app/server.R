library(shiny)
library(maps)
library(leaflet)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)

options(warn=-1)
#options(warn=0)

load("../data/workdata.Rdata")
load("../data/fulldata.Rdata")

shinyServer(function(input, output, session) {
  map = leaflet() %>%
    addTiles() %>%
    setView(lng = 360 - 95,
            lat = 40,
            zoom = 4)
  output$mymap = renderLeaflet(map)
  #Filter Data ----------------------------------------------------------------------------------------
  major <- reactive({
    major <- input$major
  })
  
  stp <- reactive({
    stp <- input$schtype
  })
  
  ct <- reactive({
    ct <- input$city
  })
  
  hd <- reactive({
    hd <- input$hdeg
  })
  
  st <- reactive({
    st <- input$location
  })
  
  cost <- reactive({
    cost <- input$cost
  })
  sat <- reactive({
    sat <- input$sat
  })
  act <- reactive({
    act <- input$act
  })
  
  
  d6 <- reactive({
    d6 <-
      filter(work.data,
             as.numeric(COSTT4_A) >= cost()[1] &
               as.numeric(COSTT4_A) <= cost()[2])
  })
  
  d7 <- reactive({
    d7 <- filter(d6(), as.numeric(SAT_AVG) < sat())
    
  })
  
  d8 <- reactive({
    d8 <- filter(d7(), as.numeric(ACTCMMID) < act())
  })
  
  
  d1 <- reactive({
    if (major() == "-----") {
      d1 <- d8()
    }
    else {
      d1 <- d8()[d8()[, major()] == 1, ]
    }
  })
  
  d2 <- reactive({
    if (stp() == "-----") {
      d2 <- d1()
    }
    else {
      d2 <- filter(d1(), CONTROL == stp())
    }
  })
  
  d3 <- reactive({
    if (ct() == "-----") {
      d3 <- d2()
    }
    else {
      d3 <- filter(d2(), LOCALE == ct())
    }
  })
  
  d4 <- reactive({
    if (hd() == "-----") {
      d4 <- d3()
    }
    else {
      d4 <- filter(d3(), HIGHDEG == hd())
    }
  })
  
  d5 <- reactive({
    if (st() == "-----") {
      d5 <- d4()
    }
    else {
      d5 <- filter(d4(), STABBR == st())
    }
  })
  
  
  # leaflet(data =d5())%>%
  #   addTiles()%>%
  #   addMarkers(~long, ~lat)
  
  output$mymap <- renderLeaflet({
    urls <-
      paste0(
        as.character("<b><a href='http://"),
        as.character(d5()$INSTURL),
        "'>",
        as.character(d5()$INSTNM),
        as.character("</a></b>")
      )
    content <- paste(sep = "<br/>",
                     urls,
                     paste("Rank:", as.character(d5()$Rank)))
    
    s = input$universities.table_rows_selected
    
    url2 <-
      paste0(
        as.character("<b><a href='http://"),
        as.character(d5()$INSTURL[s]),
        "'>",
        as.character(d5()$INSTNM[s]),
        as.character("</a></b>")
      )
    
    url3 <-
      paste0(
        as.character("<b><a href='http://"),
        as.character(d5()$INSTURL[s]),
        "'>",
        as.character(d5()$INSTURL[s]),
        as.character("</a></b>")
      )
    
    
    content2 <- paste(sep = "<br/>",
                      as.character(d5()$INSTNM[s]),
                      url3,
                      paste("Rank:", as.character(d5()$Rank[s])),
                      paste(sep = "",as.character(d5()$CITY[s]),
                            ", ",
                            as.character(d5()$STABBR[s]),as.character(d5()$ZIP[s])),
                      paste("Type:",as.character(d5()$CONTROL[s]))
                      )
    
    content3 <- paste(as.character(d5()$INSTNM[s]))
    
    CollegeIcon <- makeIcon(
      iconUrl = "https://icon-icons.com/icons2/510/PNG/512/university_icon-icons.com_49967.png",
      iconWidth = 25, iconHeight = 25,
      iconAnchorX = 12.5, iconAnchorY = 12.5
    )
    
    if (length(s)) {
      mapStates = map("state", fill = TRUE, plot = FALSE)
      leaflet(data = mapStates) %>% addTiles() %>%
        addCircleMarkers(
                  clusterOptions = markerClusterOptions(),
                  as.numeric(d5()$LONGITUDE[-s]),
                  as.numeric(d5()$LATITUDE[-s]),
                  popup = content) %>%
        addMarkers(
                  as.numeric(d5()$LONGITUDE[s]),
                  as.numeric(d5()$LATITUDE[s]),
                  icon = CollegeIcon,
                  popup = content2,
                  label = content3,
                  labelOptions = labelOptions(noHide = T,direction = "top",offset = c(0, -8), clickable=TRUE)
        )
      
    }
    
    else{
      mapStates = map("state", fill = TRUE, plot = FALSE)
      leaflet(data = mapStates) %>% addTiles() %>%
        addCircleMarkers(
                  clusterOptions = markerClusterOptions(),
                  as.numeric(d5()$LONGITUDE),
                  as.numeric(d5()$LATITUDE),
                  popup = content)
    }
    
  })
  
  
  
  #Table with University List --------------------------------------------------------------------------
  
  #Filtered data frame
  
  #Table output
  output$universities.table = DT::renderDataTable({
    work.data.table <-
      subset(
        d5(),
        select = c(
          "Rank",
          "INSTNM",
          "STABBR",
          "ADM_RATE",
          "ACTCMMID",
          "SAT_AVG",
          "TUITIONFEE_IN",
          "TUITIONFEE_OUT"
        )
      )
    
    colnames(work.data.table) <-
      c(
        "Forbes Rank",
        "Institution",
        "State",
        "Admission Rate",
        "ACT Mid Point",
        "Average SAT (admitted students)",
        "Tuition (in-state)",
        "Tuition (out-of-state)"
      )
    
    datatable(
      work.data.table,
      rownames = F,
      selection = "multiple",
      options = list(order = list(list(0, 'asc'), list(1, "asc")))
    )  %>%
      formatPercentage(c("Admission Rate"), digits = 0) %>%
      formatCurrency(c("Tuition (in-state)", "Tuition (out-of-state)"), digits = 0)
  }, server = T)
  
  
  
  #Introduction-------------------------------------------------------------------------------------------
  
 
  #Selected indices--------------------------------------------------------------------------------------
  
  output$table.summary1 = DT::renderDataTable({
    s = input$universities.table_rows_selected
    #s = input$universities.table_row_last_clicked
    if (length(s)) {
      sub <- d5()[s, ]
      n = length(s)
      
      university <- sub$INSTNM
      sub_1 <- filter(fulldata, INSTNM == university)
      
      sub_2 <- filter(fulldata, INSTNM == university)
      #sub_2[sub_2 == "NULL"] <- NA
      
      Basic <-
        c("Name",
          "Website",
          "City",
          "Highest Degree",
          "Type of Institution",
          "Location",
          "Male %",
          "Female %",
          "Average age of entry",
          "% of Undergraduates aged 25+",
          "Undergraduate students receiving federal loan %",
          "Median Debt: Students who have completed",
          "Median Debt: Students who have NOT completed",
          "Median Earnings: Students 10 years after entry",
          "Tuition in state",
          "Tuition out state")
      
      Info <-c()
      Info<-cbind(Info,Basic)
      for (i in 1:n){
        Institution<-c(sub[i,]$INSTNM ,
                       #sub[i,]$INSTURL,
                       paste0(
                         #as.character("<b><a href='http://"),
                         as.character(sub[i,]$INSTURL)
                         #"'>",
                         #as.character(sub[i,]$INSTNM),
                         #as.character("</a></b>")
                       ),
                       sub[i,]$CITY,
                       sub[i,]$HIGHDEG,
                       sub[i,]$CONTROL,
                       sub[i,]$LOCALE,
                       
                       as.numeric(sub_1[i,]$UGDS_MEN) * 100,
                       as.numeric(sub_1[i,]$UGDS_WOMEN) * 100,
                       round(as.numeric(sub_1[i,]$AGE_ENTRY), digits = 2),
                       as.numeric(sub_1[i,]$UG25ABV) * 100,
                       
                       round(as.numeric(sub_2[i,]$PCTFLOAN) * 100, 2),
                       round(as.numeric(sub_2[i,]$GRAD_DEBT_MDN), 0),
                       round(as.numeric(sub_2[i,]$WDRAW_DEBT_MDN), 0),
                       round(as.numeric(sub_2[i,]$MD_EARN_WNE_P10), 0),
                       as.numeric(sub_1[i,]$TUITIONFEE_IN),
                       as.numeric(sub_1[i,]$TUITIONFEE_OUT))
        Info<-cbind(Info,Institution)
      }
      as.data.frame(Info)
      Name<-Info[1,]
      table_result<-Info
      colnames(table_result)<-Name
      #my.summary <- data.frame(cbind(Institution, Info))
      #my.summary
      datatable(table_result[-1,])
      
    } else
      datatable(d5()[s, ])
  })
  
  
  
  
  output$table.summary2 = DT::renderDataTable({
    s = input$universities.table_rows_selected
    #s = input$universities.table_row_last_clicked
    if (length(s)) {
      sub <- d5()[s, ]
      n = length(s)
      
      university <- sub$INSTNM
      sub_1 <- filter(fulldata, INSTNM == university)
      
      sub_2 <- filter(fulldata, INSTNM == university)
      sub_2[sub_2 == "NULL"] <- NA
      
      Basic <-
        c("Name",
          "Website",
          "City",
          "Highest Degree",
          "Type of Institution",
          "Location",
          "Male %",
          "Female %",
          "Average age of entry",
          "% of Undergraduates aged 25+",
          
          "Undergraduate students receiving federal loan %",
          "Median Debt: Students who have completed",
          "Median Debt: Students who have NOT completed",
          "Median Earnings: Students 10 years after entry",
          "Tuition in state",
          "Tuition out state")
      
      Info <-c()
      Info<-cbind(Info,Basic)
      for (i in 1:n){
        Institution<-c(sub[i,]$INSTNM ,
                       sub[i,]$INSTURL,
                       sub[i,]$CITY,
                       sub[i,]$HIGHDEG,
                       sub[i,]$CONTROL,
                       sub[i,]$LOCALE,
                       
                       as.numeric(sub_1[i,]$UGDS_MEN) * 100,
                       as.numeric(sub_1[i,]$UGDS_WOMEN) * 100,
                       round(as.numeric(sub_1[i,]$AGE_ENTRY), digits = 2),
                       as.numeric(sub_1[i,]$UG25ABV) * 100,
                       
                       
                       round(as.numeric(sub_2[i,]$PCTFLOAN) * 100, 2),
                       round(as.numeric(sub_2[i,]$GRAD_DEBT_MDN), 0),
                       round(as.numeric(sub_2[i,]$WDRAW_DEBT_MDN), 0),
                       round(as.numeric(sub_2[i,]$MD_EARN_WNE_P10), 0),
                       as.numeric(sub_1[i,]$TUITIONFEE_IN),
                       as.numeric(sub_1[i,]$TUITIONFEE_OUT))
        Info<-cbind(Info,Institution)
      }
      #as.data.frame(Info)
      Name<-Info[1,]
      table_result<-Info
      colnames(table_result)<-Name
      #my.summary <- data.frame(cbind(Institution, Info))
      #my.summary
      #table_result
      
      
      
      datasetInput1<-reactive({
        switch(input$universities.table1,
               "Name" = cbind(Name[-1],table_result[1,-1]),
               "Website" = cbind(Name[-1],table_result[2,-1]),
               "City" = cbind(Name[-1],table_result[3,-1]),
               "Highest Degree" = cbind(Name[-1],table_result[4,-1]),
               "Type of Institution" = cbind(Name[-1],table_result[5,-1]),
               "Location" = cbind(Name[-1],table_result[6,-1]),
               "Male %" = cbind(Name[-1],table_result[7,-1]),
               "Female %" = cbind(Name[-1],table_result[8,-1]),
               "Average age of entry" = cbind(Name[-1],table_result[9,-1]),
               "% of Undergraduates aged 25+" = cbind(Name[-1],table_result[10,-1]),
               "Undergraduate students receiving federal loan %" = cbind(Name[-1],table_result[11,-1]),
               "Median Debt: Students who have completed" = cbind(Name[-1],table_result[12,-1]),
               "Median Debt: Students who have NOT completed" = cbind(Name[-1],table_result[13,-1]),
               "Median Earnings: Students 10 years after entry" = cbind(Name[-1],table_result[14,-1])
        )})
      
      dataset<-datasetInput1()
      colnames(dataset)<-c("Institution","Attribute")
      dataset
      
    } else
      datatable(d5()[s, ])
  })
  
  
  
  output$graph.summary3 = renderPlot({
    s = input$universities.table_rows_selected
    #s = input$universities.table_row_last_clicked
    if (length(s)) {
      sub <- d5()[s, ]
      n = length(s)
      
      university <- sub$INSTNM
      sub_1 <- filter(fulldata, INSTNM == university)
      
      sub_2 <- filter(fulldata, INSTNM == university)
      sub_2[sub_2 == "NULL"] <- NA
      
      Basic <-
        c("Name",
          "Website",
          "City",
          "Highest Degree",
          "Type of Institution",
          "Location",
          "Male %",
          "Female %",
          "Average age of entry",
          "% of Undergraduates aged 25+",
          
          "Undergraduate students receiving federal loan %",
          "Median Debt: Students who have completed",
          "Median Debt: Students who have NOT completed",
          "Median Earnings: Students 10 years after entry",
          "Tuition in state",
          "Tuition out state")
      
      Info <-c()
      Info<-cbind(Info,Basic)
      for (i in 1:n){
        Institution<-c(sub[i,]$INSTNM ,
                       sub[i,]$INSTURL,
                       sub[i,]$CITY,
                       sub[i,]$HIGHDEG,
                       sub[i,]$CONTROL,
                       sub[i,]$LOCALE,
                       
                       as.numeric(sub_1[i,]$UGDS_MEN) * 100,
                       as.numeric(sub_1[i,]$UGDS_WOMEN) * 100,
                       round(as.numeric(sub_1[i,]$AGE_ENTRY), digits = 2),
                       as.numeric(sub_1[i,]$UG25ABV) * 100,
                       
                       round(as.numeric(sub_2[i,]$PCTFLOAN) * 100, 2),
                       round(as.numeric(sub_2[i,]$GRAD_DEBT_MDN), 0),
                       round(as.numeric(sub_2[i,]$WDRAW_DEBT_MDN), 0),
                       round(as.numeric(sub_2[i,]$MD_EARN_WNE_P10), 0),
                       as.numeric(sub_1[i,]$TUITIONFEE_IN),
                       as.numeric(sub_1[i,]$TUITIONFEE_OUT))
        Info<-cbind(Info,Institution)
      }
      as.data.frame(Info)
      Name<-Info[1,]
      table_result<-Info
      colnames(table_result)<-Name
      
      
      
      datasetInput2<-reactive({
        switch(input$universities.table2,
               #"Name" = cbind(Name[-1],table_result[1,-1]),
               #"Website" = cbind(Name[-1],table_result[2,-1]),
               #"City" = cbind(Name[-1],table_result[3,-1]),
               #"Highest Degree" = cbind(Name[-1],table_result[4,-1]),
               #"Type of Institution" = cbind(Name[-1],table_result[5,-1]),
               #"Location" = cbind(Name[-1],table_result[6,-1]),
               "Male %" = cbind(Name[-1],table_result[7,-1]),
               "Female %" = cbind(Name[-1],table_result[8,-1]),
               "Average age of entry" = cbind(Name[-1],table_result[9,-1]),
               "% of Undergraduates aged 25+" = cbind(Name[-1],table_result[10,-1]),
               "Undergraduate students receiving federal loan %" = cbind(Name[-1],table_result[11,-1]),
               "Median Debt: Students who have completed" = cbind(Name[-1],table_result[12,-1]),
               "Median Debt: Students who have NOT completed" = cbind(Name[-1],table_result[13,-1]),
               "Median Earnings: Students 10 years after entry" = cbind(Name[-1],table_result[14,-1]),
               "Tuition in state" = cbind(Name[-1],table_result[15,-1]),
               "Tuition out state" = cbind(Name[-1],table_result[16,-1])
        )})
      
      dataset1<-datasetInput2()
      n<-nrow(dataset1)
      v1<-c(1:n)
      v2<-as.numeric(c(dataset1[,2]))
      #dataset2<-data.frame(dataset1)
      #colnames(dataset2)<-c("Institution","Attribute")
      
      #dataset2$Institution <-as.numeric(dataset2$Institution)
      #dataset2$Attribute <-as.numeric(dataset2$Attribute)
      plot(v1,v2,type = "h", xlab = "Instuitions", ylab = "Attribute")
      
    } else
      ggplot(ggplot() + ggtitle("Please select a university."))
    #datatable(d5()[s, ])
  })
  
  
  #------------------------------------------------------------------------------------------------------


  #output$Univeristy1 <- as.character(input$universities.table_rows_selected[1])
  #output$Univeristy2 <- as.character(input$universities.table_rows_selected[2])
 
  
  #Graphical Analysis
  
  output$ADM_1 <- renderPlotly({
    s = input$universities.table_rows_selected[1]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$ADM_RATE = as.numeric(edu$ADM_RATE)
      edu$Year = as.numeric(edu$Year)
      p <-
        ggplot(data = edu, aes(x = Year, y = ADM_RATE)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Admission Rate with Trending")
      ggplotly(p)
    }
    else {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  output$SAT_1 <- renderPlotly({
    s = input$universities.table_rows_selected[1]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$SAT_AVG = as.numeric(edu$SAT_AVG)
      edu$Year = as.numeric(edu$Year)
      b <-
        ggplot(data = edu, aes(x = Year, y = SAT_AVG)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Average SAT with Trending")
      ggplotly(b)
    }
    else  {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  
  output$ACT_1 <- renderPlotly({
    s = input$universities.table_rows_selected[1]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$ACTCMMID = as.numeric(edu$ACTCMMID)
      edu$Year = as.numeric(edu$Year)
      a <-
        ggplot(data = edu, aes(x = Year, y = ACTCMMID)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year ACT MID with Trending")
      ggplotly(a)
    }
    else  {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  output$FEM_1 <- renderPlotly({
    s = input$universities.table_rows_selected[1]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$UGDS_WOMEN = as.numeric(edu$UGDS_WOMEN)
      edu$Year = as.numeric(edu$Year)
      d <-
        ggplot(data = edu, aes(x = Year, y = UGDS_WOMEN)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Share of Female Undergrads with Trending")
      ggplotly(d)
    }
    else  {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  output$ENR_1 <- renderPlotly({
    s = input$universities.table_rows_selected[1]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$UGDS = as.numeric(edu$UGDS)
      edu$Year = as.numeric(edu$Year)
      e <-
        ggplot(data = edu, aes(x = Year, y = UGDS)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Enrollments with Trending")
      ggplotly(e)
    }
    else {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  output$ADM_2 <- renderPlotly({
    s = input$universities.table_rows_selected[2]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$ADM_RATE = as.numeric(edu$ADM_RATE)
      edu$Year = as.numeric(edu$Year)
      p <-
        ggplot(data = edu, aes(x = Year, y = ADM_RATE)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Admission Rate with Trending")
      ggplotly(p)
    }
    else {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  output$SAT_2 <- renderPlotly({
    s = input$universities.table_rows_selected[2]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$SAT_AVG = as.numeric(edu$SAT_AVG)
      edu$Year = as.numeric(edu$Year)
      b <-
        ggplot(data = edu, aes(x = Year, y = SAT_AVG)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Average SAT with Trending")
      ggplotly(b)
    }
    else  {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  
  output$ACT_2 <- renderPlotly({
    s = input$universities.table_rows_selected[2]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$ACTCMMID = as.numeric(edu$ACTCMMID)
      edu$Year = as.numeric(edu$Year)
      a <-
        ggplot(data = edu, aes(x = Year, y = ACTCMMID)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year ACT MID with Trending")
      ggplotly(a)
    }
    else  {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  output$FEM_2 <- renderPlotly({
    s = input$universities.table_rows_selected[2]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$UGDS_WOMEN = as.numeric(edu$UGDS_WOMEN)
      edu$Year = as.numeric(edu$Year)
      d <-
        ggplot(data = edu, aes(x = Year, y = UGDS_WOMEN)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Share of Female Undergrads with Trending")
      ggplotly(d)
    }
    else  {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
  
  output$ENR_2 <- renderPlotly({
    s = input$universities.table_rows_selected[2]
    if (length(s)) {
      university <- d5()$INSTNM[s]
      edu <- filter(fulldata, INSTNM == university)
      edu$UGDS = as.numeric(edu$UGDS)
      edu$Year = as.numeric(edu$Year)
      e <-
        ggplot(data = edu, aes(x = Year, y = UGDS)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("10-Year Enrollments with Trending")
      ggplotly(e)
    }
    else {
      ggplotly(ggplot() + ggtitle("Please select a university."))
    }
  })
 
  # observe({
  #  if(input$selectall == 0) return(NULL) 
  #  else if (input$selectall%%2 == 0)
  #  {
  #    updateCheckboxGroupInput(session,"universities.table", "Variables to show:",
  #                             c("SAT" = "SAT",
  #                               "ADM" = "ADM",
  #                               "FEM" = "FEM",
  #                               "ACT" = "ACT",
  #                               "ENR" = "ENR"))
  #  }
  #  else
  #  {
  #    updateCheckboxGroupInput(session,"universities.table", "Variables to show:",choices=c("SAT" = "SAT",
  #                                                                                          "ADM" = "ADM",
  #                                                                                          "FEM" = "FEM",
  #                                                                                          "ACT" = "ACT",
  #                                                                                          "ENR" = "ENR"),
  #                             selected=c("SAT" = "SAT",
  #                                        "ADM" = "ADM",
  #                                        "FEM" = "FEM",
  #                                        "ACT" = "ACT",
  #                                        "ENR" = "ENR"))
  #  }
  #})


})
  
  

