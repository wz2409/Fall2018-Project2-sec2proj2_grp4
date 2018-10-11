library(shiny)
library(maps)
library(leaflet)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)

options(warn=-1)
#loads data
load("../data/workdata.Rdata")
load("../data/fulldata.Rdata")

#Initialize Shiny
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
  # Map tab functionality
  
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
  
 
  #Selected indices--------------------------------------------------------------------------------------
  
  output$table.summary1 = DT::renderDataTable({
    s = input$universities.table_rows_selected

    if (length(s)) {
      sub <- d5()[s, ]
      n = length(s)
      
      university <- sub$INSTNM
      sub_1 <- filter(fulldata, INSTNM == university)
      
      sub_2 <- filter(fulldata, INSTNM == university)

      
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
                       paste0(
                         as.character(sub[i,]$INSTURL)

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

      datatable(table_result[-1,],options = list(paging = FALSE,ordering = FALSE,searching = FALSE))
      
    } else
      datatable(d5()[s, ],options = list(paging = FALSE,ordering = FALSE,searching = FALSE))
  })
  
  
  output$table.summary2 = DT::renderDataTable({
    s = input$universities.table_rows_selected

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
      Name<-Info[1,]
      table_result<-Info
      colnames(table_result)<-Name

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
      datatable(dataset,options = list(paging = FALSE, searching = FALSE,ordering = FALSE))
      
    } else
      datatable(d5()[s, ],options = list(paging = FALSE, searching = FALSE,ordering = FALSE))
  })
  
  
  #summary graphs
  output$graph.summary3 = renderPlot({
    s = input$universities.table_rows_selected

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
               "Male %" = cbind(Name,table_result[7,]),
               "Female %" = cbind(Name,table_result[8,]),
               "Average age of entry" = cbind(Name,table_result[9,]),
               "% of Undergraduates aged 25+" = cbind(Name,table_result[10,]),
               "Undergraduate students receiving federal loan %" = cbind(Name,table_result[11,]),
               "Median Debt: Students who have completed" = cbind(Name,table_result[12,]),
               "Median Debt: Students who have NOT completed" = cbind(Name,table_result[13,]),
               "Median Earnings: Students 10 years after entry" = cbind(Name,table_result[14,]),
               "Tuition in state" = cbind(Name,table_result[15,]),
               "Tuition out state" = cbind(Name,table_result[16,])
        )})
      
      dataset1<-datasetInput2()
      n<-nrow(dataset1)
      v1<-as.character(c(dataset1[c(2:n),1]))
      v2<-as.numeric(c(dataset1[c(2:n),2]))
      mydata<-data.frame(Institution = v1, Statistic = v2)

      ggplot(mydata, aes(Institution, Statistic)) +
        theme(panel.background = element_rect(fill = "white"),plot.title = element_text(colour = "black", size = 16, vjust = 1),plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches")) +
        geom_text(aes(label = Statistic, vjust = -0.8, hjust = 0.5), show_guide = FALSE) +
        geom_bar(aes(fill = Institution), position = "dodge", stat="identity",width = 0.9)+
        ggtitle("Comparisons of Selected Universities")
      
    } else
      ggplot() + ggtitle("Please select a university.")+
      theme(panel.background = element_rect(fill = "white"),plot.title = element_text(colour = "black", size = 16, vjust = 1),plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))

  })
  
  
  #------------------------------------------------------------------------------------------------------
 
  
  #Graphical Analysis
  
output$graph1<- renderPlot({
    s = input$universities.table_rows_selected[1]
    x = input$universities.table_rows_selected[2]
    
    if (length(s)) {
      
      university1 <- d5()$INSTNM[s]
      edu_1 <- filter(fulldata, INSTNM == university1)

      edu_1$Year = as.numeric(edu_1$Year)
      edu_1$ADM_RATE = as.numeric(edu_1$ADM_RATE)
      edu_1$SAT_AVG = as.numeric(edu_1$SAT_AVG)
      edu_1$ACTCMMID = as.numeric(edu_1$ACTCMMID)
      edu_1$UGDS = as.numeric(edu_1$UGDS)
      
      university2 <- d5()$INSTNM[x]
      edu_2 <- filter(fulldata, INSTNM == university2)
      
      edu_2$Year = as.numeric(edu_2$Year)
      edu_2$ADM_RATE = as.numeric(edu_2$ADM_RATE)
      edu_2$SAT_AVG = as.numeric(edu_2$SAT_AVG)
      edu_2$ACTCMMID = as.numeric(edu_2$ACTCMMID)
      edu_2$UGDS = as.numeric(edu_2$UGDS)   
      
      dataInput<-reactive({
        switch(input$universities.table4,
               "ADM" = edu_1[,c("ADM_RATE","Year")],#,edu_2[,c("ADM_RATE","Year")]),
               "SAT" = edu_1[,c("SAT_AVG","Year")],#,edu_2[,c("SAT_AVG","Year")]),
               "ACT" = edu_1[,c("ACTCMMID","Year")],#,edu_2[,c("ACTCMMID","Year")]),
               "UGDS" = edu_1[,c("UGDS","Year")]#,edu_2[,c("UGDS","Year")])
        )
      })
      
      mydata<-dataInput()
      colnames(mydata)<-c("Attribute","Year")
      as.data.frame(mydata)

    ggplot(data = mydata, aes(x = Year, y = Attribute)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("Trends")
    }
    
    else {
      ggplot() + ggtitle("Please select a university.")
    }
})
  
output$graph2<- renderPlot({
    s = input$universities.table_rows_selected[1]
    x = input$universities.table_rows_selected[2]
    
    if (length(s)) {
      
      university1 <- d5()$INSTNM[s]
      edu_1 <- filter(fulldata, INSTNM == university1)

      edu_1$Year = as.numeric(edu_1$Year)
      edu_1$ADM_RATE = as.numeric(edu_1$ADM_RATE)
      edu_1$SAT_AVG = as.numeric(edu_1$SAT_AVG)
      edu_1$ACTCMMID = as.numeric(edu_1$ACTCMMID)
      edu_1$UGDS = as.numeric(edu_1$UGDS)
      
      university2 <- d5()$INSTNM[x]
      edu_2 <- filter(fulldata, INSTNM == university2)
      
      edu_2$Year = as.numeric(edu_2$Year)
      edu_2$ADM_RATE = as.numeric(edu_2$ADM_RATE)
      edu_2$SAT_AVG = as.numeric(edu_2$SAT_AVG)
      edu_2$ACTCMMID = as.numeric(edu_2$ACTCMMID)
      edu_2$UGDS = as.numeric(edu_2$UGDS)   
      
      dataInput<-reactive({
        switch(input$universities.table4,
               #"Year" = list(edu_1$Year,edu_2$Year)
               "ADM" = edu_2[,c("ADM_RATE","Year")],#,edu_2[,c("ADM_RATE","Year")]),
               "SAT" = edu_2[,c("SAT_AVG","Year")],#,edu_2[,c("SAT_AVG","Year")]),
               "ACT" = edu_2[,c("ACTCMMID","Year")],#,edu_2[,c("ACTCMMID","Year")]),
               "UGDS" = edu_2[,c("UGDS","Year")]#,edu_2[,c("UGDS","Year")])
        )
      })
      
      mydata<-dataInput()
      colnames(mydata)<-c("Attribute","Year")
      as.data.frame(mydata)
      #creates graph
      ggplot(data = mydata, aes(x = Year, y = Attribute)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("Trends")
      
    }
    
    else {
      ggplot() + ggtitle("Please select a university.")
    }
})
  
  

})
  
  

