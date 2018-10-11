packages.used=c("shiny", "plotly", "shinydashboard", "leaflet", "DT","BH","markdown","rCharts","data.table","dplyr","shinydashboardPlus","shinyWidgets")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos = "http://cran.us.r-project.org")
}

library(shiny)
library(leaflet)
library(maps)
library(DT)
library(rCharts)
library(shinydashboard)
library(plotly)
library(BH)
require(markdown)
require(data.table)
library(dplyr)
library(shinydashboardPlus)
library(shinyWidgets)

# Choices for drop-downs
# Delete states without universities: AS, FM, GU, MH, MP, PR, PW
# location = c("-----","AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE"
#              ,"FL","FM","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD"
#              ,"ME","MH","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV"
#              ,"NY","OH","OK","OR"
#              ,"PA","PR","PW","RI","SC","SD","TN","TX","UT","VA","VI","VT"
#              ,"WA","WI","WV","WY")
location = c("-----","AK","AL","AR","AZ","CA","CO","CT","DC","DE"
             ,"FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD"
             ,"ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV"
             ,"NY","OH","OK","OR"
             ,"PA","RI","SC","SD","TN","TX","UT","VA","VI","VT"
             ,"WA","WI","WV","WY")

Major = c("-----",
          "Agriculture, Agriculture Operations, and Related Sciences","Natural Resources and Conservation","Architecture and Related Services",
          "Area, Ethnic, Cultural, Gender, and Group Studies","Communication, Journalism, and Related Programs","Communications Technologies/Technicians and Support Services",
          "Computer and Information Sciences and Support Services","Personal and Culinary Services","Education","Engineering","Engineering Technologies and Engineering-Related Fields",
          "Foreign Languages, Literatures, and Linguistics","Family and Consumer Sciences/Human Sciences","Legal Professions and Studies","English Language and Literature/Letters",
          "Liberal Arts and Sciences, General Studies and Humanities","Library Science","Biological and Biomedical Sciences","Mathematics and Statistics",
          "Military Technologies and Applied Sciences","Multi/Interdisciplinary Studies","Parks, Recreation, Leisure, and Fitness Studies","Philosophy and Religious Studies",
          "Theology and Religious Vocations","Physical Sciences","Science Technologies/Technicians","Psychology","Homeland Security, Law Enforcement, Firefighting and Related Protective Services",
          "Public Administration and Social Service Professions","Social Sciences","Construction Trades","Mechanic and Repair Technologies/Technicians","Precision Production",
          "Transportation and Materials Moving","Visual and Performing Arts","Health Professions and Related Programs",
          "Business, Management, Marketing, and Related Support Services","History")


navbarPage("Find Your University!", id="nav",
           
           tabPanel("Map",setBackgroundImage(src="CU.jpg"),
                    fluidRow(
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("style.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("mymap", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 120, left = 20, right = "auto", bottom = 10,
                                      width = 330, height = 700,
                                      
                                      sliderInput("cost", label = "Cost by Year ", min = 10000, max =65000, value = c(10000,65000),step = 1000),
                                      sliderInput(inputId="sat",label = "SAT Score",value = 1600,min = 750,max = 1600,step = 10 ),
                                      sliderInput("act",label = "ACT Score",min=15,max=36, value =36,step = 1 ),
                                      
                                      selectInput("location", "Select State or Region", choices = location),
                                      selectInput("major", "Major", choices = Major),
                                      selectInput("city", "Location", choices = c("-----","City","Suburb","Town","Rural")),
                                      selectInput("schtype", "Type of Institution", choices = c("-----","Public","Private nonprofit","Private for-profit")),
                                      selectInput("hdeg", "Highest Degree", choices = c("-----","Graduate","Bachelor","Associate","Certificate","Non-degree-granting"))
                                      
                        )
                        )
                    
           )),
           
           tabPanel("Ranking",
                    fluidRow(
                      
                      dataTableOutput("universities.table"),
                      
                      hr()
                      
                    )),
           
           tabPanel(title="Overall Comparison", width = 12, solidHeader = T,
                    fluidRow(
                      dataTableOutput("table.summary1")
                    )),
                    
       
           tabPanel(title="Detailed Summary", width = 12, solidHeader = T,
                    sidebarPanel(
                      selectInput("universities.table1","Attributes to Select:",
                                  c("Website" = "Website",
                                    "City" = "City",
                                    "Highest Degree" = "Highest Degree",
                                    "Type of Institution" = "Type of Institution",
                                    "Location" = "Location"
                                  )),
                      
                      hr(),
                      hr(),
                      hr(),
                      
                      selectInput("universities.table2", "Select an Attribute to View:",
                                  c(
                                    "Male %" = "Male %",
                                    "Female %" = "Female %",
                                    "Average age of entry" = "Average age of entry",
                                    "% of Undergraduates aged 25+" = "% of Undergraduates aged 25+",
                                    "Undergraduate students receiving federal loan %",
                                    "Median Debt: Students who have completed" = "Median Debt: Students who have completed",
                                    "Median Debt: Students who have NOT completed" = "Median Debt: Students who have NOT completed",
                                    "Median Earnings: Students 10 years after entry" = "Median Earnings: Students 10 years after entry",
                                    "Tuition in state" = "Tuition in state",
                                    "Tuition out state" = "Tuition out state"))),
                    
                    mainPanel(
                      
                      tabsetPanel(
                        tabPanel(p(icon("line-chart"),"Data Comparison"),
                                 fluidRow(
                                 dataTableOutput("table.summary2")),
                                 plotOutput("graph.summary3"))
                      ))),
           
           tabPanel(title = "Data Exploration",
                    sidebarPanel(
                      selectInput("universities.table4", "Variables to show:",
                                                           c("Admission Rates" = "ADM",
                                                             "SAT Scores" = "SAT",
                                                             "ACT Scores" = "ACT",
                                                             "Admitted Undergrads" = "UGDS"))),

                    mainPanel(
                      fluidRow(
                        column(6,
                               p(icon("line-chart"), "Univeristy1"),
                               plotOutput("graph1")
                        ),
                        column(6,
                               p(icon("line-chart"), "Univeristy2"),
                               plotOutput("graph2")
                        )
                   )
           )),         
       
           tabPanel("About Us", id="about",
                    fluidRow(top=200,align="center",
                      
                      HTML('<p><img src="aboutus.png" style="width:800px;height:600px align="middle"></p>')
                      
                      
                    )),
           
           conditionalPanel("false", icon("crosshair"))
        
          
)
