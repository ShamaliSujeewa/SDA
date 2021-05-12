library(shiny)
library(shinydashboard)
library(shinyjs)
library(psych)
library(plyr)
library(faraway)
library(ROCR) 
library(pROC)
library(DT)
library(arm)
library(glmnet)
library(randomForest)
library(ggplot2)
library(ggpubr)
library(gplots)
library(car)



appCSS <- "
#loading-content {
position: absolute;
background: #5F080A;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"



shinyUI(
  
  
  
  #Start Page
  dashboardPage(skin = "purple",
                
                
                #####################################################
                
                #page Header
                dashboardHeader(
                  title = span(img(src="logo.png",height=40,width=40),"SDA",style = "color:white; font-size: 32px; font-weight:bold")),
                
                
                
                #############################################################################################################    
                #Side bar List
                dashboardSidebar(
                  
                  sidebarMenu(
                    
                    menuItem("Home", tabName = "H", icon=icon("home")),
                    menuItem("About", tabName = "AAp", icon=icon("accusoft")),
                    menuItem("Data Preparation",tabName = "HP",icon=icon("database")),
                    
                    menuItem("Demonstration with Demo data",style = "color:white; font-size: 16 px; font-weight:bold",
                             menuItem("Factor Identification", tabName = "DFI", icon=icon("project-diagram")),
                             menuItem("Group Comparison Test", tabName = "DWMA", icon=icon("slack"))
                    ),
                   
                    
                    menuItem("Factor Identification", style = "color:white; font-size: 16 px; font-weight:bold" ,
                             menuItem("Preliminary Analysis", tabName = "PA", icon=icon("file")),
                             tags$b(align="center", p("-------Model Analysis-------")),
                             menuItem("Binary Logistic Model",tabName = "BM",icon=icon("bezier-curve")),
                             menuItem("Random Forest Model", tabName = "RM", icon=icon("sitemap")),
                             menuItem("LASSO Model", tabName = "LM", icon=icon("usb"))),
                    
                   
                    menuItem("Group Comparison Test", style = "color:white; font-size: 16 px; font-weight:bold" ,
                             
                             
                             menuItem("Import Dataset", tabName = "ID", icon=icon("angle-double-down")),
                             menuItem("Sumarry Statistics", tabName = "SS",icon=icon("border-all")),
                             
                             tags$b(align="center", p("-------Statistical Test-------")),
                             menuItem("ANOVA Test", tabName = "AT", icon=icon("battle-net")),
                             menuItem("Check ANOVA assumptions", tabName = "AAs", icon=icon("buffer")),
                             menuItem("Kruskal-Wallis rank sum test", tabName = "KT", icon=icon("campground")),
                             
                             tags$b(align="center", p("----------Hypothesis Test-----------")),
                             menuItem("t test", tabName = "ZT" , icon=icon("cross")),
                             menuItem("Wilcoxon Signed rank test", tabName = "WT", icon=icon("dice-d20"))
                    ),
                    menuItem("About Authors", tabName = "AA", icon=icon("address-card"))
                    
                  ),
                  
                  tags$br(),tags$br(),tags$br(),tags$br(), tags$br(),tags$br(), tags$br(),tags$br(),tags$br(), tags$br(),tags$br(),tags$br(),tags$br(), tags$br(),tags$br(),
                  tags$b(align="left", p("Created by: Ms.Shamali Kumari"))
                  
                  
                  
                  
                ),   #End of the side Bar
                #############################################################################################################
                
                
                #Page Body Part
                dashboardBody(
                  
                  
                  tags$style(HTML(".main-sidebar { font-size: 14px!important; font-weight:bold }.treeview-menu>li>a { font-size: 14px!important; font-weight:bold}")),
                  
                  fluidPage(
                    useShinyjs(),
                    inlineCSS(appCSS),
                    
                    # Loading message
                    div(id = "loading-content",h2("Please Wait, Until Page is Loading...")
                    )
                    
                   
                    
                    
                  ),
                  
                  
                  
                  
                  
                  basicPage(
                   h4(
                      fluidRow(align="right",style="color:brown",
                               # We give textOutput a span container to make it appear
                               # right in the h4, without starting a new line.
                              textOutput("currentTime", container = span))
                    )
                 ),
                  
                  
                  
                  tabItems(
                    
                    ########################################### HOME #####################################
                    tabItem(tabName = "H", 
                            fluidPage(
                              
                              titlePanel(title = div(strong("SDA : SURVEY DATA ANALYZER"))),
                              #div(align="center", img(src="kidney2.jpg",height=250,width=300),img(src="reason.jpg",height=250,width=300),img(src="kidney.jpg",height=250,width=300)),
                              p(""),
                              tags$br(),
                              
                              tags$span(align="justify",style="color:black;font-size: 16px",
                                        
                                        div(align="center", img(src="survey1.png",height=250,width=350)), tags$br(),
                                        p("This web application makes it easy to identify the primary factors that affect the presence-absence data, and it acts as a statistical software tool. The user might only deal with it’s user friendly interface and do the statistical analysis as a menu driven software package."),
                                        tags$br(),
                                        tags$span(align="justify",style="color:brown;font-size: 16px; font-weight:bold",
                                                  p("Some practical survey data that this application can be used are given below: ")),
                                        p("- Predicting the presence of a disease, and identifing the factors based on the predictor variables"),
                                        p("- Predicting the presence of a species in an area, and identifing the factors based on the predictor variables"),
                                        p("- Predicting whether the customer purchase a product, and identifying factors affected to improve the interest of customers"),
                                        tags$br(),
                                        p("Use the About side bar button to understand the functioning of the App.")
                                        
                                        
                              ),
                              tags$br(),tags$br(),tags$br()
                              
                              
                              
                              
                            )
                            
                    ),
                    
                    
                    
                    ########################################### About APP #####################################
                    tabItem(tabName = "AAp", 
                            fluidPage(
                              
                              titlePanel(title = div(strong("Functioning of the App"))),
                              
                              tags$span(align="justify",style="color:black;font-size: 16px",
                              tags$br(),
                              
                              
                              p("The home page of SDA will be displayed when you access this App. Introduction and the usage of the application are shown in this section. Among the other five tabs, three are related to the analysis, and one provides the guidelines for data preparation. The last tab shows the names and affiliation of the researchers who created this App. Read the Data Preparation section before starting the analysis."),
                              tags$br(),
                              
                              
                              p("Descriptions of Data Analysis Tabs are given below:"),
                              
                              
                              
                              tags$div(tags$ul(
                                tags$a(href="#demo" ,tags$li("Demonstration with Demo Data")),
                                tags$a(href="#facide" ,tags$li("Factor Identification")),
                                tags$a(href="#watana" ,tags$li("Group Comparison Analysis"))),
                                
                                style = "color:blue; font-size: 16px"),
                                tags$br(),tags$br(),
                                
                                ####Demonstration with Demo Data#########
                                h4( id="demo",
                                    fluidRow(align="left",style="color:blue;font-size: 16px",tags$b("Demonstration with Demo Data"))),
                              p("This application mainly focuses on building a model for prediction, and finding factors that affect the presence/absence data. Using an example data set, “Factor Identification” and “Group Comparison” Tests are demontrated in this tab."),
                              
                                tags$br(),
                                div(align="center", img(src="demo.png",height=350,width=650)),
                                tags$br(),
                              
                              
                              
                              p("Presence-absence of the chronic kidney disease (CKD) of the 350 objects has been used as the demonstration data. This survey was conducted to identify the most affected factors for prevalence of the CKD among society. As many researchers have found that the water metal concentration is also one of the major factors for kidney diseases, it is important to compare concentrations of water metals in sample locations that can be observed kidney disease patients with the control group areas where the patients with kidney disease are not observed. By using the demonstration data, it will be further explained how to compare groups and make the predictions."),
                              tags$br(),
                              
                              p("As demonstrated by the example data, the users of this App can upload a similar data set, and conduct the analysis using the “Factor Identification” and “Group Comparison Test” tabs."),
                              tags$br(),
                                
                                
                                ####Factor Identification#########
                                h4( id="facide",
                                    fluidRow(align="left",style="color:blue;font-size: 16px",tags$b("Factor Identification"))),
                              p("Basic summary statistics of a dataset can be done by using the “Preliminary Analysis” tab under the “Factor Identification” tab. First, click this tab, and then upload your dataset prepared as the correct format shown in Data Preparation tab. Make sure to put a tick mark in the “Header” checkbox if there are headers in your dataset, and select the correct separator. When you browse the data sets, several statistics related to that dataset will be opened automatically. Just click on that menu and select variables correctly from the dropdown lists to see the relevant outputs. You can see the complete dataset that you have uploaded from the “Data” menu and using the search option you can filter the objects."),
                              tags$br(),
                                div(align="center", img(src="facIde.png",height=350,width=650)),
                                tags$br(),
                              p("The procedures to use the other three sub-tabs (Binary Logistic Model, Random Forest Model and LASSO Model) in the Model Analysis section are also the same as the procedure for the “Preliminary Analysis” tab.  To fit Random Forest, and Binary Logistic Models, first, upload the dataset, and then, type the model in the particular format as given in the text area. Next, click the “Submit Model” button to obtain the results related to the selected model. To fit a LASSO Model, first replace the categorical variables with the relevant numeric values, and upload the dataset again. Then, select the correct variables to fit the model, and click the “Submit Model” button."),
                              tags$br(),
                                
                              
                              
                              
                              
                              
                                ####Group Comparison Test#########
                                h4( id="watana",
                                    fluidRow(align="left",style="color:blue;font-size: 16px",tags$b("Group Comparison Test"))),
                                
                                
                              
                              p("Under group comparison, an ANOVA test can be performed to identify whether the particular variable have significant differences among the different groups. Major assumptions related to ANOVA can also be tested, and if the assumptions are violated, the Kruskal-Wallis Rank Sum test can be performed as an alternative method."),
                              tags$br(),
                              p("Further, you can perform a one sample t-test if normality assumption is not violated or if you have a large sample. Otherwise, you can perform a Wilcoxon Signed Rank test."),
                              tags$br(),
                              
                              
                      
                                
                                div(align="center", img(src="watana.png",height=350,width=650)),
                                tags$br()
                              
                              
                              )
                            
                            )
                            
                    ),
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    ########################################### Data Preparation #####################################
                    tabItem(tabName = "HP", 
                            fluidPage(
                              
                              titlePanel(title = tags$b("Data Preparation")),
                              tags$br(),
                              
                              tags$span(align="justify",style="color:black;font-size: 16px",
                              p("Prepare your data set as shown in the example data sets.")),
                              tags$br(),
                              p("Structure of data for Factor Identification", style = "color:Blue; font-size: 18px; font-weight:bold"),
                              tags$span(align="justify",style="color:black;font-size: 16px",
                              
                              
                              p("Look at structure of the demonstration dataset is shown below. This dataset is collected from endemic and non-endemeic (control group) areas of Chronic Kidney Disease (CKD) prevalence, and the dataset includes of 9 variables and 350 observations. The objective is to fit models for prediction, and finding the most affected factors for the disease prevalence among the people. Here, the CKD variable represents the presence/absence of the disease, Age and Gender  variables are numeric, and the rest of the variables are categorical.  The structure of your dataset should be similar to this format for prediction and identifying factors. However, when you fit LASSO model, replace levels of the categorical variables by the appropriate numerical values."),
                              tags$br(),
                              div(align="center", img(src="dp1.png",height=400,width=600)),
                              
                             
                              tags$br(),
                              p("Structure of data for Group Comparison Test", style = "color:Blue; font-size: 18px; font-weight:bold"),
                              tags$span(align="justify",style="color:black;font-size: 16px",
                                        
                                        
                                        p("Prepare your dataset as shown below: Note that, in this dataset, one variable (group) is categorical, and others are numerical. The categorical variable can have two or more levels."),
                                        tags$br(),
                                        div(align="center", img(src="dp2.png",height=400,width=600))
                              
                              
                              )
                              
                              
                              )
                            )
                            
                    ),
                    
                    
                    ######################################### Demonstration with Demo data #####################################
                    ################# Factor Identification #####################
                    
                    tabItem(tabName = "DFI", 
                            fluidPage(
                              
                          
                                mainPanel(
                                  uiOutput("tbD1"), width=20
                                )
                              
                              
                              
                            )
                            
                    ),
                    
                    ################ Water Metal Analysis ##################
                    
                    tabItem(tabName = "DWMA", 
                            fluidPage(
                              
                              
                              mainPanel(
                                uiOutput("tbD2"), width=20
                              )
                              
                              
                              
                            )
                            
                    ),
                    
                    
                    ########################################### PRILIMINARY ANALYSIS #####################################
                    tabItem(tabName = "PA", 
                            fluidPage(
                              
                              titlePanel(title = tags$b("Priliminary Analysis")),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput("file1","Upload the file"),
                                  helpText("Default max. file is 5MB"),
                                  tags$hr(),
                                  h5(helpText("Select the read.table parameters below")),
                                  checkboxInput(inputId="header1",label ="Header", value=FALSE),
                                  checkboxInput(inputId="stringAsFactors1",label ="stringAsFactors", value=FALSE),
                                  br(),
                                  radioButtons(inputId='sep1',label='Separator',choices = c(Comma=',',Semicolon=';',Tab='\t',Space=' '),selected = ',')
                                  
                                ),
                                mainPanel(
                                  uiOutput("tb1")
                                )
                              )
                              
                              
                              
                            )
                            
                    ),
                    
                    
                    ########################################### RANDOM FOREST MODEL #####################################
                    tabItem(tabName = "RM", 
                            fluidPage(
                              
                              titlePanel(title = tags$b("Random Forest Model")),
                              
                              sidebarLayout(
                                
                                sidebarPanel(
                                  fileInput("file2","Upload the file"),
                                  checkboxInput(inputId="header2",label ="Header", value=FALSE), 
                                  checkboxInput(inputId="stringAsFactors2",label ="stringAsFactors", value=TRUE),
                                  
                                  #actionButton(inputId="btndata2", label="Data", value=FALSE), 
                                  
                                  br(),
                                  radioButtons(inputId='sep2',label='Separator',choices = c(Comma=',',Semicolon=';',Tab='\t',Space=' '),selected = ',')
                                  
                                  
                                  
                                ),
                                mainPanel(
                                  
                                  uiOutput("tb2")
                                )
                              )
                              
                            )
                            
                    ),
                    
                    
                    
                    ########################################### BINARY LOGISTIC REGRESSION MODEL #####################################
                    tabItem(tabName = "BM", 
                            fluidPage(
                              
                              titlePanel(title = tags$b("Binary Logistic Regression Model")),
                              
                              sidebarLayout(
                                
                                sidebarPanel(
                                  fileInput("file3","Upload the file"),
                                  checkboxInput(inputId="header3",label ="Header", value=FALSE), 
                                  checkboxInput(inputId="stringAsFactors3",label ="stringAsFactors", value=FALSE),
                                  
                                  
                                  br(),
                                  radioButtons(inputId='sep3',label='Separator',choices = c(Comma=',',Semicolon=';',Tab='\t',Space=' '),selected = ',')
                                  
                                ),
                                
                                mainPanel(
                                  
                                  uiOutput("tb3")
                                )
                              )
                              
                            )
                            
                            
                            
                    ),
                    
                    
                    ########################################### LASSO MODEL #####################################
                    tabItem(tabName = "LM", 
                            fluidPage(
                              
                              titlePanel(title = tags$b("LASSO Model")),
                              
                              
                              sidebarLayout(
                                
                                sidebarPanel(
                                  fileInput("file4","Upload the file"),
                                  checkboxInput(inputId="header4",label ="Header", value=FALSE), 
                                  checkboxInput(inputId="stringAsFactors4",label ="stringAsFactors", value=FALSE),
                                  
                                  
                                  br(),
                                  radioButtons(inputId='sep4',label='Separator',choices = c(Comma=',',Semicolon=';',Tab='\t',Space=' '),selected = ',')
                                  
                                ),
                                
                                mainPanel(
                                  
                                  uiOutput("tb4")
                                )
                              )
                              
                            )
                            
                    ),
                    
                    ########################################### Group Comparison Test #####################################
                    
                    
                    tabItem(tabName = "ID", 
                            fluidPage(
                              
                              titlePanel(title = tags$b("Import Your Dataset")),
                              
                              
                              sidebarLayout(
                                
                                sidebarPanel(
                                  fileInput("file5","Upload the file"),
                                  checkboxInput(inputId="header5",label ="Header", value=FALSE), 
                                  checkboxInput(inputId="stringAsFactors5",label ="stringAsFactors", value=FALSE),
                                  
                                  
                                  br(),
                                  radioButtons(inputId='sep5',label='Separator',choices = c(Comma=',',Semicolon=';',Tab='\t',Space=' '),selected = ',')
                                  
                                ),
                                
                                mainPanel(
                                  
                                  uiOutput("tb5")
                                )
                              )
                              
                              
                            )
                            
                    ),
                    
                    
                    
                    
                    tabItem(tabName = "SS", 
                            fluidPage(
                              
                              sidebarLayout( 
                                
                                sidebarPanel(
                                  p(align = "justify",style="color:black;font-weight:bold;font-size:18px","Please make sure to upload your dataset into *Import Dataset* option to continue the analysis."),
                                  tags$br(),
                                  p(style="color:brown;font-weight:bold;font-size:16px","Steps"),
                                  p(style="color:Blue;font-weight:bold;font-size:16px","Group Comparison Test --> Import Dataset")
                                ),
                                
                               mainPanel(
                                uiOutput("tb6")
                              )
                            )
                            )
                            
                    ),
                    
                    
                    tabItem(tabName = "AT", 
                            fluidPage(
                  
                              sidebarLayout( 
                                
                                sidebarPanel(
                                  p(align = "justify",style="color:black;font-weight:bold;font-size:18px","Please make sure to upload your dataset into *Import Dataset* option to continue the analysis."),
                                  tags$br(),
                                  p(style="color:brown;font-weight:bold;font-size:16px","Steps"),
                                  p(style="color:Blue;font-weight:bold;font-size:16px","Group Comparison Test --> Import Dataset")
                                ),
                              mainPanel(
                                
                                uiOutput("tb7")
                              )
                              )
                              
                            )
                            
                    ),
                    
          
                    
                    tabItem(tabName = "AAs", 
                            fluidPage(
                              
                              sidebarLayout( 
                                
                                sidebarPanel(
                                  p(align = "justify",style="color:black;font-weight:bold;font-size:18px","Please make sure to perform ANOVA test to check the test assumptions."),
                                  tags$br(),
                                  p(style="color:brown;font-weight:bold;font-size:16px","Steps"),
                                  p(style="color:Blue;font-weight:bold;font-size:16px","Group Comparison Test --> ANOVA Test")
                                ),
                                mainPanel(
                                  
                                  uiOutput("tb8")
                                )
                              )
                              
                            )
                            
                    ),
                    
                    
                    
                    tabItem(tabName = "KT", 
                            fluidPage(
                              
                              sidebarLayout( 
                                
                                sidebarPanel(
                                  p(align = "justify",style="color:black;font-weight:bold;font-size:18px","Please make sure to upload your dataset into *Import Dataset* option to continue the analysis."),
                                  tags$br(),
                                  p(style="color:brown;font-weight:bold;font-size:16px","Steps"),
                                  p(style="color:Blue;font-weight:bold;font-size:16px","Group Comparison Test --> Import Dataset"),
                                  tags$br(),
                                  p(style="color:Green;font-weight:bold;font-size:16px","Please apply this test when only ANOVA test assumptions are violated")
                                ),
                                mainPanel(
                                  
                                  uiOutput("tb9")
                                )
                              )
                              
                            )
                            
                    ),
                    
                    
                    
                    tabItem(tabName = "ZT", 
                            fluidPage(
                              
                              sidebarLayout( 
                                
                                sidebarPanel(
                                  p(align = "justify",style="color:black;font-weight:bold;font-size:18px","Please make sure to upload your dataset into *Import Dataset* option to continue the analysis."),
                                  tags$br(),
                                  p(style="color:brown;font-weight:bold;font-size:16px","Steps"),
                                  p(style="color:Blue;font-weight:bold;font-size:16px","Group Comparison Test --> Import Dataset")
                                  
                                ),
                                mainPanel(
                                  
                                  uiOutput("tb10")
                                )
                              )
                              
                            )
                            
                    ),
                    
                    
                    
                    tabItem(tabName = "WT", 
                            fluidPage(
                              
                              sidebarLayout( 
                                
                                sidebarPanel(
                                  p(align = "justify",style="color:black;font-weight:bold;font-size:18px","Please make sure to upload your dataset into *Import Dataset* option to continue the analysis."),
                                  tags$br(),
                                  p(style="color:brown;font-weight:bold;font-size:16px","Steps"),
                                  p(style="color:Blue;font-weight:bold;font-size:16px","Group Comparison Test --> Import Dataset"),
                                  tags$br(),
                                  p(style="color:Green;font-weight:bold;font-size:16px","Please apply this test when only data are not normally distributed.")
                                ),
                                mainPanel(
                                  
                                  uiOutput("tb11")
                                )
                              )
                              
                            )
                            
                    ),
                    
                    
                    
                    
                    
                    
                    ##################################About Author####################################
                    
                    tabItem(tabName = "AA", 
                            fluidPage(
                              
                              titlePanel(title = tags$b("About Collaborators")),
                              ###################
                              
                              fluidRow(
                                
                                column(width = 5, 
                                       div(align="left", img(src="shamali.png",height=150,width=150))
                                       
                                       
                                ),
                                
                                column(width =5,
                                       div(align="left", img(src="pushpakanthi.png",height=150,width=150))
                                       
                                ),
                                
                                
                                
                                #########################
                                
                                
                                
                                fluidRow(),
                                
                                
                                fluidRow(
                                  
                                  
                                  column(width = 5, div(align="left", tags$span(style="color:black;font-size: 14px", p(""),HTML(paste("Miss. Shamali Kumari (B.Sc(Sp) in Statistics)", '<br/>',
                                                                                                                                      ("Department of Basic Sciences"),'<br/>',
                                                                                                                                      ("Faculty of Health Sciences"),'<br/>',("The Open University of Sri Lanka"),
                                                                                                                                      p(" "),HTML(paste("Tel: +94713641025"), '<br/>',
                                                                                                                                                  ("E-mail:shamalisujeewa07@gmail.com"),'<br/>'
                                                                                                                                      ),p( tags$a(href="https://www.linkedin.com/in/shamali-kumari-499a98100/", tags$b(p("LinkedIn:Shamali Kumari")),target="_blank"))
                                                                                                                                      
                                                                                                                                      
                                  ))))
                                  
                                  
                                  
                                  ),
                                  
                                  
                                  
                                  
                                  column(width =5,
                                         div(align="left", tags$span(style="color:black;font-size: 14px", p(""),HTML(paste("Prof. Pushpakanthie Wijekoon (B.Sc/ph.D)"), '<br/>',
                                                                                                                     ("Department of Statistics & Computer Science"),'<br/>',
                                                                                                                     ("Faculty of Science"),'<br/>',("University of Peradeniya")),
                                                                     p(" "),HTML(paste("Tel: +94812394649 , +94718061012"), '<br/>',
                                                                                 ("E-mail:pkwijekoon@gmail.com"),'<br/>'
                                                                     ),p( tags$a(href="https://www.researchgate.net/profile/Pushpakanthie-Wijekoon", tags$b(p("ResearchGate:Pushpakanthie Wijekoon")),target="_blank"))
                                                                     
                                                                     
                                                                     
                                                                     
                                         ))
                                  )
                                  
                                  
                                  
                                  
                                ))
                              
                            )
                            
                            ###############################################################################################
                            
                            
                            
                    )
                  ),  # End of the body part
                  ######################################################################
                  
                  tags$head(
                    tags$style(HTML(".main-sidebar {background-color: green !important;}"))
                  )
                  
                ) # End of the page
  )
)

