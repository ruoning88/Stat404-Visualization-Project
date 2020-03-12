#### 3. The actual app
#### Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey 
#### URL: https://www.bls.gov/cex/pumd_data.htm
#### Data Years: 2013-2017
#### About the data:
####                  This is an extensive consumer survey tracking how americans spend their money.
####                  Individuals were surveyed once every three months for a year
####                  Combined data from 2013-2017 across the four quarters of each year
####                  The survey evaluated spending in over 500 categories/subcategories.
####                  We selected the variables that allow us to evaluate how total and certain category expenditures varied across the United States, how certain category expenditures varied across the four quarters of 2013 to 2017, and how household characteristics influenced category expenditures.
####
#### Description of the APP: Our app allows users to compare household spendings geographically, chronologically,
####                         and by familiy traits. For the first tab, which is the map, the user can switch from whole 
####                         view (all US) to regional view to state view. For the whole view, users are able to see how one specific type of expense 
####                         has changed over time. For the regional view, users are able to compare each states' proportion of one particular expense to total
####                         expense by lookig at the pie charts. For the states view, users are able to compare how household is distributed by Race. 
####                         The second tab, Time Trends, allows users to obsserve how expenditures changes across quarters and years. By selecting multiple
####                         types of expenditures, they can tell how the composition of total expenditures has changed. For the third plot, users can observe how 
####                         the expenditures change by family traits, for instance of the highest degree the householder attained. The bar chart is automatically 
####                         ordered by the amount of the expenditures, from largest to smallest.

library(dplyr)
library(shiny)
library(maps)
library(statebins)
library(ggmap)
library(maptools)
library(rgdal)
library(sp)
library(tools)
library(ggplot2)
library(ggrepel)
library(stringr)
library(cowplot)
library(scatterpie)
library(shinyalert)
library(shinyWidgets)


###--------------------Preparation Files -----------------------------------------------------
#### Load Files
load("intvw_chor.RData")
load("intvwfull.RData")
load("fmli_final_quarter.Rdata")
load("fmli_final_year.Rdata")
load("BarPlot.RData")

###Set Expense Colors 
expense_colors.y <- c("#8c510a", "#66c2a5", "#feb24c", "#cb181d", "#8c2d04", "#fcc5c0", "#016450", "#084594", "#fb6a4a", "#756bb1")
names(expense_colors.y) <- unique(fmli.final.year$expensetype)
expense_colors.q <- c("#8c510a", "#66c2a5", "#feb24c", "#cb181d", "#8c2d04", "#fcc5c0", "#016450", "#084594", "#fb6a4a", "#756bb1")
names(expense_colors.q) <- unique(fmli.final.quarter$expensetype)

### Selection Bar DropDown Choices
regionchoices <- c(levels(intvw_chor$DIVISION),"All")
expchoices <- c("Total Expense"="TOT_avg","Food Expense"="FOOD_avg",
                "Housing Expense"="HOUS_avg","Apparel & Service Expense"="APP_avg",
                "Transportation Expense"="TRANS_avg","Health Care Expense"="HEALTH_avg",
                "Entertainment Expense"="ENTER_avg","Personel Care Expense"="PERSCA_avg",
                "Reading Expense"="READ_avg","Education Expense"="EDUCA_avg","Tobacco Expense"="TOBAC_avg",
                "Miscellaneous expenditures"="MISC_avg","Cash Contributions"="CASHCO_avg",
                "Personal Insurance & Pension Expense"="PERINSPQ_avg")
expbar <-c("Total Expense"="TOTEXP","Food Expense"="FOODPQ",
              "Housing Expense"="HOUSPQ","Apparel & Service Expense"="APPARPQ",
              "Transportation Expense"="TRANSPQ","Health Care Expense"="HEALTHPQ",
              "Entertainment Expense"="ENTERTPQ","Personel Care Expense"="PERSCAPQ",
              "Reading Expense"="READPQ","Education Expense"="EDUCAPQ","Tobacco Expense"="TOBACCPQ",
              "Miscellaneous expenditures"="MISCPQ","Cash Contributions"="CASHCOPQ",
              "Personal Insurance & Pension Expense"="PERINSPQ")
refstreg <- unique(data.frame(intvw_chor$SN,intvw_chor$DIVISION))[-16,]
colnames(refstreg) <- c("state","Reg")
famchar <- c("Education of Houseowner"="EDUC_REF","Marital Status of Houseowner"="MARITAL1",
             "Number of Members Age below 18"="PERSLT18","Number of Members Age greater than 64"="PERSOT64")
options(scipen=999)

#### State polygon file
states.outlines <- map_data("state")

### ScatterPie Legend Position
scatterpos <- data.frame(reg=regionchoices, x=c(-77,-83,-95,-100,-90,-95,-105,-120,-123,0),
                         y=c(40,37,37,37,27,30,27,35,33,0))


###--------------------------------------------- The UI design-------------------------------------------------
#### The UI:
ui <- fluidPage(
  titlePanel(title = "US Household Expenditure Exploratory App"),
  tabsetPanel(
    
#### Tab1: Choropleth
    tabPanel("Geographically",fluid=TRUE,
             setBackgroundColor("Ivory"),
             titlePanel(title = "Map of Household Expenditures"),    
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="!input.statecheckbox",selectInput(inputId="Region", label="Pick the Region",
                                                                    choices=regionchoices,
                                                                    selected=c("All"))),
      checkboxInput(inputId="statecheckbox",label="Select By State"),
      conditionalPanel(condition="input.statecheckbox",
                       selectInput(inputId="State", label="Pick the State",
                                   choices=toTitleCase(unique(intvw_chor$SN)),
                                   selected=c("Alabama"))),
      selectInput(inputId="Expense", label="Pick Expenditure Type",
                  choices=expchoices,
                  selected=c("Total Expense")),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
      conditionalPanel(condition="!input.statecheckbox",sliderInput(inputId="year",label="Select Years",
                  min=1915,max=2017,step=5,
                  sep="", animate = TRUE, ticks=FALSE,
                  value=2000)),
      conditionalPanel(condition="input.statecheckbox",
                       checkboxInput(inputId="racecheckbox",label="Also categorize By Race")),
      uiOutput(outputId="RC"),
  
#### HTML to define a pop up button    
      useShinyalert(), 
      tags$head(
        tags$style(HTML("
                        .btn {
                        display:block;
                        height: 30px;
                        width: 30px;
                        border-radius: 50%;
                        border: 2px solid grey;
                        }
                        "))
        ),
      actionButton("Dialog1", "?", style='padding:4px; font-size:100%;
                   color: #226600; background-color: #ddffcc'
                   )

      
    ),
    mainPanel(
      plotOutput(outputId="mymap", height="500px",width="100%")
    )
    
  )),


  
### Tab2:Line Plot
  tabPanel("Time Trend",fluid=TRUE,
           titlePanel(title = "Household Expenditures by Years"),
           sidebarLayout(
             sidebarPanel(
  
               selectInput("expensetype", label = "Select the Expense Type(s)", 
                           list("Food" = "Food",
                                "Alcohol"= "Alcohol",
                                "Housing"= "House",
                                "Furniture/Appliances"= "Furniture",
                                "Clothing"= "Clothes",
                                "Transportation"= "Transportation", 
                                "Healthcare"= "Healthcare",
                                "Entertainment"= "Entertainment",
                                "Education"= "Education"),
                           selected = "Food", 
                           multiple = TRUE),
               
               checkboxGroupInput("years", label = "Select Year(s):", 
                                  choices = list("2013" = 2013, 
                                                 "2014" = 2014, 
                                                 "2015" = 2015,
                                                 "2016" = 2016,
                                                 "2017" = 2017),
                                  selected = c(2014,2015,2016)),
               
               checkboxInput("quarter", label = "Display Quarters", value = TRUE),
               useShinyalert(), 
               actionButton("Dialog2", "?", style='padding:4px; font-size:100%;
                            color:yellow; background-color:#66c2a5'
               )
               ),
             
             mainPanel(
               plotOutput(outputId = "myplot")
             )
           )),


### Tab3: Bar Plots
tabPanel("Household Traits",fluid=TRUE,
         titlePanel(title = "Household Expenditures by Household Traits"),
         sidebarLayout(
           sidebarPanel(selectInput(inputId="HouseFeature", label="Pick the Family Characteristics",
                                    choices=famchar,
                                    selected=c("Education of Houseowner")),
           
           checkboxInput(inputId="Fill",label="Group by Another Family Characteristics"),
           
           uiOutput(outputId="FamilyChar"),
           
           selectInput(inputId="ExpBar", label="Pick the Expense Type",
                       choices=expbar,
                       selected=c("Total Expense")),
           
           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: orange; border-top: 1px solid orange ;
                                                  border-bottom: 1px solid orange;}  .irs-from, .irs-to, .irs-single { background: orange }")),
           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: orange; border-top: 1px solid orange ;
                                                  border-bottom: 1px solid orange ;}  .irs-from, .irs-to, .irs-single { background: orange }")),
           tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange; border-top: 1px solid orange ;
                                                  border-bottom: 1px solid orange ;}  .irs-from, .irs-to, .irs-single { background: orange }")),
           sliderInput(inputId="FamSize",label="Select Size of Family",
                       min=1,max=20,step=1,
                       sep="", animate = TRUE, ticks=FALSE,
                       value=c(1,20)),
           
           sliderInput(inputId="Hour",label="Select Range of Hours Houseowner work per week",
                       min=0,max=99,step=1,
                       sep="", animate = TRUE, ticks=FALSE,
                       value=c(0,99)),
           
           sliderInput(inputId="Week",label="Select Range of Week Houseowner work per year",
                       min=0,max=52,step=1,
                       sep="", animate = TRUE, ticks=FALSE,
                       value=c(0,52)),
           useShinyalert(),
           actionButton("Dialog3", "?", style='padding:4px; font-size:100%;
                            color:red; background-color:orange')
           ),
         mainPanel(
           plotOutput(outputId="mybar", height="500px",width="100%"))
         ))

)
)



##### ---------------------------------   The Server -----------------------------------------------------
server <- function(input, output,session) {

#### For Tab 1: Geographic Distribution--- Map of household Expenditures
#### When in the Reginal View, where scatterpie is displayed, Total Expenditures should not be displayed.
   observeEvent(input$Region,
        if(input$Region!="All") {
        updateSelectInput(session, "Expense", choices = expchoices[which(names(expchoices)!="Total Expense")])
          }
        )
  
  observeEvent(input$Region,
               if(input$Region=="All") {
                 updateSelectInput(session, "Expense", choices = expchoices)
               }
  )
  
#### Create a reactive dataset for the purpose of data cleaning. Depeding on the input values, how the data should be cleaned can change.      
  plotdata <- reactive({
    if(!input$statecheckbox){
      if(input$Region=="All"){
        intvwdt <- intvw_chor %>% group_by(SN,BUILT) %>% summarise(TOT_avg=mean(TOT_avg,na.rm=T),FOOD_avg=mean(FOOD_avg,na.rm=T),HOUS_avg=mean(HOUS_avg,na.rm=T),
                                                                    APP_avg=mean(APP_avg,na.rm=T),TRANS_avg=mean(TRANS_avg,na.rm=T),HEALTH_avg=mean(HEALTH_avg,na.rm=T),
                                                                    ENTER_avg=mean(ENTER_avg,na.rm=T),PERSCA_avg=mean(PERSCA_avg,na.rm=T),READ_avg=mean(READ_avg,na.rm=T),
                                                                    EDUCA_avg=mean(EDUCA_avg,na.rm=T), TOBAC_avg=mean(TOBAC_avg,na.rm=T), MISC_avg=mean(MISC_avg,na.rm=T),
                                                                    CASHCO_avg=mean(CASHCO_avg,na.rm=T), PERINSPQ_avg=mean(PERINSPQ_avg,na.rm=T))
        intvwdt <- intvwdt %>% filter(BUILT==input$year)
        intvwdt <- left_join(states.outlines,intvwdt,by=c("region"="SN"))
        intvwdt
        
      } else {
        intvwdt <- intvw_chor %>% group_by(DIVISION,SN,BUILT) %>% summarise(TOT_avg=mean(TOT_avg,na.rm=T),FOOD_avg=mean(FOOD_avg,na.rm=T),HOUS_avg=mean(HOUS_avg,na.rm=T),
                                                                   APP_avg=mean(APP_avg,na.rm=T),TRANS_avg=mean(TRANS_avg,na.rm=T),HEALTH_avg=mean(HEALTH_avg,na.rm=T),
                                                                   ENTER_avg=mean(ENTER_avg,na.rm=T),PERSCA_avg=mean(PERSCA_avg,na.rm=T),READ_avg=mean(READ_avg,na.rm=T),
                                                                   EDUCA_avg=mean(EDUCA_avg,na.rm=T), TOBAC_avg=mean(TOBAC_avg,na.rm=T), MISC_avg=mean(MISC_avg,na.rm=T),
                                                                   CASHCO_avg=mean(CASHCO_avg,na.rm=T), PERINSPQ_avg=mean(PERINSPQ_avg,na.rm=T))
        intvwdt <- intvwdt %>% filter(BUILT==input$year & DIVISION==input$Region)
        refs <- subset(refstreg,refstreg$Reg==input$Region)
        refs <- refs[,1]
        states.outlines <- subset(states.outlines,region %in% refs)
        intvwdt <- left_join(states.outlines,intvwdt,by=c("region"="SN"))
        intvwdt
        
      }} else { 
        states.outlines$region <- toTitleCase(states.outlines$region)
        states.outlines <- states.outlines %>% filter(region==input$State)
        states.outlines
      }
  })
  
#### Create another dataset for subplots(densityplots/violinplots) displayed when the "select by state" checkbox is checked.
subplotdata <- reactive({
      if(input$racecheckbox){
        r <- tolower(input$State)
        intvwst <- intvw1 %>% filter(REF_RACE %in% input$Race & SN==r)
        colnames(intvwst) <- c("FOOD_avg","HOUS_avg","APP_avg","TRANS_avg","HEALTH_avg","ENTER_avg","PERSCA_avg",
                               "READ_avg","EDUCA_avg","TOBAC_avg","MISC_avg","CASHCO_avg","PERINSPQ_avg",
                               "REF_RACE","BUILT","SN","TOT_avg")
        intvwst
      } else {
        r <- tolower(input$State)
        intvwst <- intvw1 %>% filter(SN==r)
        colnames(intvwst) <- c("FOOD_avg","HOUS_avg","APP_avg","TRANS_avg","HEALTH_avg","ENTER_avg","PERSCA_avg",
                               "READ_avg","EDUCA_avg","TOBAC_avg","MISC_avg","CASHCO_avg","PERINSPQ_avg",
                               "REF_RACE","BUILT","SN","TOT_avg")
        intvwst
      }
  })

#### Reactively Updating the Race drop-down list so that only races that exist within the dataset is selectable.
  output$RC <- renderUI({
    r <- tolower(input$State)
    intvwst <- intvw1 %>% filter(SN==r)
    vars <- as.list(unique(intvwst$REF_RACE))
    conditionalPanel(condition="input.racecheckbox",
                     selectInput(inputId="Race", label="Pick the Race",
                                 multiple=TRUE,
                                 choices=vars,
                                 selected=c("White","Black")))
  })

#### Reactive Value to reactively display the maximum value of the colorbar according to the values of the expenditures.   
legends <- reactive({
  if(!input$statecheckbox){
  df <- plotdata()
  col <- as.character(input$Expense)
  k <- round(max(df[[col]],na.rm=T),-3)} else{}
  })

#### Reactive pies displayed when a specefic region is selected. 
labels <- reactive({
  if(!input$statecheckbox){
  df <- plotdata() 
  col <- as.character(input$Expense)
  label <- df %>% group_by(region) %>%
    summarize(lat = (min(lat)+max(lat))/2,
              long= (min(long)+max(long))/2,
              Total = TOT_avg[1],
              expense = if(is.na(get(col)[1])){0}else{get(col)[1]}) %>%
    mutate(Rest=Total-expense, total_mode=sqrt(Total/5000), percent=expense/Total,
           label=paste(region,"\n",round((expense/Total)*100,1),"%",sep=""))
  }else{}
})

### Reactive Pie Size Legend
total_mod <- reactive({
  df <- labels()
  totmod <- df %>% select(Total)
  totmod <- unlist(totmod)
  proportions <- data.frame(p=c(1.1,0.55,0.275),z=c(round(max(totmod,na.rm=T),-2),round(max(totmod,na.rm=T)/2,-2),round(max(totmod,na.rm=T)/4,-2)))
  proportions
})

#### Reactive Text Label showing only the highest percent
labhigh <- reactive({
  if(!input$statecheckbox){
    df <- labels()
    labels() %>% filter(percent %in% head(sort(percent),1))
  } else {}
})

#### Reactive Value to reactively display the maximum value of the y axis of the violinplot.  
violiny <- reactive({
  df <- subplotdata()
  col <- as.character(input$Expense)
  j <- round(max(df[[col]],na.rm=T),-2)
})

### the Map:
  output$mymap <- renderPlot({
    if(!input$statecheckbox){
      if(input$Region=="All") {
       p <- ggplot() +geom_polygon(data=plotdata(),aes_string(x="long",y="lat",
                                                          color=input$Expense,fill=input$Expense,group="group"))+
          scale_fill_gradient(paste0(names(expchoices)[expchoices == input$Expense],"($)"),low="#ccffcc",high="#008000",
                              limits=c(1,round(legends(),-1)),breaks=c(1,round(legends()/4,-1),round(legends()/2,-1),round(3*legends()/4,-1),round(legends(),-1)),na.value="#e6e6e6") +
          scale_color_gradient(paste0(names(expchoices)[expchoices == input$Expense],"($)"),low="#ccffcc",high="#008000",
                               limits=c(1,round(legends(),-1)),breaks=c(1,round(legends()/4,-1),round(legends()/2,-1),round(3*legends()/4,-1),round(legends(),-1)),na.value="#e6e6e6")+
          coord_map()+
          theme(axis.ticks = element_blank(),
                axis.line=element_blank(),
                axis.text.x = element_blank(), axis.title.x=element_blank(),
                axis.text.y = element_blank(), axis.title.y=element_blank(),
                panel.border = element_blank(), 
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.background = element_blank(),
                legend.position = c(0.9, .2),
                legend.title=element_text(size=10),
                legend.text=element_text(size=10),
                plot.background = element_rect(fill = "#FFFFF0"),
                plot.title = element_text(size=13, hjust=.5, face="bold")) + 
          labs(
            caption="Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey \nBuilt by Marie Parsons, Patrick Ramatowski, & Ruoning Wang")
       ggdraw(p) + theme(panel.background = element_rect(fill = "#FFFFF0", colour = "#FFFFF0"))
        
      } else {
        df <- labels()
        
        p <- ggplot() +geom_polygon(data=plotdata(),fill="#ccffcc",color="darkgreen",aes(x=long,y=lat,
                                                  group=group))+
          geom_scatterpie(data=df, aes(x=long,y=lat,r=(total_mode/2)),cols=c("expense","Rest"),alpha = 0.5)+
           
          scale_fill_manual("Houshold Expense ($)", breaks = c("expense","Rest"),
                            labels = c(names(expchoices)[expchoices == input$Expense],"Other Expenditures"),
                            values = c("expense"="blue","Rest"="green") ) +
          geom_scatterpie_legend(radius=total_mod()[,1],x=scatterpos$x[which(scatterpos$reg==input$Region)],
                                 y=scatterpos$y[which(scatterpos$reg==input$Region)],n=3,
                                 labeller= function(x) paste0("$",sort(total_mod()[,2],decreasing = TRUE)))+
          annotate("text",x=scatterpos$x[which(scatterpos$reg==input$Region)]+1,
                   y=scatterpos$y[which(scatterpos$reg==input$Region)]+1.5,
                   label="Total Household Expense",size=3)+
          annotate("text",x=scatterpos$x[which(scatterpos$reg==input$Region)]+1.5,
                   y=scatterpos$y[which(scatterpos$reg==input$Region)]-1.5,
                   label="*Only the state with the highest \npercentage is labeled",size=3)+
          geom_text_repel(aes(x=long,y=lat,label=label),color="#6600ff",size=6,fontface="bold", 
                          data=labhigh(),force=4,fontface="bold")+
          coord_map()+
          theme(axis.ticks = element_blank(),
                axis.line=element_blank(),
                axis.text.x = element_blank(), axis.title.x=element_blank(),
                axis.text.y = element_blank(), axis.title.y=element_blank(),
                panel.border = element_blank(), 
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.background = element_blank(),
                legend.position = c(.9, .1),
                plot.background = element_rect(fill = "#FFFFF0"),
                plot.title = element_text(size=13, hjust=.5, face="bold")) + 
          labs(
            caption="Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey \nBuilt by Marie Parsons, Patrick Ramatowski, & Ruoning Wang")
        ggdraw(p) + theme(panel.background = element_rect(fill = "#FFFFF0", colour = "#FFFFF0"))
      }
    } else { 
      if(!input$racecheckbox){
      
        P1 <-  ggplot() + geom_polygon(data=plotdata(),aes_string(x="long",y="lat",group="group"),fill="#ccffcc")+
          coord_map() + 
          theme(axis.ticks = element_blank(),
                axis.line=element_blank(),
                axis.text.x = element_blank(), axis.title.x=element_blank(),
                axis.text.y = element_blank(), axis.title.y=element_blank(),
                panel.border = element_blank(), 
                panel.grid.major = element_blank(), 
                plot.background = element_rect(fill = "#FFFFF0"),
                panel.grid.minor = element_blank()) + 
          labs(caption="Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey \nBuilt by Marie Parsons, Patrick Ramatowski, & Ruoning Wang")
        P1 <- ggdraw(P1) + theme(panel.background = element_rect(fill = "#FFFFF0", colour = "#FFFFF0")) 
       P2 <- ggplot() + geom_density(data=subplotdata(),aes_string(x=input$Expense),color="darkgreen")+
         labs(x=paste0(names(expchoices)[expchoices == input$Expense],"($)"))+theme(axis.text.x = element_text(size=7),axis.text.y = element_text(size=7))
         
       ggdraw() +
         draw_plot(P1, 0, 0, 1, 1) +
         draw_plot(P2, 0.35, 0.4, 0.5, 0.4)
      
      } else {
        P1 <-  ggplot() + geom_polygon(data=plotdata(),aes_string(x="long",y="lat",group="group"),fill="#ccffcc")+
          coord_map() + 
          theme(axis.ticks = element_blank(),
                axis.line=element_blank(),
                axis.text.x = element_blank(), axis.title.x=element_blank(),
                axis.text.y = element_blank(), axis.title.y=element_blank(),
                panel.border = element_blank(), 
                panel.grid.major = element_blank(), 
                plot.background = element_rect(fill="#FFFFF0"),
                panel.grid.minor = element_blank()) + 
          labs(caption="Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey \nBuilt by Marie Parsons, Patrick Ramatowski, & Ruoning Wang")
        P1 <- ggdraw(P1) + theme(panel.background = element_rect(fill = "#FFFFF0", colour = "#FFFFF0")) 
        P2 <- ggplot() + geom_violin(data=subplotdata(),aes_string(x="REF_RACE",y=input$Expense),color="black",fill="darkgreen")+
          geom_boxplot(data=subplotdata(),aes_string(x="REF_RACE",y=input$Expense),fill="#cdf2dd",width=0.3)+
          scale_y_continuous(limits=c(0,violiny()),breaks=seq(0,violiny(),violiny()/5)) + labs(x="Race",y=paste0(names(expchoices)[expchoices == input$Expense],"($)"))+
          theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9))
        
        ggdraw() +
          draw_plot(P1, 0, 0, 1, 1) +
          draw_plot(P2, 0.35, 0.4, 0.65, 0.5)
      }
      }
  }) 
  

  
    
### For Tab2: Chronological Trends------ Household Expenditures across Time
  plotdatayear <- reactive({tbl_df(fmli.final.year) %>% 
      filter(year %in% input$years, expensetype %in% input$expensetype) })
  
  plotdataquarter <- reactive({tbl_df(fmli.final.quarter) %>% 
      filter(year %in% input$years, expensetype %in% input$expensetype) }) 
  
  output$myplot <- renderPlot({
    if(input$quarter){
      p1 <- ggplot() + geom_area(aes(x=quarter,y=expenseamount,
                                     fill=expensetype),
                                 data=plotdataquarter()) + facet_grid(~year)+
        labs(y="Expense Amount ($)", x="Quarter")+
        scale_fill_manual("Expense Type",values=expense_colors.q[input$expensetype])
    } else{
      p1 <- ggplot() + geom_area(aes(x=year,y=expenseamount, 
                                     fill=expensetype),
                                 data=plotdatayear()) +
        labs(y="Expense Amount ($)", x="Year")+
        scale_x_continuous(breaks=c(2013, 2014, 2015, 2016, 2017))+
        scale_fill_manual("Expense Type",values=expense_colors.y[input$expensetype])
      
    }

    p1 +
      theme_bw()+
      labs(caption = "Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey \nBuilt by Marie Parsons, Patrick Ramatowski, & Ruoning Wang", color="Expense Type")+
      theme(panel.grid.minor = element_blank(), legend.text=element_text(size=12), 
            axis.title=element_text(size=12), axis.text=element_text(size=10),plot.background = element_rect(fill="#FFFFF0"),
            strip.background = element_rect(fill = "#FFFFF0", colour = "black" ),
            legend.background= element_rect(fill = "#FFFFF0", colour = "#FFFFF0"),
            legend.key= element_rect(fill = "#FFFFF0", colour = "#FFFFF0"))
  })
  

  
  
  
#### Tab3 Bar Plot ---- Household Expenditure by Household Traits
  output$FamilyChar <- renderUI({
    conditionalPanel(condition="input.Fill",selectInput(inputId="FamilyChar", label="Pick the Characteristics Related to Family",
                                                        choices=famchar[famchar!= input$HouseFeature],
                                                        selected=c("Education of Houseowner")))
  })
  bardata <- reactive({
    d1 <- intvw2 %>% filter(between(INC_HRS1,input$Hour[1],input$Hour[2]) & between(INCWEEK1,input$Week[1],input$Week[2]) & 
                        between(FAM_SIZE, input$FamSize[1],input$FamSize[2]))
    col <- as.character(input$HouseFeature)
    d1 <- d1 %>% filter(!is.na(get(col)))
  })
  
  output$mybar <- renderPlot({  
    if(input$Fill){
      p1 <- ggplot()+geom_bar(aes(x=reorder(get(input$HouseFeature),get(input$ExpBar)),y=get(input$ExpBar),fill=get(input$FamilyChar)),stat="summary",
                              fun.y="mean",position=position_dodge(preserve = 'single'),data=bardata()) + 
        scale_fill_manual(values=c("#f1595f","#79c36a","#599ad3","#f9a65a","#9e66ab","#cd7058","#d77fb3","#e5b41d"))+
        labs(x= names(famchar)[famchar == input$HouseFeature],
             y= paste0(names(expbar)[expbar == input$ExpBar],"($)"),
             fill=names(famchar)[famchar == input$FamilyChar],
             caption = "Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey \nBuilt by Marie Parsons, Patrick Ramatowski, & Ruoning Wang", color="Expense Type") +
        theme(legend.background= element_rect(fill = "#FFFFF0", colour = "#FFFFF0"),
              legend.key= element_rect(fill = "#FFFFF0", colour = "#FFFFF0"))
    } else{
      p1 <- ggplot()+stat_summary(aes(x=reorder(get(input$HouseFeature),get(input$ExpBar)),y=get(input$ExpBar)),fill="#f9a65a",
                              geom="bar",fun.y="mean",
                              data=bardata()) +
        stat_summary(aes(x=reorder(get(input$HouseFeature),get(input$ExpBar)),y=get(input$ExpBar),label=paste("$",round(..y..,0),sep="")),color="#e69500",
                 geom="text",fun.y="mean",hjust=-0.000005, size=3,
                 data=bardata())+
        labs(x= names(famchar)[famchar == input$HouseFeature],
             y= paste0(names(expbar)[expbar == input$ExpBar],"($)"),
             caption = "Data Source: Bureau of Labor Statistics (BTS) - Consumer Expenditures Survey \nBuilt by Marie Parsons, Patrick Ramatowski, & Ruoning Wang", color="Expense Type")
    }
    p1 +
      theme( legend.text=element_text(size=8),
             axis.text.x = element_text(size=7,face="bold"),
             axis.text.y = element_text(size=9,face="bold"),
             legend.position = "top",
             panel.grid.major.x = element_blank(),
             panel.grid.minor = element_blank(),
             legend.background = element_blank(),
             panel.background = element_rect(fill="white"),
             plot.background = element_rect(fill="#FFFFF0"),
             panel.grid.major = element_line(colour="#f2f2f2")
             ) +
      coord_flip()
  })
  
### Miscellaneous Items: Pop out dialogue box explaining what the app is capable of doing 
  observeEvent(input$Dialog1, {
    shinyalert("What This Tab Does", "This Tab displays an interactive map illustarting the \ngeographic distribution of household expensitures:
              \n
              \n•If users select all regions, then the \nchoropleth will be showing the entire US;
              \n•If users select a specefic region, the map will only display \nthat region. Pie charts illustrating the proportion of the \nproportion of one particular expense to Total Expense \nwill also be displayed. Users will not be able to \nselect Total Expense in this view.
              \n•In the Regional View, only the state that has the highest expenditure to total expenditure proportion is Labeled. 
              \n•If users select a particular state, the map will display \nthat state along with a histogram of the expenditures \nassociated with that state;
              \n•If a particular state is chosen and the race checkbox \nis checked, the graph will display a violin plot showing the distribution of expenditures across different race;
               ", type = "info")
  })
  
  observeEvent(input$Dialog2, {
    shinyalert("What This Tab Does", "This Tab displays a line graph that explores \nthe change in expenses across the years 2013 to 2017. \nEach year is contains four quarters:
               \n
               \n•Quarter 1: January, February, March;
               \n•Quarter 2: April, May, June;
               \n•Quarter 3: July, August, September;
               \n•Quarter 4: October, November, December;
               \n
               \n Users can select the years they are interested in and observe how average household expenditures has changed across the years.
               ", type = "info")
  })
  
  observeEvent(input$Dialog3, {
    shinyalert("What This Tab Does", "This Tab displays an interactive barchart illustarting the \n distribution of household expenditures across different household traits:
              \n
              \n• Ninth through Twelfth grade means the houseowener did not complete highschool;
              \n• First through Eighth grade means the houseowner did not complete primary school or middle school;
              \n• Number of people age below 18 or above 64 is grouped into brackets;
              \n• Users may select the family size/hours \nhouseowner work per week/ weeks houseowner work \nper year and compare how the expenditures change;
               ", type = "info")
  })
  
  
}

shinyApp(ui=ui, server=server)


