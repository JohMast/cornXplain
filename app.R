## app.R ##
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(randomForest)
library(plotly)
library(leaflet)
library(RColorBrewer)
# p1 <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box", width = 300, height = 150)
# p2 <- plot_ly(midwest, x = ~percollege, type = "box", width = 300, height = 150)



#### data ####
data <- readRDS("Data/processed_data.rds") |> 
  filter(!year %in% c("2020","2021"))

counties <- st_read("Supp_Data/counties.gpkg",quiet=T)

variable_descriptions <- 
  list("mean_ndvi" = "Normalized Difference Vegetation Index, an indicator for vegetation health <a href='https://awesome-ee-spectral-indices.readthedocs.io/en/latest/list.html#vegetation'>(Details)</a>, derived from <a href='https://lpdaac.usgs.gov/documents/306/MOD09_User_Guide_V6.pdf'> MODIS </a> satellite imagery.",
       "mean_evi" = "Enhanced Vegetation Index, an indicator for vegetation health in high biomass regions<a href='https://awesome-ee-spectral-indices.readthedocs.io/en/latest/list.html#vegetation'>(Details)</a>, derived from <a href='https://lpdaac.usgs.gov/documents/306/MOD09_User_Guide_V6.pdf'> MODIS </a> satellite imagery.",
       "mean_ndwi" = "Normalized Difference Water Index, an indicator of water content <a href='https://awesome-ee-spectral-indices.readthedocs.io/en/latest/list.html#vegetation'>(Details)</a>, derived from <a href='https://lpdaac.usgs.gov/documents/306/MOD09_User_Guide_V6.pdf'> MODIS </a> satellite imagery.",
       "prcp" = "Total precipitation in the county <a href='https://daac.ornl.gov/DAYMET/guides/Daymet_Daily_V4.html'>(Details)</a>",
       
       "tmin" = "Lowest recorded temperature in the county <a href='https://daac.ornl.gov/DAYMET/guides/Daymet_Daily_V4.html'>(Details)</a>",
       "tmax" = "Highest recorded temperature in the county <a href='https://daac.ornl.gov/DAYMET/guides/Daymet_Daily_V4.html'>(Details)</a>",
       "drought"= "Average of the drought index, a weekly assessment of drought conditions on a scale from 0 to 5  <a href='https://droughtmonitor.unl.edu/'>(Details)</a>"
  )

# full season, for visualisation
data_per_county_and_year_full_season <- 
  data |> 
  group_by(id,year)  |> 
  summarize(mean_ndvi = mean(NDVI,na.rm=T),
            mean_evi=mean(EVI,na.rm=T),
            mean_ndwi=mean(NDWI,na.rm=T),
            drought=mean(drought,na.rm=T),
            prcp=sum(prcp,na.rm=T),
            tmin=min(tmin,na.rm=T),
            tmax=max(tmax,na.rm=T),
            yield=mean(yield,na.rm=T),
  ) |> 
  ungroup()

# data for modeling
data_per_county_and_year_end_of_season <- 
  data |> 
  filter(acq_within_year==27) |>
  group_by(id,year)  |> 
  summarize(srred=mean(srred,na.rm=T),
            srnir=mean(srnir,na.rm=T),
            srblue=mean(srblue,na.rm=T),
            srgreen=mean(srgreen,na.rm=T),
            srnir2=mean(srnir2,na.rm=T),
            srswir2=mean(srswir2,na.rm=T),
            srswir1=mean(srswir1,na.rm=T),
            ndvi = mean(NDVI,na.rm=T),
            evi=mean(EVI,na.rm=T),
            ndwi=mean(NDWI,na.rm=T),
            drought=mean(drought,na.rm=T),
            prcp=sum(prcp,na.rm=T),
            tmin=min(tmin,na.rm=T),
            tmax=max(tmax,na.rm=T),
            par=mean(PAR,na.rm=T),
            yield=mean(yield,na.rm=T)
  ) |> 
  ungroup()


#### model ####
fun_fpar <- function(ndvi){ndvi*1.4371 - 0.4039}  # R2 0.851 for Corn by Yang et al 2012
fun_apar <- function(par,fpar){par*fpar}
fun_biomass <- function (ndvi) {1252 *ndvi - 83.6 }  # relationship between biomass and ndvi is derived by literature by Dhillon et al 2020, 2022a, 2022b
fun_LAI <- function(ndvi){(ndvi*9.75 - 0.311)} # Extra Variable to improve the prediction accuracy Dhillon et al 2022b (in Review Process)

crop_model <- function(x){
  # Calculating FPAR
  x$FPAR <- fun_fpar(x$ndvi)
  x$APAR <- fun_apar(x$par, x$FPAR)
  x$Biomass <- fun_biomass(x$ndvi) # Literature by Dhillon et al 2020, 2022a, 2022b
  x$LAI <- fun_LAI(x$ndvi)
  return(x)
}

data_per_county_and_year_end_of_season <- 
  crop_model(data_per_county_and_year_end_of_season)


forest_data_train <- 
  data_per_county_and_year_end_of_season |>
  filter(year!=2019) |> 
  dplyr::select(-year,-id) |> 
  drop_na()

forest_data_test <- 
  data_per_county_and_year_end_of_season |>
  filter(year==2019) |> 
  # dplyr::select(-year,-id) |> 
  drop_na()

if(!file.exists("Model/forest.rds")){
  
  library(doParallel)
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)
  
  tunegrid <- expand.grid(.mtry = c(1:7)) # divided by three to the total number of predictors
  train.control <- trainControl(method = "repeatedcv", number = 2, repeats = 2, savePredictions = TRUE, search = "grid")
  
  #Train the model
  forest <- train(yield ~ . -year  - id , data = forest_data_train |> select(-year,-id), method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = train.control)

  stopCluster(cl)
  
  # forest <- randomForest(yield ~ . - year  - id ,data=forest_data_train,
  #                        localImp = TRUE)
  saveRDS(forest,"Model/forest.rds")
}else{
  forest <- read_rds("Model/forest.rds")
}

forest_data_test$default_pred_yield <- predict(forest,forest_data_test)

rrmse <-sqrt(mean((forest_data_test$yield - forest_data_test$default_pred_yield)^2))/ mean(forest_data_test$yield)
rsq <- cor(forest_data_test$yield, forest_data_test$default_pred_yield) ^ 2

### prepare for shap calculation

library(treeshap)
shapdata <-
  forest_data_test |> 
  select(-id,-year) 
if(file.exists("Model/treeshap_res.rds")){
  treeshap_res <- "Model/treeshap_res.rds" |> read_rds()
}else{
  model_unified <- randomForest.unify(forest$finalModel,shapdata)
  treeshap_res <- treeshap(model_unified,shapdata)
  treeshap_res |> saveRDS("Model/treeshap_res.rds")
}


treeshap_res$observations <- 
  treeshap_res$observations%>% 
  mutate(across(where(is.numeric), round, 5))



### plots

pyield <- counties |> 
  left_join(forest_data_test |> 
              filter(year==2019),
            by=c("GEOID"="id"))|> 
  ggplot(aes_string(fill="yield"))+
  scale_fill_gradient(low="gray",high="gold")+
  geom_sf()+
  theme_bw()+
  theme(legend.position = "none")
pyield <- ggplotly(pyield,width=NULL,height=NULL)

feature_importance <- ggplotly(plot_feature_importance(treeshap_res, max_vars = 8),width=600,height=300)

###



ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "cornXplain"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Explorer", tabName = "dex", icon = icon("earth-americas")),
      menuItem("Model Explorer", tabName = "mex", icon = icon("bar-chart")),
      menuItem("Scenario Explorer", tabName = "sex", icon = icon("question"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dex",
              h2("Data Explorer"),
              fluidRow(
                box(title = "Predictors",
                    plotOutput("t1pred", height = 250)),
                box(title = "Yield",
                    plotOutput("t1yield", height = 250)),
                
              ),
              fluidRow(
                box(height = 400,
                  # pickerInput("mapvar", "Select variable",
                  #             choices = c("mean_ndvi", "mean_evi","mean_ndwi"  ,"prcp" ,"drought"),
                  #             selected="mean_ndwi"),
                  radioButtons("mapvar","Select variable",
                               selected="mean_ndwi",
                               choices = list("NDVI" = "mean_ndvi",
                                              "EVI" = "mean_evi",
                                              "NDWI" = "mean_ndwi",
                                              "Precipitation" = "prcp",
                                              "Drought Index" = "drought",
                                              "Minimum Temperature" = "tmin",
                                              "Maximum Temperature" = "tmax")),
                  
                  pickerInput(
                    inputId = "mapyear", 
                    label = "Select years", 
                    choices = unique(data$year),
                    selected = unique(data$year),
                    options = list(
                      `actions-box` = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 3"
                    ), 
                    multiple = TRUE
                  ),
                  htmlOutput("t1_var_ex")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "mex",
              h2("Model Explorer"),
              fluidRow(
                box(title = "Model overview",width = 12,
                    img(src = "FLow_DIag_Methods.png"))
                ),
              fluidRow(
                           infoBoxOutput(width=6,"MetricBox"),
                           infoBoxOutput(width=6,"MetricBox2")
                
              ),
####Tab2####
              fluidRow(
                  tabBox(width=12,
                         title = "Variable Importance",
                         id = "tab2",
                         tabPanel("Variable Importance Global",
                                  feature_importance),
                         tabPanel("Variable Importance Local",
                                    leafletOutput("map",height = 300,width=600),
                                    plotlyOutput("t2_contrib",height = 300,width=600))
                         
                  )
                )
              ),
      
      
      # Third Tab content
      
      
      tabItem(tabName = "sex",
              h2("Scenario Explorer"),
              fluidRow(
                column(width=5,
                       box(title = "Scenario editor",solidHeader = TRUE,width=NULL,
                           # sliderInput("t3_ndvi_adjust", "NDVI increase:",min = -2, max = 2,value = 0.0,step=0.1),
                           sliderInput("t3_tmin_adjust", "Minimum temperature decrease:",min = -5, max = 0,value = 0.0,step=0.2),
                           sliderInput("t3_tmax_adjust", "Maximum temperature increase:",min = 0, max = 5,value = 0.0,step=0.2),
                           
                           sliderInput("t3_prc_adjust", "Precipitation increase:",min = -5, max = 5,value = 0.0,step=0.2),
                           radioButtons("t3_drought_adjust_rad","Set drought level",
                                        choices = list("No adjustment" = -1,
                                                       "Normal" = 0,
                                                       "Abnormally Dry" = 1,
                                                       "Moderate Drought" = 2,
                                                       "Severe Drought" = 3,
                                                       "Extreme Drought" = 4,
                                                       "Exceptional Drought" = 5), 
                                        selected = -1),
                           actionButton("t3button_run", "Run Scenario"),
                       ),
                       box(title = "Scenario",
                           width = NULL,
                           verbatimTextOutput("t3_scenario",placeholder = T))),
                column(width=7,
                       box(title = "Effect on Yield",width=NULL,
                           plotOutput(outputId = "t3results",height=180)),
                       box(title = "Spatial Effect",width=NULL,
                           plotOutput(outputId = "t3resultsmap"))
                )
              )
      )
    )
  )
)

server <- function(input, output) {

  ############### TAB 1 ########################

  output$t1pred <- renderPlot({
    counties |> 
      left_join(data |> 
                  filter(year %in% input$mapyear)|> 
                  group_by(id) |> 
                  summarize(mean_ndvi = mean(NDVI,na.rm=T),
                            mean_evi=mean(EVI,na.rm=T),
                            mean_ndwi=mean(NDWI,na.rm=T),
                            drought=mean(drought,na.rm=T),
                            prcp=sum(prcp,na.rm=T),
                            tmin=min(tmin,na.rm=T),
                            tmax=max(tmax,na.rm=T),
                            yield=mean(yield,na.rm=T),
                  ),
                by=c("GEOID"="id"))|> 
      ggplot(aes_string(fill=input$mapvar))+
      geom_sf()+
      theme_bw()+
      theme(legend.position = "none")
  })
  
  output$t1yield <- renderPlot({
    counties |>
      left_join(data |>
                  filter(year %in% input$mapyear)|>
                  group_by(id) |>
                  summarize(yield=mean(yield,na.rm=T)),
                by=c("GEOID"="id"))|>
      ggplot(aes_string(fill="yield"))+
      scale_fill_gradient(low="gray",high="gold")+
      geom_sf()+
      theme_bw()+
      theme(legend.position = "none")
  })
  
  output$t1_var_ex <- renderText(variable_descriptions[[input$mapvar]])
  ############### TAB 2 ########################
  
  observe({ 
    event <- input$map_shape_click
    output$cnty <- renderText(shape$NAME[shape$CNTY_ID == event$id])
    
  })
  
  output$MetricBox <- renderInfoBox({
    infoBox(
      "RRMSE", round(rrmse,4)*100, icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$MetricBox2 <- renderInfoBox({
    infoBox(
      "R²", round(rsq,4), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # output$t2_pyield <- 
  #   renderPlotly(pyield)
  
  
  leaflet_data <- counties |> 
    left_join(forest_data_test |> 
                filter(year==2019) ,
              by=c("GEOID"="id"))|> 
    filter(!is.na(yield))
  
  # setting the colors
  colpal <- colorNumeric(palette = "inferno", domain=leaflet_data$yield, n=10)
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      # addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = leaflet_data, 
                  fillColor =  ~colpal(yield),
                  label=~yield,
                  layerId = ~GEOID )
  })
  observe({ 
    event <- input$map_shape_click
    if(is.null(event$id)){
      obs_id <- 1}else{
        obs_id <- which(forest_data_test$id == as.character(event$id), arr.ind=TRUE)
      }
    if(is.null(obs_id)){obs_id <- 1}
    # if(is.na(obs_id)){obs_id <- 1}
    output$yieldclick <- renderText(obs_id)
    output$t2_contrib <- 
      renderPlotly(ggplotly(plot_contribution(treeshap_res, obs = obs_id,digits=5)))
  })
  
  
  output$yieldclick <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
    

  
  ############### TAB 3 ########################
  # 
  # 
  prediction <- eventReactive(input$t3button_run, {
    
    scenario_desc <- paste0("Simulating Scenario:\n")
    
    forest_data_scen <- forest_data_test |> 
      crop_model()
    if(input$t3_tmin_adjust!=0){
      forest_data_scen <- forest_data_test|>
        mutate(tmin  = tmin + input$t3_tmin_adjust)
      
      scenario_desc <- 
        paste0(scenario_desc,"Min temperature decreased by ", (-1*input$t3_tmin_adjust), "\n")
    }
    
    if(input$t3_tmax_adjust!=0){
      forest_data_scen <- forest_data_test|>
        mutate(tmax  = tmax + input$t3_tmax_adjust)
      
      scenario_desc <- 
        paste0(scenario_desc,"Max temperature increased by ", input$t3_tmax_adjust, "\n")
    }
    
    if(input$t3_prc_adjust!=0){
      forest_data_scen <- forest_data_test|>
        mutate(prcp = prcp  +input$t3_prc_adjust)
      
      scenario_desc <- 
        paste0(scenario_desc,"Precipitation adjusted by ", input$t3_prc_adjust, "\n")
    }

    if(input$t3_drought_adjust_rad!=-1){
      forest_data_scen <- forest_data_scen |>
        mutate(drought = as.integer(input$t3_drought_adjust_rad))
      
      scenario_desc <- 
        paste0(scenario_desc,"Drought category set to: ", input$t3_drought_adjust_rad, "\n")
    }
    
    forest_data_test$scen_pred_yield <- predict(forest,forest_data_scen)
    x <- 
      forest_data_test |> 
      mutate(id=fct_reorder(id,default_pred_yield,.desc = T)) |> 
      mutate(change=scen_pred_yield-default_pred_yield)
    
    
    
    output$t3_scenario <- renderText(scenario_desc)
    
    x
  })
  # 
  # output$t3results <- renderText({prediction()$scen_pred_yield})

  output$t3results <- renderPlot({
  prediction() |>
      dplyr::select(id,default_pred_yield,scen_pred_yield,change) |> 
      # pivot_longer(cols=-id) |> 
      ggplot() +
      geom_segment( aes(x=id, xend=id, y=scen_pred_yield, yend=default_pred_yield, color=change),alpha=0.3)+
      geom_point(aes(id,default_pred_yield),col="#fad605")+
      geom_point(aes(id,scen_pred_yield,col=change,shape=(change>=0)))+
      labs(y="yield",x="counties")+
      scale_color_gradient2(high = "green",low="red",mid="#fad605",midpoint = 0)+
      scale_shape_manual(values = c(6,2))+
      theme_bw()+
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank())
  })
  
  output$t3resultsmap <- renderPlot({
    counties |>
      left_join(prediction() |> 
                  dplyr::select(id,change),
                by=c("GEOID"="id")) |>
      ggplot(aes(fill=change))+
      geom_sf()+
      theme_bw()+
      theme(legend.position = "none")+
    scale_fill_gradient2(low = "red",high="green",mid="#fad605",midpoint = 0)
  })
  
}
shinyApp(ui, server)
