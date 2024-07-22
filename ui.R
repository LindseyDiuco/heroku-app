#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




dbHeader <- dashboardHeader(title = "COVID-19 Analysis Report",
                            tags$li(a(href = 'http://isaric.tghn.org/',
                                      img(src = 'ISARIClogo.png',
                                          title = "Company Home", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown"
                                    # tags$style(".main-header {max-height: 40px}"),
                                    # tags$style(".main-header .logo {height: 40px;}"),
                                    # tags$style(".sidebar-toggle {height: 40px; padding-top: 1px !important;}"),
                                    # tags$style(".navbar {min-height:40px !important}")
                            ))

# Define UI for application that draws a histogram
dashboardPage( skin = "black",
               # dashboardHeader(title = tags$a(href='http://mycompanyishere.com', tags$img(src='/www/ISARIClogo.png'))),   #
               dbHeader,
               dashboardSidebar(
                 # tags$style(".left-side, .main-sidebar {padding-top: 50px}"),
                 sidebarMenu(
                   menuItem("Summary", tabName = "Summary", icon = icon("list")),
                   menuItem("Patient Characteristics", tabName = "patients", icon = icon("bed")),
                   menuItem("Symptoms and Comborbidities", tabName = "SymCom", icon = icon("stethoscope")),
                   menuItem("Treatment", tabName = "treatment", icon = icon("pills")),
                   menuItem("Statistical Analysis", tabName = "stats", icon = icon("chart-bar")),
                   #menuItem("Country Comparisons", tabName = "ccomp", icon = icon("globe")),
                   menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
                   #menuItem("Background", tabName = "Background", icon = icon("notes-medical")),
                   menuItem("Mortality and Hospital Stay", tabName = "ml", icon = icon("heartbeat")),
                   menuItem("Download Report", tabName = "Report", icon = icon("wrench")),
                   #menuItem("Caveats", tabName = "Caveats", icon = icon("exclamation")),
                   menuItem("Summary Tables", tabName = "SummaryTables", icon = icon("table"))
                   #menuItem("Team", tabName = "TeamMembers", icon = icon("users")),
                   #menuItem("References", tabName = "References", icon = icon("bars"))
                 ),
                 
                 hr(),
                 fluidRow(column(3, verbatimTextOutput("value")))
               ),
               dashboardBody(
                 tags$head(tags$style(HTML('
                 .btn-custom {background-color: #F70656; color:  #FFFFFF;}
                 .skin-black .main-header .logo {color: #F70656; font-weight: bold;}
                 .skin-black .main-sidebar .sidebar .sidebar-menu .active a {color: #F70656; border-left-color: #F70656;}
                 .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from, .irs-grid-pol {background: #F70656; border-color: #F70656;}'
                 ))),
                 hr(),
                 tabItems(
                   tabItem(tabName = "Summary",
                           h1("ISARIC COVID-19 Report Dashboard"),
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12, title = "Summary",
                                 
                                 includeMarkdown("markdown/summary.md")
                                 
                             )
                           )),
                   tabItem(tabName = "patients",
                           fluidRow(
                             box(plotOutput("agePyramid", height = "300px"), 
                                 "Bar fills are outcome (death/discharge/censored) at the time of report.", 
                                 width = 6, height = 500, solidHeader = T, title = 'Age and sex distribution of patients'),
                             box(plotOutput("outcomesByAdmissionDate", height = "300px"),
                                 "Dates are of admission for patients admitted with COVID-19, and date of onset for diagnoses in existing hospital patients",
                                 width = 6, height = 500, solidHeader = T, title = 'Patient outcomes by epidemiological week of admission or symptom onset')
                           ),
                           fluidRow(
                             box(plotOutput("violinAgeFunc", height = "300px"),
                                 "Hospital stay considers the time to death, recovery or censorship.",
                                 width = 6, height = 500, solidHeader = T, title = 'Distribution of length of hospital stay by patient age group'),
                             box(plotOutput("violinSexFunc", height = "300px"),
                                 "Hospital stay considers the time to death, recovery or censorship.",
                                 width = 6, height = 500, solidHeader = T, title = 'Distribution of length of hospital stay by patient sex'),
                           )
                   ),
                   tabItem(tabName = "SymCom",
                           fluidRow(
                             box(plotOutput("comorbidityPrevalence", height = "300px"), 
                                 "Proportions are amongst patients for whom these data were recorded.", 
                                 width = 6, height = 500, solidHeader = T, title = 'Frequency of comorbidities seen at admission amongst COVID-19 patients.'),
                             box(plotOutput("comorbiditiesUpset", height = "300px"),
                                 "Proportions are amongst patients for whom these data were recorded. Filled and empty circles below the x-axis indicate the presence 
                                 or absence of each comorbidity. The 'Any other' category contains all remaining comorbidities in the left plot, and any other comorbidities recorded as free text by clinical staff.",
                                 width = 6, height = 500, solidHeader = T, title = 'Frequency of combinations of the four most common comorbidities')
                           ),
                           fluidRow(
                             box(plotOutput("symptomPrevalence", height = "300px"),
                                 "Proportions are amongst patients for whom these data were recorded.",
                                 width = 6, height = 500, solidHeader = T, title = 'Frequency of symptoms seen at admission amongst COVID-19 patients.'),
                             box(plotOutput("symptomsUpset", height = "300px"),
                                 "Proportions are amongst patients for whom these data were recorded. Filled and empty circles below the x-axis indicate the presence 
                                 or absence of each symptom The 'Any other' category contains all remaining symptoms in the left plot.",
                                 width = 6, height =500, solidHeader = T, title = 'Frequency of combinations of the four most common symptoms'),
                           )
                   ),
                   tabItem(tabName = "treatment",
                           fluidRow(
                             box(plotOutput("treatmentPlot", height = "300px"), 
                                 "Proportions are amongst patients for whom these data were recorded.", 
                                 width = 6, height = 500, solidHeader = T, title = 'Treatments used.'),
                             box(plotOutput("treatmentUpset", height = "300px"),
                                 "Calculated across all patients with completed hospital stay and recorded treatment data. Filled and empty circles below the x-axis indicate the presence or absence of each symptoms",
                                 width = 6, height = 500, solidHeader = T, title = 'Frequency of combinations of treatments administered during hospital stay.')
                           )
                   ),
                   tabItem(tabName = "stats",
                           fluidRow(
                             box(plotOutput("onsetAdmPlot", height = "300px"), 
                                 "The blue curve is the Gamma distribution fit to the data. The black dashed line indicates the position of the expected mean. Expected estimates were derived by fitting a Gamma distribution to the observed data. Expected estimates, accounting for unobserved outcomes, are provided under 'Summary Tables'", 
                                 width = 6, height = 500, solidHeader = T, title = 'Distribution of time from symptom onset to admission.'),
                             box(plotOutput("admOutcomePlot", height = "300px"),
                                 "The blue curve is the Gamma distribution fit to the data. The black dashed line indicated the position of the expected mean. Expected estimates were derived by fitting a Gamma distribution to the observed data. Expected estimates, accounting for unobserved outcomes, are provided under 'Summary Tables'",
                                 width = 6, height = 500, solidHeader = T, title = 'Distribution of time from admission to an outcome (death or discharge).')
                           )#,
                           #fluidRow(
                           #  box(plotOutput("modifiedKMPlot", height = "400px"),
                           #      "Times are measured from admission for patients admitted with COVID-19, and from onset for diagnoses in existing hospital patients. Deaths are in red and recoveries in green. The black line indicates the case fatality ratio. The method used here considers all cases, irrespective of whether an outcome has been observed. For a completed epidemic, the curves for death and recovery meet. Estimates were derived using a nonparametric Kaplan-Meier–based method proposed by Ghani <i>et al.</i> (2005).",
                           #      width = 6, height = 425, solidHeader = T, title = 'Nonparametric probabilities of death and recovery over time since hospital admission or symptom onset')
                           #)
                   ),
                   tabItem(tabName = "ccomp",
                           fluidRow(
                             box(plotOutput("sitesByCountry", height = "300px"),
                                 "Only sites with at least one patient meeting the current dashboard settings are present here",
                                 width = 6, height = 500, solidHeader = T, title = 'Number of sites per country'),
                             box(plotOutput("outcomesByCountry", height = "300px"),
                                 width = 6, height = 500, solidHeader = T, title = 'Distribution of patients by country and outcome')
                           )
                   ),
                   tabItem(tabName = "forecasting",
                           fluidRow(
                             box(tags$p("Note: The models used to generate the forecasts were last built on 2024-07-10."), 
                                 width = 12, solidHeader = TRUE)
                           ),
                           fluidRow(
                             box(
                               plotOutput("admissionLSTM", height = "300px"),
                               "Time Series plot of newly hospital-admitted patients per day and its 30-day forecast.",
                               width = 12, height = 500, solidHeader = TRUE, title = 'Daily New COVID-19 Hospital Admissions'
                             )
                           ),
                           fluidRow(
                             box(
                               plotOutput("positiveLSTM", height = "300px"),
                               "Time Series plot of newly-tested COVID-19 positive patients per day and its 30-day forecast.",
                               width = 12, height = 500, solidHeader = TRUE, title = 'Daily New COVID-19 Positive Cases'
                             )
                           ),
                           fluidRow(
                             box(
                               plotOutput("activeLSTM", height = "300px"),
                               "Time Series plot of active COVID-19 cases per day and its 30-day forecast.",
                               width = 12, height = 500, solidHeader = TRUE, title = 'Daily COVID-19 Active Cases'
                             )
                           ),
                           fluidRow(
                             box(
                               plotOutput("deathRNN", height = "300px"),
                               "Time Series plot of new COVID-19 deaths per day and its 30-day forecast.",
                               width = 12, height = 500, solidHeader = TRUE, title = 'Daily New COVID-19 Deaths'
                             )
                           ),
                           fluidRow(
                             box(
                               plotOutput("bedLSTM", height = "300px"),
                               "Time Series plot of bed demands per day and its 30-day forecast.",
                               width = 12, height = 500, solidHeader = TRUE, title = 'Daily COVID-19 Bed Demands'
                             )
                           ),
                           fluidRow(
                             box(
                               plotOutput("icuLSTM", height = "300px"),
                               "Time Series plot of new ICU admissions per day and its 30-day forecast.",
                               width = 12, height = 500, solidHeader = TRUE, title = 'Daily New Hospital ICU Admissions'
                             )
                           )
                          ),
                   tabItem(tabName = "ml",
                           fluidRow(
                             box(plotOutput("impDEATH1", height = "300px"), 
                                 "Significant predictors of death due to COVID-19. SHAP values indicate the contribution of each feature to COVID-19 mortality prediction. Higher SHAP values denote a stronger influence on the outcome death.", 
                                 width = 6, height = 500, solidHeader = T, title = 'Significant Predictors of COVID-19 Mortality'),
                             box(plotOutput("impDEATH2", height = "300px"),
                                 "How each significant predictor contributes to COVID-19 mortality outcomes. Predictors with positive SHAP values contribute to an increased risk of death. The further the SHAP value is from zero, the stronger the contribution to higher risk. For predictors with red points and positive SHAP values, high values of these predictors increase the risk of death. ",
                                 width = 6, height = 500, solidHeader = T, title = 'Contribution of Significant Predictors to COVID-19 Mortality')
                           ),
                           fluidRow(
                             box(plotOutput("impLOS1", height = "300px"),
                                 "Significant predictors of length of hospital stay. SHAP values indicate the contribution of each feature to hospitalization length prediction. Higher SHAP values denote a stronger influence on the length of hospital stay.", 
                                 width = 6, height = 500, solidHeader = T, title = 'Significant Predictors of Length of Hospital Stay'),
                             box(plotOutput("impLOS2", height = "300px"),
                                 "How each significant predictor contributes to hospitalization length outcomes. Predictors with positive SHAP values contribute to a longer hospital stay. The further the SHAP value is from zero, the stronger the contribution to longer hospital stay. For predictors with red points and positive SHAP values, high values of these predictors increase the risk of longer hospitalization.",
                                 width = 6, height = 500, solidHeader = T, title = 'Contribution of Significant Predictors to the Length of Hospital Stay'),
                           )
                   ),
                   tabItem(tabName = "Background",
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12, title = "Background",
                                 
                                 includeMarkdown("markdown/background.md")
                                 
                             )
                           )),
                   tabItem(tabName = "Report",
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12, title = "Download Report",
                                 
                                 downloadButton("download", "Download Report")
                             
                             )
                           )),
                   tabItem(tabName = "Caveats",
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12, title = "Caveats",
                                 
                                 includeMarkdown("markdown/caveats.md")
                                 
                             )
                           )),
                   tabItem(tabName = "SummaryTables",
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12, title = "Summary Tables",
                                 
                                 includeMarkdown("markdown/summarytables.md")
                                 
                             )
                           )),
                   tabItem(tabName = "TeamMembers",
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12, title = "ISARIC Team Members",
                                 
                                 includeMarkdown("markdown/teammembers.md")
                                 
                             )
                           )),
                   tabItem(tabName = "References",
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12, title = "References",
                                 
                                 includeMarkdown("markdown/references.md")
                                 
                             )
                           ))
                 )
               )
)
