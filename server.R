#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

privacy.text <- "Apologies, we cannot display graphs of data from\nless than five individuals for reasons of data privacy."
privacy.minimum <- 0

confidentiality.check <- function(data, fn, min.rows = 5, ...){
  args <- list(...)
  if(nrow(data) >= min.rows){
    exec(fn, data, !!!args)
  } else {
    ggplot() + annotate(geom = "text", x=0, y=0, label = privacy.text) + theme_void()
  } 
}

server <- function(input, output) {
  
  output$agePyramid <- {
    filtered.data.ap <- reactive({
      fd <- patient.data %>% 
        filter(!is.na(outcome)) %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(sex)) %>%
        filter(!is.na(consolidated.age))
    })
    renderPlot(confidentiality.check(patient.data, age.pyramid), height = 300)
  }
  
  output$comorbidityPrevalence <- {
    filtered.data.cp <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(comorbidities.recorded)
    })
    renderPlot(confidentiality.check(patient.data, comorbidity.prevalence.plot), height = 300)
  }
  
  output$symptomPrevalence <- {
    filtered.data.sp <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(symptoms.recorded)
    })
    renderPlot(confidentiality.check(patient.data, symptom.prevalence.plot), height = 300)
  }
  
  
  output$comorbiditiesUpset <- {
    filtered.data.cu <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(comorbidities.recorded)
    })
    renderPlot(confidentiality.check(patient.data, comorbidities.upset), height = 300)
  }
  
  output$symptomsUpset <- {
    filtered.data.su <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(symptoms.recorded & comorbidities.recorded)
    })
    renderPlot(confidentiality.check(patient.data, symptoms.upset), height = 300)
  }
  
  output$outcomesByAdmissionDate <- {
    filtered.data.obad <- reactive({
      fd <- patient.data %>% 
        filter(!is.na(outcome)) %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(hostdat))
    })
    renderPlot(confidentiality.check(patient.data, outcomes.by.admission.date), height = 300)
  }
  
  output$violinAgeFunc <- 
    {
      filtered.data.vaf <- reactive({
        fd <- patient.data %>% 
          filter(Country %in% input$Country) %>%
          filter(outcome %in% input$outcome) %>%
          filter(sex %in% input$sex) %>%
          filter(!is.na(consolidated.age)) %>%
          filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2])
      })
      renderPlot(confidentiality.check(backup.data, violin.age.func), height = 300)
    }
  
  output$violinSexFunc <- 
    {
      filtered.data.vsf <- reactive({
        fd <- patient.data %>% 
          filter(Country %in% input$Country) %>%
          filter(outcome %in% input$outcome) %>%
          filter(sex %in% input$sex) %>%
          filter(!is.na(sex) & sex != 3) %>%
          filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2])
      })
      renderPlot(confidentiality.check(backup.data, violin.sex.func), height = 300)
    }
  
  output$statusByTimeAfterAdmission <- {
    filtered.data.sbtaa <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(!is.na(consolidated.age)) %>%
        filter(!is.na(hostdat))
    })
    renderPlot(confidentiality.check(filtered.data.sbtaa(), status.by.time.after.admission, height = 300))
  }
  
  output$treatmentPlot <- {
    filtered.data.tp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(treatments.recorded)
    })
    renderPlot(confidentiality.check(backup.data, treatment.use.plot), height = 300)
  }


  output$treatmentUpset <- {
    filtered.data.tu <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(treatments.recorded)
    })
    renderPlot(confidentiality.check(backup.data, treatment.upset), height = 300)
  }
  
  
  output$onsetAdmPlot <- {
    filtered.data.oap <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(onset.to.admission))
    })
    renderPlot(confidentiality.check(patient.data, onset.adm.plot), height = 300)
  }
  
  output$admOutcomePlot <- {
    filtered.data.aop <- reactive({
      fd <- patient.data %>%
        filter(!is.na(outcome)) %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.to.exit) | !is.na(start.to.censored))
    })
    renderPlot(confidentiality.check(backup.data, adm.outcome.plot), height = 300)
  }
  
  output$modifiedKMPlot <- {
    filtered.data.aop <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.to.exit) | !is.na(start.to.censored))
    })
    renderPlot(confidentiality.check(filtered.data.aop(), modified.km.plot, height = 300))
  }
  
  
  output$sitesByCountry <- {
    filtered.data.sbc <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2])
    })
    renderPlot(confidentiality.check(filtered.data.sbc(), sites.by.country, height = 300))
  }
  
  output$outcomesByCountry <- {
    filtered.data.obc <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(Country))
    })
    renderPlot(confidentiality.check(filtered.data.obc(), outcomes.by.country, height = 300))
  }
  
  output$recruitmentDatPlot <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, admissions.ts), height = 300)
  }
  
  #FORECAST
  output$admissionLSTM <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, admission.lstm), height = 300)
  }
  
  output$positiveLSTM <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, positive.lstm), height = 300)
  }
  
  output$activeLSTM <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, active.lstm), height = 300)
  }
  
  output$deathRNN <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, death.rnn), height = 300)
  }
  
  output$bedLSTM <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, bed.lstm), height = 300)
  }
  
  output$icuLSTM <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, icu.lstm), height = 300)
  }
  #MORTALITY AND HOSPITAL LOS
  output$impDEATH1 <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, imp.death1), height = 300)
  }
  
  output$impDEATH2 <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, imp.death2), height = 300)
  }
  
  output$impLOS1 <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, imp.los1), height = 300)
  }
  
  output$impLOS2 <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(backup.data, imp.los2), height = 300)
  }
  
  output$download <- downloadHandler(
    filename = function(){
      paste0("report",".pdf")
    },
    content = function(file){
      x <- read_file_raw(glue("{data.path}COV-report.pdf"))
      write_file(x, file)
    }
  )
  
  
  

# output$tree <- renderTree({ 
#   list(  'I lorem impsum'= list( 
#     'I.1 lorem impsum'   =  structure(list('I.1.1 lorem impsum'='1', 'I.1.2 lorem impsum'='2'),stselected=TRUE),  
#     'I.2 lorem impsum'   =  structure(list('I.2.1 lorem impsum'='3'), stselected=TRUE))) 
# })
}
