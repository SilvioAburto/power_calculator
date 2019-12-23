#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(includeCSS(file.path("www", "style.css"))),
    useShinyjs(),
    # Application title
    title="Power Simulator",
    div(id="header",
        h2("Power Simulator"),
        span(
            style = "font-size: 1.2em",
            span("Created by "),
            a("Silvio Ortiz", href = "https://www.linkedin.com/in/silvio-ortiz-aburto/"),
            HTML("&bull;"),
            span("Code"),
            a("on GitHub", href = "https://github.com/SilvioAburto/power_calculator"),br(),
            span("December, 2019")
        )
    ),
    column(width=3, 
           wellPanel(  
                   sliderInput("cv", label = h3("%CV"), min = 0, 
                               max = 100, value = 15),
                   sliderInput("effect_percent", label = h3("% Improvement"), min = 0, 
                               max = 100, value = 20),hr(),
                   sliderInput("pwr", label = "Power", min=0.1,max=0.9, value=0.8,step = 0.05),hr(),
                   numericInput('avg', 'Average', 25, min = 0, max=10000),
                   sliderInput("ci_simulate", label = "Play Simulation", min=1,max=100, value=0,step = 1, animate = animationOptions(interval=1000))
                   
                                           
                              )),
                              column(width=9,
                                  plotOutput('power_plot'),br(),
                                  plotOutput('ci_plot')
                              )
                          )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$power_plot <- renderPlot({
        mean <- input$avg
        cv <- input$cv
        sd <- (cv*mean)/100
        
        
        improvement <- seq(1,100)
        
        ptable <- cbind(NULL,NULL)
        for(i in improvement){
            difference <- (i/100)*mean
            pwrt <- power.t.test(delta=difference,sd=sd,power=input$pwr,sig.level = .05, alternative = "two.sided")
            ptable <- rbind(ptable, cbind(i, pwrt$n))
            
        }
        
        ptable <- as.data.frame(ptable)
        colnames(ptable) <- c("percent_improvement", "sample_size")
        sample_size_tag <- ptable[ptable$percent_improvement == input$effect_percent, ]$sample_size
        
        
        ggplot(ptable %>% filter(sample_size < 100), aes(x=percent_improvement, y=sample_size, label = paste0("N=",round(sample_size_tag)))) +
            geom_line() +
            geom_point(size=3, color="blue") +
            labs(x='Effect Size(%)', y= 'Sample Size (n)', title= paste0('Effect Size (%) vs Sample Size at CV=',cv, '%' )) +
            geom_vline(xintercept = input$effect_percent, linetype="dotted", 
                       color = "red", size=1.5) +
            geom_label(aes(x=input$effect_percent, y=50), size=9) +
            scale_y_continuous(breaks = seq(0,105, by=10)) +
            scale_x_continuous(breaks = seq(0,105, by=10))+
            theme_gray(base_size = 25) +
            theme(plot.title = element_text(hjust = 0.5)) 
        
    })
    
    output$ci_plot <- renderPlot({
        input$ci_simulate
        mean1 <- input$avg
        mean2 <- input$avg + (input$avg*(input$effect_percent/100))
        sd1 <- (input$cv*mean1)/100
        sd2 <- (input$cv*mean2)/100
        rep_sequence <- seq(2,50, by=4)
        data_list <- list() # empty data list
        for(r in 1:length(rep_sequence)){
            d <- rnorm(rep_sequence[r], mean = mean1, sd = sd1)
            d2 <- rnorm(rep_sequence[r], mean = mean2, sd = sd2)
            df <- data.frame("value"=d, "sample"="Sample 1")
            df2 <- data.frame("value"=d2, "sample"="Sample 2")
            df3 <- rbind(df, df2)
            df3$rep_n <- rep_sequence[r]
            data_list[[r]] <- df3
        }
        
        all_data <- do.call(rbind, data_list)
        all_data_summ <- all_data %>%
            group_by(sample, rep_n) %>%
            summarise(N= n(),
                      mean = mean(value),
                      sd = sd(value)
            ) %>%
            ungroup() %>%
            mutate(se = sd / sqrt(N),
                   ci_lower = mean - qt(1 - (0.05/2), N -1)* se,
                   ci_upper = mean + qt(1 - (0.05/2), N -1)* se,
            ) 
        different_tag <- (all_data_summ %>% filter(sample == 'Sample 2') %>% select(N, ci_lower) ) %>%
            left_join(all_data_summ %>% filter(sample == 'Sample 1') %>% select(N, ci_upper), by=c("N"="N")) %>%
            mutate(different = ifelse( ci_lower > ci_upper ,'yes', 'no'))            
        
        all_data_summ1 <- all_data_summ %>%
            left_join(different_tag %>% select(N, different), by=c("N"="N"))

        ggplot(all_data_summ1, aes(rep_n, mean)) +
            geom_errorbar(aes(x=rep_n, ymin=ci_lower, ymax= ci_upper, group=sample),position=position_dodge(width=1.5), width=1.5,size=1, colour="red") +
            geom_point(data= filter(all_data_summ1, different == "yes"), colour="black", size=8,stroke=2, aes(rep_n,mean,group=sample),position=position_dodge(width=1.5), shape=0) + 
            geom_point(position=position_dodge(width=1.5), size=4, pch=21, aes(fill=sample)) +
            scale_x_continuous(breaks = rep_sequence) +
            labs(x="Replication", y="Average", title= paste0('Sample Means with 95% Confidence Interval Bars at CV=',input$cv, '%')) +
            theme_gray(base_size = 25) +
            theme(plot.title = element_text(hjust = 0.5)) 
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
