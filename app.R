library(shiny)
library(ggplot2)
library(latex2exp)
library(markdown)
library(gridExtra)



ui <- fluidPage(withMathJax(),
                titlePanel(h1('Steady state of a simple labor search-and-matching model ')),
                sidebarLayout(
                  sidebarPanel(
                    p(
                      'This application shows the predicted steady state wages, market tightness, vacancies, and unemployment for a perfect foresight, rational-expectations version of the labor search and matching model, a-la Diamond, Mortensen and Pissarides. This version of the model is based on the presentation in Chapter 1 of EQUILIBRIUM UNEMPLOYMENT THEORY, second edition, by Christopher Pissarides, assuming (1) no capital and (2) a Cobb-Douglas matching function.'
                    ),
                    p(
                      'Recall that \\(w\\) is wages,\\(v\\)  and \\(u\\)  are vacancies and unemployment, \\(\\theta \\equiv v/u \\)  is market tightness, \\(z\\)  is match productivity, \\(k \\) is the vacancy post cost, \\(\\delta\\)  is the exogenous rate of separation, \\(\\beta = \\frac{1}{1+r} \\) is the discount rate for payoffs where \\( r \\) is the real interest rate, \\(a \\) is the bargaining weight parameter in the Nash bargaining problem for wages, and \\(\\alpha \\) is the Cobb-Douglas matching elasticity',
                      'Wages and market tightness, in steady state, are determined jointly by a job creation condition $$ w = z - \\frac{k}{e \\cdot m \\left(\\frac{1}{\\theta}, 1\\right)} (r + \\delta) $$',
                      'and a wage curve $$ w = b + a( z- b + k \\theta)$$'
                    ),
                    p(''),
                    p('Below you can pick different values for each parameter of the model.  and for the Cobb-Douglas parameter in the matching function, and compare the steady states'),
                    actionButton("plot", "Plot and calculate"),
                    h3("Parameters"),
                    sliderInput(
                      "z1",
                      "Initial Productivity  (\\(z\\))",
                      min = 0.5,
                      max = 2,
                      value = 1
                    ),
                    sliderInput(
                      "z2",
                      "New Productivity  (\\(z\\))",
                      min = 0.5,
                      max = 2,
                      value = .75
                    ),
                    sliderInput(
                      "e1",
                      "Initial match efficiency  (\\(e\\))",
                      min = .5,
                      max = 1.5,
                      value = .5
                    ),
                    sliderInput(
                      "e2",
                      "New match efficiency  (\\(e\\))",
                      min = .5,
                      max = 1.5,
                      value = .5
                    ),
                    sliderInput(
                      "k1",
                      "Initial vacancy posting cost \\(k\\)",
                      min = 0.01,
                      max = .75,
                      value = .5
                    ),
                    sliderInput(
                      "k2",
                      "New vacancy posting cost \\(k\\)",
                      min = 0.01,
                      max = .75,
                      value = .5
                    ),
                    sliderInput(
                      "b1",
                      "Initial value of outside option \\(b\\)",
                      min = 0.001,
                      max = .999,
                      value = .5
                    ),
                    sliderInput(
                      "b2",
                      "New value of outside option \\(b\\)",
                      min = 0.001,
                      max = .999,
                      value = .5
                    ),
                    sliderInput(
                      "delta1",
                      "Initial job destruction probability (\\(\\delta\\))",
                      min = 0.001,
                      max = .999,
                      value = .025
                    ),
                    sliderInput(
                      "delta2",
                      "New job destruction probability (\\(\\delta\\))",
                      min = 0.001,
                      max = .999,
                      value = .025
                    ),
                    sliderInput(
                      "r1",
                      "Initial interest rate (\\(r\\))",
                      min = 0.001,
                      max = .2,
                      value = 0.03
                    ),
                    sliderInput(
                      "r2",
                      "New interest rate (\\(r\\))",
                      min = 0.001,
                      max = .2,
                      value = 0.03
                    ),
                    sliderInput(
                      "a1",
                      "Initial bargaining parameter (\\( a\\))",
                      min = 0.001,
                      max = .999,
                      value = .5
                    ),
                    sliderInput(
                      "a2",
                      "New bargaining parameter (\\(a\\))",
                      min = 0.001,
                      max = .999,
                      value = .5
                    ),
                    sliderInput(
                      "alfa1",
                      "Initial Cobb-Douglas matching elasticity (\\(\\alpha\\))",
                      min = 0.001,
                      max = .999,
                      value = .5
                    ),
                    sliderInput(
                      "alfa2",
                      "New Cobb-Douglas matching elasticity (\\(\\alpha\\))",
                      min = 0.001,
                      max = .999,
                      value = .5
                    )
                  ),
                  mainPanel(
                    h1("JC and WC curves"),
                    plotOutput('dmp_plot_wcjc'),
                          h1("Beveridge curve and market tightness"),
                          plotOutput('dmp_bc')
                  )
                )
)
server <- function(input, output) {
  
  
  
  # 
  # udot <- function(e,alfa,theta, delt, u ){
  #   delt*(1-u) - theta*qthet(e,alfa, theta) * u
  # }
  
  
  # v in terms of u on beveridge curve

  
  wc_jc_df <- reactive({
    input$plot
    isolate({
      
      # WC <- function(b, a, z, k, theta){
      #   b + a*(z - b+ k*theta)
      # }
      # # probability job filled 
      # qthet <- function(e,alfa, theta){
      #   e*(1/theta)^alfa
      # }
      # 
      # JC <- function(z, r, delt, k, e, alfa,theta){
      #   z - (r+delt)*k/(qthet(e,alfa,theta))
      # }
      # 
      
      df <- data.frame(theta = seq(from = 0, to = 50, length.out = 1000)) 
      
      df <- df %>% 
        mutate(`Initial JC` = input$z1 - (input$r1+input$delta1)*input$k1/( input$e1*(1/theta)^input$alfa1  ), # JC(input$z1, input$r1, input$delta1, input$k1, input$e1, input$alfa1, theta),
               `Initial WC` =  input$b1  + input$a1*(input$z1 - input$b1+ input$k1*theta)) %>%  # WC(input$b1, input$a1, input$z1, input$k1, theta)) %>%
        mutate(diff1 = `Initial JC` - `Initial WC`) %>%
         mutate(`New JC` = input$z2 - (input$r2+input$delta2)*input$k2/( input$e2*(1/theta)^input$alfa2  ), # JC(input$z1, input$r1, input$delta1, input$k1, input$e1, input$alfa1, theta),
                 `New WC` =  input$b2  + input$a2*(input$z2 - input$b2+ input$k2*theta)) %>%  # WC(input$b1, input$a1, input$z1, input$k1, theta)) %>%
         mutate(diff2 = `New JC` - `New WC`)
      #   mutate(BC1 = bevcurve(input$delta1, u, input$e1, input$alfa1),
      #          BC2 = bevcurve(input$delta2, u, input$e2, input$alfa2))
      
      wc_jc_df <- df
    })
  })
  
  
  thetastar1 <- reactive({
    input$plot
    isolate({
      thetastar1 <- wc_jc_df()$theta[which.min(abs(wc_jc_df()$diff1))]
      
    })
  })
  
  
  wstar1 <- reactive({
    input$plot
    isolate({
      wstar1 <- wc_jc_df()$`Initial WC`[which.min(abs(wc_jc_df()$diff1))]
    })
  })
  
  
  thetastar2 <- reactive({
    input$plot
    isolate({
      thetastar2 <- wc_jc_df()$theta[which.min(abs(wc_jc_df()$diff2))]
    })
  })
  
  
  wstar2 <- reactive({
    input$plot
    isolate({
      wstar2 <- wc_jc_df()$`New WC`[which.min(abs(wc_jc_df()$diff2))]
      
    })
  })
  
  
  ustar1 <- reactive({
    input$plot
    isolate({
      ustar2 <- input$delta1/(input$delta1 + thetastar1()*( input$e1*(1/thetastar1())^input$alfa1  ))
      
    })
  })
  
  ustar2 <- reactive({
    input$plot
    isolate({
      ustar2 <- input$delta2/(input$delta2 + thetastar2()*( input$e2*(1/thetastar2())^input$alfa2  ))
      
    })
  })
  
  
  
  output$dmp_plot_wcjc <- renderPlot({
    input$plot
    
    isolate({
      
      wc_jc_long <- wc_jc_df() %>% 
        select(-starts_with("diff")) %>% 
        pivot_longer(cols = -theta)
      
      
      ggplot(wc_jc_long, aes(x = theta, color = name, shape = name, y = value, linetype = name)) +
        geom_line(size = 1)+
      #  geom_point(size = 3)+
        
        scale_color_manual(values = c("darkred", "red", "darkblue", "purple"))+
        scale_linetype_manual(values = c("dotdash", "dotdash", "solid", "solid"))+
    #    scale_shape_manual(values = c(0,4,0,4))+
        
      #  geom_line(aes(y = job_creation_curve1), color = "red", size = 1.25)+
       # geom_line(aes(y = WC_curve1), color = "red", size = 1.25)+
       # geom_line(aes(y = job_creation_curve2), color = "blue", size = 1.25)+ geom_point(aes(y = job_creation_curve2), color = "blue", size = 4, shape = 1)+
       # geom_line(aes(y = WC_curve2), color = "blue", size = 1.25)+ geom_point(aes(y = WC_curve2), color = "blue", size = 4, shape = 1)+
        # geom_segment(
        #   x = thetastar1(),
        #   y = 0,
        #   xend = thetastar1(),
        #   yend = wstar1(),
        #   linetype = "dotted",
        #   color = "red"
        # ) +
        # geom_segment(
        #   x = 0,
        #   y = wstar1(),
        #   xend = thetastar1(),
        #   yend = wstar1(),
        #   linetype = "dotted",
        #   color = "red"
        # ) +
        # geom_segment(
        #   x = thetastar2(),
        #   y = 0,
        #   xend = thetastar2(),
        #   yend = wstar2(),
        #   linetype = "dotted",
        #   color = 'blue'
        # ) +
        # geom_segment(
        #   x = 0,
        #   y = wstar2(),
        #   xend = thetastar2(),
        #   yend = wstar2(),
        #   linetype = "dotted",
        #   color = 'blue'
        # ) +
        theme_classic() +
        
        
       # scale_linetype_manual(name = "Guide", values = c("dotdash", "dotdash", "solid", "solid")) +
       # scale_colour_manual(name = "Guide",
                            #    "Function",
       #                     values = c("red", "blue", "red", "blue"))+
        scale_x_continuous(expand = c(0, 0), limits = c(.75*min(thetastar1(), thetastar2()), 1.25 * max(thetastar1(), thetastar2()))) +
        scale_y_continuous(expand = c(0, 0), limits = c(.75*min(wstar1(), wstar2()), 1.25 * max(wstar1(), wstar2()))) +
        xlab(TeX('Market tightness, $\\theta$')) +
        ylab(
          TeX(
            'Real wage, $w$'
          )
        ) +
              theme(legend.position = "left", axis.text.x = element_blank(), axis.text.y = element_blank())+
        labs(color = element_blank(), linetype = element_blank())
      

    })
  })
  
  bcurve_ump <- reactive({
    input$plot
    
    isolate({
      df <- data.frame(thetaaxis = wc_jc_df()$theta,
                               u = seq(from = 0, to = .5, length.out = 1000 )) %>% 
        mutate(vaxis = thetaaxis*u) %>% 
        mutate(`Initial market tightness` = thetastar1()*u,
               `New market tightness` = thetastar2()*u,
               `Initial Beveridge curve` = ((input$delta1*(1-u))/(input$e1*u^(input$alfa1)))^(1/(1-input$alfa1)),
               `New Beveridge curve` = ((input$delta2*(1-u))/(input$e2*u^(input$alfa2)))^(1/(1-input$alfa2)))
      
      
      bcurve_ump <- df
    })
  })
  


  

output$dmp_bc <- renderPlot({
  input$plot
  
  isolate({
    

    bcurve_reshape <-bcurve_ump() %>% 
      select(u, starts_with("Initial"), starts_with("New")) %>% 
      pivot_longer(cols = -u) 

    

    
    ggplot(bcurve_reshape, aes(x = u, color = name, y = value, shape = name, linetype = name))+
      geom_line(size = 1)+
    #  geom_point(size = 6)+
    # geom_line(aes(y = JC_initial), size = 1, color = "red")+
    # geom_line(aes(y = JC_new), size = 1, color = "blue")+
    # geom_point(aes(y = JC_new), color = "blue", size = 4, shape = 1)+
    # geom_line(aes(y = BC_initial), color = "green")+
    # geom_line(aes(y = BC_new), color = "purple")+ 
    # geom_point(aes(y = BC_new), color = "purple", size = 4, shape = 1)+
      scale_x_continuous(expand = c(0, 0), limits = c(0, 2 * max(ustar1(), ustar2()))) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 2 * max(thetastar1()*ustar1(),thetastar2()*ustar2()))) +
      scale_color_manual(values = c("darkred", "red", "darkblue", "purple"))+
      scale_linetype_manual(values = c("dotdash", "dotdash", "solid",  "solid"))+
      
     # scale_shape_manual(values = c(0,4,0,4))+
      
    geom_segment(
      x = ustar1(),
      y = 0,
      xend = ustar1(),
      yend = thetastar1()*ustar1(),
      linetype = "dotted",
      color = "red"
    ) +
      # geom_segment(
      #   x = 0,
      #   y = thetastar1()*ustar1(),
      #   xend = ustar1(),
      #   yend = thetastar1()*ustar1(),
      #   linetype = "dotted",
      #   color = "red"
      # ) +
      # geom_segment(
      #   x = ustar2(),
      #   y = 0,
      #   xend = ustar2(),
      #   yend = thetastar2()*ustar2(),
      #   linetype = "dotted",
      #   color = 'blue'
      # ) +
      # geom_segment(
      #   x = 0,
      #   y = thetastar2()*ustar2(),
      #   xend = ustar2(),
      #   yend = thetastar2()*ustar2(),
      #   linetype = "dotted",
      #   color = 'blue'
      # ) +
      theme_classic() +
      xlab(TeX('Unemployment, $u$')) +
      ylab(
        TeX(
          'Vacancies, $v$'
        )
      ) +
      theme(legend.position = "left", axis.text.x = element_blank(), axis.text.y = element_blank()) +
      labs(color = element_blank(), linetype = element_blank())
      
      #coord_cartesian(xlim = c(0,.15), ylim = c(0,.15))+
      #geom_segment(aes(x = ustar1, xend = ustar1, y = thetastar1*ustar1, yend = thetastar2*ustar1), size = 2, color = "red", arrow =arrow())+
     # geom_path(data = upath, aes(x = u, y = theta*u), size = 2, color = "red", linejoin = "bevel", arrow = arrow())
    
    
  })
})

}

shinyApp(ui = ui, server = server)