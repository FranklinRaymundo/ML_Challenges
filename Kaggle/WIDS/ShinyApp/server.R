library(lime)
library(shiny)
library(shinymaterial)
library(ggplot2)
library(ggthemes)
library(dplyr)

options(encoding = 'UTF-8')

# Loading explanation:
explainer_xgb <- readRDS('explainer_xgb.rda')

# Loading test set:
test <- readRDS("test_feat.rda")

# Loading Original set
test_orig <- readRDS("test_orig.rda")
estado_civil <- ifelse(test_orig$DG3==1,'Soltero',
                ifelse(test_orig$DG3==2,'Matrimonio polígamo',
                ifelse(test_orig$DG3==3,'Matrimonio monógamo',
                ifelse(test_orig$DG3==4,'Divorciado',
                ifelse(test_orig$DG3==5,'Separado',
                ifelse(test_orig$DG3==6,'Viudo',
                ifelse(test_orig$DG3==7,'Conviviendo',
                ifelse(test_orig$DG3==8,'Cohabitando',
                ifelse(test_orig$DG3==96,'Otro','No sabe')))))))))

rm(list = c('test_orig'))

# Loading worked train set
train <- readRDS("train_feat.rda")

server <- function(input, output) {
    
    # Plots:
    
    tb_1 <- reactive({
        
        train %>% 
            group_by(main_earner_other,is_female) %>% summarise(Cuenta=n()) %>% 
            mutate(freq = Cuenta/(sum(Cuenta)),
                   perc = paste0(round(abs(freq)*100,1),'%'))
    })
    
    output$aportador <- renderPlot({
        
        ggplot(tb_1(),aes(x=factor(main_earner_other,labels = c('No','Si')),
                          y=Cuenta,fill=factor(is_female,labels = c('Hombre','Mujer'))))+
            geom_bar(stat = 'identity',position = 'fill') + theme_few()+ 
            scale_fill_manual(values = c('#A9A9A9','#2F4F4F'),name = 'Sexo\n') +
            labs(x='', y = '%',
                 title = '¿Es usted el principal aportador en su hogar?') +
            geom_text(aes(y=freq,label=perc,hjust = 0.5),position = position_stack(vjust=0.5),color='white')
        
    })
    
    
    tb_2 <- reactive({
        
        train %>% 
            group_by(ama_de_casa,is_female) %>% summarise(Cuenta=n()) %>% 
            mutate(freq = Cuenta/(sum(Cuenta)),
                   perc = paste0(round(abs(freq)*100,1),'%'))
    })
    
    output$trabajador_hogar <- renderPlot({
        
        ggplot(tb_2(),aes(x=factor(ama_de_casa,labels = c('No','Si')),
                          y=Cuenta,fill=factor(is_female,labels = c('Hombre','Mujer'))))+
            geom_bar(stat = 'identity',position = 'fill') + theme_few() + 
            scale_fill_manual(values = c('#A9A9A9','#2F4F4F'),name = 'Sexo\n') +
            labs(x='', y = '%',
                 title = '¿Es usted trabajador/a del hogar?') +
            geom_text(aes(y=freq,label=perc,hjust = 0.5),position = position_stack(vjust=0.5),color='white')
        
    })
    
    #decide_celular, asesor_finan
    
    tb_3 <- reactive({
        
        train %>% 
            group_by(spouse_decide_cel,is_female) %>% summarise(Cuenta=n()) %>% 
            mutate(freq = Cuenta/(sum(Cuenta)),
                   perc = paste0(round(abs(freq)*100,1),'%'))
    })
    
    output$decide_celular <- renderPlot({
        
        ggplot(tb_3(),aes(x=factor(spouse_decide_cel,labels = c('No','Si')),
                          y=Cuenta,fill=factor(is_female,labels = c('Hombre','Mujer'))))+
            geom_bar(stat = 'identity',position = 'fill') + theme_few() + 
            scale_fill_manual(values = c('#A9A9A9','#2F4F4F'),name = 'Sexo\n') +
            labs(x='', y = '%',
                 title = '¿Su esposo/a decide quien debe tener celular en casa?') +
            geom_text(aes(y=freq,label=perc,hjust = 0.5),position = position_stack(vjust=0.5),color='white')
        
    })
    
    tb_4 <- reactive({
        
        train %>% 
            group_by(depende_esposo_cons_financ,is_female) %>% summarise(Cuenta=n()) %>% 
            mutate(freq = Cuenta/(sum(Cuenta)),
                   perc = paste0(round(abs(freq)*100,1),'%'))
    })
    
    output$asesor_finan <- renderPlot({
        
        ggplot(tb_4(),aes(x=factor(depende_esposo_cons_financ,labels = c('No','Si')),
                          y=Cuenta,fill=factor(is_female,labels = c('Hombre','Mujer'))))+
            geom_bar(stat = 'identity',position = 'fill') + theme_few() + 
            scale_fill_manual(values = c('#A9A9A9','#2F4F4F'),name = 'Sexo\n') +
            labs(x='', y = '%',
                 title = '¿Depende de su esposo/a para tener asesoramiento financiero?') +
            geom_text(aes(y=freq,label=perc,hjust = 0.5),position = position_stack(vjust=0.5),color='white')
        
    })
    
    df_exp <- reactive({
        
        #input$insert_id_obs
        
        req(input$insert_id_obs)
        
        set.seed(123)
        
        explanation_xgb <- lime::explain(
            x = test[as.numeric(input$insert_id_obs)+1,-1],
            explainer = explainer_xgb,
            n_permutations = 5000,
            dist_fun = 'gower',
            kernel_width = .75,
            n_features = 5,
            feature_select = 'highest_weights',
            labels = '1'
        )
        
        explanation_xgb
        
    })
    
    output$lime_plot <- renderPlot({
        # plot_features(df_exp())
        
        ggplot(df_exp(),aes(x=factor(feature_desc),y=feature_weight,
                            fill=factor(ifelse(feature_weight>0,1,0),labels = c('Negativo ','Positivo'))))+
            geom_bar(stat = 'identity') + coord_flip() + theme_few() +
            labs(title = 'Contribución de Variables',
                 subtitle = 'En la probabilidad estimada de que el entrevistado sea mujer\n',
                 x = 'Feature pattern',y = 'Weight') + 
            scale_fill_manual(values = c('#2F4F4F','#A9A9A9'),name = '') + 
            theme(legend.position="bottom",
                  plot.title = element_text(size = 25,face = 'bold'),
                  plot.subtitle = element_text(size = 12, color = 'dark gray'))
        
    })
    
    output$prob <- renderText({
        
        round(df_exp()$label_prob[1],4)
        
        
    })
    
    output$label_gen <- renderText({
        
        ifelse(round(df_exp()$label_prob[1],4)<0.5,'Hombre','Mujer')
        
    })
    
    age <- reactive({
        
        edad <- test$edad[as.numeric(input$insert_id_obs)+1]
        ifelse(is.na(edad),'',edad)
    })
    
    output$age <- renderText({
        
        age()
        
    })
    
    civil_state <- reactive({
        
        civil_state <- estado_civil[as.numeric(input$insert_id_obs)+1]
        ifelse(is.na(civil_state),'',civil_state)
    })
    
    output$civil_state <- renderText({
        
        civil_state()
        
    })
    
    
}
