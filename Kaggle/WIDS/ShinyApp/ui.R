ui <- material_page(
    
    title = ' WIDS Competition ',nav_bar_color = 'blue-grey',nav_bar_fixed = T,
    
    material_side_nav(
        
        fixed = T,image_source = 'wids.jpg',
        
        material_side_nav_tabs(
            
            side_nav_tabs = c(
                'Feature Patterns' = 'feat_patt',
                'Interpreting Predictions' = 'interpret_preds'
            ),
            
            icons = c("insert_chart","search")
            
        )
        
    ),
    
    material_side_nav_tab_content(
        
        side_nav_tab_id = 'feat_patt',
        
        material_row(
            
            material_column(
                material_card(title = NULL,
                              plotOutput(outputId = 'aportador',height = '300px')
                )
            ),
            material_column(
                material_card(title = NULL,
                              plotOutput(outputId = 'trabajador_hogar',height = '300px')
                )
            )
        ),
        
        material_row(
            
            material_column(
                material_card(title = NULL,
                              plotOutput(outputId = 'decide_celular',height = '300px')
                )
            ),
            material_column(
                material_card(title = NULL,
                              plotOutput(outputId = 'asesor_finan',height = '300px')
                )
            )
        )
    ),
    
    material_side_nav_tab_content(
        
        side_nav_tab_id = 'interpret_preds',
        
        material_row(
            
            material_column(
                
                title = '',
                width = 4,
                
                material_card(
                    
                    title = 'Entrevistado',
                    depth = 4,
                    
                    
                    material_text_box(
                        input_id = 'insert_id_obs',
                        label = 'Ingrese id:'
                    )#,
                    
                    # material_button(
                    #     input_id = 'obtain_exp',
                    #     label = 'Go',
                    #     color = 'blue-grey'
                    # )
                    
                ),
                
                material_card(
                    title = 'Información:',
                    depth = 4,
                    tags$b('Sexo: '),
                    textOutput(outputId = 'label_gen'),
                    br(),
                    tags$b('Probabilidad de ser mujer: '),
                    textOutput(outputId = 'prob'),
                    br(),
                    tags$b('Edad: '),
                    textOutput(outputId = 'age'),
                    br(),
                    tags$b('Estado civil: '),
                    textOutput(outputId = 'civil_state')
                    
                )
                
                
            ),
            
            material_column(
                
                title = NULL,
                width = 8,
                
                material_card(
                    title = NULL,
                    plotOutput(
                        outputId = 'lime_plot'
                    )
                    
                    
                )
                
            )
            
            
        )
        
    )
    
)
