#Feb 12, 2022

## Setup libraries and seed ----
rm(list=ls())

#check if seed file exists,and if it does not, create
if(!file.exists("seed.txt")) {
  #read seed file o, create if not written
  writeLines("999", "seed.txt")
}
#remove existing edgelist if rerunning and in environment
if(exists('network_edgelist')) {
  rm('network_edgelist')
}
#remove existing edgelist if rerunning and in environment
if(exists('network_nodelist')) {
  rm('network_nodelist')
}


## Create Fluid Page ----
#App start page, start of fluid page, creation of initial output
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  div(style = "padding: 1px 0px; width: '100%'",
      titlePanel(
        title = "",
        windowTitle = "IDEANET NETWORK VISUALIZER"
      )
  ),
  
### Upload node and edge data ----
 #code to upload node data
  navbarPage(
    title = "NETWORK VISUALIZER WITH IDEANET",
    tabPanel(
      "Upload",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Upload Files",
          sidebarPanel(
            uiOutput('select_file_type_edges'),
            uiOutput('edge_format'),
            checkboxInput("edge_names", tags$b("Does the file have an first id column"), FALSE),
            checkboxInput("edge_header", tags$b("Does the file have a header?"), TRUE),
            tags$p(span("Large datasets may take a few seconds to render.", style = "color:red")),
            tags$p(HTML("<b>Continue</b> on to process the data before visualizing it.")),
            fileInput(
              'raw_edges', "Upload Edge Data", multiple = FALSE, 
              buttonLabel = "Browse...", placeholder = "No file selected"
            ),
            checkboxInput('nodes_exist', tags$b("Does the dataset have a nodelist?"),FALSE),
            conditionalPanel(
              condition = 'input.nodes_exist',
              fileInput(
                'raw_nodes', "Upload Node Data", multiple = FALSE, 
                buttonLabel = "Browse...", placeholder = "No file selected"
              ),
              tags$p(span("Large datasets may take a few seconds to render.", style = "color:red")))
            
            
          ),
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Edge Data",
                style = "overflow-x: auto;",
                dataTableOutput('edge_raw_upload')
              ),
              tabPanel(
                "Node Data",
                style = "overflow-x: auto;",
                dataTableOutput('node_raw_upload')
              )
            )
          )
        )
      )
    ),
### Process node and edge data ----
    #Code to process Node Data
    tabPanel(
      "Process",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Process Node Data",
          sidebarPanel(
            uiOutput("node_ids"),
            uiOutput("node_labels"),
            uiOutput("node_factor"),
            uiOutput("node_numeric"),
            tags$p(span("Questions with an asterisk are required.", style = "color:red")),
            tags$p(HTML("<b>Process</b> the node data by assigning the columns to their function.")),
            tags$p(HTML("The <b>node</b> <b>ids</b> should reflect ids in the edge list. It's required to correctly link the node attributes.")),
          ),
          mainPanel(
            style = "overflow-x: auto;",
            dataTableOutput('node_processed')
          )
        ),
        #Code to process edge Data
        tabPanel(
          "Process Edge Data ",
          sidebarPanel(
            uiOutput("edge_in"),
            uiOutput("edge_out"),
            uiOutput("edge_weight"),
            checkboxInput("direction_toggle", tags$b("Check if the graph is directed"), FALSE),
            uiOutput('multi_relational_toggle'),
            conditionalPanel(
              condition = "input.multi_relational_toggle",
              uiOutput('relational_column')
              ),
            tags$p(span("Questions with an asterisk are required.", style = "color:red")),
            tags$p(HTML("<b>Process</b> the edge data by assigning the columns to their function.")),
            tags$p(HTML("If the graph is undirected, the order of sender and alter id columns doesn't matter.")),
          ),
          mainPanel(
            style = "overflow-x: auto;",
            dataTableOutput('edge_processed')
          )
        )
      )
    ),
### Visualize network and user options ---- 
    tabPanel(
      "Visualize",
      sidebarLayout(
      sidebarPanel(
        style = "height: 90vh; overflow-y: auto;",
        uiOutput("set_seed"),
        br(),
        uiOutput("save_image"),
        br(),
        uiOutput('image_type'),
        uiOutput("plot_scalar"),
        checkboxInput("isolate_toggle", tags$b("Remove isolates?"), FALSE),
        checkboxInput("simplify_toggle", tags$b("Remove self-loops and duplicate edges?"), FALSE),
        uiOutput("layout_picker"),
        tags$p(HTML("<u>Node Features</u>")),
        uiOutput("node_size_method"),
        uiOutput("node_size_scalar"),
        uiOutput("community_detection"),
        uiOutput("palette_choice"),
        uiOutput("uniform_choice"),
        tags$p(HTML("<u>Edge Features</u>")),
        conditionalPanel(
          condition = "input.multi_relational_toggle",
          uiOutput('filter_relation_type'),
          uiOutput('toggle_relational_coloring')
        ),
        uiOutput('interactive'),
        uiOutput('edge_weight_method')
        #uiOutput('edge_weight_scalar'),
      ),
      mainPanel(
        uiOutput("network_ui")
      )
    )),
### Network Metrics ----
    tabPanel(
      "Network Metrics",
      sidebarPanel(
        uiOutput("measure_chooser"),
        conditionalPanel(
          condition = "input.measure_chooser == 'System'",
          uiOutput('system_level_chooser')
        ),
        conditionalPanel(
          condition = "input.measure_chooser == 'Node'",
          uiOutput('node_level_chooser')
        ),
      ),
      mainPanel(
        plotOutput( 'stats1')
      )
    ),

### Networks DataTable ----
tabPanel(
  "Network Table",
  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;",
      uiOutput('show_vars')
      ),
      mainPanel(
        style = "overflow-x: auto;",
        DT::DTOutput('statistics_table'))
    )
),
### Analysis tab ----
    tabPanel(
      "Analysis",
      sidebarPanel(
        uiOutput('analysis_chooser'),
        conditionalPanel(
          tags$p(HTML("<u>QAP Setup Options</u>")),
          condition = "input.analysis_chooser == 'QAP'",
          tags$p(span("You must choose analysis type and variable as paired selections.", style = "color:red")),
          uiOutput('method_chooser'),
          uiOutput('var_cols'),
          tags$p(span("Method selections:", style = "color:black")),
          verbatimTextOutput("method_list"),
          tags$p(span("Variable selections:", style = "color:black")),
          verbatimTextOutput("var_list"),
          uiOutput('run_QAP_setup'),
          tags$p(HTML("<u>QAP Run Options</u>")),
          uiOutput('qap_run_dependent'),
          uiOutput('qap_run_choices'),
          uiOutput('run_QAP_model')
        ),
        conditionalPanel(
          condition = "input.analysis_chooser == 'Role Detection'",
          uiOutput('select_role_type'),
          uiOutput('select_role_viz'),
          uiOutput('role_det_min'),
          uiOutput('role_det_max'),
          uiOutput('min_cluster_size'),
          shinycssloaders::withSpinner(
          uiOutput('run_role_detect')
          )
        )
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.analysis_chooser == 'Role Detection'",
          tags$h3(HTML("<b>Visualize Role Detection Output</b>")),
          shinycssloaders::withSpinner(
          plotOutput('role_viz')
          )
        )
      )
    )
  )
)

## Server Function ----

#Create server
server <- function(input, output, session) {
  

  
### Upload Node  and Edge Data ----
  
  #Upload Node Data
  
  
  output$select_file_type_edges <- renderUI({
    selectInput('select_file_type_edges', label = "Choose file type", choices = c('csv', 'excel'))
  })
  
  output$edge_format <- renderUI({
    selectInput('edge_format', label = "Choose edge format", choices = c('edgelist','adjacency_matrix','adjacency_list'))
  })
  
  
  edge_data <- reactive({
    if (input$nodes_exist & !is.null(input$raw_nodes) & !is.null(input$raw_edges)) {
    ideanet::netread(
      path = input$raw_edges$datapath,
      filetype = input$select_file_type_edges,
      nodelist = input$raw_nodes$datapath,
      col_names = input$edge_header,
      row_names = input$edge_names,
      format = input$edge_format,
      net_name = "network",
      missing_code = 99999
    )
    }
    else if (!is.null(input$raw_edges)) {
      ideanet::netread(
        path = input$raw_edges$datapath,
        filetype = input$select_file_type_edges,
        format = input$edge_format,
        col_names = input$edge_header,
        row_names = input$edge_names,
        nodelist = NULL,
        net_name = "network",
        missing_code = 99999
      )
    }
    as.data.frame(network_edgelist)
  })
  
  node_data <- reactive({
    path_edges = input$raw_edges$datapath
    path_nodes = input$raw_nodes$datapath
    as.data.frame(network_nodelist)})
  
  #Display Node Data
  output$node_raw_upload <- renderDataTable({
    validate(
       need(input$raw_nodes, 'Upload Node Data!'),
     )
    node_data()
  })
  
  
  #Display Edge Data
  output$edge_raw_upload <- renderDataTable({
     validate(
     need(input$raw_edges, 'Upload Edge Data!'),
     )
    edge_data()
  })
  
  
### Process edge and node data ----
  # Redisplay Datatables
  output$node_processed <- renderDataTable({
    # validate(
    #   need(input$raw_nodes, 'Upload Node Data!'),
    # )
    node_data()
  })
  output$edge_processed <- renderDataTable({
    # validate(
    #   need(input$raw_edges, 'Upload Edge Data!'),
    # )
    edge_data()
  })
  
  #Node Processing Options
  output$node_ids <- renderUI({
    selectInput(inputId = "node_id_col", label = "Column with node ids*", choices = append("Empty",colnames(node_data())), selected = "id", multiple = FALSE)
  })
  output$node_labels <- renderUI({
    selectInput(inputId = "node_label_col", label = "Column with node labels", choices = append("Empty",colnames(node_data())), selected = "Empty", multiple = FALSE)
  })
  output$node_factor <- renderUI({
    selectInput(inputId = "node_factor_col", label = "Column with groups", choices = append("Empty",colnames(node_data())), selected = NULL, multiple = TRUE)
  })
  output$node_numeric <- renderUI({
    selectInput(inputId = "node_numeric_col", label = "Column with node sizes", choices = append("Empty",colnames(node_data())), multiple = FALSE)
  })
  
  #Edge Processing Options
  output$edge_in <- renderUI({
    selectInput(inputId = "edge_in_col", label = "Column with sender id*", choices = append("Empty",colnames(edge_data())), selected = 'N/A', multiple = FALSE)
  })
  output$edge_out <- renderUI({
    selectInput(inputId = "edge_out_col", label = "Column with the alter id*", choices = append("Empty",colnames(edge_data())), selected = 'N/A', multiple = FALSE)
  })
  output$edge_weight <- renderUI({
    selectInput(inputId = "edge_weight_col", label = "Column with edge weights", choices = append("Empty",colnames(edge_data())), selected = NULL, multiple = FALSE)
  })
  
  output$multi_relational_toggle <- renderUI({
    checkboxInput("multi_relational_toggle", tags$b("Check if the graph is multirelational"), FALSE)
  })

  output$relational_column <- renderUI({
    selectInput('relational_column', label = "Column with relation type", choices = append("Empty",colnames(edge_data())), selected = 'Empty', multiple = FALSE)
  })
  
### Network Generation ----

  #Edge Weight Setting
  initial_edge <- reactive ({
    if (input$edge_weight_col == 'Empty') {
      NULL
    } else {
      temp <- edge_data()[,input$edge_weight_col]
      temp
    }
  })
  

#### Create network 0 to run IDEANet ----
net0 <- reactive({
  type_ret <- c()
  if (is.null(input$relational_column)) {
    type_ret = NULL
  } else if (input$relational_column == "Empty") {
    type_ret = NULL } else {
    type_ret <- edge_data()[,input$relational_column]
  }
  if (!is.null(input$raw_nodes) & isTruthy(input$node_id_col))  {
    if (input$node_id_col != "Empty") {
      print('started netwrite 1')
      ideanet::netwrite(data_type = c('edgelist'), adjacency_matrix=FALSE, 
                                adjacency_list=FALSE, nodelist=node_data(),
                                node_id=input$node_id_col,
                                i_elements=edge_data()[,input$edge_in_col], 
                                j_elements=edge_data()[,input$edge_out_col], 
                                weights=initial_edge(), 
                                type=type_ret, package='igraph', 
                                missing_code=99999, weight_type='frequency', 
                                directed=input$direction_toggle,
                                net_name='init_net',
                                shiny=TRUE)
      print('processed netwrite')
      init_net
      
    } else {
      print('started netwrite 2')
      ideanet::netwrite(data_type = c('edgelist'), adjacency_matrix=FALSE, 
                                 adjacency_list=FALSE,
                                 i_elements=edge_data()[,input$edge_in_col], 
                                 j_elements=edge_data()[,input$edge_out_col], 
                                 weights=initial_edge(), 
                                 type=type_ret, package='igraph', 
                                 missing_code=99999, weight_type='frequency', 
                                 directed=input$direction_toggle,
                                 net_name='init_net',shiny=TRUE)
      print('processed netwrite')
      init_net
    }
  } else {
    print('started netwrite 3')
    ideanet::netwrite(data_type = c('edgelist'), adjacency_matrix=FALSE, 
                               adjacency_list=FALSE,
                               i_elements=edge_data()[,input$edge_in_col], 
                               j_elements=edge_data()[,input$edge_out_col], 
                               weights=initial_edge(), 
                               type=type_ret, package='igraph', 
                               missing_code=99999, weight_type='frequency', 
                               directed=input$direction_toggle,
                               net_name='init_net',shiny=TRUE)
    print('processed netwrite')
    init_net
  }
})

#### Add node attributes ----

# Joining all node_data to ideanet to preserve ordering

  
### MAKE SURE TO ADD CHECK FOR PROCESSING BACK IN!!!!

# join node data with nodelist  
nodelist2 <- reactive({
  if (!is.null(node_data())) {
    node_data3 <- node_data()
    node_data3[,input$node_id_col] <- as.character(node_data3[,input$node_id_col])
    node_measures <- node_measures %>% dplyr::mutate(id = as.character(id))
    node_measures <- node_measures %>%
      dplyr::left_join(node_data3)
    node_measures %>% dplyr::mutate(id = as.double(id))
  } else {
    node_measures
  }  
})

#Run Community detection
nodelist3 <- reactive({
  validate(
    need(input$raw_edges, 'Upload Edge Data!'),
  )
  
  net <- net0()
  
  nodes <- nodelist2()
  print('started community detection')
  ideanet::communities(net, shiny  = TRUE)
  print('finished community detection')
  comm_members_net <- comm_members_net %>% 
    dplyr::mutate_all(~replace(., is.na(.), 0))
  #comm_members_net$id <- as.character(comm_members_net$id)
  nodes <- nodes %>%
    dplyr::left_join(comm_members_net, by = "id")
  if (ran_toggle_qap$x==1) {
    nodes <- nodes %>%
      dplyr::left_join(cluster_assignments %>% dplyr::select('best_fit','id'), by = "id")
  }
  as.data.frame(nodes)
})

#Add labels in network 1
net1 <- reactive({
  net <- net0()
  # Adding Vector Labels
  if (input$node_label_col != 'Empty') {
    print('reached')
    igraph::V(net)$label <- nodelist3()[,input$node_label_col]
    print('finished')
  } else {
    igraph::V(net)$label <- nodelist3()[,'id']
  }
  
  #Add group elements (manually selected, automatically applied)
   if (!is.null(input$node_factor_col)) { 
      if (length(input$node_factor_col) > 2) {
      igraph::V(net)$group <- nodelist3() %>% pull(input$node_factor_col[1])
      } else {
       igraph::V(net)$group <- nodelist3() %>% pull(input$node_factor_col[1])
      }} else {
        igraph::V(net)$group <- rep("A", length(nodelist3()$id))
      }
  net
  
})


#### Set Node Size ----
output$node_size_method <- renderUI({
  selectInput(inputId = "node_size_method", label = "Node size method", choices = c("Uniform", "Node Data", "Degree", "Eigen Centrality", "Betweenness Centrality"), selected = "Uniform", multiple = FALSE)
})

output$node_size_scalar <- renderUI({
  sliderInput(inputId = "node_scalar_value", label = "Node size scalar", min = 0, max = 4, value =2, step = .1) 
})

net2 <- reactive({
  net <- net1()
  
  rescale2 = function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
  if (input$node_size_method == "Uniform") {
    igraph::V(net)$size <- rep(10, length(igraph::V(net)$label)) * input$node_scalar_value
  } else if (input$node_size_method == "Node Data") {
    if (isTruthy(input$node_numeric_col)) {
      if (input$node_numeric_col == "Empty") {
        igraph::V(net)$size <- rep(10, length(V(net)$label)) * input$node_scalar_value
      } else {
        igraph:: V(net)$size <- rescale2(nodelist3()[,input$node_numeric_col], min(nodelist3()[,input$node_numeric_col]), max(nodelist3()[,input$node_numeric_col]), 3,17) * input$node_scalar_value
      }
    } else {
      igraph::V(net)$size <- rep(10, length(V(net)$label)) * input$node_scalar_value
    }
  } else if (input$node_size_method == "Degree") {
    igraph::V(net)$size <- rescale2(igraph::degree(net, mode = "all"), min(igraph::degree(net, mode= "all")), max(igraph::degree(net, mode= "all")), 3,17) * input$node_scalar_value
  } else if (input$node_size_method == "Eigen Centrality") {
    igraph::V(net)$size <- rescale2(eigen_centrality(net)$vector, min(eigen_centrality(net)$vector), max(eigen_centrality(net)$vector), 3,17) * input$node_scalar_value
  } else if (input$node_size_method == "Betweenness Centrality") {
    igraph::V(net)$size <- rescale2(centr_betw(net)$res, min(centr_betw(net)$res), max(centr_betw(net)$res), 3,17) * input$node_scalar_value
  }
  net
})



#### Handle output community detection ----
output$community_detection <- renderUI({
  if (ran_toggle_qap$x==1) {
    vals <- nodelist3() %>%
      dplyr::select(ends_with('membership'),'best_fit') %>% 
      dplyr::select(-c("strong_membership", "weak_membership")) %>% 
      colnames()
  } else {
  vals <- nodelist3() %>%
    dplyr::select(ends_with('membership')) %>% 
    dplyr::select(-c("strong_membership", -"weak_membership")) %>% 
                          colnames()}
  selectInput(inputId = "community_input", label = "Node Coloring", choices = append(append("None", input$node_factor_col),vals[!vals %in% "id"]), selected = "None", multiple = FALSE)
})


#Create network to handle community attributes
net6 <- reactive({
  net <- net2()
  if (!(input$community_input == "None")) {
    val <-  input$community_input
    igraph::V(net)$communities <- nodelist3()[,val]
    net
  } else {
    net
  }
})

#### Choose colors ----
output$palette_choice <- renderUI({
  selectInput(inputId = "palette_input", label = "Color Palette", choices = c("Uniform", "Rainbow", "Heat", "Terrain", "Topo", "CM"), selected = "Uniform", multiple = FALSE)
})

output$uniform_choice <- renderUI({
  textInput(inputId = "uniform_hex_code", label = "Uniform color HEX", value = "#ADD8E6")
})



#### Set node colors ----
#Get number of necessary colors based on input
number_of_color_groups <- reactive({
  if (input$community_input != "None") {
    length(unique(igraph::V(net6())$communities))
  }
  else {
    length(unique(igraph::V(net6())$group))
  }
})

#Generate Color patterns/hues
color_generator <- reactive({
  if (input$palette_input == 'Uniform') {
    rep(input$uniform_hex_code, number_of_color_groups())
  } else if (input$palette_input == 'Rainbow') {
    if (number_of_color_groups() == 1) {
      rainbow(10)[5]
    } else {
      rainbow(number_of_color_groups())
    }
  } else if (input$palette_input == 'Heat') {
    if (number_of_color_groups() == 1) {
      heat.colors(10)[5]
    } else {
      heat.colors(number_of_color_groups())
    }
  } else if (input$palette_input == 'Terrain') {
    if (number_of_color_groups() == 1) {
      terrain.colors(10)[5]
    } else {
      terrain.colors(number_of_color_groups())
    }
  } else if (input$palette_input == 'Topo') {
    if (number_of_color_groups() == 1) {
      topo.colors(10)[5]
    } else {
      topo.colors(number_of_color_groups())
    }
  } else if (input$palette_input == 'CM') {
    if (number_of_color_groups() == 1) {
      cm.colors(10)[5]
    } else {
      cm.colors(number_of_color_groups())
    }
  }
})

#match by color or groups using groups or community
color_matcher <- reactive({
  if (input$community_input != "None") {
    groups <- unique(igraph::V(net6())$communities)
  } else {
    groups <- unique(igraph::V(net6())$group)
  }
    colrs <- color_generator()
    data.frame(groups, colrs)
})

#NOTE: actual application to the network condained in edge coloring

#### Set edge colors (type of edge relational) ----
#Get number of necessary colors based on input
number_of_color_groups_edges <- reactive({
  if (input$multi_relational_toggle == TRUE & input$relational_column != "Empty") {
    length(unique(edge_data()[,input$relational_column]))
  }
  else {
    1
  }
})

#Generate Color patterns/hues
color_generator_edges <- reactive({
  if (input$palette_input == 'Uniform') {
    rep(input$uniform_hex_code, number_of_color_groups_edges())
  } else if (input$palette_input == 'Rainbow') {
    if (number_of_color_groups_edges() == 1) {
      rainbow(10)[5]
    } else {
      rainbow(number_of_color_groups_edges())
    }
  } else if (input$palette_input == 'Heat') {
    if (number_of_color_groups_edges() == 1) {
      heat.colors(10)[5]
    } else {
      heat.colors(number_of_color_groups_edges())
    }
  } else if (input$palette_input == 'Terrain') {
    if (number_of_color_groups_edges() == 1) {
      terrain.colors(10)[5]
    } else {
      terrain.colors(number_of_color_groups_edges())
    }
  } else if (input$palette_input == 'Topo') {
    if (number_of_color_groups_edges() == 1) {
      topo.colors(10)[5]
    } else {
      topo.colors(number_of_color_groups_edges())
    }
  } else if (input$palette_input == 'CM') {
    if (number_of_color_groups_edges() == 1) {
      cm.colors(10)[5]
    } else {
      cm.colors(number_of_color_groups_edges())
    }
  }
})

#match by color or groups using groups or community
color_matcher_edges <- reactive({
  if (input$multi_relational_toggle == TRUE & input$relational_column != "Empty") {
    groups <- unique(edge_data()[,input$relational_column])
  } else {
    groups <- rep(1,length(edge_data()))
  }
  colrs <- color_generator_edges()
  data.frame(groups, colrs)
})

#Set edge and vertex Color Attribute in network
net3 <- reactive({
  net <- net6()
  if (input$community_input != "None") {
    igraph::V(net)$color <- color_matcher()$colrs[match(igraph::V(net)$communities, color_matcher()$groups)]
  } else {
    igraph::V(net)$color <- color_matcher()$colrs[match(igraph::V(net)$group, color_matcher()$groups)]
  }
  
  if (input$multi_relational_toggle == TRUE) {
    if (input$relational_column != "Empty") {
      igraph::E(net)$type <- edge_data()[,input$relational_column]
      igraph::E(net)$color <- color_matcher_edges()$colrs[match(igraph::E(net)$type, color_matcher_edges()$groups)]
    }
  } 
  net
})
#### Update Edge weights ----
#set edge weight
# output$edge_weight_scalar <- renderUI({
#   sliderInput(inputId = "edge_weight_scalar", label = "Edge width scalar", min = 0, max = 4, value =2, step = .1) 
# })

net7 <- reactive({
  net <- net3()
  rescale1 = function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
  temp <- rep(1, length(edge_data()[,input$edge_in_col]))
  igraph::E(net)$uni_weight <-  temp * 2 #input$edge_weight_scalar
  if (input$edge_weight_col == 'Empty') {
    igraph::E(net)$weight <-  temp * 2 #input$edge_weight_scalar
      net
    } else {
      temp <- igraph::E(net)$weight
      igraph::E(net)$weight <- rescale1(temp, min(temp), max(temp), 1,5) * 2 #input$edge_weight_scalar
      net
    }
  })



#### Update isolates ----
net4 <- reactive({
  if (input$isolate_toggle == TRUE) {
    net <- net7()
    bad.vs<-igraph::V(net)[igraph::degree(net) == 0]
    net <- igraph::delete.vertices(net, bad.vs)
    net
  } else {
    net <- net7()
    net
  }
    
})
  
  
  
  # 2. Simplify (Self Loops and Repeating Edges)
  
net5 <- reactive({
  if (input$simplify_toggle == TRUE) {
    net <- net4()
    net <- igraph::simplify(net)
    #igraph::V(net)$color <- color_generator()
    net
  } else {
    net <- net4()
    #igraph::V(net)$color <- color_generator()
    net
  }
})
  

  
#### Pick Network layout ----
  
  layout_choices <- c("layout_as_star", "layout_as_tree", "layout_in_circle",
                      "layout_nicely", "layout_on_grid", "layout_on_sphere", "layout_randomly", "layout_with_dh", "layout_with_fr",
                      "layout_with_gem", "layout_with_graphopt", "layout_with_kk", "layout_with_lgl", "layout_with_mds"
  )
  
  #set the layout of the network
  output$layout_picker <- renderUI({
    selectInput(inputId = "layout_choice", label = "Network layout", choices = layout_choices, selected = "layout_with_fr", multiple = FALSE)
  })
  
  #change the plot dimentions
  output$plot_scalar <- renderUI({
    sliderInput(inputId = "plot_scalar", label = "Plot dimensions", min = 100, max = 1000, value =600, step = 50) 
  })
  
  #toggle interactivity in network vizualization
  output$interactive <- 
    renderUI({
    shinyWidgets::materialSwitch(inputId = "interactive_switch", label = "Toggle Interactivity", status = "info", value = FALSE)
  })
  
  #set method for weighting edges
  output$edge_weight_method <- 
    renderUI({
    selectInput(inputId = "edge_weight_method", label = "Edge width method", choices = c("Uniform", "Edge Data"), selected = "Uniform", multiple = FALSE)
  })
  
  
  # output$edge_color_method <- renderUI({
  #   
  # })
  
  #change seed number
  output$set_seed <- 
    renderUI({
    actionButton("set_seed", "Generate New Layout", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")  
    })
  
  seed_number <- 
    reactiveValues(seed =  as.integer(readLines("seed.txt", n = 1)))  
  
  observeEvent(input$set_seed, {
    writeLines(as.character(sample.int(1000, 1)), "seed.txt")
    seed_number$seed <-  as.integer(readLines("seed.txt", n = 1))
    })
  
### Visualize network ----
  #output for network vizualizations
  output$filter_relation_type <- renderUI({
    selectInput('filter_relation_type', label = 'Filter edges by relation', choices = append('None',edge_data()[,input$relational_column]), selected = "None",)
  })
  
  output$toggle_relational_coloring <- renderUI({
    checkboxInput(inputId = "toggle_relational_coloring", label = "Toggle Multi-relational Coloring", value = TRUE)
  })

  net8 <-
  reactive({
    net <- net5()
    if (input$multi_relational_toggle == TRUE) {
    if (input$filter_relation_type != 'None') {
      net <- igraph::delete.edges(net, igraph::E(net)[igraph::E(net)$type == input$filter_relation_type])
    }
    if (input$toggle_relational_coloring == FALSE) {
      if (input$relational_column != 'Empty') {
        net <- igraph::delete_edge_attr(net,'color')
      }
    }}
    
    net.visn <- visNetwork::toVisNetworkData(net)
    
    #set node labels manually because for SOME REASON it chooses to misbehave
    net.visn$nodes$label <- igraph::V(net)$label
    
    net.visn$nodes$label <- igraph::V(net)$label
    
    print(net.visn$nodes$label)
    
    if (input$interactive_switch) {
    if (input$edge_weight_method == "Uniform") {
      net.visn$edges$value <- net.visn$edges$uni_weight
      visNetwork::visNetwork(net.visn$nodes, net.visn$edges, width = "100%") %>% 
        visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>%
        visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T), 
                   nodesIdSelection = T) %>% 
        visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>% 
        visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date()))  %>% 
        visNetwork::visLegend()
    } else {
      net.visn$edges$value <- net.visn$edges$weight
      visNetwork::visNetwork(net.visn$nodes, net.visn$edges) %>% 
        visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>%
        visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T), 
                   nodesIdSelection = T) %>%
        visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>% 
        visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date())) %>% 
        visNetwork::visLegend()
    }} else {
      if (input$edge_weight_method == "Uniform") {
        net.visn$edges$value <- net.visn$edges$uni_weight
        visNetwork::visNetwork(net.visn$nodes, net.visn$edges) %>% 
          visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>% 
          visNetwork::visInteraction(dragNodes = FALSE, 
                         dragView = FALSE) %>% 
          visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>% 
          visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date())) %>% 
          visNetwork::visLegend()
      } else {
        net.visn$edges$value <- net.visn$edges$weight
        visNetwork::visNetwork(net.visn$nodes, net.visn$edges) %>% 
          visNetwork::visIgraphLayout(layout = input$layout_choice, randomSeed = seed_number$seed) %>% 
          visNetwork::visInteraction(dragNodes = FALSE, 
                         dragView = FALSE) %>% 
          visNetwork::visEdges(arrows =list(to = list(enabled = input$direction_toggle, scaleFactor = 2))) %>% 
          visNetwork::visExport(type = input$image_type, name = paste0(input$layout_choice, seed_number$seed,Sys.Date())) %>% 
          visNetwork::visLegend()
    }}
    
  })
  
  output$network <- visNetwork::renderVisNetwork(net8())
 
  
  output$network_ui <- 
    renderUI({
    validate(
      need(input$raw_edges, 'Upload Edge Data!'),
      need(input$edge_in_col != "Empty" | input$edge_out_col != "Empty", 'Make sure you have selected an edge in and out column!'),
      need(try(!is.null(net0())), 'Error computing network statistics. Check edge in and out columns to make sure you have uploaded the right data.')
    )
      visNetwork::visNetworkOutput('network', height = input$plot_scalar, width = input$plot_scalar) %>% shinycssloaders::withSpinner(type = 5)
    })
  
  output$save_image <- renderUI({
    actionButton("save_image", "Save Graph as HTML", icon("download"), 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")  
  })
  
  output$image_type <- renderUI({
    selectInput('image_type', 'Select Image Format', choices = c('png','jpeg','pdf'))
  })
  
  observeEvent(input$save_image, {
    visSave(net8(), file = paste0(input$layout_choice, seed_number$seed,Sys.Date(),".html"))
  })  
  
  output$measure_chooser <- renderUI({
    selectInput(inputId = "measure_chooser", label = "Choose Measures Output", choices = c("System", "Node"), selected = "System", multiple = FALSE)
  })
  
### Visualize summary statistics ----
  
  output$system_level_chooser <- renderUI({
    validate (
    need(input$raw_edges, 'Upload Edge Data!'),
    need(input$edge_in_col != "Empty", 'Select edge in column!'),
    need(input$edge_out_col != "Empty", 'Select edge out column!'))
    selectInput('system_level_chooser', 'Choose which relation you want to visualize', choices = names(system_measure_plot_list), selected = NULL)
  })
  
  output$node_level_chooser <- renderUI({
    validate (
      need(input$raw_edges, 'Upload Edge Data!'),
      need(input$edge_in_col != "Empty", 'Select edge in column!'),
      need(input$edge_out_col != "Empty", 'Select edge out column!'))
    selectInput('node_level_chooser', 'Choose which relation you want to visualize', choices = names(node_measure_plot_list), selected = NULL)
  })
  
  output$stats1 <- 
    renderPlot({
    validate(
      need(input$raw_edges, 'Upload Edge Data!'),
      need(input$edge_in_col != "Empty", 'Select edge in column!'),
      need(input$edge_out_col != "Empty", 'Select edge out column!')
    )
    if (input$measure_chooser == "System") {
      plot(system_measure_plot_list[[match(input$system_level_chooser,names(system_measure_plot_list))]])
    } else {
      plot(node_measure_plot_list[[match(input$node_level_chooser,names(node_measure_plot_list))]])
    }
  })

### Visualize nodemeasures ----
output$show_vars <- renderUI({
  checkboxGroupInput("show_vars", "Columns in node variables to show:",
                     names(node_measures), selected = names(node_measures)[1:5])
})
output$statistics_table <- renderDataTable(nodelist3()[, input$show_vars, drop = FALSE])
### Setup Analysis Tab ----
  output$analysis_chooser <- renderUI({
    selectInput(inputId = "analysis_chooser", label = "Choose Measures Output", choices = c("QAP", "Role Detection"), selected = "QAP", multiple = FALSE)
  })
  
#### QAP ----

  #CHOOSE METHODS
  output$method_chooser <- renderUI({
    selectInput(input="method_chooser", label = "Choose your method", choices = c("None","multi_category","reduced_category","both","difference"), selected="None",multiple=FALSE)
  })
  
  chosen_methods <- reactiveVal(c())
  
  observeEvent(input$method_chooser, {
    req(input$method_chooser)
    if(input$method_chooser[[1]] == "None") {
      chosen_methods()
    }
    else {
    chosen_methods(c(chosen_methods(), input$method_chooser[[1]]))
    print(chosen_methods())
    }
  })
  
  observeEvent(chosen_methods(), {
    req(chosen_methods())
    updateSelectInput(session, "chosen_methods",
                      selected = "None",
                      choices = c("None","multi_category","reduced_category","both","difference")
    )
  })
  
  output$method_list <- renderPrint({
    print(chosen_methods())
  })
  
  
  #CHOOSE VARIABLES
  output$var_cols <- renderUI({
    validate(
      need(input$raw_edges, 'Input edge data!'),
      need(input$raw_nodes, 'Input node data!')
    )
    selectInput(inputId = "var_cols", label = "Column with variable", choices = append("None",colnames(node_data())), selected = "None", multiple = FALSE)
  })
  
  chosen_var <- reactiveVal(c())
  
  observeEvent(input$var_cols, {
    req(input$var_cols)
    if(input$var_cols[[1]] == "None") {
      chosen_var()
    } else {
    chosen_var(c(chosen_var(), input$var_cols[[1]]))
    print(chosen_var())
    }
  })
  
  observeEvent(chosen_var(), {
    req(chosen_var())
    updateSelectInput(session, "var_cols",
                      selected = "None",
                      choices = append("None",colnames(node_data()))
                      
    )
  })
  
  output$var_list <- renderPrint({
    print(chosen_var())
  })
  
  #run options
  
  output$run_QAP_setup <- 
    renderUI({
      validate(
        need(input$raw_edges, 'Input edge data!'),
        need(input$raw_nodes, 'Input raw nodes!'),
        need(!is.null(chosen_methods),"Choose a method"),
        need(!is.null(chosen_var),"Choose a variable")
      )
      actionButton("run_QAP_setup", "Run Initial QAP measures", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")  
    })
  
  ran_toggle_qap <- reactiveValues(x=0)
  
  observeEvent(input$run_QAP_setup, {
    net <- net5()
    foreach(i=1:length(chosen_var())) %do% {
      net <- set_vertex_attr(net,chosen_var()[i],value=nodelist3() %>% pull(parse_expr(chosen_var()[i])))
    }
    ideanet::qap_setup(net,chosen_var(),chosen_methods())
    ran_toggle_qap$x <- 1
  })
  
  #Run QAP MODEL
  output$qap_run_choices <- renderUI({
    validate(
      need(ran_toggle_qap$x != 0, 'Run QAP Setup'),
    )
    selectInput(inputId = "qap_run_choices", label = "QAP Variable Run Choices", choices = append("None",setdiff(qap_results[[3]] %>% names(),c("to","from","weight"))), selected = "None", multiple = TRUE)
  })
  
  output$qap_run_dependent <- renderUI({
    validate(
      need(ran_toggle_qap$x != 0, 'Run QAP Setup'),
    )
    selectInput(inputId = "qap_run_dependent", label = "QAP Run Dependent Variable", choices = append("None",setdiff(qap_results[[3]] %>% names(),c("to","from","weight"))), selected = "None", multiple = FALSE)
  })
  
  
  output$run_QAP_model <- renderUI({
    validate(
      need(ran_toggle_qap$x != 0, 'Run QAP Setup'),
    )
      actionButton("run_QAP_model", "Run Initial QAP measures", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")  
    })
  
  
  observeEvent(input$run_QAP_model, {
    require(exists('qap_results'))
    print(input$qap_run_choices)
    print(input$qap_run_dependent)
    ideanet::qap_run(net = qap_results[[1]], variables = input$qap_run_choices,
            dependent = input$qap_run_dependent, directed = T)
  })
  

  
#### Role Detection ----

  ran_toggle_role_detect <- reactiveValues(x=0)
  
  output$role_det_min <- renderUI({
    sliderInput(inputId = "role_det_min", label = "Choose Minimum # of Clusters", min = 2, max = nrow(nodelist3()), round = TRUE, step = 1, value = 4)
  })
  
  output$role_det_max <- renderUI({
    sliderInput(inputId = "role_det_max", label = "Choose Max # of Clusters", min = 2, max = nrow(nodelist3()), round = TRUE, step = 1, value = 4)
  })
  
  output$run_role_detect <- 
    renderUI({
      validate(
        need(input$raw_edges, 'Input edge data!')
      )
      actionButton("run_role_detect", "Run Role Detection", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")  
    })
  
  output$select_role_type <- renderUI({
    selectInput('select_role_type', label = "Choose Role Detection Type", choices = c('concor','cluster'))
  })
  
  role_detect_choices <- reactive({
    choices_yah <- c()
    if (input$select_role_type == 'cluster') {
      choices_yah <- c('cluster_modularity',
                       'cluster_dendogram',
                       'cluster_relations_heatmaps_chisq',
                       'cluster_relations_heatmaps_density_centered',
                       'cluster_relations_heatmaps_density_std',
                       'cluster_relations_heatmaps_density',
                       'cluster_relations_sociogram',
                       'cluster_summaries_cent',
                       'cluster_summaries_triad')
    }
    else if (input$select_role_type == 'concor') {
      choices_yah <- c('concor_block_tree',
                       'concor_modularity',
                       'concor_relations_heatmaps_chisq',
                       'concor_relations_heatmaps_density',
                       'concor_relations_heatmaps_density_std',
                       'concor_relations_heatmaps_density_centered',
                       'concor_relations_sociogram')
    }
    choices_yah
  })
  
  output$role_viz <- renderPlot({
    validate(
      need(input$raw_edges, 'Upload Edge Data!'),
      need(ran_toggle_role_detect$x == 1, "Input Role Detection Parameters and Run!")
    )
    if(input$select_role_viz == "cluster_modularity") {
      replayPlot(cluster_modularity)
    }
    else if(input$select_role_viz == 'cluster_dendrogram') {
      replayPlot(cluster_dendrogram)
    }
    else if(input$select_role_viz == 'cluster_relations_sociogram') {
      replayPlot(cluster_relations_sociogram$summary_graph) 
    }
    else if(input$select_role_viz == 'cluster_sociogram') {
      replayPlot(cluster_sociogram) 
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_chisq') {
      plot(cluster_relations_heatmaps$chisq) 
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_density') {
      plot(cluster_relations_heatmaps$density) 
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_density_std') {
      plot(cluster_relations_heatmaps$density_std)
    }
    else if(input$select_role_viz == 'cluster_relations_heatmaps_density_centered') {
      plot(cluster_relations_heatmaps$density_centered) 
    }
    else if(input$select_role_viz == 'cluster_summaries_cent') {
      plot(cluster_summaries_cent$summary_graph) 
    }
    else if(input$select_role_viz == 'cluster_summaries_triad') {
      plot(cluster_summaries_triad$summary_graph)
    }
    else if(input$select_role_viz == 'concor_block_tree') {
      replayPlot(concor_block_tree) 
    }
    if(input$select_role_viz == "concor_modularity") {
      replayPlot(concor_modularity) 
    }
    else if(input$select_role_viz == 'concor_relations_sociogram') {
      replayPlot(concor_relations_sociogram$summary_graph) 
    }
    else if(input$select_role_viz == 'concor_sociogram') {
      replayPlot(concor_sociogram) 
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_chisq') {
      plot(concor_relations_heatmaps$chisq) 
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_density') {
      plot(concor_relations_heatmaps$density) 
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_density_std') {
      plot(concor_relations_heatmaps$density_std) 
    }
    else if(input$select_role_viz == 'concor_relations_heatmaps_density_std') {
      plot(concor_relations_heatmaps$density_centered) 
    }
  })
  
  output$select_role_viz <- 
    renderUI({
      selectInput('select_role_viz', label = 'Choose Role Output Statistic', choices = role_detect_choices())
    })
  
  output$min_cluster_size <- renderUI({
    selectInput(inputId = "min_cluster_size", label = "Choose Minimum Cluster Size", choices = append(NA,c(1:8)), selected = NA)
  })
  
  observeEvent(input$run_role_detect, {
    ideanet::role_analysis(init_net, 
                          nodes = node_measures, 
                          directed = input$direction_toggle, 
                          method = input$select_role_type, 
                          min_partitions = input$role_det_min, 
                          max_partitions = input$role_det_max, 
                          min_partition_size = as.integer(input$min_cluster_size), 
                          viz = TRUE)
    ran_toggle_role_detect$x <- 1
  })
  
  
  
}



shinyApp(ui, server)
