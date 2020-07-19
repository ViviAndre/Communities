#################################################################################
#### CÓDIGO PARA ENCONTRAR LAS COMUNIDADES DENTRO DE LAS REDES DE COAUTORÍAS ####
#################################################################################

# Para la ejecución de este algoritmo se precisa de un dataframe edgelist.csv,
# este dataframe debe tener tres columnas, donde las dos primeras contengan la lista de los enlaces
# la tercera el año de publicación del artículo o colaboración entre los autores
# los nombres de estas tres columnas deben ser: Source, Target y year 
# Source y Target debe tener como clase de datos factores y year enteros


###################
#### Librerías ####
###################

library(plyr)
library(igraph)
library(sqldf)
library(zoo)  
library(chron)
library(ggplot2)
library(bibliometrix)



###################################
#### Lectura de los dataframes ####
###################################

edgelist <- read.csv2("edgelist.csv", sep = ",", encoding = "UTF-8")
edgelist$X <- NULL



###################
#### FUNCIONES ####
###################

###########################################################################################
#### Identificación de los nodos a y b (se seleccionaron los dos nodos de mayor grado) ####
###########################################################################################

degree_nodes <- function(graph){
  author_degree <- as.data.frame(degree(graph, v = V(graph), loops = FALSE))
  author_degree$author <- row.names(author_degree)
  colnames(author_degree) = c("degree", "author")
  author_degree$author <- trim(author_degree$author)

  author_degree[order(author_degree[,1], decreasing = TRUE),] # Ordenar desde mayor grado a menor grado 
}


#########################################################################################
#### Enlaces adicionales al incluir la fuente y el sumidero (Capacidad de los arcos) ####
#########################################################################################

additional_links <- function(){
  edgelist_Cij$Jij <- NULL
  edgelist_source <- data.frame(from = as.integer(0), to = as.integer(node1$author), capacity = as.integer(field*2), stringsAsFactors = FALSE)
  edgelist_sink <- data.frame(from = as.integer(node2$author), to = as.integer((nrow(author_id)+1)), capacity = as.integer(field*2), stringsAsFactors = FALSE)
  edgelist_new <- rbind(edgelist_source, edgelist_sink)
  rbind(edgelist_Cij, edgelist_new)
}


##################################
#### Cálculo del flujo máximo ####
##################################

maximum_network_flow <- function(){
  g1 <- graph_from_data_frame(as.data.frame(edgelist_matrix), directed = FALSE)
  max_flow(g1, source=V(g1)["0"], target=V(g1)[as.character((nrow(author_id)+1))])
}


##############################################################
#### Data frame con los valores de flujo para cada enlace ####
##############################################################

data_frame_flow <- function(){
  flow_arcs <- as.data.frame(maximum_flow$flow)
  edgelist_Cij_flow <- edgelist_Cij
  edgelist_Cij_flow$flow <- flow_arcs$`maximum_flow$flow`
  edgelist_Cij_flow
}


###################################################
#### Nodos que son alcanzables desde la fuente ####
###################################################

source_nodes <- function(){
  edgelist_reachable <- edgelist_Cij_flow
  edgelist_reachable <- edgelist_reachable[!(edgelist_reachable$capacity == abs(edgelist_reachable$flow)), ] # Eliminación de los bordes donde el flujo es igual a la capacidad 

  Source <- 0
  
  reachable <- edgelist_reachable[(edgelist_reachable$from == Source), ]
  reachable0 <- data.frame(from = integer(), to = integer(), capacity = numeric(), flow = numeric(), stringsAsFactors = FALSE)
  nodes_reachables_s <- as.list(reachable$to)
  counter <- as.list(reachable$to)
  
  edgelist_reachable <- sqldf('SELECT * FROM edgelist_reachable EXCEPT SELECT * FROM reachable') # Eliminación de los enlaces que ya se han contado 
  
  while(length(counter) != 0){
    
    counter0 <- list()
    
    for (i in counter) {
      
      reachable <- edgelist_reachable[(edgelist_reachable$from == i), ]
      nodes_reachables_s <- append(nodes_reachables_s, reachable$to)
      edgelist_reachable <- sqldf('SELECT * FROM edgelist_reachable EXCEPT SELECT * FROM reachable')# Eliminación de los enlaces que ya se han contado
      counter0 <- append(counter0, reachable$to)
      
      reachable0 <- edgelist_reachable[(edgelist_reachable$to == i), ]
      nodes_reachables_s <- append(nodes_reachables_s, reachable0$from)
      edgelist_reachable <- sqldf('SELECT * FROM edgelist_reachable EXCEPT SELECT * FROM reachable0')# Eliminación de los enlaces que ya se han contado
      counter0 <- append(counter0, reachable0$from)
      
    }
    counter <- unique(counter0)
  }
  unique(nodes_reachables_s)
}


####################################################
#### Nodos que son alcanzables desde el sumideo ####
####################################################

sink_nodes <- function(){
  edgelist_reachable <- edgelist_Cij_flow
  edgelist_reachable <- edgelist_reachable[!(edgelist_reachable$capacity == abs(edgelist_reachable$flow)), ]# Se retoma el dataframe edgelist_reachable y se eliminan los bordes donde el flujo es igual a la capacidad 

  Sink <- as.numeric((nrow(author_id)+1))
  
  reachable <- edgelist_reachable[(edgelist_reachable$to == Sink), ]
  reachable0 <- data.frame(from = integer(), to = integer(), capacity = numeric(), flow = numeric(), stringsAsFactors = FALSE)
  nodes_reachables_t <- as.list(reachable$from)
  counter <- as.list(reachable$from)
  
  edgelist_reachable <- sqldf('SELECT * FROM edgelist_reachable EXCEPT SELECT * FROM reachable') # Eliminación de los enlaces que ya se han contado 
  
  while(length(counter) != 0){
    
    counter0 <- list()
    
    for (i in counter) {
      
      reachable <- edgelist_reachable[(edgelist_reachable$from == i), ]
      nodes_reachables_t <- append(nodes_reachables_t, reachable$to)
      edgelist_reachable <- sqldf('SELECT * FROM edgelist_reachable EXCEPT SELECT * FROM reachable')# Eliminación de los enlaces que ya se han contado
      counter0 <- append(counter0, reachable$to)
      
      reachable0 <- edgelist_reachable[(edgelist_reachable$to == i), ]
      nodes_reachables_t <- append(nodes_reachables_t, reachable0$from)
      edgelist_reachable <- sqldf('SELECT * FROM edgelist_reachable EXCEPT SELECT * FROM reachable0')# Eliminación de los enlaces que ya se han contado
      counter0 <- append(counter0, reachable0$from)
      
    }
    counter <- unique(counter0)
  }
  unique(nodes_reachables_t)
}



######################################################
################## PROGRAMA ##########################
######################################################

net <- graph.data.frame(edgelist, directed = FALSE)
edgelist <- as.data.frame(get.data.frame(net))



########################################################################################
#### Pasar los años a pasos de tiempo desde 1 hasta el número de fechas ####
########################################################################################

truetime <- data.frame(ccc = unique(edgelist$year))
truetime$ccc <- trim(truetime$ccc)
truetime <- data.frame(V1 = (1:nrow(truetime)),ccc = truetime[order(as.yearmon(truetime$ccc,format="%Y")),])
write.csv(truetime, file = "truetime.csv", fileEncoding = "UTF-8")

edgelist$ccc <- truetime$V1[match(edgelist$year, truetime$ccc)]



#########################################################
#### Dataframe con ids en los nombres de los autores #### 
#########################################################

names_Source <- data.frame(names = edgelist$from, stringsAsFactors = FALSE)
names_Target <- data.frame(names = edgelist$to, stringsAsFactors = FALSE)
names_S_T <- rbind(names_Source, names_Target)
names_authors <- data.frame(unique(names_S_T$names))
author_id <- data.frame(V1 = (1:nrow(names_authors)), coauthor.author_id = names_authors, stringsAsFactors = FALSE)
colnames(author_id) <- c("V1", "coauthor.author_id")
#write.csv(author_id, file = "author_id.csv", fileEncoding = "UTF-8")

edgelist$from <- author_id$V1[match(edgelist$from, author_id$coauthor.author_id)]
edgelist$to <- author_id$V1[match(edgelist$to, author_id$coauthor.author_id)]
#write.csv(edgelist, file = "edgelist_id.csv", fileEncoding = "UTF-8")



###################################################################################
#### Identificación de comunidades por medio de el algoritmo de Ford Fulkerson ####
###################################################################################

energy <- data.frame(ccc = character(), ite = character(), E0 = character(), cut = character(), Cst = character(), sum_Jij = character(), field = character(), N_arcs_cut = character(), d_node1 = character(), d_node2 = character(), Total_N = character(), N_nodes_S = character(), N_nodes_T = character(), id_node1 = character(), id_node2 = character(), community = character(), stringsAsFactors = FALSE)
communities_values <- data.frame(ccc = character(), nc =character(), ite = character(), sum_Jij = character(), field = character(),  d_node1 = character(), id_node1 = character(), Total_N = character(), spin = character(), stringsAsFactors = FALSE)
communities <- list()
communities_time <- list()

for (m in 1:(nrow(truetime))) {
  
  communities_time_steps <- list() 
  
  edgelist.year <- edgelist[edgelist$ccc <= m,] # Selección del paso del tiempo
  
  
  ##############################################################################
  #### Eliminación de columnas que no necesitamos para luego hayar el flujo ####
  ##############################################################################
  
  edgelist.year$year <- NULL
  edgelist.year$ccc <- NULL
  
  
  ####################################################
  #### Número de componentes conectados de la red ####
  ####################################################
  
  net <- graph.data.frame(edgelist.year, directed = FALSE)
  
  dg <- decompose.graph(net)
  num_components <- length(dg)
  
  
  for (nc in 1:num_components) {
    
  net <- dg[[nc]] # Selección de los componentes
  Gcedgelist <- as.data.frame(get.data.frame(net))
  
  
  ###############################
  #### Número total de nodos ####
  ###############################
  
  Total_N <- vcount(net)
  
  author_degree <- degree_nodes(net) # Llama a la función para obtener un dataframe con los grados de los nodos ordenados
  
  
  ###################################
  #### Selección del nodo fuente ####
  ###################################
  
  node1 <- author_degree[1,]
  
  
  ###################################################################
  #### Creación de el dataframe con el peso de los enlaces (Jij) ####
  ###################################################################
  
  edgelist_Jij <- ddply(Gcedgelist, .(Gcedgelist$from, Gcedgelist$to), nrow)
  colnames(edgelist_Jij) <- c("from", "to", "Jij")
  
  sum_Jij <- sum(edgelist_Jij$Jij) # Para el cálculo del borde fantasma
  
  
  ###################################################################################
  #### While para la identificación de las comunidades variando el nodo sumidero ####
  ###################################################################################
  
  community_indicator <- 0
  c <- 2
  
  while (community_indicator <= 1) {
    
    
    #####################################
    #### Selección del nodo sumidero ####
    #####################################
    
    node2 <- author_degree[c,]
    c <- c + 1
    
    
    #############################################################
    #### Identificación del campo local para los nodos a y b ####
    #############################################################
    
    edgelist_Cij <- edgelist_Jij
    edgelist_Cij$capacity <- 2*edgelist_Cij$Jij
    field <- sum(edgelist_Cij$capacity)
    
    
    #########################################################################################
    #### Enlaces adicionales al incluir la fuente y el sumidero (Capacidad de los arcos) ####
    #########################################################################################
    
    edgelist_Cij <- additional_links()
    
    
    #############################################
    #### Convertir el dataframe a una matriz ####
    #############################################
  
    edgelist_matrix <- data.matrix(edgelist_Cij)
  
  
    ##################################
    #### Cálculo del flujo máximo ####
    ##################################
    
    maximum_flow <- maximum_network_flow() 
    
    
    #########################
    #### Valor del corte ####
    #########################
  
    flow_value <- maximum_flow$value
    
    
    #################################
    #### Data.frame con el flujo ####
    #################################
    
    edgelist_Cij_flow <- data_frame_flow()
    
    
    ###################################
    #### Número de arcos del corte ####
    ###################################
    
    N_arcs_cut <- length(maximum_flow$cut)
    
    
    ###########################################
    #### IDENTIFICACIÓN DE LAS COMUNIDADES ####
    ###########################################
    
    ###################################################
    #### Nodos que son alcanzables desde la fuente ####
    ###################################################
    
    nodes_reachables_s <- source_nodes()
    
    #####################################################
    #### Nodos que son alcanzables desde el sumidero ####
    #####################################################
    
    nodes_reachables_t <- sink_nodes()
    
  
    #####################################################################################################
    #### Tamaño de las comunidades S y T en cada paso de tiempo, sin incluir la fuente y el sumidero ####
    #####################################################################################################
    
    N_nodes_S <- length(nodes_reachables_s)
    N_nodes_T <- length(nodes_reachables_t)
  
  
    ####################################
    #### Cálculo del borde fantasma ####
    ####################################
    
    C_s_t <- -sum_Jij - 0.5*sum(edgelist_Cij[nrow(edgelist_Cij),3],edgelist_Cij[(nrow(edgelist_Cij)-1),3])
  
  
    ########################################
    #### Energía del estado fundamental ####
    ########################################
    
    E0 <- sum(flow_value, C_s_t)
  
  
    #####################################################
    #### Indicador para la existencia de comunidades ####
    #####################################################
    
    community_indicator <- log(N_nodes_S * N_nodes_T) / log(Total_N)
  
    
    
    if(community_indicator > 1){
      
      ############################################################
      #### Almacenamiento de la energía con el paso de tiempo ####
      ############################################################
    
      energy0 = data.frame(ccc= m, ite = 1, E0 = E0, cut = flow_value, Cst = C_s_t, sum_Jij = sum_Jij, field = field, N_arcs_cut = N_arcs_cut, d_node1 = node1[1,1], d_node2 = node2[1,1], Total_N = Total_N, N_nodes_S = N_nodes_S, N_nodes_T = N_nodes_T, id_node1 = node1$author, id_node2 = node2$author, community = community_indicator, stringsAsFactors = FALSE )
      energy = rbind(energy, energy0)
  
      
      #################################
      #### Guardar las comunidades ####
      #################################
      
      communities0 <- list(S = nodes_reachables_s, T = nodes_reachables_t, ccc = m, ite = 1)
      communities[[(length(communities) + 1)]] <- communities0 
  
  
      
      ########################################################
      ########################################################
      #### ALGORITMO PARA LAS SUBREDES DE FORMA ITERATIVA ####
      ########################################################
      ########################################################
      
      
      initial <- length(communities)
      final <- length(communities)
      new_communities <- length(communities)
      ite <- 1
  
      while (new_communities != 0) {
        
        ite <- ite + 1  
  
        for (x in initial:final) {
          
          ################################################
          #### Tamaño inicial de la lista communities ####
          ################################################
          
          if(x == initial){
            initial0 <- (length(communities) + 1)
            }
    
          
          ###################################################################
          #### Selección de la lista de enlaces de las comunidades S y T ####
          ###################################################################
          
          edgelist_Jij_ite_S <- edgelist_Jij[((edgelist_Jij$from %in% communities[[x]]$S) & (edgelist_Jij$to %in% communities[[x]]$S)),]
          edgelist_Jij_ite_T <- edgelist_Jij[((edgelist_Jij$from %in% communities[[x]]$T) & (edgelist_Jij$to %in% communities[[x]]$T)),]
  
  
          ####################################
          #### CÓDIGO PARA LA COMUNIDAD S ####
          ####################################
  
          net <- graph.data.frame(edgelist_Jij_ite_S, directed = FALSE) # Convertir el dataframe edgelist_Jij_ite_S a un objeto tipo grafo 
  
          
          ###############################
          #### Número total de nodos ####
          ###############################
          
          Total_N <- vcount(net)
          
          author_degree <- degree_nodes(net) # Llama a la función para obtener un dataframe con los grados de los nodos ordenados
  
  
          ###################################
          #### Selección del nodo fuente ####
          ###################################
          
          node1 <- author_degree[1,]
  
    
          ########################################
          #### Término para el borde fantasma ####
          ########################################
          
          sum_Jij <- sum(edgelist_Jij_ite_S$Jij) 
  
          
          ###################################################################################
          #### While para la identificación de las comunidades variando el nodo sumidero ####
          ###################################################################################
          
          community_indicator <- 0
          h <- 2
  
          while (community_indicator <= 1) {
    
          
          #####################################
          #### Selección del nodo sumidero ####
          #####################################
            
          node2 <- author_degree[h,]
          h <- h + 1
  
  
          #############################################################
          #### Identificación del campo local para los nodos a y b ####
          #############################################################
          
          edgelist_Cij <- edgelist_Jij_ite_S
          edgelist_Cij$capacity <- 2*edgelist_Cij$Jij
          field <- sum(edgelist_Cij$capacity) 
          
          
            #########################################################################################
            #### Enlaces adicionales al incluir la fuente y el sumidero (Capacidad de los arcos) ####
            #########################################################################################
            
            edgelist_Cij <- additional_links()
  
  
            #############################################
            #### Convertir el dataframe a una matriz ####
            #############################################
            
            edgelist_matrix <- data.matrix(edgelist_Cij)
            
            
            ##################################
            #### Cálculo del flujo máximo ####
            ##################################
            
            maximum_flow <- maximum_network_flow() 
            
            
            #########################
            #### Valor del corte ####
            #########################
            
            flow_value <- maximum_flow$value
            
            
            #################################
            #### Data.frame con el flujo ####
            #################################
            
            edgelist_Cij_flow <- data_frame_flow()
            
            
            ###################################
            #### Número de arcos del corte ####
            ###################################
            
            N_arcs_cut <- length(maximum_flow$cut)
            
            
            ###########################################
            #### IDENTIFICACIÓN DE LAS COMUNIDADES ####
            ###########################################
            
            ###################################################
            #### Nodos que son alcanzables desde la fuente ####
            ###################################################
            
            nodes_reachables_s <- source_nodes()
            
            #####################################################
            #### Nodos que son alcanzables desde el sumidero ####
            #####################################################
            
            nodes_reachables_t <- sink_nodes()
            
            
            #####################################################################################################
            #### Tamaño de las comunidades S y T en cada paso de tiempo, sin incluir la fuente y el sumidero ####
            #####################################################################################################
            
            N_nodes_S <- length(nodes_reachables_s)
            N_nodes_T <- length(nodes_reachables_t)
            
            
            ####################################
            #### Cálculo del borde fantasma ####
            ####################################
            
            C_s_t <- -sum_Jij - 0.5*sum(edgelist_Cij[nrow(edgelist_Cij),3],edgelist_Cij[(nrow(edgelist_Cij)-1),3])
            
            
            ########################################
            #### Energía del estado fundamental ####
            ########################################
            
            E0 <- sum(flow_value, C_s_t)
            
            
            #####################################################
            #### Indicador para la existencia de comunidades ####
            #####################################################
            
            community_indicator <- log(N_nodes_S * N_nodes_T) / log(Total_N)
  
  
  
            if(community_indicator > 1){
              
              ############################################################
              #### Almacenamiento de la energía con el paso de tiempo ####
              ############################################################
              
              energy0 = data.frame(ccc= m, ite = ite, E0 = E0, cut = flow_value, Cst = C_s_t, sum_Jij = sum_Jij, field = field, N_arcs_cut = N_arcs_cut, d_node1 = node1[1,1], d_node2 = node2[1,1], Total_N = Total_N, N_nodes_S = N_nodes_S, N_nodes_T = N_nodes_T, id_node1 = node1$author, id_node2 = node2$author, community = community_indicator, stringsAsFactors = FALSE )
              energy = rbind(energy, energy0)
              
              
              #################################
              #### Guardar las comunidades ####
              #################################
              
              communities0 <- list(S = nodes_reachables_s, T = nodes_reachables_t, ccc = m, ite = ite)
              communities[[(length(communities)+1)]] <- communities0 
              
              } else {
                
                if (h == (nrow(author_degree) + 1)){
                  communities_time_steps[[(length(communities_time_steps)+1)]] <- communities[[x]]$S
                  communities_values0 = data.frame(ccc= m, nc = nc, ite = ite, sum_Jij = sum_Jij, field = field, d_node1 = node1[1,1], id_node1 = node1$author, Total_N = Total_N, spin = 1, stringsAsFactors = FALSE )
                  communities_values = rbind(communities_values, communities_values0)
                  community_indicator <- 1.2
                  }
                }
            }
  

          ####################################
          #### CÓDIGO PARA LA COMUNIDAD T ####
          ####################################
 
          net <- graph.data.frame(edgelist_Jij_ite_T, directed = FALSE) # Convertir el dataframe edgelist_Jij_ite_T a un objeto tipo grafo 
  
          
          ###############################
          #### Número total de nodos ####
          ###############################
          
          Total_N <- vcount(net)
          
          author_degree <- degree_nodes(net) # Llama a la función para obtener un dataframe con los grados de los nodos ordenados
          
  
          ###################################
          #### Selección del nodo fuente ####
          ###################################
          
          node1 <- author_degree[1,]
          
          
          ########################################
          #### Término para el borde fantasma ####
          ########################################
          
          sum_Jij <- sum(edgelist_Jij_ite_T$Jij) 
          
          
          ###################################################################################
          #### While para la identificación de las comunidades variando el nodo sumidero ####
          ###################################################################################
          
          community_indicator <- 0
          q <- 2
          
          while (community_indicator <= 1) {
            
            #####################################
            #### Selección del nodo sumidero ####
            #####################################
            
            node2 <- author_degree[q,]
            q <- q + 1
            
            
            #############################################################
            #### Identificación del campo local para los nodos a y b ####
            #############################################################
            
            edgelist_Cij <- edgelist_Jij_ite_T
            edgelist_Cij$capacity <- 2*edgelist_Cij$Jij
            field <- sum(edgelist_Cij$capacity) 
            
            
              #########################################################################################
              #### Enlaces adicionales al incluir la fuente y el sumidero (Capacidad de los arcos) ####
              #########################################################################################
              
              edgelist_Cij <- additional_links()
              
              
              #############################################
              #### Convertir el dataframe a una matriz ####
              #############################################
              
              edgelist_matrix <- data.matrix(edgelist_Cij)
              
              
              ##################################
              #### Cálculo del flujo máximo ####
              ##################################
              
              maximum_flow <- maximum_network_flow() 
              
              
              #########################
              #### Valor del corte ####
              #########################
              
              flow_value <- maximum_flow$value
              
              
              #################################
              #### Data.frame con el flujo ####
              #################################
              
              edgelist_Cij_flow <- data_frame_flow()
              
              
              ###################################
              #### Número de arcos del corte ####
              ###################################
              
              N_arcs_cut <- length(maximum_flow$cut)
              
              
              ###########################################
              #### IDENTIFICACIÓN DE LAS COMUNIDADES ####
              ###########################################
              
              ###################################################
              #### Nodos que son alcanzables desde la fuente ####
              ###################################################
              
              nodes_reachables_s <- source_nodes()
              
              #####################################################
              #### Nodos que son alcanzables desde el sumidero ####
              #####################################################
              
              nodes_reachables_t <- sink_nodes()
              
              
              #####################################################################################################
              #### Tamaño de las comunidades S y T en cada paso de tiempo, sin incluir la fuente y el sumidero ####
              #####################################################################################################
              
              N_nodes_S <- length(nodes_reachables_s)
              N_nodes_T <- length(nodes_reachables_t)
              
              
              ####################################
              #### Cálculo del borde fantasma ####
              ####################################
              
              C_s_t <- -sum_Jij - 0.5*sum(edgelist_Cij[nrow(edgelist_Cij),3],edgelist_Cij[(nrow(edgelist_Cij)-1),3])
              
              
              ########################################
              #### Energía del estado fundamental ####
              ########################################
              
              E0 <- sum(flow_value, C_s_t)
              
              
              #####################################################
              #### Indicador para la existencia de comunidades ####
              #####################################################
              
              community_indicator <- log(N_nodes_S * N_nodes_T) / log(Total_N)
    

  
              if(community_indicator > 1){
                
                ############################################################
                #### Almacenamiento de la energía con el paso de tiempo ####
                ############################################################
                
                energy0 = data.frame(ccc= m, ite = ite, E0 = E0, cut = flow_value, Cst = C_s_t, sum_Jij = sum_Jij, field = field, N_arcs_cut = N_arcs_cut, d_node1 = node1[1,1], d_node2 = node2[1,1], Total_N = Total_N, N_nodes_S = N_nodes_S, N_nodes_T = N_nodes_T, id_node1 = node1$author, id_node2 = node2$author, community = community_indicator, stringsAsFactors = FALSE )
                energy = rbind(energy, energy0)
                
                
                #################################
                #### Guardar las comunidades ####
                #################################
                
                communities0 <- list(S = nodes_reachables_s, T = nodes_reachables_t, ccc = m, ite = ite)
                communities[[(length(communities)+1)]] <- communities0 
                
                } else {
                  
                  if (q == (nrow(author_degree) + 1)){
                    communities_time_steps[[(length(communities_time_steps)+1)]] <- communities[[x]]$T
                    communities_values0 = data.frame(ccc= m, nc = nc, ite = ite, sum_Jij = sum_Jij, field = field, d_node1 = node1[1,1], id_node1 = node1$author, Total_N = Total_N, spin = -1, stringsAsFactors = FALSE )
                    communities_values = rbind(communities_values, communities_values0)
                    community_indicator <- 1.2
                    }
                  }
          
  }
  
  
  } #### for que recorre la lista communities para hacer nuevas iteraciones
  
  
        ########################################################
        #### Tamaño inicial y final de la lista communities ####
        ########################################################
  
        initial <- initial0
        final <- length(communities)
  
  
        #######################################################################
        #### Número de nuevas comunidades agregadas a la lista communities ####
        #######################################################################
        
        new_communities <- final - (initial-1)
        
        if(new_communities == 0){
          community_indicator <- 1.2
        }
  
  } #### while de new_communities
  
  
  }else { # Si en la red principal no hay comunidades  
    
    if (c == (nrow(author_degree) + 1)){
      set.seed(1)
      value_spin = sample(c(1,-1), size= 1, replace=TRUE, prob=c(0.5,0.5))
      single_communities <- as.list(author_degree$author)
      communities_time_steps[[(length(communities_time_steps)+1)]] <- single_communities
      communities_values0 = data.frame(ccc= m, nc = nc, ite = 1, sum_Jij = sum_Jij, field = field, d_node1 = node1[1,1], id_node1 = node1$author, Total_N = Total_N, spin = value_spin, stringsAsFactors = FALSE )
      communities_values = rbind(communities_values, communities_values0)
      community_indicator <- 1.2
    }
    
  } 
  
  
  } #### while communitie indicator principal
  
  
} ### for del número de componentes conectados
  
  communities_time[[(length(communities_time) + 1)]] <- communities_time_steps 
  
} ### for del paso de tiempo



##############################
#### Guardar los archivos ####
##############################

## Data.frames ##
write.csv(energy, file = "energy.csv", fileEncoding = "UTF-8")
write.csv(communities_values, file = "communities_values.csv", fileEncoding = "UTF-8")

## Listas ##
save(communities, file="communities.RData")
save(communities_time_steps, file="communities_time_steps.RData")
save(communities_time, file = "communities_time.RData")








##########################################################################################################
##########################################################################################################
##########################################################################################################
######################## ANÁLISIS DE DATOS ###############################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################



#############################################################
#### Energía de todo el sistema para cada paso de tiempo #### 
#############################################################

time_energy <- data.frame(ccc = character(), E0 = character(), mag = character(), stringsAsFactors = FALSE )

for (m in 1:(nrow(truetime))) {
  
  edgelist.year <- edgelist[edgelist$ccc <= m,] # Selección del paso del tiempo
  
  
  #####################################################
  #### Eliminación de columnas que no se necesitan ####
  #####################################################
  
  edgelist.year$year <- NULL
  edgelist.year$ccc <- NULL
  
  net <- graph.data.frame(edgelist.year, directed = FALSE)
  
  author_degree <- degree_nodes(net) # Llama a la función para obtener un dataframe con los grados de los nodos ordenados
  
  
  values_spin <- communities_values[(communities_values$ccc == m),] # para cada paso de tiempo
  
  if(length(communities_time[[(m)]]) > 0){
  
  #############################################################################
  #### Asignación de espines para los nodos que pertenecen a una comunidad ####
  #############################################################################
  
  nodes <- data.frame(author = character(), spin = character(), stringsAsFactors = FALSE )
 
  for (p in 1:(length(communities_time[[(m)]]))) {
    
    
    nodes0 <- do.call(rbind.data.frame, communities_time[[(m)]][[p]]) # revisar para que no sea un factor
    nodes0$spin <- values_spin[p,]$spin
    colnames(nodes0) <- c("author", "spin")
    nodes <- rbind(nodes, nodes0)
  }
  
  nodes$author <- as.character(nodes$author)
  
  }else{
    nodes <- data.frame(author = character(), spin = character(), stringsAsFactors = FALSE )
  }
  ################################################################################
  #### Asignación de espines para los nodos que no pertenecen a una comunidad ####
  ################################################################################
  
  nodes_n_c <- author_degree[!(author_degree$author %in% nodes$author ),]
  nodes_n_c$degree <- NULL
  set.seed(1)
  nodes_n_c$spin <- sample(c(1,-1), size= nrow(nodes_n_c), replace=TRUE, prob=c(0.5,0.5))
  
  
  ##################################################################
  #### Los nodos con sus espines se unen en el data.frame nodes ####
  ##################################################################
  
  nodes <- rbind(nodes, nodes_n_c)
  
  
  ##########################################################
  #### Magnetización del sistema en cada paso de tiempo ####
  ##########################################################
  
  magnetization <- sum(nodes$spin)
  
  
  ###################################################################
  #### Creación de el dataframe con el peso de los enlaces (Jij) ####
  ###################################################################
  
  edgelist_Jij <- ddply(edgelist.year, .(edgelist.year$from, edgelist.year$to), nrow)
  colnames(edgelist_Jij) <- c("from", "to", "Jij")
  
  
  ###############################
  #### Cálculo de la energía ####
  ###############################
  
  edgelist_Jij$spin_from <- nodes$spin[match(edgelist_Jij$from, nodes$author)]
  edgelist_Jij$spin_to <- nodes$spin[match(edgelist_Jij$to, nodes$author)]
  
  edgelist_Jij$E0 <- (edgelist_Jij$Jij * edgelist_Jij$spin_from * edgelist_Jij$spin_to)
  
  E0_nodes <- -1 * sum(edgelist_Jij$E0)
  
  sum_field <- -1 * sum(values_spin$field)
   
  E0 <- E0_nodes + sum_field
  
  time_energy0 <- data.frame(ccc = m, E0 = E0, mag = magnetization, stringsAsFactors = FALSE)
  time_energy <- rbind(time_energy, time_energy0)
}

write.csv(time_energy, file = "time_energy.csv", fileEncoding = "UTF-8")



#### Energía normalizada sobre el mínimo valor ####

E_min <- -1*(time_energy[nrow(time_energy),2])
time_energy$U <- time_energy$E0/E_min

write.csv(time_energy, file = "time_energy.csv", fileEncoding = "UTF-8")



###########################################################################################
#### Número de comunidades y tamaño promedio de las comunidades en cada paso de tiempo ####
###########################################################################################

num_mean_communities <- data.frame(ccc = character(), num = character(), mean = character(), Ncm = character(), Nuno = character(),  Ndos = character(), stringsAsFactors = FALSE)

for (n in 1:(length(communities_time))) {
  
  if((length(communities_time[[n]])) == 0){
    
    num_mean_communities0 <- data.frame(ccc = n, num = 0, mean = 0, Ncm = 0, Nuno = 0, Ndos = 0, stringsAsFactors = FALSE)
    num_mean_communities <- rbind(num_mean_communities, num_mean_communities0)
    
  }else{
    
    values_communities <- data.frame(length = character(), stringsAsFactors = FALSE)
    for (p in 1:(length(communities_time[[n]]))) {
      
      values_communities0 <- data.frame(length = length(communities_time[[n]][[p]]), stringsAsFactors = FALSE)
      values_communities <- rbind(values_communities, values_communities0)
      
    }
    
    #### Número de nodos solos ####
    
    datauno <- as.data.frame(values_communities[(values_communities$length == 1), ])
    Ndatauno <- nrow(datauno)
    
    #### Número de nodos en pareja ####
    
    datados <- as.data.frame(values_communities[(values_communities$length == 2), ])
    Ndatados <- 2*nrow(datados)
    
    #### Comunidades de minimo tres nodos ####
    
    values_communities <- values_communities[(values_communities$length > 2), ]
    values_communities <- as.data.frame(values_communities)
    colnames(values_communities) <- c("length")
    
    if(nrow(values_communities) == 0){
      
      num_mean_communities0 <- data.frame(ccc = n, num = 0, mean = 0, Ncm = 0, Nuno = Ndatauno, Ndos = Ndatados, stringsAsFactors = FALSE)
      num_mean_communities <- rbind(num_mean_communities, num_mean_communities0)
      
    }else{
      
      num_mean_communities0 <- data.frame(ccc = n, num = nrow(values_communities), mean = mean(values_communities$length), Ncm = sum(values_communities$length), Nuno = Ndatauno, Ndos = Ndatados, stringsAsFactors = FALSE)
      num_mean_communities <- rbind(num_mean_communities, num_mean_communities0)
    }
  }
  
}

write.csv(num_mean_communities, file = "num_mean_communities.csv", fileEncoding = "UTF-8")

