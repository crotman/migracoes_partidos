
library(tidyverse)
library(tidygraph)
library(ggraph)


migrantes <- read_rds("migrantes_para_envio") %>% 
    ungroup() %>% 
    slice_max(
        order_by = n, n = 50
    ) %>% 
    mutate(
        partido_anterior = str_trunc(partido_anterior, width = 4, ellipsis = ""),
        partido = str_trunc(partido, width = 4, ellipsis = ""),
    )

partidos_origem <- migrantes %>% 
    select(
        partido = partido_anterior
    )

partidos_destino <- migrantes %>% 
    select(
        partido
    )

todos_partidos <- bind_rows(
    partidos_origem,
    partidos_destino
) %>% 
    distinct()


arestas <- migrantes %>% 
    rename(
        from = partido_anterior,
        to = partido
    )

grafo <- create_empty(n =  0) %>% 
    bind_nodes(
        todos_partidos
    ) %>% 
    bind_edges(
        arestas,
        node_key = "partido"
    )




ggraph(grafo) + 
    geom_node_point(
        size = 16,
        shape = 21,
        stroke = 2
    ) +
    geom_node_text(
        aes(
            label = partido,
            
        )
        
    ) +
    geom_edge_link(
        arrow = arrow(),
        end_cap = circle(9, 'mm'), 
        start_cap = circle(9, 'mm'),
        aes(
            edge_width = n,
        ),
        edge_alpha = 0.5
    ) + 
    theme_graph()
    







