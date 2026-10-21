library(tidyverse)
library(futile.logger)


fltr_noncontentious_bills <- function(vote_choices, 
                                        vote_infos, 
                                        thresh = 0){
  
  competitive.votes <- vote_infos |>
    mutate(dissent_percent = (no / (yes + no + excused + absent) * 100)) |>
    filter(dissent_percent > thresh)

  flog.info(paste0("Dropping ", (nrow(vote_infos) - nrow(competitive.votes)), "/", nrow(vote_infos), " votes with at most ", thresh, "% opposition..."))
  
  vote_choices |>
    filter(vote_id %in% competitive.votes$vote_id)


}

build_vote_edges <- function(raw_vote_choices, raw_vote_info, thresh = 1.75){
  # Vote edges 
  # votes_raw: a df with long-format votes
  # session_year: the year to filter votes_raw by
  # -> a bipartite vote edges df
      # On one side, voters, on the other vote_id_yes/vote_id_no
    fltr_noncontentious_bills(raw_vote_choices, 
                              raw_vote_info) |>
    unite(vote_choice, vote_id, option) |>
    select(-c(voter_name, year)) |>
    rename(to = vote_choice, from = person_id) |>
    select(to, from)
}

build_vote_vertices <- function(votes_edges){
  #  Make vertices by pivoting edges
  # vote_edges: a long df of edges from make.vote.edges
  # legislator.info: legislator info with Openstates person_id
  # -> A dataframe of unique vertices, with column "type" giving bipartite group
  votes_edges |>
    # One long column
    pivot_longer(everything()) |>
    distinct(value, .keep_all = T) |>
    mutate(type = if_else(name == "to", T, F)) |>
    select(value, type) |>
    distinct()
}

build_sponsorship_edges <- function(sponsorship_relations){

  sponsorship_relations |>
      select(person_id, bill_id, year) |>
      distinct() |>
      select(-year) |>
      rename(to = person_id, from = bill_id) |>
    drop_na()
}

build_sponsorship_vertices <- function(sponsorship_edges){

  sponsorship_edges |>
    pivot_longer(cols = everything(), names_to = "type", values_to = "id") |>
    distinct(id, type) |>
    mutate(type = if_else(type == "from", T, F)) |>
    select(id, type, everything())
}


backbone_from_components <- function(vertices, 
                                      edges,
                                      session_year
                                    ) {

              
  
  edges <- edges  |>
    filter((to %in% vertices$value) &
             (from %in% vertices$value)) |>
    drop_na()
  
  
  graph <- igraph::graph_from_data_frame(edges, vertices = vertices) 
  
  backbone::backbone_from_projection(graph, alpha = .01, model = "sdsm")


}

get_vote_backbone <- function(legislators, 
                                vote_infos, 
                                vote_choices,
                                session_year,
                              chamber_name){
    
  vote_info_chamber <- vote_infos |>
    filter(.data$chamber == chamber_name)

  vote_choices_chamber <- vote_choices |>
    filter(.data$chamber == chamber_name)


  vote_edges <- build_vote_edges(raw_vote_choices = vote_choices_chamber,
                                  raw_vote_info = vote_info_chamber
                                )
 
  vote_vertices <- build_vote_vertices(votes_edges = vote_edges)
  backbone <- backbone_from_components(vertices = vote_vertices,
                                  edges = vote_edges,
                                  session_year = session_year
                      )
  
  expected_n <- if (chamber_name == "HOUSE") 70 else 35
  if (length(igraph::V(backbone)) != expected_n) {
    flog.warn(paste0("Got the wrong amount of vertices for ", chamber_name, ": ", length(igraph::V(backbone)), " /", expected_n))
  }
  
  return(backbone)
                
}

get_sponsorship_backbone <- function(session_year, sponsorship_relations, legislators){

  sponsorship_edges <- build_sponsorship_edges(sponsorship_relations = sponsorship_relations)
  
  sponsorship_vertices <- build_sponsorship_vertices(sponsorship_edges) 


igraph::graph_from_data_frame(sponsorship_edges, 
                                vertices = sponsorship_vertices) |>
    backbone::backbone_from_projection(model = "sdsm", alpha = .01)

}

