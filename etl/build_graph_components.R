fltr_noncontentious_bills <- function(vote_choices, 
                                        vote_infos, 
                                        thresh = 1.75){
  
  competitive.votes <- vote_infos |>
    mutate(dissent_percent = (no / (yes + no + excused + absent) * 100)) |>
    filter(dissent_percent > thresh)

  print(paste0("Dropping ", (nrow(vote_infos) - nrow(competitive.votes)), "/", nrow(vote_choices), " votes with less than ", thresh, "% opposition..."))
  
  vote_choices |>
    filter(vote_event_id %in% competitive.votes$id)
}

build_vote_edges <- function(raw_vote_choices, raw_vote_info, thresh = 1.75){
  # Vote edges 
  # votes_raw: a df with long-format votes
  # session_year: the year to filter votes_raw by
  # -> a bipartite vote edges df
      # On one side, voters, on the other vote_id_yes/vote_id_no
    fltr_noncontentious_bills(raw_vote_choices, 
                              raw_vote_info) |>
    unite(vote_choice, vote_event_id, option) |>
    select(-c(voter_name, year)) |>
    rename(to = vote_choice, from = person_id) |>
    select(to, from)
}



build_vote_vertices <- function(votes_edges, legislator_info){
  #  Make vertices by pivoting edges
  # vote_edges: a long df of edges from make.vote.edges
  # legislator.info: legislator info with Openstates person_id
  # -> A dataframe of unique vertices, with column "type" giving bipartite group
  votes_edges |>
    # One long column
    pivot_longer(everything()) |>
    # Join in legislator information
    left_join(legislator_info, by = c("value" = "person_id"), multiple = "first") |>
    distinct(value, .keep_all = T) |>
    mutate(type = if_else(name == "to", T, F)) |>
    select(value, type, first_name, last_name, party)
}

build_sponsorship_edges <- function(session_year, sponsorship_relations){

sponsorship_relations |>
    select(person_id, bill_id, year) |>
    distinct() |>
    filter(year == session.year) |>
    select(-year) |>
    rename(to = person_id, from = bill_id) 
}

build_sponsorship_vertices <- function(sponsorship_relations){

  sponsorship_relations |>
    pivot_longer(everything()) |>
    mutate(class = if_else(name == "person_id", T, F)) |>
    select(c(value, class)) |>
    distinct()
  
}


get_graph_backbone <- function(legislators, 
                                vertices, 
                                session_year,
                                edges
                                ) {

  
  vote.vertices <- vertices |>
    mutate(year = session_year,
           label = last_name,
           color = case_when(
            grepl("rep", party, ignore.case = TRUE) ~ "#d30b0d",
            grepl("dem", party, ignore.case = TRUE) ~ "#428bca",
             .default = "#FFDB58"
           ))
  
  
  vote.edges <- edges  |>
    filter((to %in% vote.vertices$value) &
             (from %in% vote.vertices$value)) |>
    drop_na()
  
  graph <- igraph::graph_from_data_frame(vote.edges, vertices = vote.vertices) 
  
  backbone::sdsm(graph, alpha = .01)


}