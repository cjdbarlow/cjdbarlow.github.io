# Turn the colour dataframe into a named vector that can be applied to scale_x_manual
fn.df_to_scale = function(df, nameVar, colourVar){
  names = df[[nameVar]]
  values = df[[colourVar]]
  
  x = setNames(values, names)
  x
}

# Pretty column names
fn.col_pretty = function(df) {
  names = c("State" = "StateAb",
            "Electorate" = "DivisionNm",
            "Candidate" = "name",
            "Party" = "PartyNm",
            "Votes (n)" = "prefCount",
            "Votes (%)" = "prefPC",
            "Margin (n)" = "margin",
            "Party Vote (n)" = "partyVote",
            "Party Vote (%)" = "partyVotePC",
            "Total Seats (n)" = "seatsTotalN",
            "Total Seats (%)" = "seatsTotalPC",
            "Seats: Electorate (n)" = "seatsElecN",
            "Seats: Electorate (%)" = "seatsElecPC",
            "Seats: List (n)" = "seatsListN",
            "Seats: List (n)" = "seatsListPC",
            "Entitled Seats (n)" = "seatsEntitledN",
            "Entitled Seats (%)" = "seatsEntitledPC",
            "Overhang (n)" = "overhang"
  )
  
  df %>%
    rename(any_of(names))
}


# Sainte-Laguë formula
fn.saint_lague = function(votes, seats) {
  quot = votes / (2*seats + 1)
  quot
}

# Make inset maps
fn.mini_map = function(st = ced, lat, long, rad){
  # Define circle centered on a point, with a certain radius. We will take electorates crossing htis area.
  circle = tibble(lat = lat,
                  long = long) %>% 
    st_as_sf(coords = c("long", "lat"),
             crs = 4326) %>%
    st_transform(crs = 6384) %>% 
    st_buffer(dist = rad) %>% 
    st_transform(crs = 4326)
  
  # Identify the geoms that intersect with the circle, and then filter these out (so we don't carve the electorates with circular edges)
  intersect = st_intersection(st, circle)
  st.zoom = st %>%
    filter(CED_NAME21 %in% intersect$CED_NAME21)
  
  
  
  # Make a ggplot of same
  mini.map = st.zoom %>%
    ggplot() +
    geom_sf_interactive(aes(fill = PartyAb,
                            tooltip = tooltip,
                            data_id = DivisionID),
                        lwd = 0.05) +
    # Themeing
    scale_fill_manual(values = colourScale) +
    theme_void() +
    theme(legend.position = "none")
  
  mini.map
}