
# with iris data

petal = iris %>% 
  filter(Species == "setosa") %>% 
  select(-Species)