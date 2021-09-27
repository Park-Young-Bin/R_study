# install.packages("devtools")
# devtools::install_github("allisonhorst/palmerpenguins")
# library(tidyverse) (new)
# library(palmerpenguins)

# example data set: penguins
head(penguins)
str(penguins)

# bill_length_mm: 부리 가로 길이
# bill_depth_mm: 부리 세로 길이
# flipper_length_mm: 날개 길이

# explore the data (new)
glimpse(penguins) # str() 함수와 비슷

# %>% operator
sum(1:10)
1:10 %>% sum()

# filter() data
penguins %>% 
  filter(species == 'Chinstrap', island == 'Dream')
penguins %>% 
  filter(species %in% c('Chinstrap', 'Adelie'), island == 'Dream')

# select()
penguins %>% 
  select(bill_length_mm:body_mass_g)

penguins %>% 
  select(ends_with('mm')) %>% 
  names()

penguins %>% 
  select(island, bill_length_mm, everything()) %>% 
  head()

# mutate()
penguins %>% 
  select(species, bill_length_mm, bill_depth_mm) %>% 
  mutate(bill_total = bill_length_mm + bill_depth_mm,
         bill_average = bill_total/2) %>% 
  head()

# transmute(new): 파생변수만 추출
penguins %>% 
  select(species, bill_length_mm, bill_depth_mm) %>% 
  transmute(bill_total = bill_length_mm + bill_depth_mm,
         bill_average = bill_total/2) %>% 
  head()

# arrange(): 정렬
penguins %>% 
  select(species, bill_length_mm, bill_depth_mm) %>% 
  mutate(bill_total = bill_length_mm + bill_depth_mm,
         bill_average = bill_total/2) %>% 
  arrange(bill_length_mm, desc(bill_depth_mm))

# summarize()
penguins %>% 
  group_by(species) %>% 
  summarize(bill_length_mean = mean(bill_length_mm, na.rm = T),
            bill_depth_mean = mean(bill_depth_mm, na.rm = T))

# across(): 여러 컬럼에 같은 함수 적용(new)
penguins %>% 
  group_by(species) %>% 
  summarize(bill_length_mean = mean(bill_length_mm, na.rm = T),
            bill_depth_mean = mean(bill_depth_mm, na.rm = T),
            flipper_length_mm = mean(bill_depth_mm, na.rm = T))

penguins %>% 
  group_by(species) %>% 
  summarize(across(bill_length_mm:flipper_length_mm, mean, na.rm = T))
