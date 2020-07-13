# header things
##################################################

# -- playing dice --- #

set.seed(1337)

# --- header --- #

setwd('~/Work/BME/alszavak_2/alszo_new/')

library(tidyverse)
library(stringdist)
library(fuzzyjoin)
library(glue)

# --- source --- #

cel = read_csv('src/celszavak.txt') # target words
hun = read_tsv('src/hu_list.txt') # hun spelling dict for ref

# --- main functions --- #

# convert a string to hungarian phonemic / disc (one phoneme per character) transcription
magyar2 = function(string, only_first=F){
  
  string = str_to_lower(string)
  # this is not hungarian
  string = str_replace_all(string, 'w', 'v')
  # vow
  string = str_replace_all(string, 'o":', 'W')
  string = str_replace_all(string, 'u":', 'Y')
  string = str_replace_all(string, 'a:', 'A')
  string = str_replace_all(string, 'e:', 'E')
  string = str_replace_all(string, 'i:', 'I')
  string = str_replace_all(string, 'o:', 'O')
  string = str_replace_all(string, 'u:', 'U')
  string = str_replace_all(string, 'u"', 'y')
  string = str_replace_all(string, 'o"', 'w')
  # vow vow
  string = str_replace_all(string, 'á', 'A')
  string = str_replace_all(string, 'é', 'E')
  string = str_replace_all(string, 'í', 'I')
  string = str_replace_all(string, 'ó', 'O')
  string = str_replace_all(string, 'ú', 'U')
  string = str_replace_all(string, 'ü', 'y')
  string = str_replace_all(string, 'ö', 'w')
  string = str_replace_all(string, 'õ', 'W')
  string = str_replace_all(string, 'ő', 'W')
  string = str_replace_all(string, 'û', 'Y')
  string = str_replace_all(string, 'ű', 'Y')
  # s
  string = str_replace_all(string, '(?<![cz])s(?!(z|sz))', 'S')
  # long
  string = str_replace_all(string, 'ccs', 'TT')
  string = str_replace_all(string, 'ddzs', 'DD')
  string = str_replace_all(string, 'ss(?!z)', 'SS')
  string = str_replace_all(string, 'ssz', 'ss')
  string = str_replace_all(string, 'zzs', 'ZZ')
  string = str_replace_all(string, 'tty', 'KK')
  string = str_replace_all(string, 'ggy', 'GG')
  string = str_replace_all(string, 'nny', 'NN')
  string = str_replace_all(string, 'lly', 'jj')
  
  # short
  string = str_replace_all(string, 'cs', 'T')
  string = str_replace_all(string, 'dzs', 'D')
  string = str_replace_all(string, 'sz', 's')
  string = str_replace_all(string, 'zs', 'Z')
  string = str_replace_all(string, 'ty', 'K')
  string = str_replace_all(string, 'gy', 'G')
  string = str_replace_all(string, 'ny', 'N')
  string = str_replace_all(string, 'ly', 'j')
  # voicing
  string = str_replace_all(string, 'p(?=[bdgzDGZ])', 'b')
  string = str_replace_all(string, 'f(?=[bdgzDGZ])', 'v')
  string = str_replace_all(string, 's(?=[bdgzDGZ])', 'z')
  string = str_replace_all(string, 'S(?=[bdgzDGZ])', 'Z')
  string = str_replace_all(string, 't(?=[bdgzDGZ])', 'd')
  string = str_replace_all(string, 'T(?=[bdgzDGZ])', 'D')
  string = str_replace_all(string, 'k(?=[bdgzDGZ])', 'g')
  string = str_replace_all(string, 'K(?=[bdgzDGZ])', 'G')
  # devoicing
  string = str_replace_all(string, 'b(?=[ptksTKSf])', 'p')
  string = str_replace_all(string, 'v(?=[ptksTKSf])', 'f')
  string = str_replace_all(string, 'z(?=[ptksTKSf])', 's')
  string = str_replace_all(string, 'Z(?=[ptksTKSf])', 'S')
  string = str_replace_all(string, 'd(?=[ptksTKSf])', 't')
  string = str_replace_all(string, 'D(?=[ptksTKSf])', 'T')
  string = str_replace_all(string, 'g(?=[ptksTKSf])', 'k')
  string = str_replace_all(string, 'G(?=[ptksTKSf])', 'K')
  if (only_first) {
    string = substr(string, 1, 1)
  }
  return(string)
}

# get transcription and char length for words in d
formatStrings = function(d){
  d = d %>% 
    mutate(
      disc = magyar2(word),
      length = nchar(disc)
    )
  
  return(d)
}

# target words are cvcv (4 char) / cvcvc (5 char): cross them with hun spelling list and get number of neighbours where neighbour <- levenshtein dist of pair == 1
getNeighbours = function(d,my_length){
  
  if (my_length == 4){
    my_hun = hun4
  } else if (my_length == 5){
    my_hun = hun5
  } else {break}
  
  d = d %>% 
    filter(length == my_length)
  
  d2 = d %>% 
    select(target_disc) %>% 
    crossing(my_hun) %>% 
    mutate(
      lv = stringdist(target_disc, disc, method = 'lv')
    ) %>% 
    filter(lv == 1) %>% 
    count(target_disc, name = 'neighbours')
  
  d = d %>% 
    select(target_disc) %>% 
    full_join(d2) %>% 
    mutate(neighbours = ifelse(is.na(neighbours), 0, neighbours))
  
  return(d)
}

# generate sample of nonce words for the cvcv / bvcv / cvbv / cvcvc / bvcvc / cvbvc / cvcvb template
makeNonce = function(my_syl_struc){
  
  vowels = c('a','á','e','é','i','í','o','ó','ö','ő','u','ú','ü','ű')
  consonants = c('c','cs','d','dz','dzs','f','g','h','j','k','l','m','n','ny','p','r','s','sz','t','ty','v','z','zs')
  
  bpos = case_when(
    my_syl_struc %in% c('cvcv', 'cvcvc') ~ 0,
    my_syl_struc %in% c('bvcv', 'bvcvc') ~ 1,
    my_syl_struc %in% c('cvbv', 'cvbvc') ~ 2,
    my_syl_struc == 'cvcvb' ~ 3
  )
  
  my_c1 = case_when(
    bpos == 0 ~ consonants,
    bpos == 1 ~ 'b',
    bpos == 2 ~ consonants,
    bpos == 3 ~ consonants
  )
  
  my_c2 = case_when(
    bpos == 0 ~ consonants,
    bpos == 1 ~ consonants,
    bpos == 2 ~ 'b',
    bpos == 3 ~ consonants
  )
  
  my_c3 = case_when(
    bpos == 0 ~ consonants,
    bpos == 1 ~ consonants,
    bpos == 2 ~ consonants,
    bpos == 3 ~ 'b'
  )
  
  if (nchar(my_syl_struc) == 4){
    
    my_length = 4
    
    d = crossing(
      c1 = my_c1,
      v1 = vowels,
      c2 = my_c2,
      v2 = vowels
    )
    
  } else if (nchar(my_syl_struc) == 5){
    
    my_length = 5
    
    d = crossing(
      c1 = my_c1,
      v1 = vowels,
      c2 = my_c2,
      v2 = vowels,
      c3 = my_c3
    )
    
  }
  
  d = d %>%
    unite(target_word, sep = '') %>% 
    mutate(
      target_disc = magyar2(target_word)
    ) %>% 
    select(target_word,target_disc) %>% 
    filter(!(target_word %in% hun$word))
  
  d = sample_n(d, nrow(d))
  
  d = d %>% 
    mutate(
      syl_struc = my_syl_struc
    )
  
  d = sample_n(d, 4000)

return(d)
}

# get number of neighbours from hun list for cvcv / cvcvc nonce words. also generate some nonce words with 0 neighbours. match nonce words to target words in terms of count of neighbours.
matchVanilla = function(d){
  
  my_length = d$syl_struc %>% 
    unique() %>% 
    nchar()
  
  my_cel = cel %>% 
    filter(length == my_length)
  
  hun2 = hun %>% 
    filter(length < my_length + 2, length > my_length - 2) %>% 
    select(disc)
  
  d = d %>% 
    crossing(hun2)
  
  d = d %>% 
    mutate(lv = stringdist(target_disc, disc, method = "lv"))
  
  d0 = d %>% 
    filter(lv != 1) %>% 
    sample_n(100) %>% 
    select(target_disc, target_word, syl_struc) %>% 
    mutate(neighbours = 0) # nonces with 0 neighbours
  
  d2 = d %>% 
    filter(lv == 1) %>% 
    count(target_disc, syl_struc, target_word, name = 'neighbours') # nonces with 1+ neighbours
  
  d2 = bind_rows(d0, d2)
  
  d3 = d2 %>% 
    rename(nonce = target_word) %>% 
    select(nonce, neighbours, syl_struc) %>% 
    difference_inner_join(my_cel, by = 'neighbours', max_dist = 2, distance_col  = 'distance_tolerance')
  
  return(d3)
}

# make a wide table in which col1: target words, col 2: # neighbours of target, cols 3-4-5: matched nonce words w/ fault tolerance (of 0, of 1, of 2). fault tolerance of 1 means nonce has +/- 1 neighbours as compared to matched target word.
tidyVanilla = function(d){
  
  d2 = d %>% 
    group_by(
      target,
      distance_tolerance
    ) %>% 
    sample_n(10, replace = T) %>% 
    distinct(
      nonce, 
      target,
      syl_struc,
      neighbours.y,
      distance_tolerance
    ) %>% 
    rename(
      target_neighbours = neighbours.y
    ) %>% 
    nest(nonce_words = nonce)
  
  d2 = d2 %>% 
    mutate(word_list = map(nonce_words, ~ 
                             pull(., nonce) %>% 
                             paste(sep="", collapse=", ") 
    )
    ) %>% 
    select(-nonce_words) %>% 
    unnest(word_list)
  
  d2 = d2 %>% 
    ungroup() %>% 
    mutate(`distance to target` = case_when(
      distance_tolerance == 0 ~ 'as many neighbours as target',
      distance_tolerance == 1 ~ '+/- 1 neighbours',
      distance_tolerance == 2 ~ '+/- 2 neighbours',
    )
           ) %>% 
    select(-distance_tolerance) %>%  
    pivot_wider(names_from = `distance to target`, values_from = word_list)
  
  d2 = d2 %>% 
    rename(
      `target word` = target,
      `syllable structure` = syl_struc,
      `number of neighbours` = target_neighbours
    )
  
  return(d2)
}

# count neighbours for one nonce word. sensitive to format of input df, see code below.
getNeighboursForOne = function(d){
  
  my_length = d$syl_struc %>% 
    unique() %>% 
    nchar()
  
  hun2 = hun %>% 
    filter(length < my_length + 2, length > my_length - 2) %>% 
    select(disc)
  
  d = d %>% 
    crossing(hun2)
  
  d = d %>% 
    mutate(lv = stringdist(target_disc, disc, method = "lv"))
  
  n_neighbours = d %>% 
    filter(lv == 1) %>% 
    nrow()
  
  return(n_neighbours)
}

# draw a quartet of nonce words where words are cvcv x2, bvcv, cvbv. words should be at an edit distance of at least 3 from each other. words should not repeat across quartets. output is wide table of quartets.
drawPairs4 = function(d){
  
  n4 = filter(d, nchar(syl_struc) == 4)
  my_min_dist = 3
  cvcv1s = as.list(NULL)
  cvcv2s = as.list(NULL)
  bvcvs = as.list(NULL)
  cvbvs = as.list(NULL)
  
  for (ii in 1:20){
    
    print(glue('row number: {ii}/10'))  
    
    big_flag = F
    
    i = 1
    
    while (!big_flag){
      
      print(glue('iteration: {i}'))  
      
      cvcv1 = filter(d, syl_struc == 'cvcv') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      cvcv2 = filter(d, syl_struc == 'cvcv') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      bvcv = filter(d, syl_struc == 'bvcv') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      cvbv = filter(d, syl_struc == 'cvbv') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      flag1 = cvcv1 != cvcv2
      
      # Nonword-b-1 és Nonword-b-2: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag2 = stringdist(bvcv, cvbv, method = 'lv') >= my_min_dist
      
      # Nonword-b-1 és Nonword-1: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag3 = stringdist(bvcv, cvcv1, method = 'lv') >= my_min_dist
      
      # Nonword-b-2 és Nonword-2: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag4 = stringdist(cvbv, cvcv2, method = 'lv') >= my_min_dist
      
      # Nonword-1 és Nonword-2: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag5 = stringdist(cvcv1, cvcv2, method = 'lv') >= my_min_dist
      
      flag6 = !(cvcv1 %in% cvcv1s | cvcv1 %in% cvcv2s | cvcv2 %in% cvcv1s | cvcv2 %in% cvcv2s | bvcv %in% bvcvs | cvbv %in% cvbvs)
      
      big_flag = ( flag1 & flag2 & flag3 & flag4 & flag5 & flag6 ) | i > 1000
      i = i + 1
    }
    
    cvcv1s[[ii]] = cvcv1
    cvcv2s[[ii]] = cvcv2
    bvcvs[[ii]] = bvcv
    cvbvs[[ii]] = cvbv
  }
  
  foils4 = tibble(
    pair_id = 1:20,
    nonword1 = unlist(cvcv1s),
    nonword2 = unlist(cvcv2s),
    nonwordb1 = unlist(bvcvs),
    nonwordb2 = unlist(cvbvs)
  )
  
  foils4 = foils4 %>% 
    pivot_longer(-pair_id, names_to = 'category', values_to = 'nonce_disc')
  
  foils4 = foils4 %>% 
    left_join(n4)
  
  foils4 = foils4 %>% 
    select(
      pair_id,
      nonce,
      category
    ) %>% 
    pivot_wider(names_from = category, values_from = nonce)
  
  return(foils4)
}

# draw a sextet of nonce words where words are cvcvc x2, bvcvc, cvbvc, cvcvb. words should be at an edit distance of at least 3 from each other. words should not repeat across sextets. output is wide table of sextets.
drawPairs5 = function(d){
  
  n5 = filter(d, nchar(syl_struc) == 5)
  my_min_dist = 3
  cvcvc1s = as.list(NULL)
  cvcvc2s = as.list(NULL)
  cvcvc3s = as.list(NULL)
  bvcvcs = as.list(NULL)
  cvbvcs = as.list(NULL)
  cvcvbs = as.list(NULL)
  
  for (ii in 1:20){
    
    print(glue('row number: {ii}/10'))  
    
    big_flag = F
    
    i = 1
    
    while (!big_flag){
      
      print(glue('iteration: {i}'))  
      
      cvcvc1 = filter(n5, syl_struc == 'cvcvc') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      cvcvc2 = filter(n5, syl_struc == 'cvcvc') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      cvcvc3 = filter(n5, syl_struc == 'cvcvc') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      bvcvc = filter(n5, syl_struc == 'bvcvc') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      cvbvc = filter(n5, syl_struc == 'cvbvc') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      cvcvb = filter(n5, syl_struc == 'cvcvb') %>% 
        sample_n(1) %>% 
        pull(nonce_disc)
      
      flag1 = cvcvc1 != cvcvc2
      
      flag1.5 = cvcvc1 != cvcvc3
      
      flag1.75 = cvcvc2 != cvcvc3
      
      # Nonword-b-1 és Nonword-b-2: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag2 = stringdist(bvcvc, cvbvc, method = 'lv') >= my_min_dist
      
      # Nonword-b-1 és Nonword-1: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag3 = stringdist(bvcvc, cvcvc1, method = 'lv') >= my_min_dist
      
      # Nonword-b-2 és Nonword-2: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag4 = stringdist(cvbvc, cvcvc2, method = 'lv') >= my_min_dist
      
      # ugyanez nwb3 es nw3
      
      flag4.5 = stringdist(cvcvb, cvcvc3, method = 'lv') >= my_min_dist
      
      # Nonword-1 és Nonword-2: minél messzebb legyenek egymástól és ugyanannyi szomszéddal rendelkezzenek
      
      flag5 = stringdist(cvcvc1, cvcvc2, method = 'lv') >= my_min_dist
      flag5.5 = stringdist(cvcvc1, cvcvc3, method = 'lv') >= my_min_dist
      flag5.75 = stringdist(cvcvc2, cvcvc3, method = 'lv') >= my_min_dist
      
      flag6 = !(cvcvc1 %in% cvcvc1s | cvcvc1 %in% cvcvc2s | cvcvc1 %in% cvcvc3s | cvcvc2 %in% cvcvc1s | cvcvc2 %in% cvcvc2s | cvcvc2 %in% cvcvc3s | cvcvc3 %in% cvcvc1s | cvcvc3 %in% cvcvc2s | cvcvc3 %in% cvcvc3s | bvcvc %in% bvcvcs | cvbvc %in% cvbvcs | cvcvb %in% cvcvbs)
      
      big_flag = ( flag1 & flag1.5 & flag1.75 & flag2 & flag3 & flag4 & flag4.5 & flag5 & flag5.5 & flag5.75 & flag6 ) | i > 1000
      i = i + 1
    }
    
    cvcvc1s[[ii]] = cvcvc1
    cvcvc2s[[ii]] = cvcvc2
    cvcvc3s[[ii]] = cvcvc3
    bvcvcs[[ii]] = bvcvc
    cvbvcs[[ii]] = cvbvc
    cvcvbs[[ii]] = cvcvb
  }
  
  foils5 = tibble(
    pair_id = 1:20,
    nonword1 = unlist(cvcvc1s),
    nonword2 = unlist(cvcvc2s),
    nonword3 = unlist(cvcvc3s),
    nonwordb1 = unlist(bvcvcs),
    nonwordb2 = unlist(cvbvcs),
    nonwordb3 = unlist(cvcvbs)
  )
  
  foils5 = foils5 %>% 
    pivot_longer(-pair_id, names_to = 'category', values_to = 'nonce_disc')
  
  foils5 = foils5 %>% 
    left_join(n5)
  
  foils5 = foils5 %>% 
    select(
      pair_id,
      nonce,
      category
    ) %>% 
    pivot_wider(names_from = category, values_from = nonce)
  
  return(foils5)
}

##################################################
# code
##################################################

# --- calc num of neighbours for celszavak --- #

hun = hun %>% 
  formatStrings()
cel = cel %>% 
  formatStrings() %>% 
  rename(target = word, target_disc = disc)

hun4 = hun %>% 
  filter(length <= 5) %>% 
  select(disc)
  
hun5 = hun %>% 
  filter(length <= 6) %>% 
  select(disc)
  
cel4 = cel %>% 
  getNeighbours(4)
cel5 = cel %>% 
  getNeighbours(5)
  
cel = bind_rows(cel4,cel5) %>% 
  left_join(cel)
  
# --- generating sets of nonce words --- #

syl_strucs = c('cvcv', 'bvcv', 'cvbv', 'cvcvc', 'bvcvc', 'cvbvc', 'cvcvb')

nonce_sets = map(
  syl_strucs, ~ makeNonce(.)
)

# --- building nonce word lists for cvcv/cvcvc targets, matched for number of neighbours, with +/-2 fault tolerance (task 1) --- #

vanilla_cvcv = matchVanilla(nonce_sets[[1]])
vanilla_cvcvc = matchVanilla(nonce_sets[[4]])

vanilla_cvcv_2 = tidyVanilla(vanilla_cvcv)
vanilla_cvcvc_2 = tidyVanilla(vanilla_cvcvc)

# --- tidying up big nonce list for generator (task 2) --- #

nonce_list = nonce_sets %>% 
  bind_rows()

nonce_n_neighbours = nonce_list %>% 
  nest(neighbour_corner = c(target_disc, syl_struc))

nonce_n_neighbours = nonce_n_neighbours %>% 
  mutate(
    n_neighbours = map(
      neighbour_corner, ~ getNeighboursForOne(.)
    )
  ) %>% 
  select(-neighbour_corner) %>% 
  unnest(cols = c(n_neighbours))

nonce_list = left_join(nonce_list, nonce_n_neighbours)

nonce_list = nonce_list %>% 
  rename(
    nonce = target_word,
    nonce_disc = target_disc
  )

# --- generator: building "pairs" (quartets / sextets) (task 2) --- #

nonce_list_n_neighbours = nonce_list %>%
  filter(n_neighbours %in% 0:5)

nonce_list_n_neighbours_nested = nonce_list_n_neighbours %>% 
  group_by(n_neighbours) %>% 
  nest()

nonce_list_n_neighbours_nested = nonce_list_n_neighbours_nested %>% 
  mutate(
    pairs4 = map(data, drawPairs4)
  )

pairs4 = nonce_list_n_neighbours_nested %>% 
  select(n_neighbours, pairs4) %>% 
  unnest(cols = c(pairs4))

nonce_list_n_neighbours_nested2 = nonce_list_n_neighbours_nested %>% 
  filter(n_neighbours < 4) %>% 
  mutate(
    pairs5 = map(data, drawPairs5)
  )

pairs5 = nonce_list_n_neighbours_nested2 %>% 
  select(n_neighbours, pairs5) %>% 
  unnest(cols = c(pairs5))

##################################################
# printing
##################################################

write_tsv(vanilla_cvcv_2, 'out/cvcv_alszavak.tsv', na = '')
write_tsv(vanilla_cvcvc_2, 'out/cvcvc_alszavak.tsv', na = '')

write_tsv(nonce_list, 'out/alszavak_szomszedokkal.tsv')

write_tsv(pairs4, 'out/alszavak_negyesek_cvcv.tsv')
write_tsv(pairs5, 'out/alszavak_negyesek_cvcvc.tsv')
