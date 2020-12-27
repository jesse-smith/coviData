# weekday schedule matches original

    # A tibble: 31 x 3
       date       day       scheduled
       <date>     <chr>     <lgl>    
     1 2021-01-01 Friday    TRUE     
     2 2021-01-02 Saturday  FALSE    
     3 2021-01-03 Sunday    FALSE    
     4 2021-01-04 Monday    TRUE     
     5 2021-01-05 Tuesday   TRUE     
     6 2021-01-06 Wednesday TRUE     
     7 2021-01-07 Thursday  TRUE     
     8 2021-01-08 Friday    TRUE     
     9 2021-01-09 Saturday  FALSE    
    10 2021-01-10 Sunday    FALSE    
    # ... with 21 more rows

# 4-2 schedule matches original

    # A tibble: 31 x 3
       date       day       scheduled
       <date>     <chr>     <lgl>    
     1 2021-01-01 Friday    TRUE     
     2 2021-01-02 Saturday  TRUE     
     3 2021-01-03 Sunday    TRUE     
     4 2021-01-04 Monday    FALSE    
     5 2021-01-05 Tuesday   FALSE    
     6 2021-01-06 Wednesday TRUE     
     7 2021-01-07 Thursday  TRUE     
     8 2021-01-08 Friday    TRUE     
     9 2021-01-09 Saturday  TRUE     
    10 2021-01-10 Sunday    FALSE    
    # ... with 21 more rows

# (5-2)-(5-3)-(6-2)x4-(6-3) schedule matches original

    # A tibble: 31 x 3
       date       day       scheduled
       <date>     <chr>     <lgl>    
     1 2021-01-01 Friday    TRUE     
     2 2021-01-02 Saturday  TRUE     
     3 2021-01-03 Sunday    TRUE     
     4 2021-01-04 Monday    FALSE    
     5 2021-01-05 Tuesday   FALSE    
     6 2021-01-06 Wednesday TRUE     
     7 2021-01-07 Thursday  TRUE     
     8 2021-01-08 Friday    TRUE     
     9 2021-01-09 Saturday  TRUE     
    10 2021-01-10 Sunday    FALSE    
    # ... with 21 more rows

