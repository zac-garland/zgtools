## code to prepare `ishares_tbl` dataset goes here
# https://www.ishares.com/us/products/etf-investments
ishares_tbl <- clipr::read_clip_tbl() %>%
  as_tibble() %>%
  slice(2:nrow(.)) %>%
  clean_names() %>%
  select(ticker, name, net_assets = net_assets_usd, asset_class:investment_style) %>%
  mutate(net_assets = parse_number(net_assets))


write_csv(ishares_tbl, "data-raw/ishares_tbl.csv")

usethis::use_data(ishares_tbl, overwrite = TRUE)


