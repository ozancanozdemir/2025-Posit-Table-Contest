#### Load Packages #####

library(shiny)
library(quantmod)
library(dplyr)
library(gt)
library(plotly)
library(rvest)
library(xml2)
library(stringr)
library(lubridate)
library(shinychat)
library(ellmer)
library(shinyLP)
library(riingo)
library(httr)
library(katex)
library(sentimentr)
library(gtExtras)
library(ggsci)
library(svglite) 
library(zoo) 
library(base64enc)


# Icons
ICON_MAP <- list(
  volume = tags$i(class = "fa fa-chart-bar", style="color:#2962ff;"),
  high = tags$i(class = "fa fa-arrow-up", style="color:#26a69a;"),
  low = tags$i(class = "fa fa-arrow-down", style="color:#ef5350;"),
  avg = tags$i(class = "fa fa-calculator", style="color:#787b86;")
)

# ----------------------------------------------------
############ Helper Functions #############
# ----------------------------------------------------


# Get the tickers of S&P 500 from Wikipedia 

get_sp500_tickers <- function() {
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  page <- tryCatch(read_html(url), error = function(e) NULL)
  
  
  if (is.null(page)) {
    df <- data.frame(
      ticker = c("AAPL","MSFT","GOOGL","AMZN","NVDA","META","TSLA","JPM","V","JNJ"),
      name = c("Apple Inc.","Microsoft","Alphabet Inc.","Amazon","NVIDIA",
               "Meta Platforms","Tesla","JPMorgan Chase","Visa","Johnson & Johnson"),
      sector = NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    tab_node <- html_nodes(page, "table")[[1]]
    tab <- html_table(tab_node, fill = TRUE)
    
    
    sym_col <- c("Symbol","Ticker","Tickers")
    name_col <- c("Security","Company","Name")
    sect_col <- c("GICS Sector","Sector","GICS sector")
    
    
    sym_name <- sym_col[sym_col %in% names(tab)][1]
    comp_name <- name_col[name_col %in% names(tab)][1]
    sect_name <- sect_col[sect_col %in% names(tab)][1]
    
    
    if (is.na(sym_name) || is.na(comp_name)) {
      df <- data.frame(
        ticker = c("AAPL","MSFT","GOOGL","AMZN","NVDA","META","TSLA","JPM","V","JNJ"),
        name = c("Apple Inc.","Microsoft","Alphabet Inc.","Amazon","NVIDIA",
                 "Meta Platforms","Tesla","JPMorgan Chase","Visa","Johnson & Johnson"),
        sector = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      tab <- tab[!is.na(tab[[sym_name]]) & nzchar(tab[[sym_name]]), , drop = FALSE]
      sect_vec <- if (!is.na(sect_name)) tab[[sect_name]] else NA_character_
      df <- data.frame(
        ticker = gsub("\\.", "-", tab[[sym_name]]),
        name = tab[[comp_name]],
        sector = sect_vec,
        stringsAsFactors = FALSE
      )
    }
  }
  
  
  MANUAL_DOMAIN_MAP <- c(
    "BRK.B"="berkshirehathaway.com","BRK.A"="berkshirehathaway.com",
    "GOOGL"="google.com","GOOG"="google.com","META"="meta.com",
    "PM"="pmi.com","PG"="pg.com","KO"="coca-cola-company.com","PEP"="pepsico.com",
    "XOM"="corporate.exxonmobil.com","CVX"="chevron.com","HD"="homedepot.com",
    "MA"="mastercard.com","V"="visa.com","UNH"="unitedhealthgroup.com","JNJ"="jnj.com",
    "WMT"="walmart.com","COST"="costco.com","INTC"="intel.com","NVDA"="nvidia.com",
    "AMD"="amd.com","NFLX"="netflix.com","TSLA"="tesla.com","MSFT"="microsoft.com",
    "AAPL"="apple.com","AMZN"="aboutamazon.com"
  )
  google_favicon <- function(domain, size = 64) {
    paste0("https://www.google.com/s2/favicons?sz=", size, "&domain=", domain)
  }
  
  
  domain <- ifelse(df$ticker %in% names(MANUAL_DOMAIN_MAP),
                   MANUAL_DOMAIN_MAP[df$ticker],
                   paste0(tolower(df$ticker), ".com"))
  logo <- google_favicon(domain)
  
  
  df$domain <- domain
  df$logo <- logo
  df$display <- paste0(df$name, " (", df$ticker, ")")
  df
}

# Get the price of last 30 days for sparklines 
pf_get_prices_for_sparkline <- function(tickers, days = 30){
  end_date <- Sys.Date()
  start_date <- end_date - days
  
  lst <- lapply(tickers, function(tk){ 
    x <- tryCatch(
      quantmod::getSymbols(tk, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE), 
      error = function(e) NULL
    )
    
    if (is.null(x) || NROW(x) < 2) return(NULL) 
    # only close price 
    zoo::coredata(Cl(x))[,1] 
  })
  names(lst) <- tickers
  lst
}



LOGO_SRC_MAP <- c(
  "cnbc.com"    = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/CNBC_logo.svg/2560px-CNBC_logo.svg.png",
  "bloomberg.com" = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTHIywl5QW0qg7BDqKUzx6u10njBQ5bxfDpww&s",
  "reuters.com" = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/Reuters_Logo.svg/2560px-Reuters_Logo.svg.png",
  "seekingalpha.com" = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR5nu0mRRhfXr31SyaQY7BfbZn3hCA9iL75wQ&s",
  "marketwatch.com" = "https://1000logos.net/wp-content/uploads/2024/09/MarketWatch-Logo.jpg",
  "wsj.com"  = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQhUPG0NJYW4RfZuwUf3L5_8t9p6NHNMwh6yw&s"
)

get_date_range <- function(interval) {
  end_date <- Sys.Date()
  start_date <- switch(interval,
                       "1day" = end_date - 1,
                       "7days" = end_date - 7,
                       "1month" = end_date - 30,
                       "3months"= end_date - 90,
                       "6months"= end_date - 180,
                       "1year" = end_date - 365,
                       "2years" = end_date - 730,
                       "5years" = end_date - 1825)
  list(start = start_date, end = end_date)
}

spark_area_svg <- function(values, width = 160, height = 60,
                           up_col = "#26a69a", down_col = "#ef5350",
                           highlight_col = "#e91e63") {
  if (is.null(values) || length(values) < 2 || all(is.na(values))) {
    return("data:image/svg+xml;utf8, ")
  }
  vals <- as.numeric(values)
  trend_up <- tail(vals, 1) >= vals[1]
  line_col <- if (trend_up) up_col else down_col
  fill_col <- grDevices::adjustcolor(line_col, alpha.f = 0.20)
  idx <- if (trend_up) which.min(vals) else which.max(vals)
  df <- data.frame(x = seq_along(vals), y = vals)
  
  g <- ggplot(df, aes(x, y)) +
    geom_area(fill = fill_col) +
    geom_line(linewidth = 1, color = line_col) +
    geom_point(data = df[idx, , drop = FALSE], aes(x, y),
               size = 2.5, color = highlight_col) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  svg_txt <- svglite::stringSVG(
    width = width/96, height = height/96, bg = "transparent",
    { print(g) }
  )
  paste0("data:image/svg+xml;utf8,", URLencode(svg_txt, reserved = TRUE))
}


# ----------------------------------------------------
############ Stock Data Table ###############
# ----------------------------------------------------

get_stock_data <- function(ticker, interval) {
  dr <- get_date_range(interval)
  tryCatch({
    getSymbols(ticker, src = "yahoo", from = dr$start, to = dr$end, auto.assign = FALSE)
  }, error = function(e) NULL)
}

favicon_url <- function(domain) {
  paste0("https://www.google.com/s2/favicons?sz=64&domain=", domain)
}

domain_from_url_or_source <- function(url, source) {
  if (!is.null(source) && nzchar(source) && grepl("\\.", source)) return(tolower(source))
  dom <- tryCatch({
    dom1 <- sub("^https?://(www\\.)?", "", url)
    sub("/.*$", "", dom1)
  }, error = function(e) NA_character_)
  tolower(ifelse(is.na(dom), "news.google.com", dom))
}

make_riingo_gt <- function(df) {
  if (!NROW(df)) {
    return(
      gt::gt(data.frame(Message = "No recent headlines found.")) |>
        gt::tab_options(table.width = gt::pct(100))
    )
  }
  
  gt::gt(df) |>
    gt::cols_label(
      when = "Published",
      title_md = "Title",
      source = "source", # logo
      sentiment = "Sentiment"
    ) |>
    gt::fmt_datetime(columns = when, date_style = 6, time_style = 4) |>
    gt::fmt_markdown(columns = title_md) |>
    gt::text_transform(
      locations = gt::cells_body(columns = source),
      fn = function(x) {
        lapply(x, function(u) {
          if (is.na(u) || !nzchar(u)) gt::html("") else gt::html(sprintf("<img src='%s' style='height:18px;'>", u))
        })
      }
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "rgba(38,166,154,0.18)"),
                   gt::cell_text(color = "#26a69a", weight = "bold")),
      locations = gt::cells_body(columns = sentiment, rows = sentiment == "Positive")
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "rgba(239,83,80,0.18)"),
                   gt::cell_text(color = "#ef5350", weight = "bold")),
      locations = gt::cells_body(columns = sentiment, rows = sentiment == "Negative")
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill(color = "rgba(128,128,128,0.20)"),
                   gt::cell_text(color = "#9aa3af", weight = "bold")),
      locations = gt::cells_body(columns = sentiment, rows = sentiment == "Neutral")
    ) |>
    gt::tab_options(
      table.width = gt::pct(100),
      table.font.size = gt::px(12),
      table.font.names = "JetBrains Mono",
      table.background.color = "#1e2431",
      column_labels.background.color = "#252b3d",
      column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "#2962ff",
      column_labels.border.bottom.width = gt::px(3),
      data_row.padding = gt::px(8)
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "#ffffff", weight = "bold"),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "#e0e6ed"),
      locations = gt::cells_body(columns = c(when, title_md)) # diğer metin sütunları
    )
}

# ----------------------------------------------------
############ Market Cap Table ###############
# ----------------------------------------------------


market_cap_fallback_df <- function() {
  df <- data.frame(Ticker = c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA"),
                   Name = c("Apple", "Microsoft", "Alphabet", "Amazon", "NVIDIA"),
                   MarketCap = c(3000, 2800, 2200, 1900, 1500),
                   Price = c(200, 400, 180, 150, 1000),
                   Change = c(1.5, 0.8, 2.1, -0.5, 3.2),
                   Revenue = c(383, 211, 305, 575, 76), stringsAsFactors = FALSE)
  logo_map <- setNames(get_sp500_tickers()$logo, get_sp500_tickers()$ticker)
  df$Logo <- logo_map[df$Ticker]
  df <- df |> mutate(Name_Logo = paste0(Name, "||", Logo))
  return(df)
}

get_market_cap_data <- function(){
  url <- "https://stockanalysis.com/list/sp-500-stocks/"
  
  df <- tryCatch({
    
    page <- xml2::read_html(url)
    market_cap_table <- page |> rvest::html_node(xpath = "/html/body/div[1]/div[1]/div[2]/main/div[2]/div/div/div[5]/table") |> rvest::html_table()
    market_cap_table_top_10 <- market_cap_table |> head(10)
    
    # df_clean
    market_cap_table_top_10$`Stock Price` <- market_cap_table_top_10$`Stock Price` |> as.numeric()
    market_cap_table_top_10$`% Change` <- gsub("%","",market_cap_table_top_10$`% Change`) |> as.numeric()
    market_cap_table_top_10$Revenue <- gsub("B","",market_cap_table_top_10$Revenue) |> as.numeric()
    market_cap_table_top_10$`Market Cap`<-gsub("B","",market_cap_table_top_10$`Market Cap`)
    market_cap_table_top_10$`Market Cap`<- gsub(",","",market_cap_table_top_10$`Market Cap`)|>as.numeric()
    df_success <- market_cap_table_top_10[,-1]
    colnames(df_success) <- c("Ticker","Name","MarketCap","Price","Change","Revenue")
    df_success$Ticker[df_success$Ticker == "BRK.B"]<- "BRK-B" 
    
    # match logos 
    logo_map <- setNames(get_sp500_tickers()$logo, get_sp500_tickers()$ticker)
    df_success$Logo <- logo_map[df_success$Ticker]
    df_success <- df_success |> mutate(Name_Logo = paste0(Name, "||", Logo))
    return(df_success)
    
  }, error = function(e) {
    message(paste("Market Cap web scraping failed:", e$message))
    
    return(market_cap_fallback_df())
  })
  
  return(df)
}

make_key_stats_gt_alternative <- function(data, spark_data) {
  data$Price_Sparkline_Data <- lapply(data$Ticker, function(t) spark_data[[t]])
  
  
  df_display <- data |>
    dplyr::select(c("Name_Logo","MarketCap","Price_Sparkline_Data","Revenue"))
  colnames(df_display)[3] <- "30D_Trend"
  
  
  
  series_list <- df_display$`30D_Trend`
  
  
  tbl <- gt::gt(df_display) |>
    gt::cols_label(
      Name_Logo = "Name",
      MarketCap = "Market Cap (B)",
      `30D_Trend` = "30D Trend",
      Revenue = "Revenue (B)"
    ) |>
    
    gt::text_transform(
      locations = gt::cells_body(columns = Name_Logo),
      fn = function(x) {
        lapply(x, function(val) {
          parts <- stringr::str_split(val, "\\|\\|")[[1]]
          name <- parts[1]; logo <- parts[2]
          gt::html(
            paste0(
              '<div style="display:flex;align-items:center;padding:5px 0;">',
              '<img src="', logo, '" style="width:20px;height:20px;margin-right:8px;border-radius:4px;object-fit:contain;">',
              '<span>', name, '</span></div>'
            )
          )
        })
      }
    ) |># MarketCap bar
    gtExtras::gt_plt_bar(MarketCap, labels = TRUE, color = "#2962ff",
                         scale_type = "number", width = 70)
  
  
 
  tbl <- tbl |>
    gt::text_transform(
      locations = gt::cells_body(columns = `30D_Trend`),
      fn = function(x) {
        lapply(seq_along(x), function(i) {
          vals <- series_list[[i]]
          if (is.null(vals) || length(vals) < 2) {
            return(gt::html('<span style="color:#787b86;">n/a</span>'))
          }
          uri <- spark_area_svg(vals)
          gt::html(paste0(
            "<img src='", uri, "' style='width:140px;height:48px;border-radius:6px;'>"
          ))
        })
      }
    ) |>
    gtExtras::gt_color_box(
      columns = Revenue,
      domain = c(min(data$Revenue, na.rm = TRUE) - 5,
                 max(data$Revenue, na.rm = TRUE) + 5),
      palette = "ggsci::blue_material", accuracy = 1
    ) |>
    gt::tab_options(
      table.width = gt::pct(100), table.font.size = gt::px(12),
      table.font.names = "JetBrains Mono",
      table.background.color = "#131722",
      column_labels.background.color = "#1e2431",
      column_labels.font.weight = "bold",
      column_labels.border.bottom.color = "#2962ff",
      column_labels.border.bottom.width = gt::px(3),
      data_row.padding = gt::px(8), table.border.top.style = "none"
    ) |>
    gt::tab_style(
      locations = gt::cells_column_labels(columns = gt::everything()),
      style = gt::cell_text(color = "#ffffff", weight = "bold")
    ) |>
    gt::tab_style(
      locations = gt::cells_body(columns = c(`30D_Trend`)),
      style = gt::cell_text(color = "#e0e6ed")
    ) |>
    gt::tab_header(
      title = gt::md("**Top 10 Companies by Market Cap**"),
      subtitle = gt::md("Source: stockanalysis.com")
    ) |>
    gt::tab_style(
      locations = gt::cells_title(groups = "title"),
      style = gt::cell_text(font = "JetBrains Mono", size = "medium",
                            weight = "bold", color = "#fff")
    ) |>
    gt::tab_style(
      locations = gt::cells_title(groups = "subtitle"),
      style = gt::cell_text(size = "small", color = "#787b86")
    )
  
  
  tbl
}

# ----------------------------------------------------
######### Fundamental Table  ################
# ----------------------------------------------------

get_fund_data <- function() {
  
  sp500_info <- get_sp500_tickers()
  logo_map <- setNames(sp500_info$logo, sp500_info$ticker)
  
  fallback_df <- data.frame(
    Symbol_Cleaned = c("AAPL", "MSFT", "GOOGL", "NVDA", "JPM"),
    Name = c("Apple Inc.", "Microsoft Corp.", "Alphabet Inc.", "NVIDIA Corp.", "JPMorgan Chase & Co."),
    P_E = c(30.2, 35.8, 27.1, 74.3, 11.5),
    EPS_dilTTM = c(6.40, 10.30, 5.50, 12.50, 15.10),
    Div_Yield_Pct = c(0.005, 0.007, 0, 0.001, 0.025),
    Analyst_Rating = c("Buy", "Strong buy", "Buy", "Strong buy", "Neutral"),
    stringsAsFactors = FALSE
  ) |>
    mutate(
      Logo = logo_map[Symbol_Cleaned],
      Name_Logo = paste0(Name, "||", Logo)
    ) |>
    select(Name_Logo, P_E, EPS_dilTTM, Div_Yield_Pct, Analyst_Rating)
  
  
  df <- tryCatch({
    url<-"https://www.tradingview.com/symbols/SPX/components/"
    page <- read_html(url)
    table_node <- page |> rvest::html_nodes("table")
    
    
    if (length(table_node) == 0) stop("No table found on TradingView page.")
    
    fund_table <- table_node[[1]] |> html_table(fill = TRUE, header = TRUE)
    
   
    data <- fund_table[,c(1,7,8,10,12)]
    colnames(data)<- c("Symbol","P_E", "EPS_dilTTM", "Div_Yield_Pct", "Analyst_Rating")
    
    ticker_list <- names(logo_map)
    
    parsed_data <- data |>
      rowwise() |>
      mutate(
        Symbol_Cleaned = {
          best_ticker <- ""
          for (t in ticker_list) {
            if (startsWith(Symbol, t) && nchar(t) > nchar(best_ticker)) {
              best_ticker <- t
            }
          }
          best_ticker
        },
        Name = {
          if (nchar(Symbol_Cleaned) > 0) {
            trimws(sub(pattern = Symbol_Cleaned, replacement = "", x = Symbol, fixed = TRUE))
          } else {
            Symbol
          }
        }
      ) |>
      ungroup()
    
    df_success <- parsed_data |>
      mutate(
       
        P_E = as.numeric(gsub(pattern = "[^0-9.-]", replacement = "", x = P_E)),
        EPS_dilTTM = as.numeric(gsub(pattern = "[^0-9.-]", replacement = "", x = EPS_dilTTM)),
        Div_Yield_Pct = as.numeric(gsub(pattern = "[^0-9.-]", replacement = "", x = Div_Yield_Pct)) / 100,
        Logo = logo_map[Symbol_Cleaned],
        Name_Logo = paste0(Name, "||", Logo)
      ) |>
      select(Name_Logo, P_E, EPS_dilTTM, Div_Yield_Pct, Analyst_Rating)
    
    return(df_success)
  }, error = function(e) {
    message(paste("Fundamental web scraping failed. Using static fallback data. Error:", e$message))
    
    return(fallback_df)
  })
  
  return(df)
}

render_name_logo <- function(x) {
  lapply(x, function(val) {
    parts <- stringr::str_split(val, "\\|\\|")[[1]]
    name <- parts[1]
    logo <- parts[2]
    gt::html(
      paste0(
        '<div style="display: flex; align-items: center; padding: 5px 0;">',
        '<img src="', logo, '" style="width: 20px; height: 20px; margin-right: 8px; border-radius: 4px; object-fit: contain;">',
        '<span>', name, '</span>',
        '</div>'
      ))
  })
}

make_fund_gt <- function(data) {
  
  rating_colors <- c(
    "Strong buy" = "#034C02", "Buy" = "#037302", "Neutral" = "darkorange",
    "Sell" = "#ef5350", "Strong sell" = "darkred"
  )
  
  df_styled <- data |>
    mutate(`Analyst Rating` = factor(Analyst_Rating, levels = names(rating_colors))) |> select(!(Analyst_Rating))
  
  gt_table <- gt(df_styled) |>
    
    cols_label(
      Name_Logo = "Company", P_E = "P/E Ratio", EPS_dilTTM = "EPS (TTM)",
      Div_Yield_Pct = "Div. Yield", `Analyst Rating`= "Rating"
    ) |>
    
    text_transform(locations = cells_body(columns = Name_Logo), fn = render_name_logo) |>
    
    
    fmt_currency(columns = EPS_dilTTM, currency = "USD") |>
    fmt_percent(columns = Div_Yield_Pct, decimals = 2) |>
    
    gt_plt_bar(P_E, color = "#2962ff", background = "#131722", labels = TRUE, width = 40,scale_type = "number") |>
    gt_color_rows(EPS_dilTTM, palette = c("steelblue","#2962ff")) |>
    gt_color_rows(Div_Yield_Pct, palette = c("steelblue","#2962ff")) |>
    
    # Colorize Analyst Rating
    tab_style(style = list(cell_fill(color = rating_colors["Strong buy"]), cell_text(color = "#131722", weight = "bold")), locations = cells_body(columns = `Analyst Rating`, rows = `Analyst Rating` == "Strong buy")) |>
    tab_style(style = list(cell_fill(color = rating_colors["Buy"]), cell_text(color = "#131722", weight = "bold")), locations = cells_body(columns = `Analyst Rating`, rows = `Analyst Rating` == "Buy")) |>
    tab_style(style = list(cell_fill(color = rating_colors["Neutral"]), cell_text(color = "#131722", weight = "bold")), locations = cells_body(columns = `Analyst Rating`, rows = `Analyst Rating` == "Neutral")) |>
    
    # Table Theme
    opt_all_caps() |>
    tab_options(
      table.width = pct(100), table.font.size = px(12), table.background.color = "#131722",table.font.names = "JetBrains Mono",
      column_labels.background.color = "#1e2431", column_labels.border.bottom.color = "#2962ff",
      column_labels.border.bottom.width = px(3), data_row.padding = px(8)
    ) |>
    tab_style(locations = cells_column_labels(columns = everything()), style = cell_text(color = "#ffffff", weight = "bold")) |>
    tab_style(locations = cells_body(columns = -`Analyst Rating`), style = cell_text(color = "#e0e6ed")) |>
    tab_header(title = "",subtitle = md("Source: tradingview.com"))
  
  return(gt_table)
}

weight_pill_html <- function(v_prop) {
  
  v <- suppressWarnings(as.numeric(v_prop))
  if (!is.finite(v) || is.na(v)) v <- 0
  pct <- max(0, min(1, v))
  label <- sprintf("%.1f%%", pct * 100)
  fill_w <- round(100 * pct) 
  htmltools::HTML(sprintf(
    "<div style='position:relative;width:120px;height:26px;background:#2a2e39;border-radius:13px;overflow:hidden;display:flex;align-items:center;justify-content:center;'>\n <div style='position:absolute;left:0;top:0;bottom:0;width:%dpx;background:#1f7a74;border-radius:13px;opacity:.85;'></div>\n <span style='position:relative;color:#e0e6ed;font-weight:700;'>%s</span>\n </div>",
    fill_w, label
  ))
}


# ----------------------------------------------------
########### UI ############
# ----------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$style(HTML("
 @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500;600;700&display=swap');
 body { background: linear-gradient(135deg, #0a0e27 0%, #1a1f3a 100%); font-family: 'Inter', sans-serif; color: #e0e6ed; line-height: 1.5; }

 .header-image-container {
  width: 98%;
  margin: 20px auto 0 auto;
  border-radius: 12px;
  overflow: hidden;
  box-shadow: 0 8px 32px rgba(0,0,0,.4);
  max-height: 200px;
 }
 .header-image-container img {
  width: 100%;
  height: 100%;
  display: block;
  object-fit: cover;
 }

 .app-footer{
  position: fixed; left: 0; right: 0; bottom: 0;
  height: 56px; z-index: 1000;
  display: flex; align-items: center; justify-content: center; gap: 18px;
  background: linear-gradient(180deg, #252b3d 0%, #1e2431 100%); /* Koyu, şık degrade */
  border-top-left-radius: 12px; border-top-right-radius: 12px;
  border-top: 1px solid #2a2e39;
  box-shadow: 0 -8px 24px rgba(0,0,0,0.5);
  font-family: 'JetBrains Mono', monospace; /* Yeni font */
 }
 .app-footer, .app-footer *:not(i) {
  color: #e0e6ed !important; /* Beyaza yakın, uyumlu renk */
  font-family: 'JetBrains Mono', monospace !important; /* Font zorlaması */
 }
 
 .app-footer a{ text-decoration: none; font-weight: 600; }
 .app-footer a:hover{ text-decoration: underline; opacity: .95; }
 @media (max-width: 480px){
  .app-footer{ height: 60px; gap: 16px; padding: 0 10px; }
  .app-footer a, .app-footer span{ font-size: 13px; }
 }
 
 .main-container { background: #131722; border-radius: 12px; padding: 20px; margin: 20px auto; width: 98%; box-shadow: 0 8px 32px rgba(0,0,0,.4); }
 .stock-header { background: linear-gradient(135deg, #1e2431 0%, #252b3d 100%); border-radius: 10px; padding: 20px; margin-bottom: 20px; border: 1px solid #2a2e39; }
 .stock-logo { width: 48px; height: 48px; border-radius: 8px; background: transparent; padding: 0; margin-right: 15px; }
 .stock-logo img { width: 100%; height: 100%; object-fit: contain; border-radius: 8px; }
 .stock-name { font-size: clamp(18px, 2.2vw, 24px); font-weight: 700; color: #fff; margin: 0; font-family: 'JetBrains Mono', monospace; }
 .stock-ticker { font-size: 14px; color: #9aa3af; font-weight: 600; font-family: 'JetBrains Mono', monospace; letter-spacing: 1px; }
 .price-big { font-size: clamp(24px, 3.2vw, 36px); font-weight: 700; color: #fff; margin: 10px 0 5px 0; font-family: 'JetBrains Mono', monospace; line-height: 1.2; }
 .price-change { font-size: 18px; font-weight: 600; padding: 6px 12px; border-radius: 6px; display: inline-block; font-family: 'JetBrains Mono', monospace; }
 .price-up { color: #26a69a; background: rgba(38,166,154,.1); }
 .price-down { color: #ef5350; background: rgba(239,83,80,.1); }
 .stat-card { background: #1e2431; border-radius: 8px; padding: 15px; border: 1px solid #2a2e39; margin-bottom: 10px; }
 .stat-label { font-size: 12px; color: #787b86; text-transform: uppercase; letter-spacing: .5px; font-weight: 600; margin-bottom: 5px; display: flex; align-items: center; gap: 8px; }
 .stat-value { font-size: 20px; font-weight: 700; color: #fff; font-family: 'JetBrains Mono', monospace; }
 .stat-high { color: #26a69a; }
 .stat-low { color: #ef5350; }
 .btn-primary { background: linear-gradient(135deg, #2962ff 0%, #1e88e5 100%); border: none; border-radius: 8px; padding: 12px 24px; font-weight: 600; transition: all .3s; }
 .btn-primary:hover { background: linear-gradient(135deg, #1e88e5 0%, #1565c0 100%); transform: translateY(-2px); box-shadow: 0 4px 12px rgba(41,98,255,.4); }
 .selectize-input, .form-control, .form-select, select,
 .api-input input[type='text'], .api-input input[type='password'] {
  background: #1e2431 !important; border: 1px solid #2a2e39 !important;
  color: #e0e6ed !important; font-weight: 600;
 }
 .selectize-input .item, .selectize-input .item * {
  color: #fff !important; font-weight: 700; background-color: #252b3d !important;
 }
 .selectize-input input { color: #e0e6ed !important; }
 .selectize-dropdown { background: #1e2431 !important; border: 1px solid #2a2e39 !important; }
 .selectize-dropdown .option { color: #e0e6ed !important; border-bottom: 1px solid #2a2e39; }
 .selectize-dropdown .option:hover { background: #252b3d !important; }
 .well { background: #131722; border: 1px solid #2a2e39; border-radius: 12px; box-shadow: none; }
 h3, h4 { color: #fff; font-weight: 700; font-family: 'JetBrains Mono', monospace; letter-spacing: .5px; }
 .api-input { background: #1e2431; border: 1px solid #2a2e39; border-radius: 8px; padding: 10px; margin-bottom: 15px; }
 hr { border-color: #2a2e39; opacity: .3; }
 .checkbox label { color: #e0e6ed; font-weight: 500; }
 ::-webkit-scrollbar { width: 8px; height: 8px; }
 ::-webkit-scrollbar-track { background: #131722; }
 ::-webkit-scrollbar-thumb { background: #2a2e39; border-radius: 4px; }
 ::-webkit-scrollbar-thumb:hover { background: #363a45; }
 body { padding-bottom: 72px; }
 "))
  ),
  
  tags$div(
    class = "app-footer",
    tags$span(class = "copyright-text", HTML("&copy;"), "2025 Ozancan Ozdemir"),
    tags$span("•"),
    tags$a(
      href = "https://github.com/ozancanozdemir/2025-Posit-Table-Contest",
      target = "_blank",
      tags$i(class = "fa-brands fa-github", style = "font-size: 12px;"),
      "View Source"
    ),
    tags$span("•"),
    tags$a(href = "https://github.com/ozancanozdemir/2025-Posit-Table-Contest/issues/app.R", target = "_blank", "Edit this page"),
    tags$span("•"),
    tags$a(href = "https://github.com/ozancanozdemir/2025-Posit-Table-Contest/issues", target = "_blank", "Report Issue")
  ),
  
  # Header Cover
  tags$div(
    class = "header-image-container",
    tags$img(
      src = "https://ozancanozdemir.github.io/shiny_cover.png"
      
    )
  ),
  
  div(class = "main-container",
      
      # Upper side
      fluidRow(
        # Left part
        column(
          width = 6,
          # Market Cap Table
          div(
            style = "background: #131722; border-radius: 10px; padding: 15px; margin-bottom: 20px; border: 1px solid #2a2e39;",
            h4("🏛️ S&P 500 Market Overview", style="margin-top: 0;"),
            fluidRow(
              column(6, selectInput("market_cap_sort_col", "Sort Column", choices = c("MarketCap", "Price", "Revenue"), selected = "MarketCap")),
              column(6, selectInput("market_cap_sort_dir", "Direction", choices = c("Descending" = "desc", "Ascending" = "asc"), selected = "desc"))
            ),
            div(style = "max-height: 640px; overflow-y: auto;", gt_output("key_stats_table"))
          ),
          
          # Fundamental Indicators
          div(
            style = "background: #1e2431; border-radius: 10px; padding: 15px; margin-bottom: 20px; border: 1px solid #2a2e39;",
            h4("📊 S&P 500 Fundamental Metrics", style="margin-top: 0;"),
            fluidRow(
              column(6,
                     selectInput("fund_sort_col", "Sort By",
                                 choices = c("P/E Ratio" = "P_E", "EPS (TTM)" = "EPS_dilTTM", "Div. Yield" = "Div_Yield_Pct", "Rating" = "Analyst_Rating"),
                                 selected = "P_E")),
              column(6,
                     selectInput("fund_sort_dir", "Direction",
                                 choices = c("Ascending" = "asc", "Descending" = "desc"),
                                 selected = "asc"))
            ),
            fluidRow(
              column(12,
                     selectInput("fund_table_rows", "Rows to Show",
                                 choices = c("10", "20", "50", "All"),
                                 selected = "10")
              )
            ),
            div(style = "max-height: 400px; overflow-y: auto;", gt_output("fundamental_table"))
          )
        ),
        
        # Middle Part
        column(
          width = 6,
          # Stock Header
          uiOutput("top_header_ui"),
          
          # Selections and Stats
          fluidRow(
            column(3, selectInput("interval", "TIME PERIOD", choices = c("1D"="1day","1W"="7days","1month"="1month","3M"="3months","6M"="6months","1Y"="1year","2Y"="2years","5Y"="5years"), selected = "3months")),
            column(3, uiOutput("ticker1_select_ui")),
            column(3, conditionalPanel(condition = "input.compare_mode == true", uiOutput("ticker2_select_ui"))),
            column(3, checkboxInput("compare_mode", "Compare Mode", FALSE), actionButton("refresh", "REFRESH DATA", class = "btn-primary", style = "width: 100%; margin-top: 10px;"))
          ),
          fluidRow(
            column(3, div(class = "stat-card", div(class="stat-label", ICON_MAP$volume, "VOLUME"), div(class="stat-value", textOutput("volume_stat", inline=TRUE)))),
            column(3, div(class = "stat-card", div(class="stat-label", ICON_MAP$high, "52W HIGH"), div(class="stat-value stat-high", textOutput("high_52w_stat", inline=TRUE)))),
            column(3, div(class = "stat-card", div(class="stat-label", ICON_MAP$low, "52W LOW"), div(class="stat-value stat-low", textOutput("low_52w_stat", inline=TRUE)))),
            column(3, div(class = "stat-card", div(class="stat-label", ICON_MAP$avg, "AVG VOLUME"),div(class="stat-value", textOutput("avg_volume_stat", inline=TRUE))))
          ),
          
          br(),
          
          # Previous Prices
          conditionalPanel(condition = "input.compare_mode == false", tags$div(style = "font-family: 'JetBrains Mono', monospace; font-size: 18px; font-weight: 700; color: #fff; margin: 20px 0 15px 0; padding: 15px 20px; background: linear-gradient(135deg, #1e2431 0%, #252b3d 100%); border-radius: 10px; border-left: 5px solid #2962ff;", "📊 RECENT TRADING DATA - ", tags$span(style = "color: #2962ff;", textOutput("table_ticker1", inline = TRUE))), gt_output("price_table1")),
          conditionalPanel(condition = "input.compare_mode == true", tags$h3(style = "font-family: 'JetBrains Mono', monospace; color: #fff; margin: 20px 0;", "⚖️ COMPARISON DATA"), fluidRow(column(6, tags$div(style = "font-family: 'JetBrains Mono', monospace; font-size: 16px; font-weight: 700; color = #fff; margin-bottom: 15px; padding: 12px 15px; background: linear-gradient(135deg, #1e2431 0%, #252b3d 100%); border-radius: 8px; border-left: 4px solid #2962ff; text-align: center;", "📈 ", textOutput("table_ticker1_compare", inline = TRUE)), gt_output("price_table1_compare")), column(6, tags$div(style = "font-family: 'JetBrains Mono', monospace; font-size: 16px; font-weight: 700; color = #fff; margin-bottom: 15px; padding: 12px 15px; background: linear-gradient(135deg, #1e2431 0%, #252b3d 100%); border-radius: 8px; border-left: 4px solid #26a69a; text-align: center;", "📈 ", textOutput("table_ticker2_compare", inline = TRUE)), gt_output("price_table2_compare")))),
          br(),
          
          # Plotly plot
          plotlyOutput("price_plot", height = "500px"),
          br()
        )
      ),
      
      # Lower Part
      fluidRow(
        column(
          width = 6,
          # Riingo News
          div(class = "main-container", style="margin: 0; padding: 15px;",
              h4("📰 MARKET NEWS"),
              uiOutput("riingo_selected_info"),
              br(),
              fluidRow(
                column(8, selectInput("riingo_source", "SOURCE DOMAIN", choices = c("bloomberg.com","reuters.com","wsj.com","seekingalpha.com","marketwatch.com","cnbc.com"), selected = "bloomberg.com")),
                column(4, numericInput("riingo_limit", "SHOW LAST", value = 10, min = 5, max = 50, step = 1))),
              gt_output("riingo_news_gt")
          )
        ),
        column(
          width = 6,
          # AI Chat
          div(class = "main-container", style="margin: 0; padding: 15px; height: 100%;", # Height 100% for alignment
              h4("🤖 AI TRADING ASSISTANT"),
              div(class = "api-input",
                  passwordInput("gemini_api_key", "GEMINI API KEY", placeholder = "Enter your API key...", width = "100%"),
                  tags$small(style = "color: #787b86;",
                             a(href = "https://aistudio.google.com/app/apikey", target = "_blank", style = "color: #2962ff;", "Get API Key →")),
                  br(),
                  actionButton("set_api_key", "SET API KEY", class = "btn-primary", style = "width: 100%;")),
              br(),
              div(style = "height: 400px;", chat_mod_ui("stock_chat"))
          )
        )
      ),
      
      
      
      br(), br(),
      fluidRow(
        column(
          width = 12,
          div(
            class = "main-container", style = "margin: 0; padding: 15px;",
            h4("💼 Portfolio Calculator"),
            fluidRow(
              column(6,
                     # Ticker selection
                     uiOutput("pf_tickers_ui"),
                    # weight box 
                     uiOutput("pf_weights_ui")
              ),
              column(3,
                     selectInput("pf_interval", "PERIOD",
                                 choices = c("3M" = "3months", "6M" = "6months", "1Y" = "1year", "2Y" = "2years"),
                                 selected = "1year"),
                     numericInput("pf_capital", "Initial Capital (USD)", value = 10000, min = 100, step = 100),
                     checkboxInput("pf_normalize", "Normalize weights to 100% if needed", TRUE),
                     actionButton("pf_run", "CALCULATE", class = "btn-primary", style = "width:100%;")
              ),
              column(3,
                     div(class = "stat-card",
                         div(class = "stat-label", ICON_MAP$avg, "CAGR"),
                         div(class = "stat-value", textOutput("pf_cagr", inline = TRUE))
                     ),
                     div(class = "stat-card",
                         div(class = "stat-label", ICON_MAP$volume, "Volatility (ann.)"),
                         div(class = "stat-value", textOutput("pf_vol", inline = TRUE))
                     ),
                     div(class = "stat-card",
                         div(class = "stat-label", ICON_MAP$high, "Sharpe (rf=0)"),
                         div(class = "stat-value stat-high", textOutput("pf_sharpe", inline = TRUE))
                     )
              )
            ),
            br(),
            fluidRow(
              column(6, gt_output("pf_alloc_table")),
              column(6, plotlyOutput("pf_equity_plot", height = "360px"))
            )
          )
        )
      ),
  )
)

# ----------------------------------------------------
########### Server ###################
# ----------------------------------------------------
server <- function(input, output, session) {
  
  TIINGO_TOKEN <- "8c7094ec74e7fc1ceca99a468fc4770df03dd0ec"
  riingo_set_token(TIINGO_TOKEN)
  
  sp500_tickers <- get_sp500_tickers()
  
  
  sp500_tickers <- get_sp500_tickers()
  

  output$ticker1_select_ui <- renderUI({
    labels_with_logo <- paste0(sp500_tickers$display, "||", sp500_tickers$logo)
    choices_named <- setNames(sp500_tickers$ticker, labels_with_logo)
    
    selectizeInput(
      "ticker1", "PRIMARY STOCK",
      choices = choices_named, selected = "AAPL", width = "100%",
      options = list(
        render = I(
          "{
 option: function(item, escape) {
 	var parts = String(item.label).split('||');
 	var text = parts[0] || '';
 	var logo = parts[1] || '';
 	var img = logo ? ('<img src=\"' + escape(logo) + '\" width=\"20\" height=\"20\" style=\"margin-right:8px;vertical-align:middle;border-radius:3px;\">') : '';
 	return '<div>' + img + escape(text) + '</div>';
 },
 item: function(item, escape) {
 	var parts = String(item.label).split('||');
 	var text = parts[0] || '';
 	var logo = parts[1] || '';
 	var img = logo ? ('<img src=\"' + escape(logo) + '\" width=\"18\" height=\"18\" style=\"margin-right:6px;vertical-align:middle;border-radius:3px;\">') : '';
 	return '<div>' + img + escape(text) + '</div>';
 }
 }"
        )
      )
    )
  })
  
  
  output$ticker2_select_ui <- renderUI({
    labels_with_logo <- paste0(sp500_tickers$display, "||", sp500_tickers$logo)
    choices_named <- setNames(sp500_tickers$ticker, labels_with_logo)
    
    selectizeInput(
      "ticker2", "COMPARE WITH",
      choices = choices_named, selected = "MSFT", width = "100%",
      options = list(
        render = I(
          "{
 option: function(item, escape) {
 	var parts = String(item.label).split('||');
 	var text = parts[0] || '';
 	var logo = parts[1] || '';
 	var img = logo ? ('<img src=\"' + escape(logo) + '\" width=\"20\" height=\"20\" style=\"margin-right:8px;vertical-align:middle;border-radius:3px;\">') : '';
 	return '<div>' + img + escape(text) + '</div>';
 },
 item: function(item, escape) {
 	var parts = String(item.label).split('||');
 	var text = parts[0] || '';
 	var logo = parts[1] || '';
 	var img = logo ? ('<img src=\"' + escape(logo) + '\" width=\"18\" height=\"18\" style=\"margin-right:6px;vertical-align:middle;border-radius:3px;\">') : '';
 	return '<div>' + img + escape(text) + '</div>';
 }
 }"
        )
      )
    )
  })
  
  
  # --- Data Reactives ---
  stock_data1 <- reactive({
    req(input$ticker1)
    get_stock_data(input$ticker1, input$interval)
  }) |> bindEvent(input$ticker1, input$interval, input$refresh)
  
  stock_data2 <- reactive({
    req(input$ticker2, input$compare_mode)
    get_stock_data(input$ticker2, input$interval)
  }) |> bindEvent(input$ticker2, input$interval, input$refresh, input$compare_mode)
  
  # --- Top Header ---
  make_stock_header <- function(ticker) {
    tinfo <- sp500_tickers[sp500_tickers$ticker == ticker, ]
    logo <- if (nrow(tinfo)) tinfo$logo else get_logo_url(ticker)
    name <- if (nrow(tinfo)) tinfo$name else ticker
    div(style = "display: flex; align-items: center;",
        tags$div(class = "stock-logo",
                 tags$img(src = logo)),
        div(
          div(class = "stock-name", name),
          div(class = "stock-ticker", ticker)
        )
    )
  }
  
  make_price_bar <- function(data_xts) {
    req(data_xts)
    last_price <- round(as.numeric(tail(Cl(data_xts), 1)), 2)
    prev_price <- round(as.numeric(tail(Cl(data_xts), 2)[1]), 2)
    change <- last_price - prev_price
    change_pct <- (change / prev_price) * 100
    change_class <- if (change >= 0) "price-up" else "price-down"
    change_symbol <- if (change >= 0) "▲" else "▼"
    div(style = "text-align: right;",
        div(class = "price-big", paste0("$", format(last_price, big.mark = ","))),
        div(class = paste("price-change", change_class),
            paste0(change_symbol, " $", abs(round(change, 2)), " (", sprintf("%.2f", abs(change_pct)), "%)")
        )
    )
  }
  
  output$top_header_ui <- renderUI({
    if (isTRUE(input$compare_mode)) {
      div(class = "stock-header",
          fluidRow(
            column(6, make_stock_header(input$ticker1)),
            column(6, make_stock_header(input$ticker2))
          ),
          br(),
          fluidRow(
            column(6, make_price_bar(stock_data1())),
            column(6, make_price_bar(stock_data2()))
          )
      )
    } else {
      div(class = "stock-header",
          fluidRow(
            column(6, make_stock_header(input$ticker1)),
            column(6, make_price_bar(stock_data1()))
          )
      )
    }
  })
  
  # --- Stats (primary) ----
  output$volume_stat <- renderText({
    data <- stock_data1(); req(data)
    vol <- as.numeric(tail(Vo(data), 1))
    paste0(format(round(vol/1e6, 1), big.mark = ","), "M")
  })
  output$high_52w_stat <- renderText({
    data <- stock_data1(); req(data)
    paste0("$", format(round(max(Hi(data), na.rm = TRUE), 2), big.mark = ","))
  })
  output$low_52w_stat <- renderText({
    data <- stock_data1(); req(data)
    paste0("$", format(round(min(Lo(data), na.rm = TRUE), 2), big.mark = ","))
  })
  output$avg_volume_stat <- renderText({
    data <- stock_data1(); req(data)
    avg_vol <- mean(as.numeric(Vo(data)), na.rm = TRUE)
    paste0(format(round(avg_vol/1e6, 1), big.mark = ","), "M")
  })
  
  # --- Tablo Haeders ---
  output$table_ticker1 <- renderText({
    req(input$ticker1)
    input$ticker1
  })
  
  output$table_ticker1_compare <- renderText({
    req(input$ticker1)
    input$ticker1
  })
  
  output$table_ticker2_compare <- renderText({
    req(input$ticker2)
    input$ticker2
  })
  
  # --- Market Cap Data ---
  
  market_cap_data_raw <- reactive({
    get_market_cap_data()
  })
  
  last_month_prices <- reactive({
    req(market_cap_data_raw())
    tickers <- market_cap_data_raw()$Ticker
    pf_get_prices_for_sparkline(tickers, days = 30)
  })
  
  
  market_cap_data_sorted <- reactive({
    df <- market_cap_data_raw()
    req(df, input$market_cap_sort_col, input$market_cap_sort_dir)
    
    sort_col <- input$market_cap_sort_col
    sort_dir <- input$market_cap_sort_dir
    
    if (sort_dir == "asc") {
      df <- df |> arrange(!!sym(sort_col))
    } else {
      df <- df |> arrange(desc(!!sym(sort_col)))
    }
    
    
    df
  })
  
  
  output$key_stats_table <- render_gt({
    data <- market_cap_data_sorted()
    spark_data <- last_month_prices()
    make_key_stats_gt_alternative(data, spark_data) 
  })
  
  # --- Tables ---
  create_table <- function(data, ticker) {
    req(data)
    
    Close_Prices <- Cl(data)
    
    # Calculate Technical Indicators
    rsi <- round(as.numeric(RSI(Close_Prices, n = 14)), 2)
    macd_values <- MACD(Close_Prices, nFast = 12, nSlow = 26, nSig = 9)
    macd <- round(as.numeric(macd_values[, "macd"]), 2)
    signal <- round(as.numeric(macd_values[, "signal"]), 2)
    
    # Create full data frame
    df_full <- data.frame(
      Date = index(data),
      Open = round(as.numeric(Op(data)), 2),
      High = round(as.numeric(Hi(data)), 2),
      Low = round(as.numeric(Lo(data)), 2),
      Close = round(as.numeric(Cl(data)), 2),
      Volume = round(as.numeric(Vo(data))/1e6, 1),
      Change = c(NA, round(diff(as.numeric(Cl(data))), 2)),
      ChangePct = c(NA, round((diff(as.numeric(Cl(data))) / as.numeric(Cl(data))[-NROW(data)]) * 100, 2)),
      RSI = rsi,
      MACD = macd,
      Signal = signal
    )
    
    # Get last 5 rows
    last_5 <- tail(df_full, 5)
    
    gt(last_5) |>
      # Update Column Labels
      cols_label(
        Date = "Date", Open = "Open", High = "High", Low = "Low",
        Close = "Close", Volume = "Volume", Change = "Change", ChangePct = "Change %",
        RSI = "RSI(14)", MACD = "MACD", Signal = "Signal"
      ) |>
      
    
      text_transform(
        locations = cells_body(columns = Change),
        fn = function(x) {
          x_num <- as.numeric(x) 
          lapply(x_num, function(val) {
            if (is.na(val) || is.infinite(val)) return(gt::html('<span style="color:#787b86;">-</span>'))
            
            if (val > 0) {
              color <- "#26a69a"; icon <- "▲" 
            } else if (val < 0) {
              color <- "#ef5350"; icon <- "▼" 
            } else {
              color <- "#e0e6ed"; icon <- ""
            }
            
            display_val <- paste0("$", sprintf("%.2f", abs(val))) # $ format
            
            gt::html(paste0('<span style="color:', color, '; font-weight: 700; margin-right: 4px;">', icon, '</span>', '<span style="color:#e0e6ed;">', display_val, '</span>'))
          })
        }
      ) |>
      text_transform(
        locations = cells_body(columns = ChangePct),
        fn = function(x) {
          x_num <- as.numeric(x) 
          lapply(x_num, function(val) {
            if (is.na(val) || is.infinite(val)) return(gt::html('<span style="color:#787b86;">-</span>'))
            
            if (val > 0) {
              color <- "#26a69a"; icon <- "▲" 
            } else if (val < 0) {
              color <- "#ef5350"; icon <- "▼" 
            } else {
              color <- "#e0e6ed"; icon <- ""
            }
            
            display_val <- paste0(sprintf("%.2f", abs(val)), "%") 
            
            gt::html(paste0('<span style="color:', color, '; font-weight: 700; margin-right: 4px;">', icon, '</span>', '<span style="color:#e0e6ed;">', display_val, '</span>'))
          })
        }
      ) |>
      
      
      fmt_currency(columns = c(Open, High, Low, Close), currency = "USD") |> 
      fmt_number(columns = Volume, decimals = 1, pattern = "{x}M") |>
      
     
      tab_style(
        style = list(cell_fill(color = "rgba(38,166,154,0.1)"),
                     cell_text(color = "#26a69a", weight = "bold")),
        locations = cells_body(columns = RSI, rows = RSI >= 70)
      ) |>
      tab_style(
        style = list(cell_fill(color = "rgba(239,83,80,0.1)"),
                     cell_text(color = "#ef5350", weight = "bold")),
        locations = cells_body(columns = RSI, rows = RSI <= 30)
      ) |>
      
      
      tab_style(
        style = list(cell_text(color = "#2962ff", weight = "bold")),
        locations = cells_body(columns = MACD)
      ) |>
      tab_style(
        style = list(cell_text(color = "#ff9800", weight = "bold")),
        locations = cells_body(columns = Signal)
      ) |>
      
      # General Table Options
      tab_options(
        table.width = pct(100),
        table.font.size = px(12),
        table.font.names = "JetBrains Mono",
        table.background.color = "#1e2431",
        column_labels.background.color = "#252b3d",
        column_labels.font.weight = "bold",
        column_labels.border.bottom.color = "#2962ff",
        column_labels.border.bottom.width = px(3),
        data_row.padding = px(8)
      ) |>
      tab_style(
        style = cell_text(color = "#ffffff", weight = "bold"),
        locations = cells_column_labels(columns = everything())
      ) |>
      tab_style(
        style = cell_text(color = "#e0e6ed"),
        locations = cells_body(columns = c(Date, Open, High, Low, Close, Volume, RSI, MACD, Signal)) # Okların uygulandığı kolonlar hariç
      )
  }
  
  output$price_table1 <- render_gt({ create_table(stock_data1(), input$ticker1) })
  output$price_table1_compare <- render_gt({
    
    create_table(stock_data1(), input$ticker1)
  })
  output$price_table2_compare <- render_gt({
    
    create_table(stock_data2(), input$ticker2)
  })
  
  # --- Plot ---
  output$price_plot <- renderPlotly({
    data1 <- stock_data1(); req(data1)
    
    if (!input$compare_mode) {
      df <- data.frame(
        Date = index(data1),
        Open = as.numeric(Op(data1)),
        High = as.numeric(Hi(data1)),
        Low = as.numeric(Lo(data1)),
        Close= as.numeric(Cl(data1))
      )
      
      plot_ly(df, type = "candlestick",
              x = ~Date, open = ~Open, close = ~Close, high = ~High, low = ~Low,
              increasing = list(fillcolor = "#26a69a", line = list(color = "#26a69a")),
              decreasing = list(fillcolor = "#ef5350", line = list(color = "#ef5350"))) |>
        layout(
          title = list(text = paste("The Stock Price of", input$ticker1), color = "#787b86"),
          paper_bgcolor = "#131722", plot_bgcolor = "#131722",
          xaxis = list(title = "", gridcolor = "#2a2e39", color = "#787b86"),
          yaxis = list(title = "Price (USD)", gridcolor = "#2a2e39", color = "#787b86"),
          font = list(color = "#e0e6ed"),
          hovermode = "x unified",
          legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(30,36,49,.8)",
                        bordercolor = "#2a2e39", font = list(color = "#e0e6ed")),
          margin = list(l = 60, r = 40, t = 20, b = 40)
        )
      
    } else {
      data2 <- stock_data2(); req(data2)
      df1 <- data.frame(Date = index(data1), Close = as.numeric(Cl(data1)))
      df2 <- data.frame(Date = index(data2), Close = as.numeric(Cl(data2)))
      df1$Normalized <- (df1$Close / df1$Close[1] - 1) * 100
      df2$Normalized <- (df2$Close / df2$Close[1] - 1) * 100
      
      plot_ly() |>
        add_lines(data = df1, x = ~Date, y = ~Normalized,
                  name = input$ticker1, line = list(color = '#2962ff', width = 3)) |>
        add_lines(data = df2, x = ~Date, y = ~Normalized,
                  name = input$ticker2, line = list(color = '#26a69a', width = 3)) |>
        layout(
          title = list(text = paste("The Daily Price Change Comparison Between", input$ticker1, "and", input$ticker2), color = "#787b86"),
          paper_bgcolor = "#131722", plot_bgcolor = "#131722",
          xaxis = list(title = "", gridcolor = "#2a2e39", color = "#787b86"),
          yaxis = list(title = "Change (%)", gridcolor = "#2a2e39", color = "#787b86"),
          font = list(color = "#e0e6ed"),
          hovermode = "x unified",
          legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(30,36,49,.8)",
                        bordercolor = "#2a2e39", font = list(color = "#e0e6ed")),
          margin = list(l = 60, r = 40, t = 20, b = 40)
        )
    }
  })
  
  # --- AI Chat and Riingo News ---
  chat_client <- reactiveVal(NULL)
  
  observeEvent(input$set_api_key, {
    req(input$gemini_api_key)
    api_key <- trimws(input$gemini_api_key)
    
    if (!nzchar(api_key)) {
      showNotification("Please enter a valid API key.", type = "error")
      return()
    }
    
    if (!requireNamespace("ellmer", quietly = TRUE)) {
      showNotification("❌ ellmer package not found. Please install it: install.packages('ellmer')", type = "error", duration = 8)
      return()
    }
    
    
    
    client <- tryCatch({
      ellmer::chat_google_gemini(
        model = "gemini-2.5-flash",
        api_key = api_key,
        system_prompt = paste0(
          "You are an expert financial advisor and stock analyst. ",
          "Be concise and data-driven. Respond in the same language as the user's input."
        )
      )
    }, error = function(e) e)
    
    if (inherits(client, "error")) {
      showNotification(
        paste0("❌ Gemini init failed (check API key/model name): ", client$message),
        type = "error", duration = 10
      )
      return()
    }
    
    chat_client(client)
    showNotification("✅ Gemini connected (model: gemini-2.5-flash). Language is set to auto-detect.", type = "message", duration = 4)
  })
  
  observe({
    if (!is.null(chat_client())) {
      chat_mod_server("stock_chat", chat_client())
    }
  }) |> bindEvent(chat_client())
  
  # --- Tiingo token settings (Riingo) ---
  observeEvent(input$set_tiingo, {
    req(input$tiingo_token)
    tok <- trimws(input$tiingo_token)
    if (nchar(tok) == 0) {
      showNotification("Please enter a valid Tiingo token", type = "error")
      return()
    }
    tryCatch({
      riingo_set_token(tok)
      showNotification("✅ Tiingo token set!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$riingo_selected_info <- renderUI({
    req(input$ticker1)

    idx <- match(input$ticker1, sp500_tickers$ticker)
    nm <- sp500_tickers$name[idx]
    lg <- sp500_tickers$logo[idx]
    
    

    if (is.na(nm) || !nzchar(nm)) nm <- input$ticker1
    if (is.na(lg) || !nzchar(lg)) lg <- paste0("https://www.google.com/s2/favicons?sz=64&domain=",
                                               tolower(input$ticker1), ".com")
    
    
    htmltools::HTML(sprintf(
      '<div style="display:flex;align-items:center;gap:8px;margin:6px 0 4px 0;">
<img src="%s" style="width:18px;height:18px;border-radius:3px;object-fit:contain;">
<span style="color:#9aa3af;">Now showing news for <strong style="color:#e0e6ed">%s</strong> (<code>%s</code>)</span>
</div>',
      lg, nm, input$ticker1
    ))
  })
  
  # --- News received from Riingo ---
  riingo_news_data <- reactive({
    req(input$ticker1)
    src <- if (!is.null(input$riingo_source) && nzchar(input$riingo_source)) input$riingo_source else "bloomberg.com"
    lim <- if (!is.null(input$riingo_limit)) input$riingo_limit else 10
    
    logo_url <- LOGO_SRC_MAP[[src]]
    if (is.null(logo_url)) logo_url <- ""
    
    raw <- tryCatch(riingo_news(ticker = input$ticker1, source = src),
                    error = function(e) data.frame())
    if (!NROW(raw)) {
      return(data.frame(when = as.POSIXct(character()),
                        title_md = character(),
                        source = character(),
                        sentiment = character()))
    }
    
    df <- raw |>
      arrange(desc(publishedDate)) |>
      head(lim)
    
    score <- tryCatch(sentimentr::sentiment_by(df$title)$ave_sentiment,
                      error = function(e) rep(0, nrow(df)))
    label <- ifelse(score > 0.2, "Positive",
                    ifelse(score < -0.2, "Negative", "Neutral"))
    
    df |>
      mutate(
        when  = lubridate::with_tz(as.POSIXct(publishedDate, tz = "UTC"), tzone = Sys.timezone()),
        title_md = sprintf("[%s](%s)", title, url),
        source = logo_url, 
        sentiment = label  
      ) |>
      dplyr::select(when, title_md, source, sentiment)
  }) |> bindEvent(input$ticker1, input$riingo_source, input$riingo_limit, input$refresh)
  
  output$riingo_news_gt <- render_gt({
    df <- riingo_news_data()
    make_riingo_gt(df)
  })
  
  # Fund Table
  fundamental_data_all <- reactive({
    get_fund_data()
  })
  
  # Sort Fund table
  fundamental_data_sorted <- reactive({
    data <- fundamental_data_all()
    req(data, input$fund_sort_col, input$fund_sort_dir)
    
    sort_col <- input$fund_sort_col
    sort_dir <- input$fund_sort_dir
    
    #Analyst Rating Sorting
    if (sort_col == "Analyst_Rating") {
      
      rating_levels <- c("Strong buy", "Buy", "Neutral", "Sell", "Strong sell")
      
      
      data$rating_order <- match(data$Analyst_Rating, rating_levels)
      sort_target <- "rating_order"
      
      
      if (sort_dir == "desc") {
        
        data <- data |> arrange(!!sym(sort_target))
      } else {
        
        data <- data |> arrange(desc(!!sym(sort_target)))
      }
      data <- data |> select(-rating_order)
      
    } else {
      
      if (sort_dir == "asc") {
        data <- data |> arrange(!!sym(sort_col))
      } else {
        data <- data |> arrange(desc(!!sym(sort_col)))
      }
    }
    
    rows <- input$fund_table_rows
    if (rows != "All") {
      num_rows <- as.numeric(rows)
      data <- head(data, num_rows)
    }
    
    return(data)
  })
  
  # Fund Table
  output$fundamental_table <- render_gt({
    
    req(fundamental_data_sorted())
    make_fund_gt(fundamental_data_sorted())
  })
  
  
 ### Portfolio Table ###
  
  output$pf_tickers_ui <- renderUI({
    labels_with_logo <- paste0(sp500_tickers$display, "||", sp500_tickers$logo)
    choices_named <- setNames(sp500_tickers$ticker, labels_with_logo)
    selectizeInput(
      "pf_tickers", "TICKERS",
      choices = choices_named, multiple = TRUE, width = "100%",
      options = list(
        maxItems = 8,
        plugins = list("remove_button"),
        render = I('{
    		option: function(item, escape) {
    			var parts = String(item.label).split("||");
    			var text = parts[0] || ""; var logo = parts[1] || "";
    			var img = logo ? ("<img src=\\""+escape(logo)+"\\" width=\\"18\\" height=\\"18\\" style=\\"margin-right:6px;vertical-align:middle;border-radius:3px;\\">") : "";
    			return "<div>" + img + escape(text) + "</div>";
    		},
    		item: function(item, escape) {
    			var parts = String(item.label).split("||");
    			var text = parts[0] || ""; var logo = parts[1] || "";
    			var img = logo ? ("<img src=\\""+escape(logo)+"\\" width=\\"16\\" height=\\"16\\" style=\\"margin-right:6px;vertical-align:middle;border-radius:3px;\\">") : "";
    			return "<div>" + img + escape(text) + "</div>";
    		}
    	}')
      )
    )
  })
  
  

  output$pf_weights_ui <- renderUI({
    req(input$pf_tickers)
    n <- length(input$pf_tickers)
    if (n == 0) return(NULL)
    eq <- round(100 / n, 2)
    tagList(
      tags$label(style = "color:#787b86; font-weight:600;", "WEIGHTS (%)"),
      fluidRow(lapply(seq_len(n), function(i){
        column(4, numericInput(paste0("pf_w_", i), input$pf_tickers[i], value = eq, min = 0, max = 100, step = 1))
      }))
    )
  })
  
 #### merge prices ###
  pf_get_prices <- function(tickers, interval){
    if (length(tickers) == 0) return(NULL)
    lst <- lapply(tickers, function(tk){ x <- get_stock_data(tk, interval); if (is.null(x)) return(NULL); Cl(x) })
    keep <- !sapply(lst, is.null); lst <- lst[keep]; tickers <- tickers[keep]
    if (length(lst) == 0) return(NULL)
    px <- do.call(merge, lst)
    colnames(px) <- tickers
    
    px <- zoo::na.locf(px, na.rm = FALSE)
    px <- zoo::na.locf(px, fromLast = TRUE)
    px
  }
  

  pf_weights <- reactive({
    req(input$pf_tickers)
    ws <- sapply(seq_along(input$pf_tickers), function(i) input[[paste0("pf_w_", i)]])
    ws <- as.numeric(ws)
    if (any(is.na(ws))) ws[is.na(ws)] <- 0
    if (isTRUE(input$pf_normalize) && sum(ws) != 100) ws <- if (sum(ws) == 0) rep(100/length(ws), length(ws)) else ws/sum(ws)*100
    ws/100
  })
  
  pf_prices <- eventReactive(input$pf_run, { pf_get_prices(input$pf_tickers, input$pf_interval) })
  
  pf_returns <- reactive({ px <- pf_prices(); req(px); na.omit(px/lag(px) - 1) })
  
  
  pf_port_ret <- reactive({
    R <- pf_returns(); req(NCOL(R) > 0)
    w <- pf_weights(); req(length(w) == NCOL(R))
    xts::xts(as.numeric(R %*% matrix(w, ncol = 1)), order.by = zoo::index(R))
  })
  
  pf_equity <- reactive({
    R <- pf_port_ret(); req(R, input$pf_capital)
    capital <- as.numeric(input$pf_capital)
    cum <- cumprod(1 + R)
    val <- capital * as.numeric(cum)
    xts::xts(val, order.by = zoo::index(R))
  })
  
  
  output$pf_cagr <- renderText({
    eq <- pf_equity(); req(eq)
    n <- NROW(eq); if (n < 2) return("-")
    cagr <- (as.numeric(tail(eq,1))/as.numeric(head(eq,1)))^(252/n) - 1
    paste0(sprintf("%.2f", cagr*100), "%")
  })
  
  output$pf_vol <- renderText({
    R <- pf_port_ret(); req(R)
    vol_ann <- stats::sd(as.numeric(R), na.rm = TRUE) * sqrt(252)
    paste0(sprintf("%.2f", vol_ann*100), "%")
  })
  
  output$pf_sharpe <- renderText({
    R <- pf_port_ret(); req(R)
    mu <- mean(as.numeric(R), na.rm = TRUE) * 252
    sig <- stats::sd(as.numeric(R), na.rm = TRUE) * sqrt(252)
    if (sig == 0) return("-")
    sprintf("%.2f", mu/sig)
  })
  
  
  ### allocation table ### 

  output$pf_alloc_table <- render_gt({
    px <- pf_prices(); req(px)
    tick <- as.character(colnames(px))
    
    

    w_raw <- pf_weights() 
    w_named <- setNames(as.numeric(w_raw), input$pf_tickers)
    w <- as.numeric(w_named[tick])
    w[is.na(w)] <- 0
    
    
    last_px <- as.numeric(tail(px, 1))
    
    
   
    info <- sp500_tickers[match(tick, sp500_tickers$ticker), c("name","logo","sector")]
    name_safe <- ifelse(is.na(info$name) | info$name == "", tick, info$name)
    logo_safe <- ifelse(is.na(info$logo) | info$logo == "", paste0("https://www.google.com/s2/favicons?sz=64&domain=", tolower(tick), ".com"), info$logo)
    sector_safe <- ifelse(is.na(info$sector) | info$sector == "", "—", info$sector)
    
    
    df <- dplyr::tibble(
      Name_Logo = paste0(name_safe, "||", logo_safe),
      Ticker = tick,
      Sector = sector_safe,
      Weight = w, 
      `Last Price` = last_px
    ) |> dplyr::arrange(dplyr::desc(Weight))
    
    
   
    w_vals <- df$Weight
    
    
    gt::gt(df) |>
      
      gt::text_transform(
        locations = gt::cells_body(columns = Name_Logo),
        fn = function(x){ lapply(x, function(val){
          parts <- strsplit(val, "\\|\\|")[[1]]; nm <- parts[1]; lg <- parts[2]
          gt::html(paste0(
            '<div style="display:flex;align-items:center;padding:4px 0;">',
            '<img src="', lg, '" style="width:18px;height:18px;margin-right:8px;border-radius:3px;object-fit:contain;">',
            '<span>', nm, '</span></div>'
          ))
        })}
      ) |>
      gt::cols_label(
        Name_Logo = "Name",
        Ticker = "Ticker",
        Sector = "Sector",
        Weight = "Weight",
        `Last Price` = "Last Price (USD)"
      ) |>
     
      gt::text_transform(
        locations = gt::cells_body(columns = Weight),
        fn = function(x){
          lapply(seq_along(x), function(i){ weight_pill_html(w_vals[i]) })
        }
      ) |>
      gt::fmt_currency(columns = `Last Price`, currency = "USD") |>
      gt::tab_options(
        table.width = gt::pct(100), table.font.size = gt::px(12),
        table.font.names = "JetBrains Mono", table.background.color = "#1e2431",
        column_labels.background.color = "#252b3d", column_labels.font.weight = "bold",
        column_labels.border.bottom.color = "#2962ff", column_labels.border.bottom.width = px(3),
        data_row.padding = gt::px(8)
      ) |>
      gt::tab_style(style = gt::cell_text(color = "#ffffff", weight = "bold"),
                    locations = gt::cells_column_labels(columns = gt::everything())) |>
      gt::tab_style(style = gt::cell_text(color = "#e0e6ed"),
                    locations = gt::cells_body(columns = gt::everything()))
  })
  
  
  ### plot of portfolio ## 
  output$pf_equity_plot <- renderPlotly({
    eq <- pf_equity(); req(eq)
    df_port <- data.frame(Date = zoo::index(eq), Value = as.numeric(eq))
    
    
   
    start_d <- min(df_port$Date, na.rm = TRUE)
    end_d <- max(df_port$Date, na.rm = TRUE)
    idx_xts <- tryCatch(quantmod::getSymbols("^GSPC", src = "yahoo", from = start_d, to = end_d, auto.assign = FALSE),
                        error = function(e) NULL)
    if (!is.null(idx_xts)) {
      idx_close <- as.numeric(Cl(idx_xts))
      idx_dates <- zoo::index(idx_xts)
      if (length(idx_close) > 1) {
        idx_norm <- (idx_close / idx_close[1]) * as.numeric(input$pf_capital)
        df_spx <- data.frame(Date = idx_dates, Value = idx_norm)
      } else {
        df_spx <- NULL
      }
    } else {
      df_spx <- NULL
    }
    
    
    plt <- plotly::plot_ly(df_port, type = "scatter", mode = "lines",
                           x = ~Date, y = ~Value, name = "Portfolio")
    if (!is.null(df_spx)) {
      plt <- plt |>
        plotly::add_lines(data = df_spx, x = ~Date, y = ~Value, name = "S&P 500 (^GSPC)")
    }
    
    
    plt |>
      plotly::layout(
        title = list(text = "Portfolio Value (vs S&P 500)", color = "#787b86"),
        paper_bgcolor = "#131722", plot_bgcolor = "#131722",
        xaxis = list(title = "", gridcolor = "#2a2e39", color = "#787b86"),
        yaxis = list(title = "USD", gridcolor = "#2a2e39", color = "#787b86"),
        font = list(color = "#e0e6ed"), hovermode = "x unified",
        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(30,36,49,.8)",
                      bordercolor = "#2a2e39", font = list(color = "#e0e6ed")),
        margin = list(l = 60, r = 40, t = 20, b = 40)
      )
  })
  
}

shinyApp(ui = ui, server = server)
