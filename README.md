# 2025 Posit Table Contest 

## Stock Market Dashboard with Advanced Analytics (R Shiny & GT)

### Full Description

[This application](https://ozancanozdemir.shinyapps.io/sp500-monitoring-dashboard/) is designed as a comprehensive **stock market analysis dashboard** for investors and financial professionals. Using real-time and historical data for stocks within the **S&P 500** index, the app offers critical functions like market tracking, fundamental analysis, technical analysis, news monitoring, and portfolio simulation via modern and interactive tables.

The core data sources for the application are the `quantmod` package, which retrieves financial data and technical indicators from **Yahoo Finance**, and the **Tiingo API** (via the `riingo` package) for news headlines.

#### Key Features:

1.  **Market Overview:** Market capitalization, revenue, and the last 30-day price trends of the largest stocks are visualized using `gtExtras` and custom **SVG sparkline** functions.
2.  **Stock Analysis:** A detailed candlestick chart (`plotly`) for the selected stock's historical price movements and the last 5 trading days' key technical indicators such as Open, Close, Volume, Percentage Change, **RSI**, and **MACD** are displayed in conditionally formatted tables (`gt` package).
3.  **Comparison Mode:** Allows comparison of the normalized daily returns of two different stocks over the selected time period.
4.  **Fundamental Metrics:** Key indicators for S&P 500 companies, such as **P/E Ratio**, **Earnings Per Share (EPS)**, and **Dividend Yield**, are listed along with analyst ratings. These tables also utilize the rich formatting capabilities of the `gt` and `gtExtras` packages.
5.  **Market News:** The latest news headlines for the selected stock are shown, complete with the source logo and a simple **Sentiment** analysis tag based on the headline.
6.  **Portfolio Calculator:** Users can build a portfolio with selected tickers and weights. Performance metrics like **Compound Annual Growth Rate (CAGR)**, **Volatility**, and **Sharpe Ratio** are calculated, and an equity curve is plotted (`plotly`) for comparison against the S&P 500 index.
7.  **AI Trading Assistant:** Integrated with a user-provided **Gemini API Key**, the app allows users to interact with an AI-powered chatbot to ask questions about financial analysis and the market (`shinychat` and `ellmer` packages).

The application interface utilizes a **dark theme** and custom **CSS** to provide a modern and professional data dashboard experience.

---

### Project Details

| Detail | Description |
| :--- | :--- |
| **Development Environment** | R Shiny |
| **R Packages Used** | `shiny`, `quantmod`, `gt`, `plotly`, `rvest`, `xml2`, `riingo`, `gtExtras`, `shinychat`, `ellmer`, and others. |
| **Data Sources** | Yahoo Finance (stock prices), stockanalysis.com, tradingview.com (fundamental/market data via web scraping), Tiingo API (news), Gemini API (AI assistant). |
| **Table Library** | `gt` |
| **Dashboard Type** | Interactive Finance/Analysis Dashboard |
| **Code Repository** | (To be filled in by the user) |

--- 

### A Small Glimpse 

<img width="1441" height="668" alt="image" src="https://github.com/user-attachments/assets/664c2af7-20e7-4893-8cee-1a656cb25df4" />

<img width="663" height="602" alt="image" src="https://github.com/user-attachments/assets/2a1aa84e-2036-4d66-8924-50dcda579a67" />

<img width="1368" height="668" alt="image" src="https://github.com/user-attachments/assets/e73c15b2-43a5-404c-af0f-0c018dde11a5" />

<img width="1338" height="732" alt="image" src="https://github.com/user-attachments/assets/556888c8-5cf5-473f-a31b-3c0e4702bb84" />


