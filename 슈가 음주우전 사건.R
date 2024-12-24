install.packages("KoNLP")
install.packages("rvest")
install.packages("httr")
install.packages("dplyr")
install.packages("wordcloud2")
install.packages("jsonlite")
install.packages("tidytext")

library(KoNLP)
library(rvest)
library(httr)
library(dplyr)
library(wordcloud2)
library(jsonlite)
library(tidytext)

# KoNLP 설정
useNIADic()

# 감성 사전 로드
sentiment_dict <- fromJSON("/Users/jeemins/Desktop/bigdata/data_modified.json")
sentiment_dict <- as.data.frame(sentiment_dict)

# 감성 사전 구조 확인
colnames(sentiment_dict) <- c("word", "word_root", "polarity")

# 뉴스 데이터 수집 함수
get_news_data <- function(query, start_date, end_date, pages = 5) {
  base_url <- "https://search.naver.com/search.naver?where=news&query="
  date_param <- paste0("&ds=", start_date, "&de=", end_date, "&nso=so:r,p:from", 
                       gsub("-", "", start_date), "to", gsub("-", "", end_date), ",a:all")
  page_param <- "&start="
  
  news_data <- data.frame(title = character(), content = character(), link = character(), stringsAsFactors = FALSE)
  
  for (page in seq(1, pages * 10, by = 10)) {
    url <- paste0(base_url, query, date_param, page_param, page)
    page_content <- read_html(url)
    
    # 뉴스 제목, 링크 추출
    titles <- page_content %>% html_nodes(".news_tit") %>% html_attr("title")
    links <- page_content %>% html_nodes(".news_tit") %>% html_attr("href")
    
    # 뉴스 제목과 링크 확인
    print(head(titles))  # 제목 확인
    print(head(links))   # 링크 확인
    
    # 뉴스 기사 내용 추출
    contents <- sapply(links, function(link) {
      article_page <- tryCatch({
        read_html(link)
      }, error = function(e) NULL)  # 오류가 발생하면 NULL 반환
      
      if (!is.null(article_page)) {
        # 뉴스 콘텐츠가 포함된 HTML 요소 찾기
        content <- article_page %>% html_node(".newsct_article") %>% html_text()
        return(content)
      } else {
        return(NA)  # 오류 발생 시 NA 반환
      }
    })
    
    # 데이터 추가
    news_data <- rbind(news_data, data.frame(title = titles, content = contents, link = links, stringsAsFactors = FALSE))
  }
  
  return(news_data)
}

# 날짜 설정 및 뉴스 데이터 가져오기
query <- URLencode("슈가")  # 검색 키워드
start_date <- "2024-09-11"  # 시작 날짜
end_date <- "2024-09-30"    # 종료 날짜

news_data <- get_news_data(query, start_date, end_date, pages = 5)  # 5페이지 크롤링
head(news_data)

# 뉴스 제목과 기사 내용 결합
all_text <- c(news_data$title, news_data$content)

# 텍스트 전처리 및 토큰화
tokens <- data.frame(text = all_text, stringsAsFactors = FALSE) %>%
  unnest_tokens(word, text)  # 'word' 열에 단어가 저장됨

# 명사 빈도 계산
nouns_freq <- tokens %>%
  count(word, sort = TRUE)

# 감성 사전과 병합 (색상 정보가 포함된 'polarity' 컬럼 사용)
nouns_freq_sentiment <- merge(nouns_freq, sentiment_dict, by = "word", all.x = TRUE)

# 감성 정보가 없는 단어는 회색으로 처리 (NA를 gray로 대체)
nouns_freq_sentiment$polarity[is.na(nouns_freq_sentiment$polarity)] <- "gray"

# 특정 단어를 수동으로 처리 (감성 사전에서 누락된 경우)
nouns_freq_sentiment$polarity[nouns_freq_sentiment$word == "슈가"] <- "red"
nouns_freq_sentiment$polarity[nouns_freq_sentiment$word == "bts"] <- "blue"

# "NULL" 문제 처리: NULL 값 제거
nouns_freq_sentiment <- nouns_freq_sentiment %>%
  filter(!is.na(word))  # NA나 NULL인 단어는 제거

# 빈도수 대로 정리
nouns_freq_sentiment <- nouns_freq_sentiment %>%
  arrange(desc(n))  # 빈도수 기준으로 내림차순 정렬

# 워드클라우드 생성 (색상 정보 반영)
wordcloud2(nouns_freq_sentiment[1:50, c("word", "n", "polarity")], 
           size = 1.5, 
           color = nouns_freq_sentiment$polarity[1:50],  # 'polarity' 컬럼을 색상으로 사용
           backgroundColor = "black")
