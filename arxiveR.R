library(rvest)
library(tokenizers)
categories <- c('stat', 'math', 'physics', 'cs', 'q-bio', 'q-fin')

get_css <- function(h, css, tokenize = TRUE, ...) {
  ht <- html_text(html_nodes(h, css))
  if(tokenize) tokenize_words(ht, stopwords = stopwords(), ...) else ht
}

get_arxiv_html_now <- function(category = categories) {
  url <- paste0('https://arxiv.org/list/', match.arg(category), '/new')
  read_html(url)
}

# h <- get_arxiv_html_now('stat')

get_arxiv_now <- function(h) {
  type <- as.character(lapply(get_css(h, '.list-identifier'), function(x) {
    if('cross' %in% x ) 'cross-list'
    else if('replaced' %in% x) 'replacement' else 'new'
  }))
  first_replace <- which(type == 'replacement')[1]
  titles_abs <- get_css(h, '.mathjax')
  abs <- vector('list', length(type))
  abs[1:(first_replace-1)] <- titles_abs[seq(2, first_replace, 2)]
  titles <- lapply(c(titles_abs[seq(1, 2*(first_replace-1), 2)],
                     titles_abs[seq(2*first_replace-1, length(titles_abs))]),
                   function(x) x[-1])
  authors <- lapply(strsplit(get_css(h, '.list-authors', F), '[\n,]'),
                    function(x) x[! x %in% c('', ' ', 'Authors: ')])
  subjects <- lapply(get_css(h, '.list-subjects'),
                     function(x) grep('[.]', x[-1], value = TRUE))
  # comments <- get_css(h, '.list-comments')
  list(
    type = type,
    titles = titles,
    abs = abs,
    authors = authors,
    subjects = subjects
  )
}

# a <- get_arxiv_now(h)

keyword_counts <- function(keywords, arxiv) {
  counts <- numeric(length(keywords))
  for(i in seq_along(keywords))
    counts[i] <- do.call(sum, lapply(arxiv, function(x) keywords[i] == x))
  list(absolute = counts, relative = counts / sum(counts))
}

# learning <- keyword_counts(c('learning', 'deep', 'neural', 'network'), a$abs)

