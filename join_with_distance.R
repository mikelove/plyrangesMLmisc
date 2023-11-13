library(plyranges)
x <- data.frame(seqnames=1,
                start=sample(10,10,FALSE),
                width=2, id=1:10) %>%
  as_granges()
y <- data.frame(seqnames=1,
                start=sample(10,10,FALSE),
                width=2, id=letters[1:10]) %>%
  as_granges()

join_overlap_inner_add_distance <- function(x, y, ...) {
  mcols(y)$.y_start <- start(y)
  mcols(y)$.y_end <- end(y)
  olaps <- join_overlap_inner(x, y, ...)
  sx <- start(olaps)
  ex <- end(olaps)
  sy <- mcols(olaps)$.y_start
  ey <- mcols(olaps)$.y_end
  distance <- ifelse(ex < sy,
                     sy - ex - 1,
              ifelse(sx > ey,
                     sx - ey - 1,
                     0))
  mcols(olaps)$distance <- distance
  mcols(olaps)$.y_start <- NULL
  mcols(olaps)$.y_end <- NULL
  olaps
}

x |>
  join_overlap_inner_add_distance(y, maxgap=1L)
