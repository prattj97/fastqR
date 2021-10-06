test_that("read_fast works", {
  expect_warning(read_fastq("C:/Users/kmlm014/My Documents/R/Training/fastq_examples/good.fq"))
  #expect_equal(ncol(read_fastq("C:/Users/kmlm014/My Documents/R/Training/fastq_examples/good.fq")), 4)
  expect_error(read_fastq("C:/Users/kmlm014/My Documents/R/Training/fastq_examples/broken_format.fq"))
})
