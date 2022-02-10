test_that("swclose works", {
  withr::with_tempdir({
    pdfDocument <- swopen(outfile = "test.pdf")
    swlatex(pdfDocument, "tttteeeesssstttt")
    expect_false(file.exists("test.pdf"))
    swclose(pdfDocument)
    expect_true(file.exists("test.pdf"))
  })
})
