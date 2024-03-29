test_that("swclose works", {
  skip_if(Sys.which("pdflatex") == "", "pdflatex was not found")
  withr::with_tempdir({
    pdfDocument <- swopen(outfile = "test.pdf")
    swlatex(pdfDocument, "tttteeeesssstttt")
    expect_false(file.exists("test.pdf"))
    suppressMessages(swclose(pdfDocument))
    expect_true(file.exists("test.pdf"))
  })
})
