test_that("swclose works", {
  skip_if(Sys.which("pdflatex") == "", "pdflatex was not found")
  skip_if(Sys.getenv("UNIVERSE_NAME") == "pik-piam", "test is incompatible with r-universe LaTeX setup")
  withr::with_tempdir({
    pdfDocument <- swopen(outfile = "test.pdf")
    swlatex(pdfDocument, "tttteeeesssstttt")
    expect_false(file.exists("test.pdf"))
    suppressMessages(swclose(pdfDocument))
    expect_true(file.exists("test.pdf"))
  })
})
