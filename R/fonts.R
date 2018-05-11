require(lattice, quietly = TRUE)

## define the CM fonts metrics

setCM <- function(){
  CM <- Type1Font("CM",
                  c(file.path("cm-lgc", "fonts", "afm",
                              "public", "cm-lgc",
                              c("fcmr8a.afm", "fcmb8a.afm",
                                "fcmri8a.afm", "fcmbi8a.afm")),
                    "./cmsyase.afm"))
  pdfFonts(CM=CM)
  postscriptFonts(CM=CM)
}

setFutura <- function(){

  ## Define the appropriate local locations of the font files
  ## Note: the relevant postscript fonts must be installed
  FuturaType1 <- Type1Font("Futura",
                           c(file.path("fonts",
                                       c("FuturaStd-Medium.afm", "FuturaStd-Bold.afm",
                                         "FuturaStd-MediumOblique.afm","FuturaStd-BoldOblique.afm"))))
  FuturaPostscript <- quartzFont(
    c("FuturaStd-Medium",
      "FuturaStd-Bold",
      "FuturaStd-MediumOblique",
      "FuturaStd-BoldOblique"))

  ## define the fonts for pdf, postscript and Quartz devices
  if (is.null(pdfFonts()$Futura)) pdfFonts(Futura = FuturaType1)
  if (is.null(postscriptFonts()$Futura)) postscriptFonts(Futura = FuturaType1)
  if (is.null(quartzFonts()$Futura)) quartzFonts(Futura = FuturaPostscript)

  ## set defaults for pdf and postscript devices
  ## Note: embed the fonts after dev.off()
  pdf.options(family="Futura")
  ps.options(family="Futura")

  ## set the defaults for traditional and lattice plots
  par(family="Futura")
  trellis.par.set(grid.pars = list(fontfamily = "Futura"))
}

setFutura()


# ps.options(family="Futura")
# ps.options(family="CM")
# ps.options(family="Helvetica")

# pdf.options(family="Futura")
# pdf.options(family="CM")
# pdf.options(family="Helvetica")

if (print) pdf.options(useDingbats=FALSE, colormodel="rgb")
if (!print) pdf.options(useDingbats=FALSE, colormodel="rgb")

embedMyFonts <- function(figurefolder = "fig") {
  files <- dir(figurefolder)
  files <- file.path(figurefolder,files)
  for (i in 1:length(files)) {
    embedFonts(files[i], "pdfwrite",
               fontpaths=c(
                 file.path("fonts"),
                 "/System/Library/Fonts",
                 "/Library/Fonts",
                 path.expand("~/Library/Fonts"))
    )
  }
}
