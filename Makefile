render:
	Rscript -e 'Sys.setenv("RSTUDIO_PANDOC" = "/Applications/RStudio.app/Contents/MacOS/pandoc"); library(knitr); rmarkdown::render("index.Rmd", output_file = "index.html")'

serve:
	Rscript -e 'Sys.setenv("RSTUDIO_PANDOC" = "/Applications/RStudio.app/Contents/MacOS/pandoc"); xaringan::infinite_moon_reader("index.Rmd")'

deploy:
	zsh deploy.sh
