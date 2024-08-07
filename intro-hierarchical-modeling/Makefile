RERUN := "(undefined references|Rerun to get (cross-references|the bars|point totals) right|Table widths have changed. Rerun LaTeX.|Linenumber reference failed)"
RERUNBIB := "No file.*\.bbl|Citation.*undefined|.*There were undefined references"

all: pdf/01-the-aggregation-fallacy.pdf \
     pdf/02-the-aggregation-fallacy-solution.pdf \
     pdf/03-if-youre-a-bayesian.pdf \
     pdf/04-online-sellers-revisited.pdf \
     pdf/05-online-sellers-revisited-solution.pdf \
     pdf/06-student-grades.pdf \
     pdf/07-explanatory-models.pdf

pdf/%.pdf: src/%.Rmd
	Rscript -e "rmarkdown::render(input='$<',output_dir='pdf')"

pdf/%.pdf: src/%.tex
	pdflatex -jobname=$(basename $@) $<
	bibtex $(basename $@)
	bibtex $(basename $@)
	bibtex $(basename $@)
	pdflatex -jobname=$(basename $@) $<
	pdflatex -jobname=$(basename $@) $<

latexmk:
	-latexmk -pvc -pdf $(DOC)

push:
	git add . && git commit -am"Automated commit" && git push

tidy:
	-rm -f pdf/*.aux pdf/*.dvi src/*.log pdf/*.log src/*.log pdf/*.bbl \
               pdf/*.blg pdf/*.brf pdf/*.fls pdf/*.toc pdf/*.thm \
               pdf/*.out pdf/*.vrb pdf/*.snm pdf/*.nav pdf/*.fdb_latexmk

clean:
	-rm -f pdf/*

.PHONY: all tidy clean
