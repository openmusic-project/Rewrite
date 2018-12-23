.SUFFIXES:
.SUFFIXES: .fig .ps .dvi .tex

NAME = manual

SRCTEX = $(NAME).tex

FIGFIG = 
FIGPS =  

CFIGFIG =
CFIGPS = 

all : $(NAME).ps

.dvi.ps :
	dvips -o $*.ps $*.dvi

.fig.ps :
#	fig2dev -Lps -Pc $*.fig > $*.ps
	fig2dev -Lps  $*.fig > $*.ps

.tex.dvi:
	latex $*

.ps:
	dvips $@.dvi -o

html: $(NAME).ps
	rm -rf manual
	latex2html $(NAME).tex
	make export

$(NAME).bbl: $(SRCTEX) references.bib
	latex $(NAME)
	bibtex $(NAME)

$(NAME).dvi: $(SRCTEX) $(NAME).bbl  $(FIGPS)
	latex $(NAME)

$(NAME): $(NAME).tex $(NAME.bbl)
	-rm -rf $(NAME)
	latex2html $(NAME).tex

$(NAME).pdf : $(NAME).ps
	ps2pdf $? 

export: $(NAME).ps $(NAME) $(NAME).pdf 
	rm -rf Web/manual
	cp -r manual Web/.
	cp manual.ps Web
	cp manual.pdf Web
	cp index.html Web
 
clean:
	-rm -f *.aux *.log *. *~ TAGS

spotless:
	-rm -f $(NAME).{ps,pdf,dvi,bbl}

