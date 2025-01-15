.PHONY: all build format edit demo clean

src?=0
dst?=5
graph?=graph1.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

build-autun:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/autun.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest.exe graphs/${graph} $(src) $(dst) outfile
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile

autun: build-autun
	@echo "Vous vous trouvez dans la ville de Autun, en Bourgogne dans la Saône et Loire (71).\nVous avez à votre disposition un réseau de bus doté de 12 arrêts."
	@echo "\nVoici la liste des destinations possibles :\n"
	@echo "Mairie, Boulangerie, Ecole, Parc\nGinette, Stade, Hopital, Musee\nTheatre, Supermarche, Marche, Camping"
	@echo "\nGrâce à l'algorithme suivant vous pouvez savoir combien de personnes au maximum peuvent se rendre d'un arrêt à un autre."
	@echo "Entrez la source et la destination :"
	@read -p "Source: " Source; \
	read -p "Destination: " Destination; \
	./autun.exe $$Source $$Destination; \
	cat resultat_autun && \
	dot -Tsvg "SOLUTION.dot" > SOLUTION.svg && \
	dot -Tsvg "SOLUTION_SANS_ZERO.dot" > SOLUTION_SANS_ZERO.svg && \
	xdg-open SOLUTION.svg && \
	xdg-open SOLUTION_SANS_ZERO.svg


clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
