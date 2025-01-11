# Ocaml Maxflow Project

Base project for Ocaml project on Ford-Fulkerson. This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

## Features ✨


- Creation of ford-fulkerson max flow graphs 

    ### For Autun's scenario 
- Interactive feature which allow user to input source and destination
- Display of the max flow found after the execution of the algorithm
- Display of an svg graph containing the result of the Ford-Fulkerson

## Requirements

- 🐫 OCaml

## Installation 💻

1. Clone the repository:
    ```sh
    git clone https://github.com/your-repo/ocaml-maxflow-project.git

    cd ocaml-maxflow-project
    ```

2. Open VSCode in the root directory of this repository:
    ```sh
    code .
    ```

## Usage 🚀

### Running the Application 🏃

To compile and run the application for the normal graphs (graph(1->10).txt), change the word next to "name" in the dune file to "ftest", change the "src", "dst" and "graph" at the beginning of the Makefile and use the following command:
```sh
make demo
```
To compile and run the application for the city of Autun, change the word next to "name" in the dune file to "autun", and use the following command:
```sh
make autun
```


## Project Structure 📂

```
src/
    autun.ml
    ftest.ml
    gfile.ml
    tools.ml
    graph.ml

Makefile
README.md
```

## Authors ✍️

- Mateo Blyweert ☝🤓 [mblyweer@insa-toulouse.fr](mailto:mblyweer@insa-toulouse.fr)
- Elian Boaglio 😎 [boaglio@insa-toulouse.fr](mailto:boaglio@insa-toulouse.fr)

