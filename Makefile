NAME = ft_turing

SRC_DIR = src
OBJ_DIR = obj

OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlopt

LIB = yojson 

PACKAGES = yojson ocamlfind

OCAMLFLAGS = -I $(OBJ_DIR) -package $(LIB)

FILES = test \

SRC = $(FILES:%=$(SRC_DIR)/%.ml)
OBJ = $(FILES:%=$(OBJ_DIR)/%.cmo)

all: $(NAME)

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(OBJ_DIR)/%.cmo: $(SRC_DIR)/%.ml | $(OBJ_DIR)
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

$(NAME): $(OBJ)
	$(OCAMLC) $(OCAMLFLAGS) -linkpkg -o $(NAME) $(OBJ)

clean:
	rm -rf $(OBJ_DIR)

fclean: clean
	rm -f $(NAME)

re: fclean all

install:
	opam init --no-setup
	opam switch create $(NAME) 5.4.1
	eval $(opam env)
	opam install -y $(PACKAGES)

uninstall:
	opam switch remove -y $(NAME)

.PHONY: all clean fclean re install