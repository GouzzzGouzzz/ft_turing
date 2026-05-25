NAME = ft_turing

SRC_DIR = src
OBJ_DIR = obj

OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlopt

LIB = yojson 

PACKAGES = yojson ocamlfind

OCAMLFLAGS = -package $(LIB) -I $(SRC_DIR) -I $(OBJ_DIR)

FILES = \
	types \
	print \
	parser \
	debug \
	turing \
	main \

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
	opam switch create $(NAME) 5.4.1 || true
	opam install -y $(PACKAGES) || true
	@echo "Run: eval \$$(opam env)"

dev :
	opam init --auto-setup
	opam switch create $(NAME) 5.4.1 || true
	opam install -y $(PACKAGES) || true
	opam install -y ocamlformat dot-merlin-reader ocaml-lsp-server

uninstall:
	opam switch remove -y $(NAME)

.PHONY: all clean fclean re install