#!/bin/bash

directory="machine/error"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'  # No Color

echo ""
echo "--- Testing with basic input : 111111 ---"
echo ""

for file in "$directory"/*; do
  [ -f "$file" ] || continue

  output=$(./ft_turing "$file" 111111 | tr -d '\n')

  if grep -q "Ft_turing" <<< "$output"; then
    echo -e "$file: ${GREEN}OK${NC} $output"
  else
    echo -e "$file: ${RED}KO${NC}"
  fi
done

echo ""
echo "--- Testing invalid input ---"
echo ""

echo "Empty :"
output=$(./ft_turing "machine/test.json" "" | tr -d '\n')
if grep -q "Ft_turing" <<< "$output"; then
    echo -e "$file: ${GREEN}OK${NC} $output"
else
    echo -e "$file: ${RED}KO${NC}"
fi
echo "Not alphabet : asdyauisjdajsda"
output=$(./ft_turing "machine/test.json" "asdyauisjdajsda" | tr -d '\n')
if grep -q "Ft_turing" <<< "$output"; then
    echo -e "$file: ${GREEN}OK${NC} $output"
else
    echo -e "$file: ${RED}KO${NC}"
fi

echo "Blank : ...."
output=$(./ft_turing "machine/test.json" "...." | tr -d '\n')
if grep -q "Ft_turing" <<< "$output"; then
    echo -e "$file: ${GREEN}OK${NC} $output"
else
    echo -e "$file: ${RED}KO${NC}"
fi