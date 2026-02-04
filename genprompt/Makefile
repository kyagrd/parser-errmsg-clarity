.PHONY: all clean crlbrace sqbracket atsharp unitok

# Automatically find all YAML files
YMLS := $(wildcard *.yml)

# List of TXT files to generate
TXTS := $(YMLS:.yml=.txt)

RUNS := 25
# RUNS := 50

#	gemma3:12b 
#	gemma3:27b-it-qat 
#	gpt-oss:20b 
#	qwen3:4b  # this model ignores output formatting instructions very often

# Model list and target name mapping (replace ':' with '_' for target names)
MODEL_LIST := \
	codegemma:7b \
	codegemma:7b-instruct \
	gemma3:1b \
	gemma3:4b \
	gemma3:12b-it-qat \
	granite3.1-dense:2b \
	granite3.1-dense:8b \
	llama3.2:3b \
	llama3.1:8b \
	mistral:7b \
	mistral-nemo:12b \
	phi4:14b \
	phi4-mini:3.8b \
	qwen2.5:7b \
	qwen2.5-coder:7b \
	qwen2.5-coder:7b-instruct \
	qwen3:8b \
	qwen3:14b

MODEL_TARGETS := $(subst :,_,$(MODEL_LIST))

.PHONY: $(MODEL_TARGETS)

# Common rule template: run run_manytimes for all .txt files per model
define MAKE_MODEL_TARGET
$(1)_TXTS := $(TXTS)
$(1): MODEL_NAME := $(2)
$(1): OUTPUT_DIR := $(1)
$(1): $(TXTS)
	@mkdir -p "$$(OUTPUT_DIR)"
	@set -e; \
	for f in $$($(1)_TXTS); do \
		base=$$$${f%.txt}; \
		./run_manytimes.sh -m "$$(MODEL_NAME)" "$$$$f" "$$(OUTPUT_DIR)/$$$${base}.out" $(RUNS); \
	done
endef

# Expand the template for all models in MODEL_LIST
$(foreach model,$(MODEL_LIST),$(eval $(call MAKE_MODEL_TARGET,$(subst :,_,$(model)),$(model))))

# Generate all txt files
txt: $(TXTS)

# Default target: run make for all models in MODEL_LIST
all: txt $(MODEL_TARGETS)

# Pattern rule: yml -> txt conversion
%.txt: %.yml template.prompt
	mustache $< template.prompt > $@

# clean target
clean:
	rm -f $(TXTS)

# crlbrace: use { } symbols as-is, no yml conversion needed
crlbrace: crlbrace0.txt crlbrace1.txt crlbrace2.txt crlbrace20.txt
# sqbracket: convert { } -> [ ] then generate txt
sqbracket: sqbracket0.txt sqbracket1.txt sqbracket2.txt sqbracket20.txt
# atsharp: convert { } -> @ # then generate txt
atsharp: atsharp0.txt atsharp1.txt atsharp2.txt atsharp20.txt
# unitok: convert { } -> 《 》 then generate txt
unitok: unitok0.txt unitok1.txt unitok2.txt unitok20.txt


sqbracket0.yml: crlbrace0.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace0.yml sqbracket0.yml -s '{' '[' -s '}' ']'

sqbracket1.yml: crlbrace1.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace1.yml sqbracket1.yml -s '{' '[' -s '}' ']'

sqbracket2.yml: crlbrace2.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace2.yml sqbracket2.yml -s '{' '[' -s '}' ']'

sqbracket20.yml: crlbrace20.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace20.yml sqbracket20.yml -s '{' '[' -s '}' ']'

atsharp0.yml: crlbrace0.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace0.yml atsharp0.yml -s '{' '@' -s '}' '#'

atsharp1.yml: crlbrace1.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace1.yml atsharp1.yml -s '{' '@' -s '}' '#'

atsharp2.yml: crlbrace2.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace2.yml atsharp2.yml -s '{' '@' -s '}' '#'

atsharp20.yml: crlbrace20.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace20.yml atsharp20.yml -s '{' '@' -s '}' '#'

unitok0.yml: crlbrace0.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace0.yml unitok0.yml -s '{' '《' -s '}' '》'

unitok1.yml: crlbrace1.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace1.yml unitok1.yml -s '{' '《' -s '}' '》'

unitok2.yml: crlbrace2.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace2.yml unitok2.yml -s '{' '《' -s '}' '》'

unitok20.yml: crlbrace20.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace20.yml unitok20.yml -s '{' '《' -s '}' '》'
