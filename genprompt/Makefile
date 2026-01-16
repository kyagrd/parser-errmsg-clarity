.PHONY: all clean crlbrace sqbracket atsharp $(MODEL_TARGETS)

# YAML 파일 목록 자동 탐색
YMLS := $(wildcard *.yml)

# 생성할 TXT 파일 목록
TXTS := $(YMLS:.yml=.txt)

RUNS := 50

# 모델 목록과 타겟 이름 매핑 (':'를 '_'로 치환해 타겟 이름으로 사용)
MODEL_LIST := gemma3:12b gemma3:4b
MODEL_TARGETS := $(subst :,_,$(MODEL_LIST))

# 공통 규칙 템플릿: 각 모델별로 모든 .txt에 대해 run_manytimes 실행
define MAKE_MODEL_TARGET
$(1): MODEL_NAME := $(2)
$(1): OUTPUT_DIR := $(1)
$(1): $(TXTS)
	@mkdir -p "$$(OUTPUT_DIR)"
	@set -e; \
	for f in $(TXTS); do \
		base=$$$${f%.txt}; \
		./run_manytimes.sh -m "$$(MODEL_NAME)" "$$$$f" "$$(OUTPUT_DIR)/$$$${base}.out" $(RUNS); \
	done
.PHONY: $(1)
endef

# 위 템플릿을 MODEL_LIST 전부에 대해 전개
$(foreach model,$(MODEL_LIST),$(eval $(call MAKE_MODEL_TARGET,$(subst :,_,$(model)),$(model))))

# 기본 타겟: 모든 txt 파일 생성
all: $(TXTS)

# 패턴 규칙: yml -> txt 변환
%.txt: %.yml template.prompt
	mustache $< template.prompt > $@

# clean 타겟
clean:
	rm -f $(TXTS)

# crlbrace는 { } 기호를 그대로 사용하는 기본 타겟 yml 생성 필요없음
crlbrace: crlbrace0.txt crlbrace1.txt
# sqbracket 타겟: { } -> [ ] 변환 후 txt 생성
sqbracket: sqbracket0.txt sqbracket1.txt
# atsharp 타겟: { } -> @ # 변환 후 txt 생성
atsharp: atsharp0.txt atsharp1.txt

sqbracket0.yml: crlbrace0.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace0.yml sqbracket0.yml -s '{' '[' -s '}' ']'

sqbracket1.yml: crlbrace1.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace1.yml sqbracket1.yml -s '{' '[' -s '}' ']'

atsharp0.yml: crlbrace0.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace0.yml atsharp0.yml -s '{' '@' -s '}' '#'

atsharp1.yml: crlbrace1.yml obfuscate_symbols.sh
	./obfuscate_symbols.sh crlbrace1.yml atsharp1.yml -s '{' '@' -s '}' '#'
