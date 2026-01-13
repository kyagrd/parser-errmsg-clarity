.PHONY: all clean

# YAML 파일 목록 자동 탐색
YMLS := $(wildcard *.yml)

# 생성할 TXT 파일 목록
TXTS := $(YMLS:.yml=.txt)

# 기본 타겟: 모든 txt 파일 생성
all: $(TXTS)

# 패턴 규칙: yml -> txt 변환
%.txt: %.yml template.prompt
	mustach $< template.prompt > $@

# clean 타겟
clean:
	rm -f $(TXTS)
