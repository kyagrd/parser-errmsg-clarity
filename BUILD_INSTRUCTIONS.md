# Building the LaTeX Document

## Prerequisites

To build the `draft.tex` document, you need the following tools installed:

1. **LaTeX distribution** (TeX Live recommended)
   - On Ubuntu/Debian: `sudo apt-get install texlive-full`
   - On macOS: Install MacTeX from https://www.tug.org/mactex/
   - On Windows: Install MiKTeX from https://miktex.org/

2. **latexmk** (usually included with TeX distributions)

3. **Python and Pygments** (required for minted package)
   - Install Python 3
   - Install Pygments: `pip install Pygments`

4. **Korean language support**
   - On Ubuntu/Debian: `sudo apt-get install texlive-lang-korean`

## Building the Document

Simply run:

```bash
make paper
```

This will compile `draft.tex` into `draft.pdf` using latexmk with the necessary options for the minted package.

## Cleaning Build Artifacts

To remove all generated files:

```bash
make clean
```

## Notes

- The Makefile uses the `-shell-escape` flag which is required for the minted package to work
- The document uses fullpage layout with A4 paper size
- Korean text is supported via the kotex package
