# Indrascheme

Indrascheme is a minimal, embeddable Scheme-like language. The implementation consists of a single `.h` file
that exposes `parse()` and `eval()` to interpret Scheme expressions. It can easily expanded by new 'inbuilt' functions that
access the specifics of the embedding project.

## Building of the sample repl

To build the default project (witch provides `indrascheme.cpp` that implements a small repl for tests)

```bash
git clone https://github.com/domschl/indrascheme
cd indrascheme
mkdir build
cd build
cmake ..
make
# Run REPL and directly execute a number of self-tests:
./indrascheme "../samples/selftest.is"
```

## Language description

TBD. See `samples` for the time being.

## History

- 2023-12-28: First version, very early stage!
