# Shannon-Fano compression algorithm library
> Haskell implementation

This library offers a set of functions to compress files into binary code
applying the Shannon-Fano compression algorithm.

https://en.wikipedia.org/wiki/Shannon%E2%80%93Fano_coding

## Installing / Getting started

### Stack

This package is now availablein Hackage at https://hackage.haskell.org/package/shannon-fano-0.1.0.0

So you just need to:

```shell
$ stack install shannon-fano
```

### Manually

```shell
$ git clone https://github.com/bolt12/shannon-fano.git
$ cd shannon-fano/
$ stack install
```

#### Build documentation

```shell
$ cd shannon-fano/
$ stack haddock
```

### See for yourself

You can see if it's correctly installed by doing calling the program in the terminal. You
should see the following:

```shell
> shannon-fano -h
Compress contents using the Shannon-Fano algorithm

Usage: shannon-fano [--decompress STRING] [--input STRING] [--output STRING]

Available options:
-h,--help                Show this help text
--decompress STRING      Decompress with decode table file name.
--input STRING           Input content file name. If not specified will be
read from stdin.
--output STRING          Output result file name. If not specified will be
'out.bin' or 'out.bin.dat' depending on the action
```

## Use examples

The program is able to read from stdin if no input file is provided like so:

```shell
> shannon-fano
test
>
```

This will create a 'out.bin' file and a 'out.bin.tab' file (which contains the decode
table), which you can decompress:

```shell
> shannon-fano --decompress out.bin.tab --input out.bin
```

If no output file name is provided, this should create a new file called 'out.dat':

```shell
> cat out.dat
test
```

## Performance and compression

Testing the compressor program for a lorem ipsum text file of 1000 words:

```shell
> time shannon-fano --input test.txt

real    0m0.074s
user    0m0.060s
sys     0m0.025s
```

Compression:

```shell
> ls -lh out.bin test.txt | cut -d " " -f5
3.4K
6.4K
```

_Total ~ 47%_

---

Testing the compressor program with 1M of random data:

```shell
> base64 /dev/urandom | head -c 1000000 > test.txt
> time shannon-fano --input test.txt

real    0m2.648s
user    0m2.321s
sys     0m1.305s
```

Compression:

```shell
> ls -lh out.bin test.txt | cut -d " " -f5
737K
977K
```

_Total ~ 15%_

---

Testing the compressor program with a 2.1M file containing repetitions of 5 characters:

```shell
> time shannon-fano --input test.txt

real    0m2.356s
user    0m2.069s
sys     0m1.499s
```

Compression:

```shell
> ls -lh out.bin test.txt | cut -d " " -f5
734K
2.1M
```

_Total ~ 65%_

Decompression:

```shell
> time shannon-fano --decompress out.bin.tab --input out.bin

real    0m6.374s
user    0m6.252s
sys     0m1.394s
```

---

### Conclusion

As you can see, this algorithm performs worse, in general, in terms of compression, with random and large data.

## Contributing

If you'd like to contribute, please fork the repository and use a feature
branch. Pull requests are warmly welcome.

## Links

- Repository: https://github.com/bolt12/shannon-fano/
- Issue tracker: https://github.com/bolt12/shannon-fano/issues

## Licensing

The code in this project is licensed under GPL3 license.
