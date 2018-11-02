# Shannon-Fano compression algorithm library
> Haskell implementation

This library offers a set of functions to compress files into binary code
applying the Shannon-Fano compression algorithm.

https://en.wikipedia.org/wiki/Shannon%E2%80%93Fano_coding

## Installing / Getting started

I'm currently working to get this library on Hackage. So meanwhile
you can start using it buy building it.

```shell
$ git clone https://github.com/bolt12/shannon-fano.git
$ cd shannon-fano/
$ cabal configure
$ cabal build
$ cabal install
```

You can see if it's correctly installed by going into ghci and see if you can import the
library.

```shell
$ ghci
> import Codec.Compression.ShannonFano
>
```

## Use examples

To get the compressed binary string of a certain value.

```Haskell
import Codec.Compression.ShannonFano

main :: IO ()
main = putStrLn . show . compress frequency $ "test"
```

And you should see.

```shell
> main
Just "010110"
```

---

To generate only the coding table of some value.

```Haskell
import Codec.Compression.ShannonFano

main :: IO ()
main = putStrLn . show . genCodeTable . code . frequency $ "test"
```

And you should see.

```shell
> main
[('t',"0"),('e',"10"),('s',"11")]
```

---

To make a program that compresses a given file.

```Haskell
import Codec.Compression.ShannonFano
import System.Environment

main :: IO ()
main = do
    [file] <- getArgs
    content <- readFile file
    compressTofile frequency content
```

And you should get two resulting files:
 - out.bin: Has the binary of the compressed data
 - decode.dat: Has the decoding table structure

---

To make a program that decompresses a given binary file.

```Haskell
import Codec.Compression.ShannonFano
import System.Environment

main :: IO ()
main = do
    [decTableF, binaryF] <- getArgs
    decompressFromFile decTableF binaryF ""
```

And you should get one resulting file:
 - result.dat: Has the binary of the compressed data

You can check they are equal.

```shell
$ diff result.dat test.txt
$
```

## Performance and compression

Testing the compressor program for a lorem ipsum text file of 921 words:

```shell
$ time ./compress test.txt

real	0m0.075s
user	0m0.067s
sys     0m0.007s
```

Compression:

```shell
$ ls -lh out.bin test.txt | cut -d " " -f5
5.7K
6.5K
```

_Total ~ 12%_

---

Testing the compressor program with 1M of random data:

```shell
$ base64 /dev/urandom | head -c 1000000 > test2.txt
$ time ./compress test2.txt

real	0m30.411s
user	0m27.930s
sys     0m2.187s
```

Compression:

```shell
$ ls -lh out.bin test2.txt | cut -d " " -f5
4.0M
977K
```

_Total ~ -312%_

---

Testing the compressor program with a 70K file containing repetitions of 5 characters:

```shell
$ time ./compress test3.txt

real	0m0.511s
user	0m0.489s
sys     0m0.017s
```

Compression:

```shell
$ ls -lh out.bin test3.txt | cut -d " " -f5
19K
70K
```

_Total ~ 73%_

Decompression:

```shell
$ time ./decompress decode.dat out.bin

real	0m0.128s
user	0m0.105s
sys     0m0.023s
```

---

### Conclusion

As you can see, this algorithm performs very badly, in general with random and large data.

Also my implementation is far from efficient, if you have any suggestion on how to improve my solution
I'd be more than happy to hear it!

## Contributing

If you'd like to contribute, please fork the repository and use a feature
branch. Pull requests are warmly welcome.

## Links

- Repository: https://github.com/bolt12/shannon-fano/
- Issue tracker: https://github.com/your/shannon-fano/issues

## Licensing

The code in this project is licensed under GPL3 license.
