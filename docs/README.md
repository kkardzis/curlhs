# Documentation

This is the documentation section of the [*curlhs*][hackage.htm] package.
There is not much here right now, but this should change over time.

Please check out the short [tutorial](tutorial.md) about installation
and basic usage of *curlhs* and *libcurl*.


# Examples

The examples below are translations to Haskell of the examples found at
<http://curl.haxx.se/libcurl/c/example.html>.

### simple HTTP

[Simple.hs](examples/Simple.hs)
shows how to get a remote web page in only four libcurl function calls.

### simple HTTPS

[Https.hs](examples/Https.hs)
gets a single HTTPS page.

### get a remote file in memory only
[GetInMemory.hs](examples/GetInMemory.hs)
describes how you can use the callback system to fetch documents
into a ram buffer with no file writing necessary.



[hackage.htm]: https://hackage.haskell.org/package/curlhs

![GA](https://ga-beacon.appspot.com/UA-53767359-1/curlhs/docs/readme)
