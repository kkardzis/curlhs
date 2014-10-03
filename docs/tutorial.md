# What is *curlhs*?

*curlhs* is a [Haskell][haskell.htm] library that provides
a [Haskell][haskell.htm] interface to *libcurl* - the multiprotocol
file transfer library.

> *libcurl* is most probably the most portable, most powerful
> and most often used C-based multi-platform file transfer library
> on this planet - be it open source or commercial. 

The main *libcurl* web site is at <http://curl.haxx.se/libcurl/>
and it contains a vast amount of documentation and resources.
It is highly recommended to be familiar with those materials
to comfortably use *curlhs*.


# How to install?

*curlhs* is written in the [Haskell][haskell.htm] programming language
and requires the Haskell compiler [GHC][ghc.htm] and the Haskell's
package manager [Cabal][cabal.htm] to build. The easiest way to get
them is with the [Haskell Platform][platform.htm] binary installer.

If you already have Cabal set up, then getting *curlhs*
should be as simple as running two commands:

```sh
$ cabal update
$ cabal install curlhs
```

This will download and install the latest *curlhs* package from
[Hackage][hackage.htm].

The most recent *curlhs* can be installed from GitHub:

```sh
$ git clone https://github.com/kkardzis/curlhs.git
$ git clone https://github.com/kkardzis/rtld.git
$ cabal update && cabal install ./rtld ./curlhs
```

One way or another, installation should pass without complications.
Tested on Windows, FreeBSD and Linux ([travis-ci][travis.htm]).


# Does it work?

To see if *curlhs* works, let's try to check *libcurl* version. Run GHCi:

```sh
$ ghci
```

The simple call to `curl_version` should return the *libcurl* version string:

```hs
ghci> :m Network.CURL720
ghci> curl_version
*** Exception: <curlhs> failed to call 'curl_version' (NULL)
```

Obviously that's not what was expected. But what's wrong?
The short answer is that *libcurl* was not loaded before use.
*curlhs* depends on *libcurl*, but does not link with *libcurl* at compile/build
time like most libraries would. Instead, it is necessary to explicitly load 
*libcurl* at runtime.
So let's try:

```hs
ghci> loadlib CURL720
*** Exception: <curlhs> failed to load libcurl/7.20 ["libcurl.dll"]
```

What now? Let's assume we are on Windows, where *libcurl* is not installed
by default. The dynamic loader searchs for "libcurl.dll", but it cannot find
it in the default search path. It's time to install *libcurl*.


# Where is *libcurl*?

### Windows

Using *libcurl* from Haskell was always a pain on Windows. But not anymore.
With *curlhs* it's now easy to use them together. All that *curlhs* needs
to work is a "libcurl.dll" with its dependencies like SSL, SSH etc.

*libcurl* can be build from source of course, but the recommended
way is to search for the precompiled binaries for Windows.
Good place to start may be at <http://www.confusedbycode.com/curl/>.

Be careful. *libcurl* uses many different libraries behind the scenes 
to do its work, some (but not all) of which are optional. *libcurl* may be built
with different implementations of SSL for example, or even without SSL
support at all. Similarly, other features may only be available if they were selected
at compile time for *libcurl*, so pay attention to how
a package was build and that all dependencies are included.

A good choice for *libcurl*, although not the latest version, are these packages from
<http://curl.haxx.se/download.html>:

* <http://curl.haxx.se/gknw.net/7.34.0/dist-w32/curl-7.34.0-devel-mingw32.zip>
* <http://curl.haxx.se/gknw.net/7.34.0/dist-w64/curl-7.34.0-devel-mingw64.7z>

For Win32 for example in 'curl-7.34.0-devel-mingw32\bin\' we have:

* libcurl.dll
* libeay32.dll
* libidn-11.dll
* librtmp.dll
* libssh2.dll
* ssleay32.dll
* zlib1.dll

Where should we put those DLLs? Please refer to the `LoadLibrary` function
documentation on MSDN for exhaustive information about the default
search path used to locate the libraries in Windows. Three notable
places where Windows looks for the DLLs are:

* the directory from which the application was loaded (where the exe is)
* the directories listed in the PATH environment variable
* the current directory

That's it. To test just `cd` to the directory where those DLLs
are placed and run GHCi from there.


### Linux

In the unix world *libcurl* is a widely used library and is probably
installed by default, or as a dependency for some tool, notably the
popular tool *curl*. To check it, use the command `curl --version`.
If *libcurl* has not been installed, the easiest way to install it and its
dependencies is with the system package manager like *apt-get*, *yum*
or similar. For example:

```sh
$ apt-get update
$ apt-get install curl
```

On Linux systems *curlhs* searchs for "libcurl.so.4" or "libcurl.so"
(in that order). Default search path is implementation dependent.
Please refer to the `dlopen` man page for more information.


### BSD

As with Linux systems: first check if *libcurl* is installed,
for example with `curl --version`, and if it's not, use the system package
manager to install it. For example:

```sh
$ pkg update
$ pkg install curl
```

On BSD systems *curlhs* searchs for "libcurl.so". Please refer to
the `dlopen` man page for information about the default search path
used to locate libraries.

### OS X

On OS X system *curlhs* searchs for "libcurl.dylib".

Disclaimer: this platform was not tested, sorry.


# It works? Really?

Now, when *libcurl* is in the scope, let's try again. Let's assume we
are on Windows with "libcurl/7.34" like in the example above. Run GHCi,
import one of the *curlhs* modules, load *libcurl*, and finally check
its version:

```hs
ghci> :m Network.CURL720
ghci> loadlib CURL720
ghci> curl_version
"libcurl/7.34.0 OpenSSL/1.0.0k zlib/1.2.8 libidn/1.18 libssh2/1.4.3 librtmp/2.3"
```

Excellent! But what if...

```hs
ghci> freelib CURL720
ghci> curl_version
*** Exception: <curlhs> failed to call 'curl_version' (NULL)
```

Nice.


# Hello World!

This is about the file transfer library, so let's try to download something:

```hs
ghci> :m Network.CURL720
ghci> loadlib CURL720
ghci> c <- curl_easy_init
ghci> curl_easy_setopt c [CURLOPT_URL "http://httpbin.org/get"]
ghci> curl_easy_perform c
{
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Connection": "close",
    "Host": "httpbin.org",
    "X-Request-Id": "00000000-0000-0000-0000-000000000000"
  },
  "origin": "000.000.000.000",
  "url": "http://httpbin.org/get"
}
```

And maybe simple upload:

```hs
ghci> curl_easy_setopt c [CURLOPT_URL "http://httpbin.org/post"]
ghci> curl_easy_setopt c [CURLOPT_COPYPOSTFIELDS "Hello World!"]
ghci> curl_easy_perform c
{
  "args": {},
  "data": "",
  "files": {},
  "form": {
    "Hello World!": ""
  },
  "headers": {
    "Accept": "*/*",
    "Connection": "close",
    "Content-Length": "12",
    "Content-Type": "application/x-www-form-urlencoded",
    "Host": "httpbin.org",
    "X-Request-Id": "00000000-0000-0000-0000-000000000000"
  },
  "json": null,
  "origin": "000.000.000.000",
  "url": "http://httpbin.org/post"
}
```


# That's all now

I would be grateful for any comments or concerns you may have.



[haskell.htm]:  http://www.haskell.org/
[ghc.htm]:      http://www.haskell.org/ghc/
[cabal.htm]:    http://www.haskell.org/cabal/
[platform.htm]: http://www.haskell.org/platform/
[hackage.htm]:  https://hackage.haskell.org/package/curlhs
[travis.htm]:   https://travis-ci.org/kkardzis/curlhs

![GA](https://ga-beacon.appspot.com/UA-53767359-1/curlhs/docs/tutorial)
