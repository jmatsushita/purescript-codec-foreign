# purescript-codec-foreign

Purescript library to convert Purescript data <-> `Foreign` data using the ["codecs"](https://github.com/garyb/purescript-codec) approach.

This library is build on [purescript-codec](https://github.com/garyb/purescript-codec) and offers an approach to dealing with encoding/decoding not based on typeclasses. 
Instead, codecs are constructed as values explicitly. As long as the basic codec values provided by this library are used, 
the codecs are guaranteed to roundtrip successfully.

For more information on the motivation behind this library, please read [what @garyb wrote about his problems with typeclass codecs](https://code.slipthrough.net/2018/03/13/thoughts-on-typeclass-codecs/) previously.
