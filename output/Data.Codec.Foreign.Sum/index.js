import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_ST_Internal from "../Control.Monad.ST.Internal/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Codec from "../Data.Codec/index.js";
import * as Data_Codec_Foreign from "../Data.Codec.Foreign/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
import * as Foreign_Object_ST from "../Foreign.Object.ST/index.js";
var pure = /* #__PURE__ */ Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST);
var lmap = /* #__PURE__ */ Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither);
var bind1 = /* #__PURE__ */ Control_Bind.bind(Data_Either.bindEither);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Data_Either.applicativeEither);

// | A helper for defining Foreign codecs for sum types. To ensure exhaustivity
// | there needs to be a mapping to and from a tag type for the type to be
// | encoded.
// |
// | - The first argument is the name of the type being decoded, for error
// |   message purposes.
// | - The second argument maps a tag value to a string to use in the encoding.
// | - The third argument maps a string back to a tag value during decoding.
// | - The fourth argument returns either a constant value or a decoder function
// |   based on a tag value.
// | - The fifth argument returns a tag value and optional encoded value to
// |   store for a constructor of the sum.
var taggedSum = function (name) {
    return function (printTag) {
        return function (parseTag) {
            return function (f) {
                return function (g) {
                    var encodeCase = function (a) {
                        var v = g(a);
                        return Data_Codec.encode(Data_Codec_Foreign.fobject)(Foreign_Object.runST(function __do() {
                            var obj = Foreign_Object_ST["new"]();
                            Foreign_Object_ST.poke("tag")(Data_Codec.encode(Data_Codec_Foreign.string)(printTag(v.value0)))(obj)();
                            return Data_Maybe.maybe(pure(obj))(function (v1) {
                                return Foreign_Object_ST.poke("value")(v1)(obj);
                            })(v.value1)();
                        }));
                    };
                    var decodeCase = function (v) {
                        return lmap(Data_Codec_Foreign.Named.create(name))(bind1(Data_Codec.decode(Data_Codec_Foreign.fobject)(v))(function (obj) {
                            return bind1(Data_Codec.decode(Data_Codec_Foreign.prop("tag")(Data_Codec_Foreign.string))(obj))(function (tag) {
                                var v1 = parseTag(tag);
                                if (v1 instanceof Data_Maybe.Nothing) {
                                    return new Data_Either.Left(new Data_Codec_Foreign.AtKey("tag", new Data_Codec_Foreign.UnexpectedTagValue(tag)));
                                };
                                if (v1 instanceof Data_Maybe.Just) {
                                    var v2 = f(v1.value0);
                                    if (v2 instanceof Data_Either.Left) {
                                        return pure1(v2.value0);
                                    };
                                    if (v2 instanceof Data_Either.Right) {
                                        return bind1(Data_Codec.decode(Data_Codec_Foreign.prop("value")(Data_Codec_Foreign.foreign_))(obj))(function (value) {
                                            return lmap(Data_Codec_Foreign.AtKey.create("value"))(v2.value0(value));
                                        });
                                    };
                                    throw new Error("Failed pattern match at Data.Codec.Foreign.Sum (line 56, column 9 - line 60, column 49): " + [ v2.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Codec.Foreign.Sum (line 53, column 5 - line 60, column 49): " + [ v1.constructor.name ]);
                            });
                        }));
                    };
                    return Data_Codec.codec(decodeCase)(encodeCase);
                };
            };
        };
    };
};

// | A helper for defining Foreign codecs for "enum" sum types, where every
// | constructor is nullary, and the type will be encoded as a string.
var enumSum = function (printTag) {
    return function (parseTag) {
        return Data_Codec.codec(function (f) {
            return bind1(Data_Codec.decode(Data_Codec_Foreign.string)(f))(function (value) {
                var v = parseTag(value);
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Either.Right(v.value0);
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Either.Left(new Data_Codec_Foreign.UnexpectedTagValue(value));
                };
                throw new Error("Failed pattern match at Data.Codec.Foreign.Sum (line 22, column 7 - line 24, column 50): " + [ v.constructor.name ]);
            });
        })((function () {
            var $20 = Data_Codec.encode(Data_Codec_Foreign.string);
            return function ($21) {
                return $20(printTag($21));
            };
        })());
    };
};
export {
    enumSum,
    taggedSum
};
