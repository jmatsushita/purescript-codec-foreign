import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Codec from "../Data.Codec/index.js";
import * as Data_Codec_Foreign from "../Data.Codec.Foreign/index.js";
import * as Data_Codec_Foreign_Sum from "../Data.Codec.Foreign.Sum/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Profunctor from "../Data.Profunctor/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
var apply = /* #__PURE__ */ Control_Apply.apply(/* #__PURE__ */ Data_Codec.applyCodec(Data_Either.applyEither)(Data_List_Types.semigroupList));
var map1 = /* #__PURE__ */ Data_Functor.map(/* #__PURE__ */ Data_Codec.functorCodec(Data_Either.functorEither));
var profunctorCodec = /* #__PURE__ */ Data_Codec.profunctorCodec(Data_Either.functorEither);
var lcmap = /* #__PURE__ */ Data_Profunctor.lcmap(profunctorCodec);
var map2 = /* #__PURE__ */ Data_Functor.map(Data_Either.functorEither);
var dimap = /* #__PURE__ */ Data_Profunctor.dimap(profunctorCodec);
var toUnfoldable = /* #__PURE__ */ Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray);

// | A codec for `Tuple` values.
// |
// | Encodes as a two-element array in JSON.
var tuple = function (codecA) {
    return function (codecB) {
        return Data_Codec_Foreign.indexedArray("Tuple")(apply(map1(Data_Tuple.Tuple.create)(lcmap(Data_Tuple.fst)(Data_Codec_Foreign.index(0)(codecA))))(lcmap(Data_Tuple.snd)(Data_Codec_Foreign.index(1)(codecB))));
    };
};

// | A codec for `Maybe` values.
// |
// | NOTE: This is not suitable to en/decode null values. If you need these kinds of codecs,
// | look into `Data.Codec.Argonaut.Compat`
var maybe = function (codec) {
    var printTag = function (v) {
        if (!v) {
            return "Nothing";
        };
        if (v) {
            return "Just";
        };
        throw new Error("Failed pattern match at Data.Codec.Foreign.Common (line 28, column 14 - line 30, column 18): " + [ v.constructor.name ]);
    };
    var parseTag = function (v) {
        if (v === "Nothing") {
            return new Data_Maybe.Just(false);
        };
        if (v === "Just") {
            return new Data_Maybe.Just(true);
        };
        return Data_Maybe.Nothing.value;
    };
    var enc = function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return new Data_Tuple.Tuple(false, Data_Maybe.Nothing.value);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Data_Tuple.Tuple(true, new Data_Maybe.Just(Data_Codec.encode(codec)(v.value0)));
        };
        throw new Error("Failed pattern match at Data.Codec.Foreign.Common (line 38, column 9 - line 40, column 48): " + [ v.constructor.name ]);
    };
    var dec = function (v) {
        if (!v) {
            return new Data_Either.Left(Data_Maybe.Nothing.value);
        };
        if (v) {
            return new Data_Either.Right((function () {
                var $37 = map2(Data_Maybe.Just.create);
                var $38 = Data_Codec.decode(codec);
                return function ($39) {
                    return $37($38($39));
                };
            })());
        };
        throw new Error("Failed pattern match at Data.Codec.Foreign.Common (line 35, column 9 - line 37, column 47): " + [ v.constructor.name ]);
    };
    return Data_Codec_Foreign_Sum.taggedSum("Maybe")(printTag)(parseTag)(dec)(enc);
};

// | A codec for `Map` values.
// |
// | Encodes as an array of two-element key/value arrays in JSON.
var map = function (dictOrd) {
    var fromFoldable = Data_Map_Internal.fromFoldable(dictOrd)(Data_Foldable.foldableArray);
    return function (codecA) {
        var $40 = dimap(toUnfoldable)(fromFoldable);
        var $41 = tuple(codecA);
        return function ($42) {
            return $40(Data_Codec_Foreign.array($41($42)));
        };
    };
};

// | A codec for `List` values.
// |
// | Encodes as an array in JSON.
var list = /* #__PURE__ */ (function () {
    var $43 = dimap(Data_Array.fromFoldable(Data_List_Types.foldableList))(Data_List.fromFoldable(Data_Foldable.foldableArray));
    return function ($44) {
        return $43(Data_Codec_Foreign.array($44));
    };
})();

// | A codec for `StrMap` values.
// |
// | Encodes as an array of two-element key/value arrays in JSON.
var foreignObject = /* #__PURE__ */ (function () {
    var $45 = dimap(Foreign_Object.toUnfoldable(Data_Unfoldable.unfoldableArray))(Foreign_Object.fromFoldable(Data_Foldable.foldableArray));
    var $46 = tuple(Data_Codec_Foreign.string);
    return function ($47) {
        return $45(Data_Codec_Foreign.array($46($47)));
    };
})();

// | A codec for `Either` values.
var either = function (codecA) {
    return function (codecB) {
        var printTag = function (v) {
            if (v) {
                return "Left";
            };
            if (!v) {
                return "Right";
            };
            throw new Error("Failed pattern match at Data.Codec.Foreign.Common (line 55, column 14 - line 57, column 20): " + [ v.constructor.name ]);
        };
        var parseTag = function (v) {
            if (v === "Left") {
                return new Data_Maybe.Just(true);
            };
            if (v === "Right") {
                return new Data_Maybe.Just(false);
            };
            return Data_Maybe.Nothing.value;
        };
        var enc = function (v) {
            if (v instanceof Data_Either.Left) {
                return new Data_Tuple.Tuple(true, new Data_Maybe.Just(Data_Codec.encode(codecA)(v.value0)));
            };
            if (v instanceof Data_Either.Right) {
                return new Data_Tuple.Tuple(false, new Data_Maybe.Just(Data_Codec.encode(codecB)(v.value0)));
            };
            throw new Error("Failed pattern match at Data.Codec.Foreign.Common (line 65, column 9 - line 67, column 51): " + [ v.constructor.name ]);
        };
        var dec = function (v) {
            if (v) {
                return new Data_Either.Right((function () {
                    var $48 = map2(Data_Either.Left.create);
                    var $49 = Data_Codec.decode(codecA);
                    return function ($50) {
                        return $48($49($50));
                    };
                })());
            };
            if (!v) {
                return new Data_Either.Right((function () {
                    var $51 = map2(Data_Either.Right.create);
                    var $52 = Data_Codec.decode(codecB);
                    return function ($53) {
                        return $51($52($53));
                    };
                })());
            };
            throw new Error("Failed pattern match at Data.Codec.Foreign.Common (line 62, column 9 - line 64, column 50): " + [ v.constructor.name ]);
        };
        return Data_Codec_Foreign_Sum.taggedSum("Either")(printTag)(parseTag)(dec)(enc);
    };
};
export {
    maybe,
    tuple,
    either,
    list,
    map,
    foreignObject
};
