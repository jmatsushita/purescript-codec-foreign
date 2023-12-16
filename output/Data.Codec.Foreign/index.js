import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Except from "../Control.Monad.Except/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Codec from "../Data.Codec/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Data_TraversableWithIndex from "../Data.TraversableWithIndex/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Void from "../Data.Void/index.js";
import * as Foreign from "../Foreign/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_List_Types.eqNonEmptyList(Foreign.eqForeignError));
var throwError = /* #__PURE__ */ Control_Monad_Error_Class.throwError(Control_Monad_Error_Class.monadThrowEither);
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var bind = /* #__PURE__ */ Control_Bind.bind(Data_Either.bindEither);
var lmap = /* #__PURE__ */ Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither);
var pure = /* #__PURE__ */ Control_Applicative.pure(Data_Either.applicativeEither);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Data_List_Types.applicativeList);
var foldMap1 = /* #__PURE__ */ Data_Semigroup_Foldable.foldMap1(Data_List_Types.foldable1NonEmptyList)(Data_Semigroup.semigroupString);
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupString);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var show1 = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var throwError1 = /* #__PURE__ */ Control_Monad_Error_Class.throwError(/* #__PURE__ */ Control_Monad_Except_Trans.monadThrowExceptT(Data_Identity.monadIdentity));
var pure2 = /* #__PURE__ */ Control_Applicative.pure(Data_List_Types.applicativeNonEmptyList);
var pure3 = /* #__PURE__ */ Control_Applicative.pure(/* #__PURE__ */ Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity));
var bind1 = /* #__PURE__ */ Control_Bind.bind(/* #__PURE__ */ Control_Monad_Except_Trans.bindExceptT(Data_Identity.monadIdentity));
var readNull = /* #__PURE__ */ Foreign.readNull(Data_Identity.monadIdentity);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var map = /* #__PURE__ */ Data_Functor.map(Data_Either.functorEither);
var bindFlipped = /* #__PURE__ */ Control_Bind.bindFlipped(Data_Either.bindEither);
var fromFoldable = /* #__PURE__ */ Data_Array.fromFoldable(Data_List_Types.foldableList);
var fromFoldable1 = /* #__PURE__ */ Foreign_Object.fromFoldable(Data_List_Types.foldableList);
var readString = /* #__PURE__ */ Foreign.readString(Data_Identity.monadIdentity);
var traverseWithIndex = /* #__PURE__ */ Data_TraversableWithIndex.traverseWithIndex(Data_TraversableWithIndex.traversableWithIndexArray)(Data_Either.applicativeEither);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var ForeignDecodingErrors = /* #__PURE__ */ (function () {
    function ForeignDecodingErrors(value0) {
        this.value0 = value0;
    };
    ForeignDecodingErrors.create = function (value0) {
        return new ForeignDecodingErrors(value0);
    };
    return ForeignDecodingErrors;
})();
var InvalidCodePoint = /* #__PURE__ */ (function () {
    function InvalidCodePoint() {

    };
    InvalidCodePoint.value = new InvalidCodePoint();
    return InvalidCodePoint;
})();
var VoidError = /* #__PURE__ */ (function () {
    function VoidError() {

    };
    VoidError.value = new VoidError();
    return VoidError;
})();
var AtIndex = /* #__PURE__ */ (function () {
    function AtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    AtIndex.create = function (value0) {
        return function (value1) {
            return new AtIndex(value0, value1);
        };
    };
    return AtIndex;
})();
var AtKey = /* #__PURE__ */ (function () {
    function AtKey(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    AtKey.create = function (value0) {
        return function (value1) {
            return new AtKey(value0, value1);
        };
    };
    return AtKey;
})();
var Named = /* #__PURE__ */ (function () {
    function Named(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Named.create = function (value0) {
        return function (value1) {
            return new Named(value0, value1);
        };
    };
    return Named;
})();
var MissingValueAtIndex = /* #__PURE__ */ (function () {
    function MissingValueAtIndex(value0) {
        this.value0 = value0;
    };
    MissingValueAtIndex.create = function (value0) {
        return new MissingValueAtIndex(value0);
    };
    return MissingValueAtIndex;
})();
var MissingValueAtKey = /* #__PURE__ */ (function () {
    function MissingValueAtKey(value0) {
        this.value0 = value0;
    };
    MissingValueAtKey.create = function (value0) {
        return new MissingValueAtKey(value0);
    };
    return MissingValueAtKey;
})();
var UnexpectedTagValue = /* #__PURE__ */ (function () {
    function UnexpectedTagValue(value0) {
        this.value0 = value0;
    };
    UnexpectedTagValue.create = function (value0) {
        return new UnexpectedTagValue(value0);
    };
    return UnexpectedTagValue;
})();
var UnexpectedValue = /* #__PURE__ */ (function () {
    function UnexpectedValue(value0) {
        this.value0 = value0;
    };
    UnexpectedValue.create = function (value0) {
        return new UnexpectedValue(value0);
    };
    return UnexpectedValue;
})();
var genericForeignDecodingErr = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new ForeignDecodingErrors(x.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
            return InvalidCodePoint.value;
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl)) {
            return VoidError.value;
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl))) {
            return new AtIndex(x.value0.value0.value0.value0.value0, x.value0.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))) {
            return new AtKey(x.value0.value0.value0.value0.value0.value0, x.value0.value0.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))) {
            return new Named(x.value0.value0.value0.value0.value0.value0.value0, x.value0.value0.value0.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))))) {
            return new MissingValueAtIndex(x.value0.value0.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))))) {
            return new MissingValueAtKey(x.value0.value0.value0.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))))))) {
            return new UnexpectedTagValue(x.value0.value0.value0.value0.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr)))))))) {
            return new UnexpectedValue(x.value0.value0.value0.value0.value0.value0.value0.value0.value0);
        };
        throw new Error("Failed pattern match at Data.Codec.Foreign (line 40, column 1 - line 40, column 47): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof ForeignDecodingErrors) {
            return new Data_Generic_Rep.Inl(x.value0);
        };
        if (x instanceof InvalidCodePoint) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value));
        };
        if (x instanceof VoidError) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value)));
        };
        if (x instanceof AtIndex) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1)))));
        };
        if (x instanceof AtKey) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1))))));
        };
        if (x instanceof Named) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1)))))));
        };
        if (x instanceof MissingValueAtIndex) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)))))));
        };
        if (x instanceof MissingValueAtKey) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0))))))));
        };
        if (x instanceof UnexpectedTagValue) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)))))))));
        };
        if (x instanceof UnexpectedValue) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0)))))))));
        };
        throw new Error("Failed pattern match at Data.Codec.Foreign (line 40, column 1 - line 40, column 47): " + [ x.constructor.name ]);
    }
};
var eqForeignDecodingError = {
    eq: function (x) {
        return function (y) {
            if (x instanceof ForeignDecodingErrors && y instanceof ForeignDecodingErrors) {
                return eq(x.value0)(y.value0);
            };
            if (x instanceof InvalidCodePoint && y instanceof InvalidCodePoint) {
                return true;
            };
            if (x instanceof VoidError && y instanceof VoidError) {
                return true;
            };
            if (x instanceof AtIndex && y instanceof AtIndex) {
                return x.value0 === y.value0 && Data_Eq.eq(eqForeignDecodingError)(x.value1)(y.value1);
            };
            if (x instanceof AtKey && y instanceof AtKey) {
                return x.value0 === y.value0 && Data_Eq.eq(eqForeignDecodingError)(x.value1)(y.value1);
            };
            if (x instanceof Named && y instanceof Named) {
                return x.value0 === y.value0 && Data_Eq.eq(eqForeignDecodingError)(x.value1)(y.value1);
            };
            if (x instanceof MissingValueAtIndex && y instanceof MissingValueAtIndex) {
                return x.value0 === y.value0;
            };
            if (x instanceof MissingValueAtKey && y instanceof MissingValueAtKey) {
                return x.value0 === y.value0;
            };
            if (x instanceof UnexpectedTagValue && y instanceof UnexpectedTagValue) {
                return x.value0 === y.value0;
            };
            if (x instanceof UnexpectedValue && y instanceof UnexpectedValue) {
                return x.value0 === y.value0;
            };
            return false;
        };
    }
};

// | A codec for `Void` values.
var $$void = /* #__PURE__ */ (function () {
    return Data_Codec["codec$prime"](Data_Function["const"](throwError(VoidError.value)))(Data_Void.absurd);
})();

// | Used with `record` to define codecs for record types that encode into JSON
// | objects of the same shape. See the comment on `record` for an example.
var recordProp = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return function () {
        return function (p) {
            return function (codecA) {
                return function (codecR) {
                    var unsafeSet = function (key) {
                        return function (a) {
                            var $198 = Foreign_Object.insert(key)(a);
                            return function ($199) {
                                return $198($199);
                            };
                        };
                    };
                    var unsafeGet = function (s) {
                        var $201 = Foreign_Object.lookup(s);
                        return function ($202) {
                            return fromJust($201($202));
                        };
                    };
                    var enc$prime = function (key) {
                        return function (val) {
                            return new Data_List_Types.Cons(new Data_Tuple.Tuple(key, Data_Codec.encode(codecA)(unsafeGet(key)(val))), Data_Codec.encode(codecR)(val));
                        };
                    };
                    var dec$prime = function (key) {
                        return function (obj) {
                            return bind(Data_Codec.decode(codecR)(obj))(function (r) {
                                return bind(lmap(AtKey.create(key))((function () {
                                    var v = Foreign_Object.lookup(key)(obj);
                                    if (v instanceof Data_Maybe.Just) {
                                        return Data_Codec.decode(codecA)(v.value0);
                                    };
                                    if (v instanceof Data_Maybe.Nothing) {
                                        return new Data_Either.Left(new MissingValueAtKey(key));
                                    };
                                    throw new Error("Failed pattern match at Data.Codec.Foreign (line 246, column 26 - line 248, column 45): " + [ v.constructor.name ]);
                                })()))(function (a) {
                                    return pure(unsafeSet(key)(a)(r));
                                });
                            });
                        };
                    };
                    var key = reflectSymbol(p);
                    return Data_Codec.codec(dec$prime(key))(enc$prime(key));
                };
            };
        };
    };
};

// | The starting value for a object-record codec. Used with `recordProp` it
// | provides a convenient method for defining codecs for record types that
// | encode into JSON objects of the same shape.
// |
// | For example, to encode a record as the JSON object
// | `{ "name": "Karl", "age": 25 }` we would define a codec like this:
// | ```
// | import Data.Codec.Foreign as CF
// | import Type.Proxy (Proxy(..))
// |
// | type Person = { name ∷ String, age ∷ Int }
// |
// | codecPerson ∷ CF.ForeignCodec Person
// | codecPerson =
// |   CF.object "Person" $ CF.record
// |     # CF.recordProp (Proxy :: _ "name") CF.string
// |     # CF.recordProp (Proxy :: _ "age") CF.int
// | ```
// |
// | See also `Data.Codec.Foreign.Record.object` for a more commonly useful
// | version of this function.
var record = /* #__PURE__ */ (function () {
    return new Data_Codec.Codec(Data_Function["const"](pure({})), Control_Applicative.pure(Data_Tuple.applicativeTuple(Data_List_Types.monoidList)));
})();

// (mapWriter (rmap (F.unsafeToForeign <<< FO.fromFoldable)))
// | A codec for a property of an object.
var prop = function (key) {
    return function (codec) {
        return Data_Codec.codec(function (obj) {
            return lmap(AtKey.create(key))(Data_Maybe.maybe(new Data_Either.Left(new MissingValueAtKey(key)))(Data_Codec.decode(codec))(Foreign_Object.lookup(key)(obj)));
        })((function () {
            var $203 = Data_Tuple.Tuple.create(key);
            var $204 = Data_Codec.encode(codec);
            return function ($205) {
                return pure1($203($204($205)));
            };
        })());
    };
};

// | Adapts an existing codec with a pair of functions to allow a value to be
// | further refined. If the inner decoder fails an `UnexpectedValue` error will
// | be raised for Foreign input.
// |
// | This function is named as such as the pair of functions it accepts
// | correspond with the `preview` and `view` functions of a `Prism`-style lens.
// |
// | An example of this would be a codec for `Data.String.NonEmpty.NonEmptyString`:
// |
// | ```purescript
// | nonEmptyString ∷ CF.ForeignCodec NES.NonEmptyString
// | nonEmptyString =
// |   CF.prismaticCodec "NonEmptyString" 
// |     NES.fromString NES.toString CF.string
// | ```
// |
// | Another example might be to handle a mapping from a small sum type to
// | strings:
// |
// | ```purescript
// | data Direction = North | South | West | East
// |
// | directionCodec :: ForeignCodec Direction
// | directionCodec = CF.prismaticCodec "Direction" dec enc string
// |   where
// |     dec = case _ of
// |       "N" -> Just North
// |       "S" -> Just South
// |       "W" -> Just West
// |       "E" -> Just East
// |       _ -> Nothing
// |
// |     enc = case _ of
// |       North -> "N"
// |       South -> "S"
// |       West -> "W"
// |       East -> "E"
// | ```
var prismaticCodec = function (name) {
    return function (preview) {
        return function (view) {
            return function (orig) {
                var enc = (function () {
                    var $206 = Data_Codec.encode(orig);
                    return function ($207) {
                        return $206(view($207));
                    };
                })();
                var dec = function (f) {
                    return bind(Data_Codec.decode(orig)(f))(function (a) {
                        return Data_Either.note(new Named(name, new UnexpectedValue($foreign["_stringify"](f))))(preview(a));
                    });
                };
                return Data_Codec["codec$prime"](dec)(enc);
            };
        };
    };
};

// | Prints a `ForeignDecodeError` as a somewhat readable error message.
var printForeignDecodingError = function (err) {
    var go = function (v) {
        if (v instanceof ForeignDecodingErrors) {
            return foldMap1((function () {
                var $208 = append("\x09");
                return function ($209) {
                    return $208(Foreign.renderForeignError($209));
                };
            })())(v.value0);
        };
        if (v instanceof InvalidCodePoint) {
            return "\x09Invalid code point";
        };
        if (v instanceof VoidError) {
            return "\x09Void error";
        };
        if (v instanceof AtIndex) {
            return "\x09At array index " + (show(v.value0) + (":\x0a" + go(v.value1)));
        };
        if (v instanceof AtKey) {
            return "\x09At object key " + (v.value0 + (":\x0a" + go(v.value1)));
        };
        if (v instanceof Named) {
            return "\x09Under '" + (v.value0 + ("':\x0a" + go(v.value1)));
        };
        if (v instanceof MissingValueAtIndex) {
            return "\x09No value was found at index " + show(v.value0);
        };
        if (v instanceof MissingValueAtKey) {
            return "\x09No value was found at key " + show1(v.value0);
        };
        if (v instanceof UnexpectedTagValue) {
            return "\x09Unexpected tag value: " + show1(v.value0);
        };
        if (v instanceof UnexpectedValue) {
            return "\x09Unexpected value: " + show1(v.value0);
        };
        throw new Error("Failed pattern match at Data.Codec.Foreign (line 50, column 8 - line 60, column 57): " + [ v.constructor.name ]);
    };
    return "An error occurred while decoding a Foreign value:\x0a" + go(err);
};
var showForeignDecodingError = {
    show: printForeignDecodingError
};

// | A codec for `null` values in `Foreign`.
var $$null = /* #__PURE__ */ (function () {
    var f = function (x) {
        return Data_Maybe.maybe(throwError1(pure2(new Foreign.TypeMismatch("null", Foreign.typeOf(x)))))(pure3);
    };
    return Data_Codec.hoist((function () {
        var $210 = Control_Monad_Except.withExcept(ForeignDecodingErrors.create);
        return function ($211) {
            return Control_Monad_Except.runExcept($210($211));
        };
    })())(Data_Codec["codec$prime"](function (r) {
        return bind1(readNull(r))(f(r));
    })(Data_Function["const"]($foreign["_null"])));
})();

// | A codec for an item in an `indexedArray`.
var index = function (ix) {
    return function (codec) {
        return Data_Codec.codec(function (xs) {
            return lmap(AtIndex.create(ix))(Data_Maybe.maybe(new Data_Either.Left(new MissingValueAtIndex(ix)))(Data_Codec.decode(codec))(Data_Array.index(xs)(ix)));
        })((function () {
            var $212 = Data_Codec.encode(codec);
            return function ($213) {
                return pure1($212($213));
            };
        })());
    };
};
var harismaticCodec = function (view) {
    return function (preview) {
        return function (orig) {
            var enc = (function () {
                var $214 = Data_Either.either(identity)(Data_Codec.encode(orig));
                return function ($215) {
                    return $214(preview($215));
                };
            })();
            var dec = (function () {
                var $216 = map(view);
                var $217 = Data_Codec.decode(orig);
                return function ($218) {
                    return $216($217($218));
                };
            })();
            return Data_Codec["codec$prime"](dec)(enc);
        };
    };
};

// | The "identity codec" for `Foreign` values.
var foreign_ = /* #__PURE__ */ Data_Codec["codec$prime"](pure)(identity);

// | Helper function for defining recursive codecs in situations where the codec
// | definition causes a _"The value of <codec> is undefined here"_ error.
// |
// | ```purescript
// | import Codec.Foreign as CF
// | import Data.Maybe (Maybe)
// | import Data.Newtype (class Newtype)
// | import Data.Profunctor (wrapIso)
// |
// | newtype IntList = IntList { cell ∷ Int, rest ∷ Maybe IntList }
// |
// | derive instance newtypeLoopyList ∷ Newtype IntList _
// |
// | codecIntList ∷ CF.ForeignCodec IntList
// | codecIntList =
// |   CF.fix \codec →
// |     wrapIso IntList $
// |       CAR.object "IntList" { cell: CF.int, rest: CAC.maybe codec }
// | ```
var fix = function (f) {
    return Data_Codec["codec$prime"](function (x) {
        return Data_Codec.decode(f(fix(f)))(x);
    })(function (x) {
        return Data_Codec.encode(f(fix(f)))(x);
    });
};
var encodec = function (codec) {
    return Data_Codec.encode(codec);
};
var decodingError = /* #__PURE__ */ (function () {
    var $219 = Control_Monad_Except.withExcept(ForeignDecodingErrors.create);
    return function ($220) {
        return Control_Monad_Except.runExcept($219($220));
    };
})();

//
// Helper functions 
//
var decodec$prime = function (g) {
    return Data_Codec["codec$prime"](g)(Foreign.unsafeToForeign);
};

// | A codec for `Array Foreign` values in `Foreign`. This does not decode
// | the values of the array, for that use `array` for a general array decoder,
// | or `indexedArray` with `index` to decode fixed length array encodings.
var farray = /* #__PURE__ */ decodec$prime(/* #__PURE__ */ (function () {
    var $221 = Foreign.readArray(Data_Identity.monadIdentity);
    return function ($222) {
        return decodingError($221($222));
    };
})());

// | A codec for types that are encoded as an array with a specific layout.
// |
// | For example, if we'd like to encode a `Person` as a 2-element array, like
// | `["Rashida", 37]`, we could write the following codec:
// |
// | ```purescript
// | import Codec.Foreign ((~))
// | import Codec.Foreign as CF
// |
// | type Person = { name ∷ String, age ∷ Int }
// |
// | codecPerson ∷ CF.ForeignCodec Person
// | codecPerson = CF.indexedArray "Test Object" $
// |   { name: _, age: _ }
// |     <$> _.name ~ CF.index 0 CF.string
// |     <*> _.age ~ CF.index 1 CF.int
// | ```
var indexedArray = function (name) {
    return function (codec) {
        return Data_Codec["codec$prime"](function (j) {
            return lmap(Named.create(name))(bindFlipped(Data_Codec.decode(codec))(Data_Codec.decode(farray)(j)));
        })(function (a) {
            return Data_Codec.encode(farray)(fromFoldable(Data_Codec.encode(codec)(a)));
        });
    };
};

// | A codec for `Object` values in `Foreign`.
var fobject = /* #__PURE__ */ decodec$prime(function ($223) {
    return decodingError((function (f) {
        var v = Foreign.typeOf(f);
        if (v === "object") {
            return pure3(f);
        };
        return throwError1(pure2(new Foreign.TypeMismatch(v, "object")));
    })($223));
});

// object ∷ ∀ a. String → JPropCodec a → JsonCodec a
// object name =
//   bihoistGCodec
//     (\r → ReaderT (BF.lmap (Named name) <<< runReaderT r <=< decode jobject))
//     (mapWriter (BF.rmap (J.fromObject <<< FO.fromFoldable)))
// object name codec =
//   Codec.codec'
//     (\j → lmap (Named name) (Codec.decode codec =<< Codec.decode jobject j))
//     (\a → Codec.encode jobject (FO.fromFoldable (Codec.encode codec a)))
// | A codec for objects that are encoded with specific properties.
var object = function (name) {
    return function (codec) {
        return Data_Codec["codec$prime"](function (f) {
            return lmap(Named.create(name))(bindFlipped(Data_Codec.decode(codec))(Data_Codec.decode(fobject)(f)));
        })(function (a) {
            return Data_Codec.encode(fobject)(fromFoldable1(Data_Codec.encode(codec)(a)));
        });
    };
};

// | A codec for `Int` values in `Foreign`.
var $$int = /* #__PURE__ */ decodec$prime(/* #__PURE__ */ (function () {
    var $224 = Foreign.readInt(Data_Identity.monadIdentity);
    return function ($225) {
        return decodingError($224($225));
    };
})());

// | A codec for `Number` values in `Foreign`.
var number = /* #__PURE__ */ decodec$prime(/* #__PURE__ */ (function () {
    var $226 = Foreign.readNumber(Data_Identity.monadIdentity);
    return function ($227) {
        return decodingError($226($227));
    };
})());

// | A codec for `String` values in `Foreign`.
var string = /* #__PURE__ */ decodec$prime(function ($228) {
    return decodingError(readString($228));
});
var decodec = function (codec) {
    return Data_Codec.decode(codec);
};

// | A codec for `Codepoint` values in `Foreign`.
var codePoint = /* #__PURE__ */ (function () {
    var decodeCodePoint = (function () {
        var codePointError = throwError(InvalidCodePoint.value);
        var $229 = Data_Maybe.maybe(codePointError)(pure);
        var $230 = Data_String_CodePoints.codePointAt(0);
        return function ($231) {
            return $229($230($231));
        };
    })();
    return Data_Codec["codec$prime"](Control_Bind.composeKleisliFlipped(Data_Either.bindEither)(decodeCodePoint)(function ($232) {
        return decodingError(readString($232));
    }))(function ($233) {
        return Foreign.unsafeToForeign(Data_String_CodePoints.singleton($233));
    });
})();

// | A codec for `Boolean` values in `Foreign`.
var $$boolean = /* #__PURE__ */ decodec$prime(/* #__PURE__ */ (function () {
    var $234 = Foreign.readBoolean(Data_Identity.monadIdentity);
    return function ($235) {
        return decodingError($234($235));
    };
})());

// | A codec for arbitrary length `Array`s where every item in the array
// | shares the same type.
// |
// | ``` purescript
// | import Codec.Foreign as CF
// |
// | codecIntArray ∷ CF.ForeignCodec (Array Int)
// | codecIntArray = CF.array CF.int
// | ```
var array = function (codec) {
    return Data_Codec["codec$prime"](function (f) {
        return bindFlipped(traverseWithIndex(function (ix) {
            return function (a) {
                return lmap(AtIndex.create(ix))(Data_Codec.decode(codec)(a));
            };
        }))(Data_Codec.decode(farray)(f));
    })(function (a) {
        return Foreign.unsafeToForeign(map1(Data_Codec.encode(codec))(a));
    });
};
export {
    _null,
    _undefined,
    _stringify
} from "./foreign.js";
export {
    ForeignDecodingErrors,
    InvalidCodePoint,
    VoidError,
    AtIndex,
    AtKey,
    Named,
    MissingValueAtIndex,
    MissingValueAtKey,
    UnexpectedTagValue,
    UnexpectedValue,
    printForeignDecodingError,
    foreign_,
    $$null as null,
    $$boolean as boolean,
    number,
    $$int as int,
    string,
    codePoint,
    $$void as void,
    farray,
    fobject,
    array,
    indexedArray,
    index,
    object,
    prop,
    record,
    recordProp,
    fix,
    prismaticCodec,
    harismaticCodec,
    encodec,
    decodec,
    decodec$prime,
    decodingError,
    eqForeignDecodingError,
    genericForeignDecodingErr,
    showForeignDecodingError
};
