// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_These from "../Data.These/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Data_List_Lazy_Types.monoidList);
var apply = /* #__PURE__ */ Control_Apply.apply(Data_Lazy.applyLazy);
var map2 = /* #__PURE__ */ Data_Functor.map(Data_Lazy.functorLazy);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var map3 = /* #__PURE__ */ Data_Functor.map(Data_List_Types.functorList);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var pure = /* #__PURE__ */ Control_Applicative.pure(Control_Applicative.applicativeArray);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Data_List_Lazy_Types.applicativeList);
var pure2 = /* #__PURE__ */ Control_Applicative.pure(Data_List_Types.applicativeList);
var nil = function (dict) {
    return dict.nil;
};
var crosswalkThese = {
    crosswalk: function (dictAlignable) {
        var nil1 = nil(dictAlignable);
        var map4 = Data_Functor.map((dictAlignable.Align0()).Functor0());
        return function (f) {
            return function (v) {
                if (v instanceof Data_These.This) {
                    return nil1;
                };
                if (v instanceof Data_These.That) {
                    return map4(Data_These.That.create)(f(v.value0));
                };
                if (v instanceof Data_These.Both) {
                    return map4(Data_These.Both.create(v.value0))(f(v.value1));
                };
                throw new Error("Failed pattern match at Data.Align (line 115, column 17 - line 118, column 31): " + [ v.constructor.name ]);
            };
        };
    },
    Foldable0: function () {
        return Data_These.foldableThese;
    },
    Functor1: function () {
        return Data_These.functorThese;
    }
};
var crosswalkMaybe = {
    crosswalk: function (dictAlignable) {
        var nil1 = nil(dictAlignable);
        var map4 = Data_Functor.map((dictAlignable.Align0()).Functor0());
        return function (f) {
            return function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return nil1;
                };
                if (v instanceof Data_Maybe.Just) {
                    return map4(Data_Maybe.Just.create)(f(v.value0));
                };
                throw new Error("Failed pattern match at Data.Align (line 143, column 17 - line 145, column 27): " + [ v.constructor.name ]);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableMaybe;
    },
    Functor1: function () {
        return Data_Maybe.functorMaybe;
    }
};
var crosswalk = function (dict) {
    return dict.crosswalk;
};
var alignMaybe = {
    align: function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Data_Maybe.Nothing) {
                    return map(function ($102) {
                        return v(Data_These.This.create($102));
                    })(v1);
                };
                if (v1 instanceof Data_Maybe.Nothing) {
                    return map(function ($103) {
                        return v(Data_These.That.create($103));
                    })(v2);
                };
                if (v1 instanceof Data_Maybe.Just && v2 instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just(v(new Data_These.Both(v1.value0, v2.value0)));
                };
                throw new Error("Failed pattern match at Data.Align (line 52, column 1 - line 55, column 50): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    },
    Functor0: function () {
        return Data_Maybe.functorMaybe;
    }
};
var alignableMaybe = /* #__PURE__ */ (function () {
    return {
        nil: Data_Maybe.Nothing.value,
        Align0: function () {
            return alignMaybe;
        }
    };
})();
var align = function (dict) {
    return dict.align;
};
var alignArray = {
    align: function (v) {
        return function (v1) {
            return function (v2) {
                if (v2.length === 0) {
                    return map1(function ($104) {
                        return v(Data_These.This.create($104));
                    })(v1);
                };
                if (v1.length === 0) {
                    return map1(function ($105) {
                        return v(Data_These.That.create($105));
                    })(v2);
                };
                var ys$prime = Data_Array.drop(Data_Array.length(v1))(v2);
                var xs$prime = Data_Array.drop(Data_Array.length(v2))(v1);
                var f$prime = function (x) {
                    return function (y) {
                        return v(new Data_These.Both(x, y));
                    };
                };
                return append(Data_Array.zipWith(f$prime)(v1)(v2))(align(alignArray)(v)(xs$prime)(ys$prime));
            };
        };
    },
    Functor0: function () {
        return Data_Functor.functorArray;
    }
};
var alignableArray = {
    nil: /* #__PURE__ */ Data_Monoid.mempty(Data_Monoid.monoidArray),
    Align0: function () {
        return alignArray;
    }
};
var alignLazyList = {
    align: function (f) {
        return function (xs) {
            return function (ys) {
                var go = function (v) {
                    return function (v1) {
                        if (v instanceof Data_List_Lazy_Types.Nil && v1 instanceof Data_List_Lazy_Types.Nil) {
                            return Data_List_Lazy_Types.Nil.value;
                        };
                        if (v instanceof Data_List_Lazy_Types.Cons && v1 instanceof Data_List_Lazy_Types.Nil) {
                            return new Data_List_Lazy_Types.Cons(f(new Data_These.This(v.value0)), align(alignLazyList)(f)(v.value1)(mempty));
                        };
                        if (v instanceof Data_List_Lazy_Types.Nil && v1 instanceof Data_List_Lazy_Types.Cons) {
                            return new Data_List_Lazy_Types.Cons(f(new Data_These.That(v1.value0)), align(alignLazyList)(f)(mempty)(v1.value1));
                        };
                        if (v instanceof Data_List_Lazy_Types.Cons && v1 instanceof Data_List_Lazy_Types.Cons) {
                            return new Data_List_Lazy_Types.Cons(f(new Data_These.Both(v.value0, v1.value0)), align(alignLazyList)(f)(v.value1)(v1.value1));
                        };
                        throw new Error("Failed pattern match at Data.Align (line 47, column 5 - line 47, column 48): " + [ v.constructor.name, v1.constructor.name ]);
                    };
                };
                return apply(map2(go)(unwrap(xs)))(unwrap(ys));
            };
        };
    },
    Functor0: function () {
        return Data_List_Lazy_Types.functorList;
    }
};
var alignableLazyList = {
    nil: mempty,
    Align0: function () {
        return alignLazyList;
    }
};
var alignList = {
    align: function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Data_List_Types.Nil) {
                    return map3(function ($106) {
                        return v(Data_These.This.create($106));
                    })(v1);
                };
                if (v1 instanceof Data_List_Types.Nil) {
                    return map3(function ($107) {
                        return v(Data_These.That.create($107));
                    })(v2);
                };
                if (v1 instanceof Data_List_Types.Cons && v2 instanceof Data_List_Types.Cons) {
                    return new Data_List_Types.Cons(v(new Data_These.Both(v1.value0, v2.value0)), align(alignList)(v)(v1.value1)(v2.value1));
                };
                throw new Error("Failed pattern match at Data.Align (line 39, column 1 - line 42, column 85): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    },
    Functor0: function () {
        return Data_List_Types.functorList;
    }
};
var alignableList = {
    nil: /* #__PURE__ */ Data_Monoid.mempty(Data_List_Types.monoidList),
    Align0: function () {
        return alignList;
    }
};
var aligned = function (dictAlign) {
    return align(dictAlign)(identity);
};
var crosswalkArray = {
    crosswalk: function (dictAlignable) {
        var nil1 = nil(dictAlignable);
        var align1 = align(dictAlignable.Align0());
        return function (f) {
            return function (xs) {
                var cons = Data_These.these(pure)(identity)(Data_Array.cons);
                var v = Data_Array.uncons(xs);
                if (v instanceof Data_Maybe.Nothing) {
                    return nil1;
                };
                if (v instanceof Data_Maybe.Just) {
                    return align1(cons)(f(v.value0.head))(crosswalk(crosswalkArray)(dictAlignable)(f)(v.value0.tail));
                };
                throw new Error("Failed pattern match at Data.Align (line 121, column 20 - line 123, column 66): " + [ v.constructor.name ]);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableArray;
    },
    Functor1: function () {
        return Data_Functor.functorArray;
    }
};
var crosswalkLazyList = {
    crosswalk: function (dictAlignable) {
        var nil1 = nil(dictAlignable);
        var align1 = align(dictAlignable.Align0());
        return function (f) {
            return function (l) {
                var cons = Data_These.these(pure1)(identity)(Data_List_Lazy_Types.cons);
                var v = Data_List_Lazy_Types.step(l);
                if (v instanceof Data_List_Lazy_Types.Nil) {
                    return nil1;
                };
                if (v instanceof Data_List_Lazy_Types.Cons) {
                    return align1(cons)(f(v.value0))(crosswalk(crosswalkLazyList)(dictAlignable)(f)(v.value1));
                };
                throw new Error("Failed pattern match at Data.Align (line 136, column 5 - line 138, column 62): " + [ v.constructor.name ]);
            };
        };
    },
    Foldable0: function () {
        return Data_List_Lazy_Types.foldableList;
    },
    Functor1: function () {
        return Data_List_Lazy_Types.functorList;
    }
};
var crosswalkList = {
    crosswalk: function (dictAlignable) {
        var nil1 = nil(dictAlignable);
        var align1 = align(dictAlignable.Align0());
        return function (f) {
            var cons = Data_These.these(pure2)(identity)(Data_List_Types.Cons.create);
            return function (v) {
                if (v instanceof Data_List_Types.Nil) {
                    return nil1;
                };
                if (v instanceof Data_List_Types.Cons) {
                    return align1(cons)(f(v.value0))(crosswalk(crosswalkList)(dictAlignable)(f)(v.value1));
                };
                throw new Error("Failed pattern match at Data.Align (line 128, column 17 - line 130, column 56): " + [ v.constructor.name ]);
            };
        };
    },
    Foldable0: function () {
        return Data_List_Types.foldableList;
    },
    Functor1: function () {
        return Data_List_Types.functorList;
    }
};
export {
    align,
    crosswalk,
    nil,
    aligned,
    alignArray,
    alignList,
    alignLazyList,
    alignMaybe,
    alignableArray,
    alignableList,
    alignableLazyList,
    alignableMaybe,
    crosswalkThese,
    crosswalkArray,
    crosswalkList,
    crosswalkLazyList,
    crosswalkMaybe
};
