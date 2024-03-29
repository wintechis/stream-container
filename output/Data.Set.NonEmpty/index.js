// Generated by purs version 0.15.10
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_Set from "../Data.Set/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable1 from "../Data.Unfoldable1/index.js";
var toUnfoldable2 = /* #__PURE__ */ Data_Set.toUnfoldable(Data_List_Types.unfoldableList);
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var foldMap1 = /* #__PURE__ */ Data_Semigroup_Foldable.foldMap1(Data_List_Types.foldable1NonEmptyList);
var foldr1 = /* #__PURE__ */ Data_Semigroup_Foldable.foldr1(Data_List_Types.foldable1NonEmptyList);
var foldl1 = /* #__PURE__ */ Data_Semigroup_Foldable.foldl1(Data_List_Types.foldable1NonEmptyList);
var NonEmptySet = function (x) {
    return x;
};
var unionSet = function (dictOrd) {
    var append1 = Data_Semigroup.append(Data_Set.semigroupSet(dictOrd));
    return function (s1) {
        return function (v) {
            return append1(s1)(v);
        };
    };
};
var toUnfoldable1 = function (dictUnfoldable1) {
    var unfoldr1 = Data_Unfoldable1.unfoldr1(dictUnfoldable1);
    return function (v) {
        var go = function (v1) {
            if (v1 instanceof Data_List_Types.Cons && v1.value1 instanceof Data_List_Types.Nil) {
                return new Data_Tuple.Tuple(v1.value0, Data_Maybe.Nothing.value);
            };
            if (v1 instanceof Data_List_Types.Cons) {
                return new Data_Tuple.Tuple(v1.value0, new Data_Maybe.Just(v1.value1));
            };
            throw new Error("Failed pattern match at Data.Set.NonEmpty (line 95, column 24 - line 97, column 38): " + [ v1.constructor.name ]);
        };
        return unfoldr1(go)(toUnfoldable2(v));
    };
};
var toUnfoldable11 = /* #__PURE__ */ toUnfoldable1(Data_Array_NonEmpty_Internal.unfoldable1NonEmptyArray);
var toUnfoldable12 = /* #__PURE__ */ toUnfoldable1(Data_List_Types.unfoldable1NonEmptyList);
var toUnfoldable = function (dictUnfoldable) {
    var toUnfoldable3 = Data_Set.toUnfoldable(dictUnfoldable);
    return function (v) {
        return toUnfoldable3(v);
    };
};
var toSet = function (v) {
    return v;
};
var subset = function (dictOrd) {
    var subset1 = Data_Set.subset(dictOrd);
    return function (v) {
        return function (v1) {
            return subset1(v)(v1);
        };
    };
};
var size = function (v) {
    return Data_Set.size(v);
};
var singleton = function (a) {
    return Data_Set.singleton(a);
};
var showNonEmptySet = function (dictShow) {
    var show = Data_Show.show(Data_Array_NonEmpty_Internal.showNonEmptyArray(dictShow));
    return {
        show: function (s) {
            return "(fromFoldable1 " + (show(toUnfoldable11(s)) + ")");
        }
    };
};
var semigroupNonEmptySet = function (dictOrd) {
    return Data_Set.semigroupSet(dictOrd);
};
var properSubset = function (dictOrd) {
    var properSubset1 = Data_Set.properSubset(dictOrd);
    return function (v) {
        return function (v1) {
            return properSubset1(v)(v1);
        };
    };
};
var ordNonEmptySet = function (dictOrd) {
    return Data_Set.ordSet(dictOrd);
};
var ord1NonEmptySet = Data_Set.ord1Set;
var min = function (v) {
    return fromJust(Data_Set.findMin(v));
};
var member = function (dictOrd) {
    var member1 = Data_Set.member(dictOrd);
    return function (a) {
        return function (v) {
            return member1(a)(v);
        };
    };
};
var max = function (v) {
    return fromJust(Data_Set.findMax(v));
};
var mapMaybe = function (dictOrd) {
    var mapMaybe1 = Data_Set.mapMaybe(dictOrd);
    return function (f) {
        return function (v) {
            return mapMaybe1(f)(v);
        };
    };
};
var map = function (dictOrd) {
    var map1 = Data_Set.map(dictOrd);
    return function (f) {
        return function (v) {
            return map1(f)(v);
        };
    };
};
var insert = function (dictOrd) {
    var insert1 = Data_Set.insert(dictOrd);
    return function (a) {
        return function (v) {
            return insert1(a)(v);
        };
    };
};
var fromSet = function (s) {
    var $105 = Data_Set.isEmpty(s);
    if ($105) {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just(s);
};
var intersection = function (dictOrd) {
    var intersection1 = Data_Set.intersection(dictOrd);
    return function (v) {
        return function (v1) {
            return fromSet(intersection1(v)(v1));
        };
    };
};
var fromFoldable1 = function (dictFoldable1) {
    var foldMap11 = Data_Semigroup_Foldable.foldMap1(dictFoldable1);
    return function (dictOrd) {
        return foldMap11(semigroupNonEmptySet(dictOrd))(singleton);
    };
};
var fromFoldable = function (dictFoldable) {
    var fromFoldable2 = Data_Set.fromFoldable(dictFoldable);
    return function (dictOrd) {
        var $114 = fromFoldable2(dictOrd);
        return function ($115) {
            return fromSet($114($115));
        };
    };
};
var foldableNonEmptySet = Data_Set.foldableSet;
var foldable1NonEmptySet = {
    foldMap1: function (dictSemigroup) {
        var foldMap11 = foldMap1(dictSemigroup);
        return function (f) {
            var $116 = foldMap11(f);
            return function ($117) {
                return $116(toUnfoldable12($117));
            };
        };
    },
    foldr1: function (f) {
        var $118 = foldr1(f);
        return function ($119) {
            return $118(toUnfoldable12($119));
        };
    },
    foldl1: function (f) {
        var $120 = foldl1(f);
        return function ($121) {
            return $120(toUnfoldable12($121));
        };
    },
    Foldable0: function () {
        return foldableNonEmptySet;
    }
};
var filter = function (dictOrd) {
    var filter1 = Data_Set.filter(dictOrd);
    return function (f) {
        return function (v) {
            return filter1(f)(v);
        };
    };
};
var eqNonEmptySet = function (dictEq) {
    return Data_Set.eqSet(dictEq);
};
var eq1NonEmptySet = Data_Set.eq1Set;
var difference = function (dictOrd) {
    var difference1 = Data_Set.difference(dictOrd);
    return function (v) {
        return function (v1) {
            return fromSet(difference1(v)(v1));
        };
    };
};
var $$delete = function (dictOrd) {
    var delete1 = Data_Set["delete"](dictOrd);
    return function (a) {
        return function (v) {
            return fromSet(delete1(a)(v));
        };
    };
};
var cons = function (dictOrd) {
    var insert1 = Data_Set.insert(dictOrd);
    return function (a) {
        var $122 = insert1(a);
        return function ($123) {
            return NonEmptySet($122($123));
        };
    };
};
export {
    singleton,
    cons,
    fromSet,
    fromFoldable,
    fromFoldable1,
    toSet,
    toUnfoldable,
    toUnfoldable1,
    map,
    member,
    insert,
    $$delete as delete,
    size,
    min,
    max,
    unionSet,
    difference,
    subset,
    properSubset,
    intersection,
    filter,
    mapMaybe,
    eqNonEmptySet,
    eq1NonEmptySet,
    ordNonEmptySet,
    ord1NonEmptySet,
    semigroupNonEmptySet,
    foldableNonEmptySet,
    foldable1NonEmptySet,
    showNonEmptySet
};
