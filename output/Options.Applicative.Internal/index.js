// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Alternative from "../Control.Alternative/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Except from "../Control.Monad.Except/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Control_Monad_Reader from "../Control.Monad.Reader/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_Reader_Trans from "../Control.Monad.Reader.Trans/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Exists from "../Data.Exists/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var un = /* #__PURE__ */ Data_Newtype.un();
var map = /* #__PURE__ */ Data_Functor.map(/* #__PURE__ */ Control_Monad_Except_Trans.functorExceptT(/* #__PURE__ */ Control_Monad_State_Trans.functorStateT(/* #__PURE__ */ Control_Monad_Reader_Trans.functorReaderT(Data_Identity.functorIdentity))));
var monadReaderT = /* #__PURE__ */ Control_Monad_Reader_Trans.monadReaderT(Data_Identity.monadIdentity);
var monadStateT = /* #__PURE__ */ Control_Monad_State_Trans.monadStateT(monadReaderT);
var apply = /* #__PURE__ */ Control_Apply.apply(/* #__PURE__ */ Control_Monad_Except_Trans.applyExceptT(monadStateT));
var bind = /* #__PURE__ */ Control_Bind.bind(/* #__PURE__ */ Control_Monad_Except_Trans.bindExceptT(monadStateT));
var pure = /* #__PURE__ */ Control_Applicative.pure(/* #__PURE__ */ Control_Monad_Except_Trans.applicativeExceptT(monadStateT));
var altExceptT = /* #__PURE__ */ Control_Monad_Except_Trans.altExceptT(Options_Applicative_Types.parseErrorSemigroup);
var alt = /* #__PURE__ */ Control_Alt.alt(/* #__PURE__ */ altExceptT(monadStateT));
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT);
var lift1 = /* #__PURE__ */ lift(monadStateT);
var modify_ = /* #__PURE__ */ Control_Monad_State_Class.modify_(/* #__PURE__ */ Control_Monad_State_Trans.monadStateStateT(monadReaderT));
var lift2 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT);
var throwError = /* #__PURE__ */ Control_Monad_Error_Class.throwError(/* #__PURE__ */ Control_Monad_Except_Trans.monadThrowExceptT(monadStateT));
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Control_Applicative.applicativeArray);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var TNil = /* #__PURE__ */ (function () {
    function TNil() {

    };
    TNil.value = new TNil();
    return TNil;
})();
var TCons = /* #__PURE__ */ (function () {
    function TCons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TCons.create = function (value0) {
        return function (value1) {
            return new TCons(value0, value1);
        };
    };
    return TCons;
})();
var P = function (x) {
    return x;
};
var ListT = function (x) {
    return x;
};
var NondetT = function (x) {
    return x;
};
var ComplParser = /* #__PURE__ */ (function () {
    function ComplParser(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ComplParser.create = function (value0) {
        return function (value1) {
            return new ComplParser(value0, value1);
        };
    };
    return ComplParser;
})();
var ComplOption = /* #__PURE__ */ (function () {
    function ComplOption(value0) {
        this.value0 = value0;
    };
    ComplOption.create = function (value0) {
        return new ComplOption(value0);
    };
    return ComplOption;
})();
var ComplResult = /* #__PURE__ */ (function () {
    function ComplResult(value0) {
        this.value0 = value0;
    };
    ComplResult.create = function (value0) {
        return new ComplResult(value0);
    };
    return ComplResult;
})();
var Completion = function (x) {
    return x;
};
var withReadM = function (f) {
    var f$prime = function (v) {
        if (v instanceof Options_Applicative_Types.ErrorMsg) {
            return new Options_Applicative_Types.ErrorMsg(f(v.value0));
        };
        return v;
    };
    var $298 = Control_Monad_Reader_Trans.mapReaderT(Control_Monad_Except.withExcept(f$prime));
    var $299 = un(Options_Applicative_Types.ReadM);
    return function ($300) {
        return Options_Applicative_Types.ReadM($298($299($300)));
    };
};
var stepListT = function (v) {
    return v;
};
var runP = function (v) {
    return Control_Monad_Reader.runReader(Data_Function.flip(Control_Monad_State_Trans.runStateT)([  ])(Control_Monad_Except_Trans.runExceptT(v)));
};
var runNondetT = function (v) {
    return v;
};
var runListT = function (dictMonad) {
    var bind2 = Control_Bind.bind(dictMonad.Bind1());
    var pure4 = Control_Applicative.pure(dictMonad.Applicative0());
    var liftM1 = Control_Monad.liftM1(dictMonad);
    return function (xs) {
        return bind2(stepListT(xs))(function (s) {
            if (s instanceof TNil) {
                return pure4(Data_List_Types.Nil.value);
            };
            if (s instanceof TCons) {
                return liftM1(Data_List_Types.Cons.create(s.value0))(runListT(dictMonad)(s.value1));
            };
            throw new Error("Failed pattern match at Options.Applicative.Internal (line 200, column 3 - line 202, column 53): " + [ s.constructor.name ]);
        });
    };
};
var runCompletion = function (v) {
    return function (prefs) {
        var v1 = Control_Monad_Reader_Trans.runReaderT(Control_Monad_Except_Trans.runExceptT(v))(prefs);
        if (v1 instanceof ComplResult) {
            return Data_Maybe.Nothing.value;
        };
        if (v1 instanceof ComplParser) {
            return new Data_Maybe.Just(new Data_Either.Left(new Data_Tuple.Tuple(v1.value0, v1.value1)));
        };
        if (v1 instanceof ComplOption) {
            return new Data_Maybe.Just(new Data_Either.Right(v1.value0));
        };
        throw new Error("Failed pattern match at Options.Applicative.Internal (line 170, column 38 - line 173, column 42): " + [ v1.constructor.name ]);
    };
};
var pFunctor = {
    map: function (f) {
        return function (v) {
            return map(f)(v);
        };
    }
};
var pApply = {
    apply: function (v) {
        return function (v1) {
            return apply(v)(v1);
        };
    },
    Functor0: function () {
        return pFunctor;
    }
};
var pBind = {
    bind: function (v) {
        return function (k) {
            return bind(v)(function (a) {
                var v1 = k(a);
                return v1;
            });
        };
    },
    Apply0: function () {
        return pApply;
    }
};
var pApplicative = {
    pure: function (a) {
        return pure(a);
    },
    Apply0: function () {
        return pApply;
    }
};
var pMonad = {
    Applicative0: function () {
        return pApplicative;
    },
    Bind1: function () {
        return pBind;
    }
};
var pAlt = {
    alt: function (v) {
        return function (v1) {
            return alt(v)(v1);
        };
    },
    Functor0: function () {
        return pFunctor;
    }
};
var missingArgP = function (dict) {
    return dict.missingArgP;
};
var getPrefs = function (dict) {
    return dict.getPrefs;
};
var exitP = function (dict) {
    return dict.exitP;
};
var exitContext = function (dict) {
    return dict.exitContext;
};
var errorP = function (dict) {
    return dict.errorP;
};
var hoistEither = function (dictMonadP) {
    return Data_Either.either(errorP(dictMonadP))(Control_Applicative.pure((dictMonadP.Monad0()).Applicative0()));
};
var runReadM = function (dictMonadP) {
    var hoistEither1 = hoistEither(dictMonadP);
    return function (v) {
        return function (s) {
            return hoistEither1(Control_Monad_Except.runExcept(Control_Monad_Reader_Trans.runReaderT(v)(s)));
        };
    };
};
var hoistMaybe = function (dictMonadP) {
    var errorP1 = errorP(dictMonadP);
    var pure4 = Control_Applicative.pure((dictMonadP.Monad0()).Applicative0());
    return function (err) {
        return Data_Maybe.maybe(errorP1(err))(pure4);
    };
};
var pMonadP = {
    enterContext: function (name) {
        return function (pinfo) {
            return lift1(modify_(Data_Array.cons(new Options_Applicative_Types.Context(name, Data_Exists.mkExists(pinfo)))));
        };
    },
    exitContext: /* #__PURE__ */ lift1(/* #__PURE__ */ modify_(/* #__PURE__ */ Data_Array.drop(1))),
    getPrefs: /* #__PURE__ */ P(/* #__PURE__ */ lift1(/* #__PURE__ */ lift2(monadReaderT)(/* #__PURE__ */ Control_Monad_Reader_Class.ask(/* #__PURE__ */ Control_Monad_Reader_Trans.monadAskReaderT(Data_Identity.monadIdentity))))),
    missingArgP: function (e) {
        return function (v) {
            return errorP(pMonadP)(e);
        };
    },
    exitP: function (i) {
        return function (v) {
            return function (p) {
                var $301 = Data_Maybe.maybe(throwError(Options_Applicative_Types.MissingError.create(i)(Options_Applicative_Types.SomeParser.create(Data_Exists.mkExists(p)))))(pure);
                return function ($302) {
                    return P($301($302));
                };
            };
        };
    },
    errorP: function ($303) {
        return P(throwError($303));
    },
    Monad0: function () {
        return pMonad;
    },
    Alt1: function () {
        return pAlt;
    }
};
var enterContext = function (dict) {
    return dict.enterContext;
};
var contextNames = function (ns) {
    var go = function (v) {
        return v.value0;
    };
    return Data_Array.reverse(map1(go)(ns));
};
var complResultMonad = {
    Applicative0: function () {
        return complResultApplicative;
    },
    Bind1: function () {
        return complResultBind;
    }
};
var complResultBind = {
    bind: function (m) {
        return function (f) {
            if (m instanceof ComplResult) {
                return f(m.value0);
            };
            if (m instanceof ComplParser) {
                return new ComplParser(m.value0, m.value1);
            };
            if (m instanceof ComplOption) {
                return new ComplOption(m.value0);
            };
            throw new Error("Failed pattern match at Options.Applicative.Internal (line 134, column 14 - line 137, column 35): " + [ m.constructor.name ]);
        };
    },
    Apply0: function () {
        return $lazy_complResultApply(0);
    }
};
var complResultApplicative = /* #__PURE__ */ (function () {
    return {
        pure: ComplResult.create,
        Apply0: function () {
            return $lazy_complResultApply(0);
        }
    };
})();
var $lazy_complResultFunctor = /* #__PURE__ */ $runtime_lazy("complResultFunctor", "Options.Applicative.Internal", function () {
    return {
        map: Control_Monad.liftM1(complResultMonad)
    };
});
var $lazy_complResultApply = /* #__PURE__ */ $runtime_lazy("complResultApply", "Options.Applicative.Internal", function () {
    return {
        apply: Control_Monad.ap(complResultMonad),
        Functor0: function () {
            return $lazy_complResultFunctor(0);
        }
    };
});
var complResultFunctor = /* #__PURE__ */ $lazy_complResultFunctor(124);
var complResultApply = /* #__PURE__ */ $lazy_complResultApply(127);
var map2 = /* #__PURE__ */ Data_Functor.map(/* #__PURE__ */ Control_Monad_Except_Trans.functorExceptT(/* #__PURE__ */ Control_Monad_Reader_Trans.functorReaderT(complResultFunctor)));
var monadReaderT1 = /* #__PURE__ */ Control_Monad_Reader_Trans.monadReaderT(complResultMonad);
var alt1 = /* #__PURE__ */ Control_Alt.alt(/* #__PURE__ */ altExceptT(monadReaderT1));
var apply1 = /* #__PURE__ */ Control_Apply.apply(/* #__PURE__ */ Control_Monad_Except_Trans.applyExceptT(monadReaderT1));
var pure2 = /* #__PURE__ */ Control_Applicative.pure(/* #__PURE__ */ Control_Monad_Except_Trans.applicativeExceptT(monadReaderT1));
var bind1 = /* #__PURE__ */ Control_Bind.bind(/* #__PURE__ */ Control_Monad_Except_Trans.bindExceptT(monadReaderT1));
var lift3 = /* #__PURE__ */ lift(monadReaderT1);
var lift4 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT)(complResultMonad);
var completionFunctor = {
    map: function (f) {
        return function (v) {
            return map2(f)(v);
        };
    }
};
var completionAlt = {
    alt: function (v) {
        return function (v1) {
            return alt1(v)(v1);
        };
    },
    Functor0: function () {
        return completionFunctor;
    }
};
var completionApply = {
    apply: function (v) {
        return function (v1) {
            return apply1(v)(v1);
        };
    },
    Functor0: function () {
        return completionFunctor;
    }
};
var completionApplicative = {
    pure: function (a) {
        return pure2(a);
    },
    Apply0: function () {
        return completionApply;
    }
};
var pure3 = /* #__PURE__ */ Control_Applicative.pure(completionApplicative);
var completionBind = {
    bind: function (v) {
        return function (k) {
            return bind1(v)(function (a) {
                var v1 = k(a);
                return v1;
            });
        };
    },
    Apply0: function () {
        return completionApply;
    }
};
var completionMonad = {
    Applicative0: function () {
        return completionApplicative;
    },
    Bind1: function () {
        return completionBind;
    }
};
var completionMonadP = {
    enterContext: function (v) {
        return function (v1) {
            return pure3(Data_Unit.unit);
        };
    },
    exitContext: /* #__PURE__ */ pure3(Data_Unit.unit),
    getPrefs: /* #__PURE__ */ lift3(/* #__PURE__ */ Control_Monad_Reader_Class.ask(/* #__PURE__ */ Control_Monad_Reader_Trans.monadAskReaderT(complResultMonad))),
    missingArgP: function (v) {
        return function ($304) {
            return Completion(lift3(lift4(ComplOption.create($304))));
        };
    },
    exitP: function (v) {
        return function (a) {
            return function (p) {
                return function (v1) {
                    return Completion(lift3(lift4(new ComplParser(new Options_Applicative_Types.SomeParser(Data_Exists.mkExists(p)), a))));
                };
            };
        };
    },
    errorP: /* #__PURE__ */ (function () {
        var $305 = Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(monadReaderT1));
        return function ($306) {
            return Completion($305($306));
        };
    })(),
    Monad0: function () {
        return completionMonad;
    },
    Alt1: function () {
        return completionAlt;
    }
};
var bimapTStep = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof TNil) {
                return TNil.value;
            };
            if (v2 instanceof TCons) {
                return new TCons(v(v2.value0), v1(v2.value1));
            };
            throw new Error("Failed pattern match at Options.Applicative.Internal (line 186, column 1 - line 186, column 77): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var listTFunctor = function (dictMonad) {
    var liftM1 = Control_Monad.liftM1(dictMonad);
    return {
        map: function (f) {
            return function (v) {
                return liftM1(bimapTStep(f)(Data_Functor.map(listTFunctor(dictMonad))(f)))(stepListT(v));
            };
        }
    };
};
var listTAlt = function (dictMonad) {
    var bind2 = Control_Bind.bind(dictMonad.Bind1());
    var pure4 = Control_Applicative.pure(dictMonad.Applicative0());
    var listTFunctor1 = listTFunctor(dictMonad);
    return {
        alt: function (xs) {
            return function (ys) {
                return bind2(stepListT(xs))(function (s) {
                    if (s instanceof TNil) {
                        return stepListT(ys);
                    };
                    if (s instanceof TCons) {
                        return pure4(new TCons(s.value0, Control_Alt.alt(listTAlt(dictMonad))(s.value1)(ys)));
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Internal (line 227, column 5 - line 229, column 49): " + [ s.constructor.name ]);
                });
            };
        },
        Functor0: function () {
            return listTFunctor1;
        }
    };
};
var listTPlus = function (dictMonad) {
    var listTAlt1 = listTAlt(dictMonad);
    return {
        empty: Control_Applicative.pure(dictMonad.Applicative0())(TNil.value),
        Alt0: function () {
            return listTAlt1;
        }
    };
};
var hoistList = function (dictMonad) {
    var pure4 = Control_Applicative.pure(dictMonad.Applicative0());
    return Data_Array.foldr(function (x) {
        return function (xt) {
            return pure4(new TCons(x, xt));
        };
    })(Control_Plus.empty(listTPlus(dictMonad)));
};
var listTMonadTrans = {
    lift: function (dictMonad) {
        var empty = Control_Plus.empty(listTPlus(dictMonad));
        var $307 = Control_Monad.liftM1(dictMonad)(function (v) {
            return new TCons(v, empty);
        });
        return function ($308) {
            return ListT($307($308));
        };
    }
};
var lift5 = /* #__PURE__ */ Control_Monad_Trans_Class.lift(listTMonadTrans);
var cut = function (dictMonad) {
    return lift5(Control_Monad_State_Trans.monadStateT(dictMonad))(Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(dictMonad))(true));
};
var nondetTMonadTrans = {
    lift: function (dictMonad) {
        var $309 = lift5(Control_Monad_State_Trans.monadStateT(dictMonad));
        var $310 = lift2(dictMonad);
        return function ($311) {
            return NondetT($309($310($311)));
        };
    }
};
var listTMonad = function (dictMonad) {
    return {
        Applicative0: function () {
            return listTApplicative(dictMonad);
        },
        Bind1: function () {
            return listTBind(dictMonad);
        }
    };
};
var listTBind = function (dictMonad) {
    var bind2 = Control_Bind.bind(dictMonad.Bind1());
    var pure4 = Control_Applicative.pure(dictMonad.Applicative0());
    var alt2 = Control_Alt.alt(listTAlt(dictMonad));
    return {
        bind: function (xs) {
            return function (f) {
                return bind2(stepListT(xs))(function (s) {
                    if (s instanceof TNil) {
                        return pure4(TNil.value);
                    };
                    if (s instanceof TCons) {
                        return stepListT(alt2(f(s.value0))(Control_Bind.bind(listTBind(dictMonad))(s.value1)(f)));
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Internal (line 218, column 5 - line 220, column 53): " + [ s.constructor.name ]);
                });
            };
        },
        Apply0: function () {
            return listTApply(dictMonad);
        }
    };
};
var listTApply = function (dictMonad) {
    var listTFunctor1 = listTFunctor(dictMonad);
    return {
        apply: Control_Monad.ap(listTMonad(dictMonad)),
        Functor0: function () {
            return listTFunctor1;
        }
    };
};
var listTApplicative = function (dictMonad) {
    return {
        pure: (function () {
            var $312 = hoistList(dictMonad);
            return function ($313) {
                return $312(pure1($313));
            };
        })(),
        Apply0: function () {
            return listTApply(dictMonad);
        }
    };
};
var listTAlternative = function (dictMonad) {
    var listTApplicative1 = listTApplicative(dictMonad);
    var listTPlus1 = listTPlus(dictMonad);
    return {
        Applicative0: function () {
            return listTApplicative1;
        },
        Plus1: function () {
            return listTPlus1;
        }
    };
};
var listTMonadPlus = function (dictMonad) {
    var listTMonad1 = listTMonad(dictMonad);
    var listTAlternative1 = listTAlternative(dictMonad);
    return {
        Monad0: function () {
            return listTMonad1;
        },
        Alternative1: function () {
            return listTAlternative1;
        }
    };
};
var nondetTAltOp = function (dictMonad) {
    var monadStateT1 = Control_Monad_State_Trans.monadStateT(dictMonad);
    var alt2 = Control_Alt.alt(listTAlt(monadStateT1));
    var listTBind1 = listTBind(monadStateT1);
    var bind2 = Control_Bind.bind(listTBind1);
    var lift6 = lift5(monadStateT1);
    var get = Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(dictMonad));
    var discard1 = discard(listTBind1);
    var guard = Control_Alternative.guard(listTAlternative(monadStateT1));
    return function (m1) {
        return function (m2) {
            return NondetT(alt2(runNondetT(m1))(bind2(lift6(get))(function (s) {
                return discard1(guard(!s))(function () {
                    return runNondetT(m2);
                });
            })));
        };
    };
};
var nondetTFunctor = function (dictMonad) {
    var map3 = Data_Functor.map(listTFunctor(Control_Monad_State_Trans.monadStateT(dictMonad)));
    return {
        map: function (f) {
            var $314 = map3(f);
            return function ($315) {
                return NondetT($314(runNondetT($315)));
            };
        }
    };
};
var nondetTAlt = function (dictMonad) {
    var alt2 = Control_Alt.alt(listTAlt(Control_Monad_State_Trans.monadStateT(dictMonad)));
    var nondetTFunctor1 = nondetTFunctor(dictMonad);
    return {
        alt: function (v) {
            return function (v1) {
                return alt2(v)(v1);
            };
        },
        Functor0: function () {
            return nondetTFunctor1;
        }
    };
};
var nondetTPlus = function (dictMonad) {
    var nondetTAlt1 = nondetTAlt(dictMonad);
    return {
        empty: Control_Plus.empty(listTPlus(Control_Monad_State_Trans.monadStateT(dictMonad))),
        Alt0: function () {
            return nondetTAlt1;
        }
    };
};
var nondetTApply = function (dictMonad) {
    var apply2 = Control_Apply.apply(listTApply(Control_Monad_State_Trans.monadStateT(dictMonad)));
    var nondetTFunctor1 = nondetTFunctor(dictMonad);
    return {
        apply: function (v) {
            return function (v1) {
                return apply2(v)(v1);
            };
        },
        Functor0: function () {
            return nondetTFunctor1;
        }
    };
};
var nondetTApplicative = function (dictMonad) {
    var nondetTApply1 = nondetTApply(dictMonad);
    return {
        pure: (function () {
            var $316 = Control_Applicative.pure(listTApplicative(Control_Monad_State_Trans.monadStateT(dictMonad)));
            return function ($317) {
                return NondetT($316($317));
            };
        })(),
        Apply0: function () {
            return nondetTApply1;
        }
    };
};
var nondetTAlternative = function (dictMonad) {
    var nondetTApplicative1 = nondetTApplicative(dictMonad);
    var nondetTPlus1 = nondetTPlus(dictMonad);
    return {
        Applicative0: function () {
            return nondetTApplicative1;
        },
        Plus1: function () {
            return nondetTPlus1;
        }
    };
};
var nondetTBind = function (dictMonad) {
    var bind2 = Control_Bind.bind(listTBind(Control_Monad_State_Trans.monadStateT(dictMonad)));
    var nondetTApply1 = nondetTApply(dictMonad);
    return {
        bind: function (v) {
            return function (f) {
                return bind2(v)(function ($318) {
                    return runNondetT(f($318));
                });
            };
        },
        Apply0: function () {
            return nondetTApply1;
        }
    };
};
var nondetTMonad = function (dictMonad) {
    var nondetTApplicative1 = nondetTApplicative(dictMonad);
    var nondetTBind1 = nondetTBind(dictMonad);
    return {
        Applicative0: function () {
            return nondetTApplicative1;
        },
        Bind1: function () {
            return nondetTBind1;
        }
    };
};
var nondetTMonadPlus = function (dictMonad) {
    var nondetTMonad1 = nondetTMonad(dictMonad);
    var nondetTAlternative1 = nondetTAlternative(dictMonad);
    return {
        Monad0: function () {
            return nondetTMonad1;
        },
        Alternative1: function () {
            return nondetTAlternative1;
        }
    };
};
var takeListT = function (dictMonad) {
    var empty = Control_Plus.empty(listTPlus(dictMonad));
    var liftM1 = Control_Monad.liftM1(dictMonad);
    return function (v) {
        if (v === 0) {
            return Data_Function["const"](empty);
        };
        var $319 = liftM1(bimapTStep(identity)(takeListT(dictMonad)(v - 1 | 0)));
        return function ($320) {
            return ListT($319(stepListT($320)));
        };
    };
};
var disamb = function (dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var bind2 = Control_Bind.bind(Bind1);
    var evalStateT = Control_Monad_State_Trans.evalStateT((Bind1.Apply0()).Functor0());
    var monadStateT1 = Control_Monad_State_Trans.monadStateT(dictMonad);
    var runListT1 = runListT(monadStateT1);
    var takeListT1 = takeListT(monadStateT1);
    var pure4 = Control_Applicative.pure(dictMonad.Applicative0());
    return function (allow_amb) {
        return function (xs) {
            return bind2((function (v) {
                return evalStateT(v)(false);
            })(runListT1(takeListT1((function () {
                if (allow_amb) {
                    return 1;
                };
                return 2;
            })())(runNondetT(xs)))))(function (xs$prime) {
                return pure4((function () {
                    if (xs$prime instanceof Data_List_Types.Cons && xs$prime.value1 instanceof Data_List_Types.Nil) {
                        return new Data_Maybe.Just(xs$prime.value0);
                    };
                    return Data_Maybe.Nothing.value;
                })());
            });
        };
    };
};
export {
    enterContext,
    exitContext,
    getPrefs,
    missingArgP,
    errorP,
    exitP,
    hoistMaybe,
    hoistEither,
    runReadM,
    withReadM,
    runP,
    runCompletion,
    contextNames,
    takeListT,
    runListT,
    cut,
    nondetTAltOp,
    disamb,
    pFunctor,
    pApply,
    pApplicative,
    pAlt,
    pBind,
    pMonad,
    pMonadP,
    completionFunctor,
    completionApply,
    completionApplicative,
    completionAlt,
    completionBind,
    completionMonad,
    completionMonadP,
    listTFunctor,
    listTApply,
    listTApplicative,
    listTBind,
    listTMonad,
    listTAlt,
    listTPlus,
    listTAlternative,
    listTMonadTrans,
    listTMonadPlus,
    nondetTFunctor,
    nondetTApply,
    nondetTApplicative,
    nondetTBind,
    nondetTMonad,
    nondetTMonadPlus,
    nondetTAlt,
    nondetTPlus,
    nondetTAlternative,
    nondetTMonadTrans
};
export {
    ErrorMsg,
    ExpectsArgError,
    InfoMsg,
    MissingError,
    ShowHelpText,
    UnexpectedError
} from "../Options.Applicative.Types/index.js";
