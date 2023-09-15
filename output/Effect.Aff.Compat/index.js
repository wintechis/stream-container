// Generated by purs version 0.15.10
import * as Data_Either from "../Data.Either/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Uncurried from "../Effect.Uncurried/index.js";
var EffectFnCanceler = function (x) {
    return x;
};
var EffectFnAff = function (x) {
    return x;
};
var fromEffectFnAff = function (v) {
    return Effect_Aff.makeAff(function (k) {
        return function __do() {
            var v1 = v(function ($9) {
                return k(Data_Either.Left.create($9))();
            }, function ($10) {
                return k(Data_Either.Right.create($10))();
            });
            return function (e) {
                return Effect_Aff.makeAff(function (k2) {
                    return function __do() {
                        v1(e, function ($11) {
                            return k2(Data_Either.Left.create($11))();
                        }, function ($12) {
                            return k2(Data_Either.Right.create($12))();
                        });
                        return Effect_Aff.nonCanceler;
                    };
                });
            };
        };
    });
};
export {
    EffectFnAff,
    EffectFnCanceler,
    fromEffectFnAff
};
export {
    mkEffectFn1,
    mkEffectFn2,
    mkEffectFn3,
    runEffectFn1,
    runEffectFn2,
    runEffectFn3
} from "../Effect.Uncurried/index.js";
