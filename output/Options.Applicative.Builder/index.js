// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Number from "../Data.Number/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as ExitCodes from "../ExitCodes/index.js";
import * as Options_Applicative_Builder_Completer from "../Options.Applicative.Builder.Completer/index.js";
import * as Options_Applicative_Builder_Internal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options_Applicative_Help_Chunk from "../Options.Applicative.Help.Chunk/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
import * as Text_PrettyPrint_Leijen from "../Text.PrettyPrint.Leijen/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var over = /* #__PURE__ */ Data_Newtype.over()();
var un = /* #__PURE__ */ Data_Newtype.un();
var append = /* #__PURE__ */ Data_Semigroup.append(Options_Applicative_Builder_Internal.modSemigroup);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Options_Applicative_Types.completerMonoid);
var bind = /* #__PURE__ */ Control_Bind.bind(Options_Applicative_Types.readMBind);
var pure = /* #__PURE__ */ Control_Applicative.pure(Options_Applicative_Types.readMApplicative);
var mempty1 = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Options_Applicative_Help_Chunk.chunkMonoid(Text_PrettyPrint_Leijen.docSemigroup));
var min = /* #__PURE__ */ Data_Ord.min(Options_Applicative_Types.optVisibilityOrd);
var alt = /* #__PURE__ */ Control_Alt.alt(Options_Applicative_Types.parserAlt);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Options_Applicative_Types.parserApplicative);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var append2 = /* #__PURE__ */ Data_Semigroup.append(Options_Applicative_Types.completerSemigroup);
var append3 = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var mempty2 = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Data_Monoid.monoidRecord()(/* #__PURE__ */ Data_Monoid.monoidRecordCons({
    reflectSymbol: function () {
        return "argCompleter";
    }
})(Options_Applicative_Types.completerMonoid)()(Data_Monoid.monoidRecordNil)));
var fold = /* #__PURE__ */ Data_Foldable.fold(Data_Foldable.foldableArray)(Options_Applicative_Builder_Internal.modMonoid);
var PrefsMod = function (x) {
    return x;
};
var InfoMod = function (x) {
    return x;
};
var value = function (dictHasValue) {
    return function (x) {
        return new Options_Applicative_Builder_Internal.Mod(identity, new Options_Applicative_Builder_Internal.DefaultProp(new Data_Maybe.Just(x), Data_Maybe.Nothing.value), identity);
    };
};
var value1 = /* #__PURE__ */ value(Options_Applicative_Builder_Internal.optionFieldsHasValue);
var subparserInline = /* #__PURE__ */ over(Options_Applicative_Types.ParserPrefs)(function (p) {
    return {
        prefBacktrack: Options_Applicative_Types.SubparserInline.value,
        prefColumns: p.prefColumns,
        prefDisambiguate: p.prefDisambiguate,
        prefMultiSuffix: p.prefMultiSuffix,
        prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
        prefShowHelpOnError: p.prefShowHelpOnError
    };
});
var style = function (x) {
    return Options_Applicative_Builder_Internal.optionMod(over(Options_Applicative_Types.OptProperties)(function (p) {
        return {
            propDescMod: new Data_Maybe.Just(x),
            propHelp: p.propHelp,
            propMetaVar: p.propMetaVar,
            propShowDefault: p.propShowDefault,
            propVisibility: p.propVisibility
        };
    }));
};
var str = Options_Applicative_Types.readerAsk;
var showHelpOnError = /* #__PURE__ */ over(Options_Applicative_Types.ParserPrefs)(function (p) {
    return {
        prefShowHelpOnError: true,
        prefBacktrack: p.prefBacktrack,
        prefColumns: p.prefColumns,
        prefDisambiguate: p.prefDisambiguate,
        prefMultiSuffix: p.prefMultiSuffix,
        prefShowHelpOnEmpty: p.prefShowHelpOnEmpty
    };
});
var showHelpOnEmpty = /* #__PURE__ */ over(Options_Applicative_Types.ParserPrefs)(function (p) {
    return {
        prefShowHelpOnEmpty: true,
        prefBacktrack: p.prefBacktrack,
        prefColumns: p.prefColumns,
        prefDisambiguate: p.prefDisambiguate,
        prefMultiSuffix: p.prefMultiSuffix,
        prefShowHelpOnError: p.prefShowHelpOnError
    };
});
var showDefaultWith = function (s) {
    return new Options_Applicative_Builder_Internal.Mod(identity, new Options_Applicative_Builder_Internal.DefaultProp(Data_Maybe.Nothing.value, new Data_Maybe.Just(s)), identity);
};
var showDefault = function (dictShow) {
    return showDefaultWith(Data_Show.show(dictShow));
};
var $$short = function (dictHasName) {
    var $121 = Options_Applicative_Builder_Internal.name(dictHasName);
    return function ($122) {
        return Options_Applicative_Builder_Internal.fieldMod($121(Options_Applicative_Types.OptShort.create($122)));
    };
};
var progDescDoc = function (doc) {
    return over(Options_Applicative_Types.ParserInfo)(function (i) {
        return {
            infoProgDesc: doc,
            infoFailureCode: i.infoFailureCode,
            infoFooter: i.infoFooter,
            infoFullDesc: i.infoFullDesc,
            infoHeader: i.infoHeader,
            infoParser: i.infoParser,
            infoPolicy: i.infoPolicy
        };
    });
};
var progDesc = function (s) {
    return over(Options_Applicative_Types.ParserInfo)(function (i) {
        return {
            infoProgDesc: Options_Applicative_Help_Chunk.paragraph(s),
            infoFailureCode: i.infoFailureCode,
            infoFooter: i.infoFooter,
            infoFullDesc: i.infoFullDesc,
            infoHeader: i.infoHeader,
            infoParser: i.infoParser,
            infoPolicy: i.infoPolicy
        };
    });
};
var noIntersperse = /* #__PURE__ */ over(Options_Applicative_Types.ParserInfo)(function (p) {
    return {
        infoPolicy: Options_Applicative_Types.NoIntersperse.value,
        infoFailureCode: p.infoFailureCode,
        infoFooter: p.infoFooter,
        infoFullDesc: p.infoFullDesc,
        infoHeader: p.infoHeader,
        infoParser: p.infoParser,
        infoProgDesc: p.infoProgDesc
    };
});
var noBacktrack = /* #__PURE__ */ over(Options_Applicative_Types.ParserPrefs)(function (p) {
    return {
        prefBacktrack: Options_Applicative_Types.NoBacktrack.value,
        prefColumns: p.prefColumns,
        prefDisambiguate: p.prefDisambiguate,
        prefMultiSuffix: p.prefMultiSuffix,
        prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
        prefShowHelpOnError: p.prefShowHelpOnError
    };
});
var noArgError = function (e) {
    return Options_Applicative_Builder_Internal.fieldMod(over(Options_Applicative_Builder_Internal.OptionFields)(function (p) {
        return {
            optNoArgError: Data_Function["const"](e),
            optCompleter: p.optCompleter,
            optNames: p.optNames
        };
    }));
};
var newtypePrefsMod = {
    Coercible0: function () {
        return undefined;
    }
};
var prefs = function (m) {
    var base = {
        prefMultiSuffix: "",
        prefDisambiguate: false,
        prefShowHelpOnError: false,
        prefShowHelpOnEmpty: false,
        prefBacktrack: Options_Applicative_Types.Backtrack.value,
        prefColumns: 80
    };
    return un(PrefsMod)(m)(base);
};
var prefsModSemigroup = {
    append: function (m1) {
        return function (m2) {
            var $123 = un(PrefsMod)(m2);
            var $124 = un(PrefsMod)(m1);
            return function ($125) {
                return $123($124($125));
            };
        };
    }
};
var prefsModMonoid = {
    mempty: identity,
    Semigroup0: function () {
        return prefsModSemigroup;
    }
};
var newtypeInfoMod = {
    Coercible0: function () {
        return undefined;
    }
};
var multiSuffix = function (s) {
    return over(Options_Applicative_Types.ParserPrefs)(function (p) {
        return {
            prefMultiSuffix: s,
            prefBacktrack: p.prefBacktrack,
            prefColumns: p.prefColumns,
            prefDisambiguate: p.prefDisambiguate,
            prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
            prefShowHelpOnError: p.prefShowHelpOnError
        };
    });
};
var metavar = function (dictHasMetavar) {
    return function ($$var) {
        return Options_Applicative_Builder_Internal.optionMod(over(Options_Applicative_Types.OptProperties)(function (p) {
            return {
                propMetaVar: $$var,
                propDescMod: p.propDescMod,
                propHelp: p.propHelp,
                propShowDefault: p.propShowDefault,
                propVisibility: p.propVisibility
            };
        }));
    };
};
var metavar1 = /* #__PURE__ */ metavar(Options_Applicative_Builder_Internal.optionFieldsHasMetavar);
var metavar2 = /* #__PURE__ */ metavar(Options_Applicative_Builder_Internal.commandFieldsHasMetavar);
var option = function (r) {
    return function (m) {
        var v = append(metavar1("ARG"))(m);
        var v1 = v.value0({
            optNames: [  ],
            optCompleter: mempty,
            optNoArgError: Options_Applicative_Types.ExpectsArgError.create
        });
        var crdr = {
            crCompleter: v1.optCompleter,
            crReader: r
        };
        var rdr = new Options_Applicative_Types.OptReader(v1.optNames, crdr, v1.optNoArgError);
        return Options_Applicative_Builder_Internal.mkParser(v.value1)(v.value2)(rdr);
    };
};
var strOption = /* #__PURE__ */ option(str);
var subparser = function (m) {
    var v = append(metavar2("COMMAND"))(m);
    var v1 = Options_Applicative_Builder_Internal.mkCommand(m);
    var rdr = new Options_Applicative_Types.CmdReader(v1.value0, v1.value1.value0, v1.value1.value1.value0);
    return Options_Applicative_Builder_Internal.mkParser(v.value1)(v.value2)(rdr);
};
var maybeReader = function (f) {
    return bind(Options_Applicative_Types.readerAsk)(function (arg) {
        return Data_Maybe.maybe(Options_Applicative_Types.readerError("cannot parse value `" + (arg + "'")))(pure)(f(arg));
    });
};
var $$long = function (dictHasName) {
    var $126 = Options_Applicative_Builder_Internal.name(dictHasName);
    return function ($127) {
        return Options_Applicative_Builder_Internal.fieldMod($126(Options_Applicative_Types.OptLong.create($127)));
    };
};
var infoModSemigroup = {
    append: function (m1) {
        return function (m2) {
            var $128 = un(InfoMod)(m2);
            var $129 = un(InfoMod)(m1);
            return function ($130) {
                return $128($129($130));
            };
        };
    }
};
var infoModMonoid = {
    mempty: identity,
    Semigroup0: function () {
        return infoModSemigroup;
    }
};
var info = function (parser) {
    return function (m) {
        var base = {
            infoParser: parser,
            infoFullDesc: true,
            infoProgDesc: mempty1,
            infoHeader: mempty1,
            infoFooter: mempty1,
            infoFailureCode: ExitCodes["Error"].value,
            infoPolicy: Options_Applicative_Types.Intersperse.value
        };
        return un(InfoMod)(m)(base);
    };
};
var idm = function (dictMonoid) {
    return Data_Monoid.mempty(dictMonoid);
};
var hidden = /* #__PURE__ */ Options_Applicative_Builder_Internal.optionMod(/* #__PURE__ */ over(Options_Applicative_Types.OptProperties)(function (p) {
    return {
        propVisibility: min(Options_Applicative_Types.Hidden.value)(p.propVisibility),
        propDescMod: p.propDescMod,
        propHelp: p.propHelp,
        propMetaVar: p.propMetaVar,
        propShowDefault: p.propShowDefault
    };
}));
var helpDoc = function (doc) {
    return Options_Applicative_Builder_Internal.optionMod(over(Options_Applicative_Types.OptProperties)(function (p) {
        return {
            propHelp: doc,
            propDescMod: p.propDescMod,
            propMetaVar: p.propMetaVar,
            propShowDefault: p.propShowDefault,
            propVisibility: p.propVisibility
        };
    }));
};
var help = function (s) {
    return Options_Applicative_Builder_Internal.optionMod(over(Options_Applicative_Types.OptProperties)(function (p) {
        return {
            propHelp: Options_Applicative_Help_Chunk.paragraph(s),
            propDescMod: p.propDescMod,
            propMetaVar: p.propMetaVar,
            propShowDefault: p.propShowDefault,
            propVisibility: p.propVisibility
        };
    }));
};
var headerDoc = function (doc) {
    return over(Options_Applicative_Types.ParserInfo)(function (i) {
        return {
            infoHeader: doc,
            infoFailureCode: i.infoFailureCode,
            infoFooter: i.infoFooter,
            infoFullDesc: i.infoFullDesc,
            infoParser: i.infoParser,
            infoPolicy: i.infoPolicy,
            infoProgDesc: i.infoProgDesc
        };
    });
};
var header = function (s) {
    return over(Options_Applicative_Types.ParserInfo)(function (i) {
        return {
            infoHeader: Options_Applicative_Help_Chunk.paragraph(s),
            infoFailureCode: i.infoFailureCode,
            infoFooter: i.infoFooter,
            infoFullDesc: i.infoFullDesc,
            infoParser: i.infoParser,
            infoPolicy: i.infoPolicy,
            infoProgDesc: i.infoProgDesc
        };
    });
};
var fullDesc = /* #__PURE__ */ over(Options_Applicative_Types.ParserInfo)(function (i) {
    return {
        infoFullDesc: true,
        infoFailureCode: i.infoFailureCode,
        infoFooter: i.infoFooter,
        infoHeader: i.infoHeader,
        infoParser: i.infoParser,
        infoPolicy: i.infoPolicy,
        infoProgDesc: i.infoProgDesc
    };
});
var forwardOptions = /* #__PURE__ */ over(Options_Applicative_Types.ParserInfo)(function (p) {
    return {
        infoPolicy: Options_Applicative_Types.ForwardOptions.value,
        infoFailureCode: p.infoFailureCode,
        infoFooter: p.infoFooter,
        infoFullDesc: p.infoFullDesc,
        infoHeader: p.infoHeader,
        infoParser: p.infoParser,
        infoProgDesc: p.infoProgDesc
    };
});
var footerDoc = function (doc) {
    return over(Options_Applicative_Types.ParserInfo)(function (i) {
        return {
            infoFooter: doc,
            infoFailureCode: i.infoFailureCode,
            infoFullDesc: i.infoFullDesc,
            infoHeader: i.infoHeader,
            infoParser: i.infoParser,
            infoPolicy: i.infoPolicy,
            infoProgDesc: i.infoProgDesc
        };
    });
};
var footer = function (s) {
    return over(Options_Applicative_Types.ParserInfo)(function (i) {
        return {
            infoFooter: Options_Applicative_Help_Chunk.paragraph(s),
            infoFailureCode: i.infoFailureCode,
            infoFullDesc: i.infoFullDesc,
            infoHeader: i.infoHeader,
            infoParser: i.infoParser,
            infoPolicy: i.infoPolicy,
            infoProgDesc: i.infoProgDesc
        };
    });
};
var flag$prime = function (actv) {
    return function (v) {
        var rdr = (function () {
            var v1 = v.value0({
                flagNames: [  ],
                flagActive: actv
            });
            return new Options_Applicative_Types.FlagReader(v1.flagNames, v1.flagActive);
        })();
        return Options_Applicative_Builder_Internal.mkParser(v.value1)(v.value2)(rdr);
    };
};
var flag = function (defv) {
    return function (actv) {
        return function (m) {
            return alt(flag$prime(actv)(m))(pure1(defv));
        };
    };
};
var $$switch = /* #__PURE__ */ flag(false)(true);
var failureCode = function (n) {
    return over(Options_Applicative_Types.ParserInfo)(function (i) {
        return {
            infoFailureCode: n,
            infoFooter: i.infoFooter,
            infoFullDesc: i.infoFullDesc,
            infoHeader: i.infoHeader,
            infoParser: i.infoParser,
            infoPolicy: i.infoPolicy,
            infoProgDesc: i.infoProgDesc
        };
    });
};
var eitherReader = function (f) {
    return bind(Options_Applicative_Types.readerAsk)((function () {
        var $131 = Data_Either.either(Options_Applicative_Types.readerError)(pure);
        return function ($132) {
            return $131(f($132));
        };
    })());
};
var $$int = /* #__PURE__ */ eitherReader(function (s) {
    var v = Data_Int.fromString(s);
    if (v instanceof Data_Maybe.Nothing) {
        return new Data_Either.Left("Can't parse as Int: `" + (show(s) + "`"));
    };
    if (v instanceof Data_Maybe.Just) {
        return new Data_Either.Right(v.value0);
    };
    throw new Error("Failed pattern match at Options.Applicative.Builder (line 124, column 28 - line 126, column 20): " + [ v.constructor.name ]);
});
var number = /* #__PURE__ */ eitherReader(function (s) {
    var v = Data_Number.fromString(s);
    if (v instanceof Data_Maybe.Nothing) {
        return new Data_Either.Left("Can't parse as Number: `" + (show(s) + "`"));
    };
    if (v instanceof Data_Maybe.Just) {
        return new Data_Either.Right(v.value0);
    };
    throw new Error("Failed pattern match at Options.Applicative.Builder (line 130, column 31 - line 132, column 20): " + [ v.constructor.name ]);
});
var disambiguate = /* #__PURE__ */ over(Options_Applicative_Types.ParserPrefs)(function (p) {
    return {
        prefDisambiguate: true,
        prefBacktrack: p.prefBacktrack,
        prefColumns: p.prefColumns,
        prefMultiSuffix: p.prefMultiSuffix,
        prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
        prefShowHelpOnError: p.prefShowHelpOnError
    };
});
var disabled = /* #__PURE__ */ Options_Applicative_Types.readerError("disabled option");
var defaultPrefs = /* #__PURE__ */ prefs(/* #__PURE__ */ idm(prefsModMonoid));
var completer = function (dictHasCompleter) {
    var modCompleter = Options_Applicative_Builder_Internal.modCompleter(dictHasCompleter);
    return function (f) {
        return Options_Applicative_Builder_Internal.fieldMod(modCompleter(function (v) {
            return append2(v)(f);
        }));
    };
};
var completeWith = function (dictHasCompleter) {
    var $133 = completer(dictHasCompleter);
    return function ($134) {
        return $133(Options_Applicative_Builder_Completer.listCompleter($134));
    };
};
var commandGroup = function (g) {
    return Options_Applicative_Builder_Internal.fieldMod(over(Options_Applicative_Builder_Internal.CommandFields)(function (p) {
        return {
            cmdGroup: new Data_Maybe.Just(g),
            cmdCommands: p.cmdCommands
        };
    }));
};
var command = function (cmd) {
    return function (pinfo) {
        return Options_Applicative_Builder_Internal.fieldMod(over(Options_Applicative_Builder_Internal.CommandFields)(function (p) {
            return {
                cmdCommands: append3([ new Data_Tuple.Tuple(cmd, pinfo) ])(p.cmdCommands),
                cmdGroup: p.cmdGroup
            };
        }));
    };
};
var columns = function (cols) {
    return over(Options_Applicative_Types.ParserPrefs)(function (p) {
        return {
            prefColumns: cols,
            prefBacktrack: p.prefBacktrack,
            prefDisambiguate: p.prefDisambiguate,
            prefMultiSuffix: p.prefMultiSuffix,
            prefShowHelpOnEmpty: p.prefShowHelpOnEmpty,
            prefShowHelpOnError: p.prefShowHelpOnError
        };
    });
};
var briefDesc = /* #__PURE__ */ over(Options_Applicative_Types.ParserInfo)(function (i) {
    return {
        infoFullDesc: false,
        infoFailureCode: i.infoFailureCode,
        infoFooter: i.infoFooter,
        infoHeader: i.infoHeader,
        infoParser: i.infoParser,
        infoPolicy: i.infoPolicy,
        infoProgDesc: i.infoProgDesc
    };
});
var $$boolean = /* #__PURE__ */ eitherReader(function ($135) {
    return (function (v) {
        if (v === "true") {
            return new Data_Either.Right(true);
        };
        if (v === "false") {
            return new Data_Either.Right(false);
        };
        return new Data_Either.Left("Can't parse as Boolean: `" + (show(v) + "`"));
    })(Data_String_Common.toLower($135));
});
var argument = function (p) {
    return function (v) {
        var v1 = v.value0(mempty2);
        var rdr = {
            crCompleter: v1.argCompleter,
            crReader: p
        };
        return Options_Applicative_Builder_Internal.mkParser(v.value1)(v.value2)(new Options_Applicative_Types.ArgReader(rdr));
    };
};
var strArgument = /* #__PURE__ */ argument(str);
var action = function (dictHasCompleter) {
    var $136 = completer(dictHasCompleter);
    return function ($137) {
        return $136(Options_Applicative_Builder_Completer.bashCompleter($137));
    };
};
var abortOption = function (err) {
    return function (m) {
        return option(Options_Applicative_Types.readerAbort(err))((function (v) {
            return append(v)(m);
        })(fold([ noArgError(err), value1(identity), metavar1("") ])));
    };
};
var infoOption = function ($138) {
    return abortOption(Options_Applicative_Types.InfoMsg.create($138));
};
export {
    subparser,
    strArgument,
    argument,
    flag,
    flag$prime,
    $$switch as switch,
    abortOption,
    infoOption,
    strOption,
    option,
    $$short as short,
    $$long as long,
    help,
    helpDoc,
    value,
    showDefaultWith,
    showDefault,
    metavar,
    noArgError,
    hidden,
    style,
    command,
    commandGroup,
    completeWith,
    action,
    completer,
    idm,
    str,
    $$int as int,
    number,
    $$boolean as boolean,
    maybeReader,
    eitherReader,
    disabled,
    InfoMod,
    fullDesc,
    briefDesc,
    header,
    headerDoc,
    footer,
    footerDoc,
    progDesc,
    progDescDoc,
    failureCode,
    noIntersperse,
    forwardOptions,
    info,
    PrefsMod,
    multiSuffix,
    disambiguate,
    showHelpOnError,
    showHelpOnEmpty,
    noBacktrack,
    subparserInline,
    columns,
    prefs,
    defaultPrefs,
    newtypeInfoMod,
    infoModMonoid,
    infoModSemigroup,
    newtypePrefsMod,
    prefsModMonoid,
    prefsModSemigroup
};
export {
    internal
} from "../Options.Applicative.Builder.Internal/index.js";
export {
    ErrorMsg,
    ExpectsArgError,
    InfoMsg,
    MissingError,
    ShowHelpText,
    UnexpectedError,
    readerAbort,
    readerError
} from "../Options.Applicative.Types/index.js";
