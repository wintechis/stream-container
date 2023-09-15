// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Ref from "../Effect.Ref/index.js";
import * as HTTPure_Headers from "../HTTPure.Headers/index.js";
import * as Node_Buffer from "../Node.Buffer/index.js";
import * as Node_Buffer_Class from "../Node.Buffer.Class/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
import * as Node_HTTP from "../Node.HTTP/index.js";
import * as Node_Stream from "../Node.Stream/index.js";
import * as Type_Equality from "../Type.Equality/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Effect.bindEffect);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit);
var $$void = /* #__PURE__ */ Data_Functor["void"](Effect.functorEffect);
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var concat = /* #__PURE__ */ Node_Buffer_Class.concat(Node_Buffer.mutableBufferEffect);
var pure = /* #__PURE__ */ Control_Applicative.pure(Effect.applicativeEffect);
var bind1 = /* #__PURE__ */ Control_Bind.bind(Effect_Aff.bindAff);
var liftEffect = /* #__PURE__ */ Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var discard2 = /* #__PURE__ */ discard(Effect_Aff.bindAff);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Effect_Aff.applicativeAff);
var toString1 = /* #__PURE__ */ Node_Buffer_Class.toString(Node_Buffer.mutableBufferEffect);
var map = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorFn);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var size = /* #__PURE__ */ Node_Buffer_Class.size(Node_Buffer.mutableBufferEffect);
var fromString = /* #__PURE__ */ Node_Buffer_Class.fromString(Node_Buffer.mutableBufferEffect);
var write = function (dict) {
    return dict.write;
};
var toStream = function (v) {
    return v.stream;
};
var toBuffer = function (requestBody) {
    var streamToBuffer = function (stream) {
        return Effect_Aff.makeAff(function (done) {
            return function __do() {
                var bufs = Effect_Ref["new"]([  ])();
                Node_Stream.onData(stream)(function (buf) {
                    return $$void(Effect_Ref.modify(function (v) {
                        return append(v)([ buf ]);
                    })(bufs));
                })();
                Node_Stream.onEnd(stream)(function __do() {
                    var body = bind(Effect_Ref.read(bufs))(concat)();
                    return done(new Data_Either.Right(body))();
                })();
                return Effect_Aff.nonCanceler;
            };
        });
    };
    return bind1(liftEffect(Effect_Ref.read(requestBody.buffer)))(function (maybeBuffer) {
        if (maybeBuffer instanceof Data_Maybe.Nothing) {
            return bind1(streamToBuffer(requestBody.stream))(function (buffer) {
                return discard2(liftEffect(Effect_Ref.write(new Data_Maybe.Just(buffer))(requestBody.buffer)))(function () {
                    return pure1(buffer);
                });
            });
        };
        if (maybeBuffer instanceof Data_Maybe.Just) {
            return pure1(maybeBuffer.value0);
        };
        throw new Error("Failed pattern match at HTTPure.Body (line 75, column 3 - line 81, column 31): " + [ maybeBuffer.constructor.name ]);
    });
};
var toString = function (requestBody) {
    return bind1(liftEffect(Effect_Ref.read(requestBody.string)))(function (maybeString) {
        if (maybeString instanceof Data_Maybe.Nothing) {
            return bind1(toBuffer(requestBody))(function (buffer) {
                return bind1(liftEffect(toString1(Node_Encoding.UTF8.value)(buffer)))(function (string) {
                    return discard2(liftEffect(Effect_Ref.write(new Data_Maybe.Just(string))(requestBody.string)))(function () {
                        return pure1(string);
                    });
                });
            });
        };
        if (maybeString instanceof Data_Maybe.Just) {
            return pure1(maybeString.value0);
        };
        throw new Error("Failed pattern match at HTTPure.Body (line 56, column 3 - line 64, column 31): " + [ maybeString.constructor.name ]);
    });
};
var read = function (request) {
    return function __do() {
        var buffer = Effect_Ref["new"](Data_Maybe.Nothing.value)();
        var string = Effect_Ref["new"](Data_Maybe.Nothing.value)();
        return {
            buffer: buffer,
            stream: Node_HTTP.requestAsStream(request),
            string: string
        };
    };
};
var defaultHeaders = function (dict) {
    return dict.defaultHeaders;
};
var bodyChunked = function (dictTypeEquals) {
    var to = Type_Equality.to(dictTypeEquals);
    return {
        defaultHeaders: function (v) {
            return pure(HTTPure_Headers.header("Transfer-Encoding")("chunked"));
        },
        write: function (body) {
            return function (response) {
                return Effect_Aff.makeAff(function (done) {
                    var stream = to(body);
                    return function __do() {
                        $$void(Node_Stream.pipe(stream)(Node_HTTP.responseAsStream(response)))();
                        Node_Stream.onEnd(stream)(done(new Data_Either.Right(Data_Unit.unit)))();
                        return Effect_Aff.nonCanceler;
                    };
                });
            };
        }
    };
};
var bodyBuffer = {
    defaultHeaders: function (buf) {
        return map(map1(HTTPure_Headers.header("Content-Length"))(show))(size(buf));
    },
    write: function (body) {
        return function (response) {
            return Effect_Aff.makeAff(function (done) {
                var stream = Node_HTTP.responseAsStream(response);
                return function __do() {
                    $$void(Node_Stream.write(stream)(body)(Data_Function["const"](Node_Stream.end(stream)(Data_Function["const"](done(new Data_Either.Right(Data_Unit.unit)))))))();
                    return Effect_Aff.nonCanceler;
                };
            });
        };
    }
};
var defaultHeaders1 = /* #__PURE__ */ defaultHeaders(bodyBuffer);
var bodyString = {
    defaultHeaders: function (body) {
        return function __do() {
            var v = fromString(body)(Node_Encoding.UTF8.value)();
            return defaultHeaders1(v)();
        };
    },
    write: function (body) {
        return function (response) {
            return Effect_Aff.makeAff(function (done) {
                var stream = Node_HTTP.responseAsStream(response);
                return function __do() {
                    $$void(Node_Stream.writeString(stream)(Node_Encoding.UTF8.value)(body)(Data_Function["const"](Node_Stream.end(stream)(Data_Function["const"](done(new Data_Either.Right(Data_Unit.unit)))))))();
                    return Effect_Aff.nonCanceler;
                };
            });
        };
    }
};
export {
    defaultHeaders,
    write,
    read,
    toBuffer,
    toStream,
    toString,
    bodyString,
    bodyBuffer,
    bodyChunked
};
