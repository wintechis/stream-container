// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Except from "../Control.Monad.Except/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Data_Options from "../Data.Options/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Effect from "../Effect/index.js";
import * as Foreign from "../Foreign/index.js";
import * as Foreign_Index from "../Foreign.Index/index.js";
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Options.semigroupOptions);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Effect.monoidEffect(Data_Monoid.monoidUnit));
var apply = /* #__PURE__ */ Control_Apply.apply(/* #__PURE__ */ Control_Monad_Except_Trans.applyExceptT(Data_Identity.monadIdentity));
var map = /* #__PURE__ */ Data_Functor.map(/* #__PURE__ */ Control_Monad_Except_Trans.functorExceptT(Data_Identity.functorIdentity));
var bind = /* #__PURE__ */ Control_Bind.bind(/* #__PURE__ */ Control_Monad_Except_Trans.bindExceptT(Data_Identity.monadIdentity));
var readProp = /* #__PURE__ */ Foreign_Index.readProp(Data_Identity.monadIdentity);
var readString = /* #__PURE__ */ Foreign.readString(Data_Identity.monadIdentity);
var readInt = /* #__PURE__ */ Foreign.readInt(Data_Identity.monadIdentity);
var alt = /* #__PURE__ */ Control_Alt.alt(Data_Maybe.altMaybe);
var bind2 = /* #__PURE__ */ Control_Bind.bind(Data_Maybe.bindMaybe);
var serverPauseOnConnect = /* #__PURE__ */ Data_Options.opt("pauseOnConnect");
var serverAllowHalfOpen = /* #__PURE__ */ Data_Options.opt("allowHalfOpen");
var onListening = function (server) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("listening", server, callback);
        };
    };
};
var onError = function (server) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("error", server, function (error) {
                return callback(error)();
            });
        };
    };
};
var onConnection = function (server) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("connection", server, function (socket) {
                return callback(socket)();
            });
        };
    };
};
var onClose = function (server) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("close", server, callback);
        };
    };
};
var listening = function (server) {
    return function () {
        return $foreign.listeningImpl(server);
    };
};
var listenWritableAll = /* #__PURE__ */ Data_Options.opt("writableAll");
var listenReadableAll = /* #__PURE__ */ Data_Options.opt("readableAll");
var listenPort = /* #__PURE__ */ Data_Options.opt("port");
var listenPath = /* #__PURE__ */ Data_Options.opt("path");
var listenIpv6Only = /* #__PURE__ */ Data_Options.opt("ipv6Only");
var listenHost = /* #__PURE__ */ Data_Options.opt("host");
var listenExclusive = /* #__PURE__ */ Data_Options.opt("exclusive");
var listenBacklog = /* #__PURE__ */ Data_Options.opt("backlog");
var listenICP = function (server) {
    return function (path) {
        return function (backlog) {
            return function (callback) {
                return function () {
                    return $foreign.listenImpl(server, Data_Options.options(append(Data_Options.assoc(listenBacklog)(backlog))(Data_Options.assoc(listenPath)(path))), callback);
                };
            };
        };
    };
};
var listenTCP = function (server) {
    return function (port) {
        return function (host) {
            return function (backlog) {
                return function (callback) {
                    return function () {
                        return $foreign.listenImpl(server, Data_Options.options(append(Data_Options.assoc(listenBacklog)(backlog))(append(Data_Options.assoc(listenHost)(host))(Data_Options.assoc(listenPort)(port)))), callback);
                    };
                };
            };
        };
    };
};
var listen = function (server) {
    return function (opts) {
        return function (callback) {
            return function () {
                return $foreign.listenImpl(server, Data_Options.options(opts), callback);
            };
        };
    };
};
var getConnections = function (server) {
    return function (callback) {
        return function () {
            return $foreign.getConnectionsImpl(server, function (err$prime, count$prime) {
                var v = Data_Nullable.toMaybe(count$prime);
                var v1 = Data_Nullable.toMaybe(err$prime);
                if (v1 instanceof Data_Maybe.Just) {
                    return callback(new Data_Either.Left(v1.value0))();
                };
                if (v instanceof Data_Maybe.Just) {
                    return callback(new Data_Either.Right(v.value0))();
                };
                return mempty();
            });
        };
    };
};
var createServer = function (opts) {
    return function (callback) {
        return function () {
            return $foreign.createServerImpl(Data_Options.options(opts), function (socket) {
                return callback(socket)();
            });
        };
    };
};
var close = function (server) {
    return function (callback) {
        return function () {
            return $foreign.closeImpl(server, function (err) {
                return callback(Data_Nullable.toMaybe(err))();
            });
        };
    };
};
var address = function (server) {
    var readAddress = function (value) {
        return apply(apply(map(function (v) {
            return function (v1) {
                return function (v2) {
                    return {
                        address: v,
                        family: v1,
                        port: v2
                    };
                };
            };
        })(bind(readProp("address")(value))(readString)))(bind(readProp("family")(value))(readString)))(bind(readProp("port")(value))(readInt));
    };
    var hush = function (f) {
        return Data_Either.either(function (v) {
            return Data_Maybe.Nothing.value;
        })(Data_Maybe.Just.create)(Control_Monad_Except.runExcept(f));
    };
    var read = function (value) {
        return alt(hush(map(Data_Either.Left.create)(readAddress(value))))(hush(map(Data_Either.Right.create)(readString(value))));
    };
    return function __do() {
        var x = $foreign.addressImpl(server);
        return bind2(Data_Nullable.toMaybe(x))(read);
    };
};
export {
    address,
    close,
    createServer,
    getConnections,
    listen,
    listenBacklog,
    listenExclusive,
    listenHost,
    listenICP,
    listenIpv6Only,
    listenPath,
    listenPort,
    listenReadableAll,
    listenTCP,
    listenWritableAll,
    listening,
    onClose,
    onConnection,
    onError,
    onListening,
    serverAllowHalfOpen,
    serverPauseOnConnect
};
