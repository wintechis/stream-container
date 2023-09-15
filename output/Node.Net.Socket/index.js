// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Data_Options from "../Data.Options/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Effect from "../Effect/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
var mempty = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Effect.monoidEffect(Data_Monoid.monoidUnit));
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Options.semigroupOptions);
var writeString = function (socket) {
    return function (str) {
        return function (encoding) {
            return function (callback) {
                return function () {
                    return $foreign.writeStringImpl(socket, str, Node_Encoding.encodingToNode(encoding), callback);
                };
            };
        };
    };
};
var write = function (socket) {
    return function (buffer) {
        return function (callback) {
            return function () {
                return $foreign.writeImpl(socket, buffer, callback);
            };
        };
    };
};
var socketWritable = /* #__PURE__ */ Data_Options.opt("writable");
var socketTimeout = /* #__PURE__ */ Data_Options.opt("timeout");
var socketReadable = /* #__PURE__ */ Data_Options.opt("readable");
var socketPort = /* #__PURE__ */ Data_Options.opt("port");
var socketPath = /* #__PURE__ */ Data_Options.opt("path");
var socketHost = /* #__PURE__ */ Data_Options.opt("host");
var socketFd = /* #__PURE__ */ Data_Options.opt("fd");
var socketAllowHalfOpen = /* #__PURE__ */ Data_Options.opt("allowHalfOpen");
var setTimeout = function (socket) {
    return function (timeout) {
        return function (callback) {
            return function () {
                return $foreign.setTimeoutImpl(socket, timeout, callback);
            };
        };
    };
};
var setNoDelay = function (socket) {
    return function (noDelay) {
        return function () {
            return $foreign.setNoDelayImpl(socket, noDelay);
        };
    };
};
var setKeepAlive = function (socket) {
    return function (enable) {
        return function (initialDelay) {
            return function () {
                return $foreign.setKeepAliveImpl(socket, enable, initialDelay);
            };
        };
    };
};
var setEncoding = function (socket) {
    return function (encoding) {
        return function () {
            return $foreign.setEncodingImpl(socket, Node_Encoding.encodingToNode(encoding));
        };
    };
};
var resume = function (socket) {
    return function () {
        return $foreign.resumeImpl(socket);
    };
};
var remotePort = function (socket) {
    return function __do() {
        var port = $foreign.remotePortImpl(socket);
        return Data_Nullable.toMaybe(port);
    };
};
var remoteFamily = function (socket) {
    return function __do() {
        var family = $foreign.remoteFamilyImpl(socket);
        return Data_Nullable.toMaybe(family);
    };
};
var remoteAddress = function (socket) {
    return function __do() {
        var address$prime = $foreign.remoteAddressImpl(socket);
        return Data_Nullable.toMaybe(address$prime);
    };
};
var pending = function (socket) {
    return function () {
        return $foreign.pendingImpl(socket);
    };
};
var pause = function (socket) {
    return function () {
        return $foreign.pauseImpl(socket);
    };
};
var onTimeout = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("timeout", socket, callback);
        };
    };
};
var onReady = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("ready", socket, callback);
        };
    };
};
var onLookup = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("lookup", socket, function (err$prime, address$prime$prime, family$prime, host$prime) {
                var v = Data_Nullable.toMaybe(host$prime);
                var v1 = Data_Nullable.toMaybe(family$prime);
                var v2 = Data_Nullable.toMaybe(address$prime$prime);
                var v3 = Data_Nullable.toMaybe(err$prime);
                if (v3 instanceof Data_Maybe.Just) {
                    return callback(new Data_Either.Left(v3.value0))();
                };
                if (v3 instanceof Data_Maybe.Nothing && (v2 instanceof Data_Maybe.Just && (v1 instanceof Data_Maybe.Just && v instanceof Data_Maybe.Just))) {
                    return callback(new Data_Either.Right({
                        address: v2.value0,
                        family: Data_Nullable.toMaybe(v1.value0),
                        host: v.value0
                    }))();
                };
                return mempty();
            });
        };
    };
};
var onError = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("error", socket, function (err) {
                return callback(err)();
            });
        };
    };
};
var onEnd = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("end", socket, callback);
        };
    };
};
var onDrain = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("drain", socket, callback);
        };
    };
};
var onData = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onDataImpl(socket, function (buffer) {
                return callback(new Data_Either.Left(buffer))();
            }, function (str) {
                return callback(new Data_Either.Right(str))();
            });
        };
    };
};
var onConnect = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("connect", socket, callback);
        };
    };
};
var onClose = function (socket) {
    return function (callback) {
        return function () {
            return $foreign.onImpl("close", socket, function (hadError) {
                return callback(hadError)();
            });
        };
    };
};
var localPort = function (socket) {
    return function __do() {
        var port = $foreign.localPortImpl(socket);
        return Data_Nullable.toMaybe(port);
    };
};
var localAddress = function (socket) {
    return function __do() {
        var address$prime = $foreign.localAddressImpl(socket);
        return Data_Nullable.toMaybe(address$prime);
    };
};
var endString = function (socket) {
    return function (str) {
        return function (encoding) {
            return function (callback) {
                return function () {
                    return $foreign.endStringImpl(socket, str, Node_Encoding.encodingToNode(encoding), callback);
                };
            };
        };
    };
};
var end = function (socket) {
    return function (buffer) {
        return function (callback) {
            return function () {
                return $foreign.endImpl(socket, buffer, callback);
            };
        };
    };
};
var destroyed = function (socket) {
    return function () {
        return $foreign.destroyedImpl(socket);
    };
};
var destroy = function (socket) {
    return function (err) {
        return function () {
            return $foreign.destroyImpl(socket, Data_Nullable.toNullable(err));
        };
    };
};
var createConnectionTCP = function (port) {
    return function (host) {
        return function (callback) {
            return function () {
                return $foreign.createConnectionImpl(Data_Options.options(append(Data_Options.assoc(socketHost)(host))(Data_Options.assoc(socketPort)(port))), callback);
            };
        };
    };
};
var createConnectionICP = function (path) {
    return function (callback) {
        return function () {
            return $foreign.createConnectionImpl(Data_Options.options(Data_Options.assoc(socketPath)(path)), callback);
        };
    };
};
var createConnection = function (opts) {
    return function (callback) {
        return function () {
            return $foreign.createConnectionImpl(Data_Options.options(opts), callback);
        };
    };
};
var connecting = function (socket) {
    return function () {
        return $foreign.connectingImpl(socket);
    };
};
var connectPort = /* #__PURE__ */ Data_Options.opt("port");
var connectPath = /* #__PURE__ */ Data_Options.opt("path");
var connectLocalPort = /* #__PURE__ */ Data_Options.opt("localPort");
var connectLocalAddress = /* #__PURE__ */ Data_Options.opt("localAddress");
var connectICP = function (socket) {
    return function (path) {
        return function (callback) {
            return function () {
                return $foreign.connectImpl(socket, Data_Options.options(Data_Options.assoc(connectPath)(path)), callback);
            };
        };
    };
};
var connectHost = /* #__PURE__ */ Data_Options.opt("host");
var connectTCP = function (socket) {
    return function (port) {
        return function (host) {
            return function (callback) {
                return function () {
                    return $foreign.connectImpl(socket, Data_Options.options(append(Data_Options.assoc(connectHost)(host))(Data_Options.assoc(connectPort)(port))), callback);
                };
            };
        };
    };
};
var connectHints = /* #__PURE__ */ Data_Options.opt("hints");
var connectFamily = /* #__PURE__ */ Data_Options.opt("family");
var connect = function (socket) {
    return function (opts) {
        return function (callback) {
            return function () {
                return $foreign.connectImpl(socket, Data_Options.options(opts), callback);
            };
        };
    };
};
var bytesWritten = function (socket) {
    return function () {
        return $foreign.bytesWrittenImpl(socket);
    };
};
var bytesRead = function (socket) {
    return function () {
        return $foreign.bytesReadImpl(socket);
    };
};
var bufferSize = function (socket) {
    return function __do() {
        var size = $foreign.bufferSizeImpl(socket);
        return Data_Nullable.toMaybe(size);
    };
};
export {
    bufferSize,
    bytesRead,
    bytesWritten,
    connect,
    connectFamily,
    connectHints,
    connectHost,
    connectICP,
    connectLocalAddress,
    connectLocalPort,
    connectPath,
    connectPort,
    connectTCP,
    connecting,
    createConnection,
    createConnectionICP,
    createConnectionTCP,
    destroy,
    destroyed,
    end,
    endString,
    localAddress,
    localPort,
    onClose,
    onConnect,
    onData,
    onDrain,
    onEnd,
    onError,
    onLookup,
    onReady,
    onTimeout,
    pause,
    pending,
    remoteAddress,
    remoteFamily,
    remotePort,
    resume,
    setEncoding,
    setKeepAlive,
    setNoDelay,
    setTimeout,
    socketAllowHalfOpen,
    socketFd,
    socketHost,
    socketPath,
    socketPort,
    socketReadable,
    socketTimeout,
    socketWritable,
    write,
    writeString
};