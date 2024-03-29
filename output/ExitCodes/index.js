// Generated by purs version 0.15.10
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
var Success = /* #__PURE__ */ (function () {
    function Success() {

    };
    Success.value = new Success();
    return Success;
})();
var $$Error = /* #__PURE__ */ (function () {
    function $$Error() {

    };
    $$Error.value = new $$Error();
    return $$Error;
})();
var MisuseOfShellBuiltins = /* #__PURE__ */ (function () {
    function MisuseOfShellBuiltins() {

    };
    MisuseOfShellBuiltins.value = new MisuseOfShellBuiltins();
    return MisuseOfShellBuiltins;
})();
var CLIUsageError = /* #__PURE__ */ (function () {
    function CLIUsageError() {

    };
    CLIUsageError.value = new CLIUsageError();
    return CLIUsageError;
})();
var DataFormatError = /* #__PURE__ */ (function () {
    function DataFormatError() {

    };
    DataFormatError.value = new DataFormatError();
    return DataFormatError;
})();
var CannotOpenInput = /* #__PURE__ */ (function () {
    function CannotOpenInput() {

    };
    CannotOpenInput.value = new CannotOpenInput();
    return CannotOpenInput;
})();
var AddresseeUnknown = /* #__PURE__ */ (function () {
    function AddresseeUnknown() {

    };
    AddresseeUnknown.value = new AddresseeUnknown();
    return AddresseeUnknown;
})();
var HostNameUnknown = /* #__PURE__ */ (function () {
    function HostNameUnknown() {

    };
    HostNameUnknown.value = new HostNameUnknown();
    return HostNameUnknown;
})();
var ServiceUnavailable = /* #__PURE__ */ (function () {
    function ServiceUnavailable() {

    };
    ServiceUnavailable.value = new ServiceUnavailable();
    return ServiceUnavailable;
})();
var InternalSoftwareError = /* #__PURE__ */ (function () {
    function InternalSoftwareError() {

    };
    InternalSoftwareError.value = new InternalSoftwareError();
    return InternalSoftwareError;
})();
var SystemError = /* #__PURE__ */ (function () {
    function SystemError() {

    };
    SystemError.value = new SystemError();
    return SystemError;
})();
var CriticalOSFileMissing = /* #__PURE__ */ (function () {
    function CriticalOSFileMissing() {

    };
    CriticalOSFileMissing.value = new CriticalOSFileMissing();
    return CriticalOSFileMissing;
})();
var CannotCreateOutputFile = /* #__PURE__ */ (function () {
    function CannotCreateOutputFile() {

    };
    CannotCreateOutputFile.value = new CannotCreateOutputFile();
    return CannotCreateOutputFile;
})();
var IOError = /* #__PURE__ */ (function () {
    function IOError() {

    };
    IOError.value = new IOError();
    return IOError;
})();
var TemporaryFailure = /* #__PURE__ */ (function () {
    function TemporaryFailure() {

    };
    TemporaryFailure.value = new TemporaryFailure();
    return TemporaryFailure;
})();
var RemoteError = /* #__PURE__ */ (function () {
    function RemoteError() {

    };
    RemoteError.value = new RemoteError();
    return RemoteError;
})();
var PermissionDenied = /* #__PURE__ */ (function () {
    function PermissionDenied() {

    };
    PermissionDenied.value = new PermissionDenied();
    return PermissionDenied;
})();
var ConfigurationError = /* #__PURE__ */ (function () {
    function ConfigurationError() {

    };
    ConfigurationError.value = new ConfigurationError();
    return ConfigurationError;
})();
var CannotExecute = /* #__PURE__ */ (function () {
    function CannotExecute() {

    };
    CannotExecute.value = new CannotExecute();
    return CannotExecute;
})();
var CommandNotFound = /* #__PURE__ */ (function () {
    function CommandNotFound() {

    };
    CommandNotFound.value = new CommandNotFound();
    return CommandNotFound;
})();
var InvalidExitArgument = /* #__PURE__ */ (function () {
    function InvalidExitArgument() {

    };
    InvalidExitArgument.value = new InvalidExitArgument();
    return InvalidExitArgument;
})();
var SIGHUP = /* #__PURE__ */ (function () {
    function SIGHUP() {

    };
    SIGHUP.value = new SIGHUP();
    return SIGHUP;
})();
var SIGINT = /* #__PURE__ */ (function () {
    function SIGINT() {

    };
    SIGINT.value = new SIGINT();
    return SIGINT;
})();
var SIGQUIT = /* #__PURE__ */ (function () {
    function SIGQUIT() {

    };
    SIGQUIT.value = new SIGQUIT();
    return SIGQUIT;
})();
var SIGILL = /* #__PURE__ */ (function () {
    function SIGILL() {

    };
    SIGILL.value = new SIGILL();
    return SIGILL;
})();
var SIGABRT = /* #__PURE__ */ (function () {
    function SIGABRT() {

    };
    SIGABRT.value = new SIGABRT();
    return SIGABRT;
})();
var SIGFPE = /* #__PURE__ */ (function () {
    function SIGFPE() {

    };
    SIGFPE.value = new SIGFPE();
    return SIGFPE;
})();
var SIGKILL = /* #__PURE__ */ (function () {
    function SIGKILL() {

    };
    SIGKILL.value = new SIGKILL();
    return SIGKILL;
})();
var SIGSEGV = /* #__PURE__ */ (function () {
    function SIGSEGV() {

    };
    SIGSEGV.value = new SIGSEGV();
    return SIGSEGV;
})();
var SIGPIPE = /* #__PURE__ */ (function () {
    function SIGPIPE() {

    };
    SIGPIPE.value = new SIGPIPE();
    return SIGPIPE;
})();
var SIGALRM = /* #__PURE__ */ (function () {
    function SIGALRM() {

    };
    SIGALRM.value = new SIGALRM();
    return SIGALRM;
})();
var SIGTERM = /* #__PURE__ */ (function () {
    function SIGTERM() {

    };
    SIGTERM.value = new SIGTERM();
    return SIGTERM;
})();
var showExitCode = {
    show: function (v) {
        if (v instanceof Success) {
            return "Success";
        };
        if (v instanceof $$Error) {
            return "Error";
        };
        if (v instanceof MisuseOfShellBuiltins) {
            return "MisuseOfShellBuiltins";
        };
        if (v instanceof CLIUsageError) {
            return "CLIUsageError";
        };
        if (v instanceof DataFormatError) {
            return "DataFormatError";
        };
        if (v instanceof CannotOpenInput) {
            return "CannotOpenInput";
        };
        if (v instanceof AddresseeUnknown) {
            return "AddresseeUnknown";
        };
        if (v instanceof HostNameUnknown) {
            return "HostNameUnknown";
        };
        if (v instanceof ServiceUnavailable) {
            return "ServiceUnavailable";
        };
        if (v instanceof InternalSoftwareError) {
            return "InternalSoftwareError";
        };
        if (v instanceof SystemError) {
            return "SystemError";
        };
        if (v instanceof CriticalOSFileMissing) {
            return "CriticalOSFileMissing";
        };
        if (v instanceof CannotCreateOutputFile) {
            return "CannotCreateOutputFile";
        };
        if (v instanceof IOError) {
            return "IOError";
        };
        if (v instanceof TemporaryFailure) {
            return "TemporaryFailure";
        };
        if (v instanceof RemoteError) {
            return "RemoteError";
        };
        if (v instanceof PermissionDenied) {
            return "PermissionDenied";
        };
        if (v instanceof ConfigurationError) {
            return "ConfigurationError";
        };
        if (v instanceof CannotExecute) {
            return "CannotExecute";
        };
        if (v instanceof CommandNotFound) {
            return "CommandNotFound";
        };
        if (v instanceof InvalidExitArgument) {
            return "InvalidExitArgument";
        };
        if (v instanceof SIGHUP) {
            return "SIGHUP";
        };
        if (v instanceof SIGINT) {
            return "SIGINT";
        };
        if (v instanceof SIGQUIT) {
            return "SIGQUIT";
        };
        if (v instanceof SIGILL) {
            return "SIGILL";
        };
        if (v instanceof SIGABRT) {
            return "SIGABRT";
        };
        if (v instanceof SIGFPE) {
            return "SIGFPE";
        };
        if (v instanceof SIGKILL) {
            return "SIGKILL";
        };
        if (v instanceof SIGSEGV) {
            return "SIGSEGV";
        };
        if (v instanceof SIGPIPE) {
            return "SIGPIPE";
        };
        if (v instanceof SIGALRM) {
            return "SIGALRM";
        };
        if (v instanceof SIGTERM) {
            return "SIGTERM";
        };
        throw new Error("Failed pattern match at ExitCodes (line 49, column 10 - line 81, column 25): " + [ v.constructor.name ]);
    }
};
var eqExitCode = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Success && y instanceof Success) {
                return true;
            };
            if (x instanceof $$Error && y instanceof $$Error) {
                return true;
            };
            if (x instanceof MisuseOfShellBuiltins && y instanceof MisuseOfShellBuiltins) {
                return true;
            };
            if (x instanceof CLIUsageError && y instanceof CLIUsageError) {
                return true;
            };
            if (x instanceof DataFormatError && y instanceof DataFormatError) {
                return true;
            };
            if (x instanceof CannotOpenInput && y instanceof CannotOpenInput) {
                return true;
            };
            if (x instanceof AddresseeUnknown && y instanceof AddresseeUnknown) {
                return true;
            };
            if (x instanceof HostNameUnknown && y instanceof HostNameUnknown) {
                return true;
            };
            if (x instanceof ServiceUnavailable && y instanceof ServiceUnavailable) {
                return true;
            };
            if (x instanceof InternalSoftwareError && y instanceof InternalSoftwareError) {
                return true;
            };
            if (x instanceof SystemError && y instanceof SystemError) {
                return true;
            };
            if (x instanceof CriticalOSFileMissing && y instanceof CriticalOSFileMissing) {
                return true;
            };
            if (x instanceof CannotCreateOutputFile && y instanceof CannotCreateOutputFile) {
                return true;
            };
            if (x instanceof IOError && y instanceof IOError) {
                return true;
            };
            if (x instanceof TemporaryFailure && y instanceof TemporaryFailure) {
                return true;
            };
            if (x instanceof RemoteError && y instanceof RemoteError) {
                return true;
            };
            if (x instanceof PermissionDenied && y instanceof PermissionDenied) {
                return true;
            };
            if (x instanceof ConfigurationError && y instanceof ConfigurationError) {
                return true;
            };
            if (x instanceof CannotExecute && y instanceof CannotExecute) {
                return true;
            };
            if (x instanceof CommandNotFound && y instanceof CommandNotFound) {
                return true;
            };
            if (x instanceof InvalidExitArgument && y instanceof InvalidExitArgument) {
                return true;
            };
            if (x instanceof SIGHUP && y instanceof SIGHUP) {
                return true;
            };
            if (x instanceof SIGINT && y instanceof SIGINT) {
                return true;
            };
            if (x instanceof SIGQUIT && y instanceof SIGQUIT) {
                return true;
            };
            if (x instanceof SIGILL && y instanceof SIGILL) {
                return true;
            };
            if (x instanceof SIGABRT && y instanceof SIGABRT) {
                return true;
            };
            if (x instanceof SIGFPE && y instanceof SIGFPE) {
                return true;
            };
            if (x instanceof SIGKILL && y instanceof SIGKILL) {
                return true;
            };
            if (x instanceof SIGSEGV && y instanceof SIGSEGV) {
                return true;
            };
            if (x instanceof SIGPIPE && y instanceof SIGPIPE) {
                return true;
            };
            if (x instanceof SIGALRM && y instanceof SIGALRM) {
                return true;
            };
            if (x instanceof SIGTERM && y instanceof SIGTERM) {
                return true;
            };
            return false;
        };
    }
};
var ordExitCode = {
    compare: function (x) {
        return function (y) {
            if (x instanceof Success && y instanceof Success) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Success) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Success) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof $$Error && y instanceof $$Error) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof $$Error) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof $$Error) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof MisuseOfShellBuiltins && y instanceof MisuseOfShellBuiltins) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof MisuseOfShellBuiltins) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof MisuseOfShellBuiltins) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof CLIUsageError && y instanceof CLIUsageError) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof CLIUsageError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof CLIUsageError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof DataFormatError && y instanceof DataFormatError) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof DataFormatError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof DataFormatError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof CannotOpenInput && y instanceof CannotOpenInput) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof CannotOpenInput) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof CannotOpenInput) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof AddresseeUnknown && y instanceof AddresseeUnknown) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof AddresseeUnknown) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof AddresseeUnknown) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof HostNameUnknown && y instanceof HostNameUnknown) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof HostNameUnknown) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof HostNameUnknown) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ServiceUnavailable && y instanceof ServiceUnavailable) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof ServiceUnavailable) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ServiceUnavailable) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof InternalSoftwareError && y instanceof InternalSoftwareError) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof InternalSoftwareError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof InternalSoftwareError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SystemError && y instanceof SystemError) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SystemError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SystemError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof CriticalOSFileMissing && y instanceof CriticalOSFileMissing) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof CriticalOSFileMissing) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof CriticalOSFileMissing) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof CannotCreateOutputFile && y instanceof CannotCreateOutputFile) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof CannotCreateOutputFile) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof CannotCreateOutputFile) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof IOError && y instanceof IOError) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof IOError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof IOError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof TemporaryFailure && y instanceof TemporaryFailure) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof TemporaryFailure) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof TemporaryFailure) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof RemoteError && y instanceof RemoteError) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof RemoteError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof RemoteError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof PermissionDenied && y instanceof PermissionDenied) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof PermissionDenied) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof PermissionDenied) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ConfigurationError && y instanceof ConfigurationError) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof ConfigurationError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ConfigurationError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof CannotExecute && y instanceof CannotExecute) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof CannotExecute) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof CannotExecute) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof CommandNotFound && y instanceof CommandNotFound) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof CommandNotFound) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof CommandNotFound) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof InvalidExitArgument && y instanceof InvalidExitArgument) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof InvalidExitArgument) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof InvalidExitArgument) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGHUP && y instanceof SIGHUP) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGHUP) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGHUP) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGINT && y instanceof SIGINT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGINT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGINT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGQUIT && y instanceof SIGQUIT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGQUIT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGQUIT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGILL && y instanceof SIGILL) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGILL) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGILL) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGABRT && y instanceof SIGABRT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGABRT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGABRT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGFPE && y instanceof SIGFPE) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGFPE) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGFPE) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGKILL && y instanceof SIGKILL) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGKILL) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGKILL) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGSEGV && y instanceof SIGSEGV) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGSEGV) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGSEGV) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGPIPE && y instanceof SIGPIPE) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGPIPE) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGPIPE) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGALRM && y instanceof SIGALRM) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGALRM) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGALRM) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGTERM && y instanceof SIGTERM) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at ExitCodes (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqExitCode;
    }
};
var enumExitCode = {
    succ: function (v) {
        if (v instanceof Success) {
            return new Data_Maybe.Just($$Error.value);
        };
        if (v instanceof $$Error) {
            return new Data_Maybe.Just(MisuseOfShellBuiltins.value);
        };
        if (v instanceof MisuseOfShellBuiltins) {
            return new Data_Maybe.Just(CLIUsageError.value);
        };
        if (v instanceof CLIUsageError) {
            return new Data_Maybe.Just(DataFormatError.value);
        };
        if (v instanceof DataFormatError) {
            return new Data_Maybe.Just(CannotOpenInput.value);
        };
        if (v instanceof CannotOpenInput) {
            return new Data_Maybe.Just(AddresseeUnknown.value);
        };
        if (v instanceof AddresseeUnknown) {
            return new Data_Maybe.Just(HostNameUnknown.value);
        };
        if (v instanceof HostNameUnknown) {
            return new Data_Maybe.Just(ServiceUnavailable.value);
        };
        if (v instanceof ServiceUnavailable) {
            return new Data_Maybe.Just(InternalSoftwareError.value);
        };
        if (v instanceof InternalSoftwareError) {
            return new Data_Maybe.Just(SystemError.value);
        };
        if (v instanceof SystemError) {
            return new Data_Maybe.Just(CriticalOSFileMissing.value);
        };
        if (v instanceof CriticalOSFileMissing) {
            return new Data_Maybe.Just(CannotCreateOutputFile.value);
        };
        if (v instanceof CannotCreateOutputFile) {
            return new Data_Maybe.Just(IOError.value);
        };
        if (v instanceof IOError) {
            return new Data_Maybe.Just(TemporaryFailure.value);
        };
        if (v instanceof TemporaryFailure) {
            return new Data_Maybe.Just(RemoteError.value);
        };
        if (v instanceof RemoteError) {
            return new Data_Maybe.Just(PermissionDenied.value);
        };
        if (v instanceof PermissionDenied) {
            return new Data_Maybe.Just(ConfigurationError.value);
        };
        if (v instanceof ConfigurationError) {
            return new Data_Maybe.Just(CannotExecute.value);
        };
        if (v instanceof CannotExecute) {
            return new Data_Maybe.Just(CommandNotFound.value);
        };
        if (v instanceof CommandNotFound) {
            return new Data_Maybe.Just(InvalidExitArgument.value);
        };
        if (v instanceof InvalidExitArgument) {
            return new Data_Maybe.Just(SIGHUP.value);
        };
        if (v instanceof SIGHUP) {
            return new Data_Maybe.Just(SIGINT.value);
        };
        if (v instanceof SIGINT) {
            return new Data_Maybe.Just(SIGQUIT.value);
        };
        if (v instanceof SIGQUIT) {
            return new Data_Maybe.Just(SIGILL.value);
        };
        if (v instanceof SIGILL) {
            return new Data_Maybe.Just(SIGABRT.value);
        };
        if (v instanceof SIGABRT) {
            return new Data_Maybe.Just(SIGFPE.value);
        };
        if (v instanceof SIGFPE) {
            return new Data_Maybe.Just(SIGKILL.value);
        };
        if (v instanceof SIGKILL) {
            return new Data_Maybe.Just(SIGSEGV.value);
        };
        if (v instanceof SIGSEGV) {
            return new Data_Maybe.Just(SIGPIPE.value);
        };
        if (v instanceof SIGPIPE) {
            return new Data_Maybe.Just(SIGALRM.value);
        };
        if (v instanceof SIGALRM) {
            return new Data_Maybe.Just(SIGTERM.value);
        };
        if (v instanceof SIGTERM) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at ExitCodes (line 87, column 1 - line 151, column 30): " + [ v.constructor.name ]);
    },
    pred: function (v) {
        if (v instanceof Success) {
            return Data_Maybe.Nothing.value;
        };
        if (v instanceof $$Error) {
            return new Data_Maybe.Just(Success.value);
        };
        if (v instanceof MisuseOfShellBuiltins) {
            return new Data_Maybe.Just($$Error.value);
        };
        if (v instanceof CLIUsageError) {
            return new Data_Maybe.Just(MisuseOfShellBuiltins.value);
        };
        if (v instanceof DataFormatError) {
            return new Data_Maybe.Just(CLIUsageError.value);
        };
        if (v instanceof CannotOpenInput) {
            return new Data_Maybe.Just(DataFormatError.value);
        };
        if (v instanceof AddresseeUnknown) {
            return new Data_Maybe.Just(CannotOpenInput.value);
        };
        if (v instanceof HostNameUnknown) {
            return new Data_Maybe.Just(AddresseeUnknown.value);
        };
        if (v instanceof ServiceUnavailable) {
            return new Data_Maybe.Just(HostNameUnknown.value);
        };
        if (v instanceof InternalSoftwareError) {
            return new Data_Maybe.Just(ServiceUnavailable.value);
        };
        if (v instanceof SystemError) {
            return new Data_Maybe.Just(InternalSoftwareError.value);
        };
        if (v instanceof CriticalOSFileMissing) {
            return new Data_Maybe.Just(SystemError.value);
        };
        if (v instanceof CannotCreateOutputFile) {
            return new Data_Maybe.Just(CriticalOSFileMissing.value);
        };
        if (v instanceof IOError) {
            return new Data_Maybe.Just(CannotCreateOutputFile.value);
        };
        if (v instanceof TemporaryFailure) {
            return new Data_Maybe.Just(IOError.value);
        };
        if (v instanceof RemoteError) {
            return new Data_Maybe.Just(TemporaryFailure.value);
        };
        if (v instanceof PermissionDenied) {
            return new Data_Maybe.Just(RemoteError.value);
        };
        if (v instanceof ConfigurationError) {
            return new Data_Maybe.Just(PermissionDenied.value);
        };
        if (v instanceof CannotExecute) {
            return new Data_Maybe.Just(ConfigurationError.value);
        };
        if (v instanceof CommandNotFound) {
            return new Data_Maybe.Just(CannotExecute.value);
        };
        if (v instanceof InvalidExitArgument) {
            return new Data_Maybe.Just(CommandNotFound.value);
        };
        if (v instanceof SIGHUP) {
            return new Data_Maybe.Just(InvalidExitArgument.value);
        };
        if (v instanceof SIGINT) {
            return new Data_Maybe.Just(SIGHUP.value);
        };
        if (v instanceof SIGQUIT) {
            return new Data_Maybe.Just(SIGINT.value);
        };
        if (v instanceof SIGILL) {
            return new Data_Maybe.Just(SIGQUIT.value);
        };
        if (v instanceof SIGABRT) {
            return new Data_Maybe.Just(SIGILL.value);
        };
        if (v instanceof SIGFPE) {
            return new Data_Maybe.Just(SIGABRT.value);
        };
        if (v instanceof SIGKILL) {
            return new Data_Maybe.Just(SIGFPE.value);
        };
        if (v instanceof SIGSEGV) {
            return new Data_Maybe.Just(SIGKILL.value);
        };
        if (v instanceof SIGPIPE) {
            return new Data_Maybe.Just(SIGSEGV.value);
        };
        if (v instanceof SIGALRM) {
            return new Data_Maybe.Just(SIGPIPE.value);
        };
        if (v instanceof SIGTERM) {
            return new Data_Maybe.Just(SIGALRM.value);
        };
        throw new Error("Failed pattern match at ExitCodes (line 87, column 1 - line 151, column 30): " + [ v.constructor.name ]);
    },
    Ord0: function () {
        return ordExitCode;
    }
};
var boundedExitCode = /* #__PURE__ */ (function () {
    return {
        bottom: Success.value,
        top: SIGTERM.value,
        Ord0: function () {
            return ordExitCode;
        }
    };
})();
var boundedEnumExitCode = {
    cardinality: 32,
    toEnum: function (v) {
        if (v === 0) {
            return new Data_Maybe.Just(Success.value);
        };
        if (v === 1) {
            return new Data_Maybe.Just($$Error.value);
        };
        if (v === 2) {
            return new Data_Maybe.Just(MisuseOfShellBuiltins.value);
        };
        if (v === 64) {
            return new Data_Maybe.Just(CLIUsageError.value);
        };
        if (v === 65) {
            return new Data_Maybe.Just(DataFormatError.value);
        };
        if (v === 66) {
            return new Data_Maybe.Just(CannotOpenInput.value);
        };
        if (v === 67) {
            return new Data_Maybe.Just(AddresseeUnknown.value);
        };
        if (v === 68) {
            return new Data_Maybe.Just(HostNameUnknown.value);
        };
        if (v === 69) {
            return new Data_Maybe.Just(ServiceUnavailable.value);
        };
        if (v === 70) {
            return new Data_Maybe.Just(InternalSoftwareError.value);
        };
        if (v === 71) {
            return new Data_Maybe.Just(SystemError.value);
        };
        if (v === 72) {
            return new Data_Maybe.Just(CriticalOSFileMissing.value);
        };
        if (v === 73) {
            return new Data_Maybe.Just(CannotCreateOutputFile.value);
        };
        if (v === 74) {
            return new Data_Maybe.Just(IOError.value);
        };
        if (v === 75) {
            return new Data_Maybe.Just(TemporaryFailure.value);
        };
        if (v === 76) {
            return new Data_Maybe.Just(RemoteError.value);
        };
        if (v === 77) {
            return new Data_Maybe.Just(PermissionDenied.value);
        };
        if (v === 78) {
            return new Data_Maybe.Just(ConfigurationError.value);
        };
        if (v === 126) {
            return new Data_Maybe.Just(CannotExecute.value);
        };
        if (v === 127) {
            return new Data_Maybe.Just(CommandNotFound.value);
        };
        if (v === 128) {
            return new Data_Maybe.Just(InvalidExitArgument.value);
        };
        if (v === 129) {
            return new Data_Maybe.Just(SIGHUP.value);
        };
        if (v === 130) {
            return new Data_Maybe.Just(SIGINT.value);
        };
        if (v === 131) {
            return new Data_Maybe.Just(SIGQUIT.value);
        };
        if (v === 132) {
            return new Data_Maybe.Just(SIGILL.value);
        };
        if (v === 134) {
            return new Data_Maybe.Just(SIGABRT.value);
        };
        if (v === 136) {
            return new Data_Maybe.Just(SIGFPE.value);
        };
        if (v === 137) {
            return new Data_Maybe.Just(SIGKILL.value);
        };
        if (v === 139) {
            return new Data_Maybe.Just(SIGSEGV.value);
        };
        if (v === 141) {
            return new Data_Maybe.Just(SIGPIPE.value);
        };
        if (v === 142) {
            return new Data_Maybe.Just(SIGALRM.value);
        };
        if (v === 143) {
            return new Data_Maybe.Just(SIGTERM.value);
        };
        return Data_Maybe.Nothing.value;
    },
    fromEnum: function (v) {
        if (v instanceof Success) {
            return 0;
        };
        if (v instanceof $$Error) {
            return 1;
        };
        if (v instanceof MisuseOfShellBuiltins) {
            return 2;
        };
        if (v instanceof CLIUsageError) {
            return 64;
        };
        if (v instanceof DataFormatError) {
            return 65;
        };
        if (v instanceof CannotOpenInput) {
            return 66;
        };
        if (v instanceof AddresseeUnknown) {
            return 67;
        };
        if (v instanceof HostNameUnknown) {
            return 68;
        };
        if (v instanceof ServiceUnavailable) {
            return 69;
        };
        if (v instanceof InternalSoftwareError) {
            return 70;
        };
        if (v instanceof SystemError) {
            return 71;
        };
        if (v instanceof CriticalOSFileMissing) {
            return 72;
        };
        if (v instanceof CannotCreateOutputFile) {
            return 73;
        };
        if (v instanceof IOError) {
            return 74;
        };
        if (v instanceof TemporaryFailure) {
            return 75;
        };
        if (v instanceof RemoteError) {
            return 76;
        };
        if (v instanceof PermissionDenied) {
            return 77;
        };
        if (v instanceof ConfigurationError) {
            return 78;
        };
        if (v instanceof CannotExecute) {
            return 126;
        };
        if (v instanceof CommandNotFound) {
            return 127;
        };
        if (v instanceof InvalidExitArgument) {
            return 128;
        };
        if (v instanceof SIGHUP) {
            return 128 + 1 | 0;
        };
        if (v instanceof SIGINT) {
            return 128 + 2 | 0;
        };
        if (v instanceof SIGQUIT) {
            return 128 + 3 | 0;
        };
        if (v instanceof SIGILL) {
            return 128 + 4 | 0;
        };
        if (v instanceof SIGABRT) {
            return 128 + 6 | 0;
        };
        if (v instanceof SIGFPE) {
            return 128 + 8 | 0;
        };
        if (v instanceof SIGKILL) {
            return 128 + 9 | 0;
        };
        if (v instanceof SIGSEGV) {
            return 128 + 11 | 0;
        };
        if (v instanceof SIGPIPE) {
            return 128 + 13 | 0;
        };
        if (v instanceof SIGALRM) {
            return 128 + 14 | 0;
        };
        if (v instanceof SIGTERM) {
            return 128 + 15 | 0;
        };
        throw new Error("Failed pattern match at ExitCodes (line 153, column 1 - line 219, column 30): " + [ v.constructor.name ]);
    },
    Bounded0: function () {
        return boundedExitCode;
    },
    Enum1: function () {
        return enumExitCode;
    }
};
export {
    Success,
    $$Error as Error,
    MisuseOfShellBuiltins,
    CLIUsageError,
    DataFormatError,
    CannotOpenInput,
    AddresseeUnknown,
    HostNameUnknown,
    ServiceUnavailable,
    InternalSoftwareError,
    SystemError,
    CriticalOSFileMissing,
    CannotCreateOutputFile,
    IOError,
    TemporaryFailure,
    RemoteError,
    PermissionDenied,
    ConfigurationError,
    CannotExecute,
    CommandNotFound,
    InvalidExitArgument,
    SIGHUP,
    SIGINT,
    SIGQUIT,
    SIGILL,
    SIGABRT,
    SIGFPE,
    SIGKILL,
    SIGSEGV,
    SIGPIPE,
    SIGALRM,
    SIGTERM,
    eqExitCode,
    ordExitCode,
    showExitCode,
    boundedExitCode,
    enumExitCode,
    boundedEnumExitCode
};
