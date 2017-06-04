
(function (globalContext) {
    var hasOwnProperty = function(object, property) {
      if (object) {
        return Object.prototype.hasOwnProperty.call(object, property) || object.hasOwnProperty(property);
      }
      return false;
    };
    var isGlobalProperty = function(property) {
      var value = globalContext[property];
      if (hasOwnProperty(globalContext, property)) {
          return !(value instanceof Element || value instanceof HTMLCollection) || Object.getOwnPropertyNames(globalContext).includes(property);
      } else {
        return (typeof(EventTarget) !== 'undefined' && hasOwnProperty(EventTarget.prototype, property)) ||
               (typeof(ContentScriptGlobalScope) !== 'undefined' && hasOwnProperty(ContentScriptGlobalScope.prototype, property));
      }
    };
    var proxiedFunctions = Object.create(null);
    var proxy = new Proxy(Object.create(null), {
        get: function (target, property, receiver) {
            var isProxiedFunction = Object.prototype.hasOwnProperty.call(proxiedFunctions, property);

            if (property === Symbol.unscopables || !(isGlobalProperty(property) || isProxiedFunction)) {
                return void 0;
            }

            var value = isProxiedFunction ? proxiedFunctions[property] : globalContext[property];

            if (!isProxiedFunction && typeof(value) === 'function' && /^[a-z]/.test(property)) {
                value = proxiedFunctions[property] = new Proxy(value, {
                    construct: function (target, argumentsList, newTarget) {
                        return Reflect.construct(target, argumentsList, newTarget);
                    },
                    apply: function (target, thisArg, argumentsList) {
                        return Reflect.apply(target, thisArg === proxy ? globalContext : thisArg, argumentsList);
                    }
                });
            }

            return value;
        },
        set: function (target, property, value) {
            globalContext[property] = value;
            delete proxiedFunctions[property];
        },
        has: function () {
            return true;
        }
    });
    with (proxy) {
var sendBackground=LPPlatform.requestFrameworkInitializer(function(n){window.postMessage(n,window.location.origin)});window.addEventListener("message",function(n){n.origin!==window.location.origin||!n.data||n.data.cmd&&"Provisioning"!==n.data.cmd[0]||sendBackground(n.data)});

    }
})(this);