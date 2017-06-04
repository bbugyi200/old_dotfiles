
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
var LPUtils;!function(t){function i(t,i){if(t&&i){Array.isArray(i)||(i=[i]);for(var n=0;n<i.length;++n)if(t.indexOf(i[n])>-1)return!0}return!1}t.stringContains=i}(LPUtils||(LPUtils={}));var lastpass;!function(t){var i=function(){function t(){var t=this;bg.basicAuth.hasFeature(function(i){i&&bg.basicAuth.isBasicAuth(function(i,n){return t.init(i,n)})})}return t.prototype.init=function(t,i){var n=this;t&&(this.backgroundInterface=bg,this.loadContent(function(){n.initContent(i)}))},t.prototype.loadContent=function(t){var i=this;$.get(chrome.runtime.getURL("/basicAuth/views/basicAuthFrame.html"),function(n){var o=i.replaceRelativePaths(n);$(document.body).append(o),t()})},t.prototype.initContent=function(t){$("#ba_username").focus(),t||$("#wronguserpass").hide(),document.title="Lastpass basic auth login",$("#title").text(location.host),this.subscribeEvents(),this.openPopupNotification(),this.backgroundInterface.sendLpImprove("basicauth::triggered")},t.prototype.subscribeEvents=function(){var t=this;$("#login").click(function(){$(".js-notification").toggle("notification-visible"),t.doLogin()}),$("#password").keypress(function(i){13==i.which&&t.doLogin()}),$("#cancel").click(function(){t.cancelBasicAuth(),t.backgroundInterface.sendLpImprove("basicauth::cancel")}),$(".notification__close__button").click(function(){$(".notification__body").removeClass("slide-in").addClass("slide-out"),t.backgroundInterface.basicAuth.closeNotification()})},t.prototype.doLogin=function(){var t=$("#ba_username").val(),i=$("#ba_password").val();this.backgroundInterface.sendLpImprove("basicauth::login"),this.backgroundInterface.basicAuth.setAuthCredential(t,i,function(){return location.reload()})},t.prototype.cancelBasicAuth=function(){this.backgroundInterface.basicAuth.cancelBasicAuth(),$("#login-body").hide(),location.reload()},t.prototype.replaceRelativePaths=function(t){var i=chrome.runtime.getURL("");return t.replace(new RegExp("{extensionUrl}","g"),i)},t.prototype.openPopupNotification=function(){this.backgroundInterface.basicAuth.isNotificationClosed(function(t){t?$(".notification__body").hide():$(".notification__body").addClass("slide-in")})},t}();t.BasicAuthContentScript=i}(lastpass||(lastpass={})),this.basicAuth=new lastpass.BasicAuthContentScript;

    }
})(this);