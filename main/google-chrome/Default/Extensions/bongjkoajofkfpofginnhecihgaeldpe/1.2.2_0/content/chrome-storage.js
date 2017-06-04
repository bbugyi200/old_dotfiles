/* Copyright (c) Appteligent, 2012 All Rights Reserved */
(function(pkg) {
    var proto = {
        /**
         * Initializes the storage with given storage type, which is one of:
         * "sync" or "local". Defaults to "local" if not specified.
         *
         * @param {String} [storageType="local"]
         * @param {Function} [callback]
         */
        init: function(storageType, callback) {
            this._storage = storageType && storageType === "sync"
                ? chrome.storage.sync : chrome.storage.local;

            call(callback, null);
        },

        /**
         * Retrieves item(s) from the storage by given key(s).
         * @param {String|Array} [key]
         * @param {Function} [callback]
         */
        get: function(key, callback) {
            this._storage.get(key, function(items) {
                if(chrome.runtime.lastError) {
                    call(callback, chrome.runtime.lastError.message);
                    return;
                }

                call(callback, null, items);
            });
        },

        /**
         * Saves given object in teh store under specified key.
         * @param {String} key
         * @param {Object} object
         * @param {Function} [callback]
         */
        set: function(key, object, callback) {
            var objToStore = {};
            if(!key || !object) {
                return;
            }

            objToStore[key] = object;
            this._storage.set(objToStore, function() {
                if(chrome.runtime.lastError) {
                    call(callback, chrome.runtime.lastError.message);
                    return;
                }

                call(callback, null);
            });
        },

        /**
         * Removes objects, indicated by the keys given from the store.
         * @param {String|Array} [key]
         * @param {Function} [callback]
         */
        remove: function(key, callback) {
            if(!key) {
                this._storage.clear();
                call(callback, null);
                return;
            }

            this._storage.remove(key, function() {
                if(chrome.runtime.lastError) {
                    call(callback, chrome.runtime.lastError.message);
                    return;
                }

                call(callback, null);
            });
        }
    };

    function call(cb) {
        if(typeof cb === "function") {
            cb.apply(null, Array.prototype.slice.call(arguments, 1));
        }
    }

    pkg.newChromeStorage = function(storageType, callback) {
        var storage = Object.create(proto);
        storage.init(storageType, callback);
        return storage;
    };
})(this.VR || (this.VR = {}));
