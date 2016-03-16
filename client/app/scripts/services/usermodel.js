'use strict';
angular.module('groupNoteApp')
  .service('UserModel', [
    '$q',
    '$http',
    '$state',
    'Restangular',
    'ApiConstant',
    function($q, $http, $state, Restangular, ApiConstant) {
      var self = this;
      //var _token = null;
      var _token = "dummy token";
      this.setToken = function(data) { _token = data; };
      this.getToken = function() { return _token; };
      this.isLoggedIn = function() { return !!_token; };
      this.acquireToken = function() {
        var defer = $q.defer();
        $http({method: 'POST', url: ApiConstant.contextPath + '/token'})
          .then(function(token) {
            self.setToken(token);
            $http.defaults.headers.common.Authorization = 'Bearer ' + token;
            defer.resolve(self.getToken());
          }, function(e) {
            defer.reject(e);
          });
        return defer.promise;
      };
      this.logout = function() { console.log("TODO"); };
    }
  ])
;
