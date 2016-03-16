'use strict';
angular.module('groupNoteApp')
  .service('TeamModel', [
    '$q',
    '$http',
    '$state',
    'Restangular',
    'ApiConstant',
    function($q, $http, $state, Restangular, ApiConstant) {
      var Teams = Restangular.all(ApiConstant.teams);
      var self = this;
      this.listTeams = function() {
        return Teams.getList();
      };
    }
  ])
;
