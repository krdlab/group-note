'use strict';
angular.module('groupNoteApp')
  .controller('GroupsDetailCtrl', function ($scope, $stateParams) {
    var gid = $stateParams.groupId;
    $scope.group = {id: gid, name: 'test group'};
  })
;
