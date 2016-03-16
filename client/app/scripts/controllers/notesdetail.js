'use strict';
angular.module('groupNoteApp')
  .controller('NotesDetailCtrl', function ($scope, $stateParams) {
    var nid = $stateParams.noteId;
    $scope.note = {id: nid, title: 'detail', content: '# detail' + nid};
  });
