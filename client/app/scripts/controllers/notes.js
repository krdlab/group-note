'use strict';

angular.module('groupNoteApp')
  .controller('NotesCtrl', function ($scope) {
    $scope.notes = [
      {id: '1', title: 'ほげほげ', author: 'krdlab', content: '# Note 1'},
      {id: '2', title: 'aaa', author: 'krdlab', content: '# Note 2'}
    ];
  });
