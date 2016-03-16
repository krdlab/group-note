'use strict';
angular.module('groupNoteApp')
  .controller('NotesNewCtrl', ['$scope', 'UserModel', function ($scope, UserModel) {
    $scope.content = "# Hello!\n:+1: :smile: :see_no_evil: :pray:\n![サンプル画像](http://f.st-hatena.com/images/fotolife/K/KrdLab/20110416/20110416174153_original.png)\n";
    $scope.saveNote = function() {
      // TODO: POST /services/group-note/notes
      //$scope.notes.push({id: ???, title: 'new!', author: 'krdlab', content: $scope.content});
      // current group id
      // UserModel.saveNote($scope.group_id);
    };
  }])
;
