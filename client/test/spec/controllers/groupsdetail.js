'use strict';

describe('Controller: GroupsdetailctrlCtrl', function () {

  // load the controller's module
  beforeEach(module('groupNoteApp'));

  var GroupsdetailctrlCtrl,
    scope;

  // Initialize the controller and a mock scope
  beforeEach(inject(function ($controller, $rootScope) {
    scope = $rootScope.$new();
    GroupsdetailctrlCtrl = $controller('GroupsdetailctrlCtrl', {
      $scope: scope
      // place here mocked dependencies
    });
  }));

  it('should attach a list of awesomeThings to the scope', function () {
    expect(GroupsdetailctrlCtrl.awesomeThings.length).toBe(3);
  });
});
