'use strict';

describe('Service: TeamModel', function () {

  // load the service's module
  beforeEach(module('groupNoteApp'));

  // instantiate service
  var TeamModel;
  beforeEach(inject(function (_TeamModel_) {
    TeamModel = _TeamModel_;
  }));

  it('should do something', function () {
    expect(!!TeamModel).toBe(true);
  });

});
