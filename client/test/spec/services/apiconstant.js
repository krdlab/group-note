'use strict';

describe('Service: ApiConstant', function () {

  // load the service's module
  beforeEach(module('groupNoteApp'));

  // instantiate service
  var ApiConstant;
  beforeEach(inject(function (_ApiConstant_) {
    ApiConstant = _ApiConstant_;
  }));

  it('should do something', function () {
    expect(!!ApiConstant).toBe(true);
  });

});
